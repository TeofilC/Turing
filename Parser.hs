module Parser where
-- import Data.Text
import Data.Char
import Data.Label
import qualified Data.Map as M
import Text.Parsec
import Macro
import Machine hiding (states)

spaces1 = skipMany1 space
inBrackets = between (string "(") (string ")")


pMConfigName, pSymbol :: Parsec String u String
pMConfigName = (:) <$> upper <*> many alphaNum
pSymbol = (:) <$> (lower <|> digit) <*> many alphaNum

pSymbolExpr :: Parsec String u ASym
pSymbolExpr =
  fmap Sym pSymbol
  <|> try (string "AnyAndNone" >> return AnyAndNone)
  <|> (string "Any" >> return Any)
  <|> try (string "Not" >> spaces1 >> fmap Not pSymbol)
  <|> (string "None" >> return None)
  <|> (string "Read" >> spaces1 >> fmap Read pSymbol)



pState :: Parsec String u ASt
pState = do
  name <- pMConfigName
  (Func name <$> args) <|> return (Name name)
  where args = inBrackets (sepBy (Sy <$> many1 (lower<|> digit) <|> (St <$> pState)) (spaces >> string "," >> spaces))

pDir :: Parsec String u Dir
pDir =  (string "L" >> return L)
    <|> (string "R" >> return R)
    <|> (string "N" >> return N)

pAction :: Parsec String u (Act)
pAction =  (string "P" >> spaces1 >> Print <$> pSymbol)
       <|> (string "E" >> return (Print " "))
       <|> (Move <$> pDir)

pActions = sepBy1 pAction  (try (spaces >> string "," >> spaces))

pClause :: Parsec String u (ASym, [Act], ASt)
pClause = try $ do
           sym <- pSymbolExpr
           spaces1
           act <- pActions
           spaces
           string ";"
           spaces
           st  <- pState
           return (sym,act,st)

pClauses :: Parsec String u [(ASym, [Act], ASt)]
pClauses = sepEndBy1 pClause spaces -- (string "\r\n" <|> string "\n")

pRule :: Parsec String u (ASt, [(ASym, [Act], ASt)])
pRule = do
           st <- pState
           spaces >> oneOf ":" >> spaces
           clauses <- pClauses
           return (st,clauses)

rulesToAbbrv :: [(ASt, [(ASym, [Act], ASt)])] -> (Env, Status, [ARow])
rulesToAbbrv = foldl f i
  where
    i = (env M.empty (M.fromList [(" ", 0), ("0", 1), ("1", 2)]), emptyStatus, [])
    f (a, status, tt) (Name n, clauses) = (a, modify states (insSt (Name n)) status, (map (\(a,b,c) -> ((Name n, a), (b,c))) clauses) ++ tt)
    f (a, status, tt) (Func n args, clauses) = (modify macroMap (M.insert (n,map SType args) (args,clauses)) a, status, tt)
    ins v m = if M.member v m then m else M.insert v (M.size m)  m
    insSt v m = M.insert v (M.size m) m

-- scanSymbols adds all symbols to the symbolTable
scanSymbols :: (Env,Status, [ARow]) -> (Env,Status, [ARow])
scanSymbols (a,status, tt) = (foldl g a tt, status, tt)
  where
    g a ((_,Sym sym1), (acts, _)) = foldl h (ins sym1 a) acts
    g a ((_,_), (acts, _)) = foldl h a acts
    h a (Print sym) = ins sym a
    h a _ = a
    ins s a = case M.lookup s (get symbolMap a) of
                Just _ -> a
                Nothing -> modify symbolMap (M.insert s (M.size (get symbolMap a))) a

parser :: Parsec String () (Env, Status, [ARow])
parser = scanSymbols . rulesToAbbrv <$> many1 pRule

parse' :: Parsec String () a  -> String -> Either ParseError a
parse' p = parse (p <* (spaces >> eof)) ""

parseAbbrv :: String -> Either ParseError (Env, Status, [ARow])
parseAbbrv = parse' parser

parseSymbolExpr = parse' pSymbolExpr
parseState = parse' pState
parseActions = parse' pActions
