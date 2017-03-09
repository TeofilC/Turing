module Parser where
-- import Data.Text
import Data.Char
import qualified Data.Map as M
import Text.Parsec
import Macro
import Machine


pSymbol :: Parsec String u ASym
pSymbol = fmap Sym (many1 (lower <|> digit))
        <|> try (string "AnyAndNone" >> return AnyAndNone)
        <|> (string "Any" >> return Any)
        <|> try (string "Not" >> spaces >> fmap Not (many1 (lower<|>digit)))
        <|> (string "None" >> return None)
        <|> (string "Read" >> spaces >> fmap Read (many1 (lower <|> digit)))



pState :: Parsec String u ASt
pState = try (do
          name <- many1 (upper <|> digit)
          args <- between (string "(" ) (string ")") (sepBy ((Sy <$> many1 (lower<|> digit) <|> (St <$> pState))) (spaces >> string "," >> spaces))
          return (Func name args))
         <|> Name <$> many1 (upper <|> digit)

pDir :: Parsec String u Dir
pDir =  (string "L" >> return L)
    <|> (string "R" >> return R)
    <|> (string "N" >> return N)

pAction :: Parsec String u (Act)
pAction = (string "P" >> spaces >> Print <$> many1 (lower <|> digit)) <|> (string "E" >> return (Print " ")) <|> (Move <$> pDir)

pClause :: Parsec String u (ASym, [Act], ASt)
pClause = try $ do
           sym <- pSymbol
           spaces
           act <- sepBy1 pAction  (try (spaces >> string "," >> spaces))
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
           return (st, clauses)

rulesToAbbrv :: [(ASt, [(ASym, [Act], ASt)])] -> (Abbrv, M.Map ASt [(ASym, [Act], ASt)])
rulesToAbbrv = foldl f i
  where
    i = (Abbrv (M.fromList [(" ", 0), ("0", 1), ("1", 2), ("e", 3)]) (M.fromList [(Name "B", 0)]) M.empty M.empty, M.empty)
    f (a, tt) (Name n, clauses) = (a {aStateTable = ins (Name n) (aStateTable a)}, M.insert (Name n) clauses tt)
    f (a, tt) (Func n args, clauses) = (a {aMacroTable = M.insert (n,map SType args) (args,clauses) (aMacroTable a)}, tt)
    ins v m = if M.member v m then m else M.insert v (M.size m)  m

-- scanSymbols adds all symbols to the symbolTable
scanSymbols :: (Abbrv, M.Map ASt [(ASym, [Act], ASt)]) -> (Abbrv, M.Map ASt [(ASym, [Act], ASt)])
scanSymbols (a, tt) = (foldl f a (M.toList tt), tt)
  where
    f a ((Name n), clauses) = foldl g a clauses
    g a (Sym sym1, acts, _) = foldl h (ins sym1 a) acts
    g a (_ , acts, _) = foldl h a acts
    h a (Print sym) = ins sym a
    h a _ = a
    ins s a = case M.lookup s (aSymbolTable a) of
                Just _ -> a
                Nothing -> a {aSymbolTable = M.insert s (M.size (aSymbolTable a)) (aSymbolTable a)}

parser :: Parsec String u (Abbrv, M.Map ASt [(ASym, [Act], ASt)])
parser = scanSymbols . rulesToAbbrv <$> many1 pRule



parseAbbrv :: String -> Either ParseError (Abbrv, M.Map ASt [(ASym, [Act], ASt)])
parseAbbrv = parse parser ""

