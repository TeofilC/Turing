{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Macro where
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Machine

import Control.Monad.State
import Data.List
import Data.Maybe

data ASym   = Sym String | Not String | Read String | Any | None
              deriving (Eq, Ord, Show)
data AState a = Func a [Type] | Name a
              deriving (Eq, Ord, Functor)

type ASt = AState String

data Act    = Print String | Move Dir
              deriving Show

instance Show (AState String) where
  show (Func n args) = n ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (Name n) = n

data Abbrv = Abbrv { aSymbolTable     :: M.Map String Int
                   , aStateTable      :: M.Map ASt Int
                   , aMacroTable      :: M.Map (String,[ShallowType]) ([Type], [(ASym, [Act] ,ASt)])
                   , result           :: M.Map (St, Symbol) (Action Symbol, St)}
             deriving Show

data TransitionTable = TransTable { symbolTable     :: IM.IntMap String
                                  , stateTable      :: IM.IntMap ASt
                                  , transTable      :: M.Map (St, Symbol) (Action Symbol, St)}
                       deriving Show

data Type = Sy String | St ASt
            deriving (Eq, Ord, Show)

newtype ShallowType = SType {unShallowType :: Type}

instance Eq ShallowType where
  (SType(Sy _)) == (SType (Sy _)) = True
  (SType (St _)) == (SType (St _)) = True
  _ == _ = False

instance Ord ShallowType where
  (SType (Sy _)) < (SType (St _)) = True
  _      <  _     = False

instance Show ShallowType where
  show (SType (Sy s)) = show s
  show (SType (St s)) = show s

invert :: M.Map a Int -> IM.IntMap a
invert = IM.fromList . map (\(a,b) -> (b,a)). M.toList

getOrAssign :: Ord a => a -> M.Map a Int -> (Int, M.Map a Int)
getOrAssign a m = case M.lookup a m of
                    Just r  -> (r, m)
                    Nothing -> (M.size m, M.insert a (M.size m) m)

lookupSymbol :: String -> State Abbrv Symbol
lookupSymbol s = do
                  state <- get
                  let (r,m) = getOrAssign s (aSymbolTable state)
                  put state {aSymbolTable = m}
                  return r


partitionType :: [Type] -> ([String],[ASt])
partitionType []      = ([], [])
partitionType (t:ts)  = case t of
                                Sy s -> (s:ls,rs)
                                St s -> (ls,s:rs)
  where
    (ls,rs) = partitionType ts

insertClause :: (St,Symbol) -> (Action Symbol, St) -> State Abbrv ()
insertClause k v = modify (\s -> s {result = M.insertWith (flip const) k v (result s)})

expandMacro :: String -> [Type] -> State Abbrv St
expandMacro n args = do
                            (args1,trans) <- fromJust . M.lookup (n,map SType args) . aMacroTable <$> get
                            let (sym1, st1) = partitionType args
                            let (sym2, st2) = partitionType args1
                            let symMa = M.fromList (zip sym2 sym1)
                            let stMa = M.fromList (zip st2 st1)
                            v <- mapM (\(asy, acts, ast) -> return (replaceSym asy symMa, replaceAc symMa <$> acts, replaceSt ast stMa symMa)) trans
                            state <- get
                            let c = M.size (aStateTable state)
                            put (state {aStateTable = M.insert (Func n args) c (aStateTable state)})
                            expandRule (Func n args) v 
                            return c
                              where
                                replaceSym (Sym s) ma = case M.lookup s ma of
                                                          Just b -> Sym b
                                                          Nothing -> Sym s
                                replaceSym (Not s) ma = case M.lookup s ma of
                                                          Just b -> Not b
                                                          Nothing -> Not s
                                replaceSym  a _ = a
                                replaceAc ma (Print sy) = case M.lookup sy ma of
                                                            Just b -> Print b
                                                            Nothing -> Print sy
                                replaceAc ma a = a
                                replaceSt :: ASt -> M.Map ASt ASt -> M.Map String String -> ASt
                                replaceSt (Func n args)  stMa syMa = (Func n (map f args))
                                  where
                                    f (Sy s) = Sy $ fromMaybe s (M.lookup s syMa)
                                    f (St s) = St $ fromMaybe s (M.lookup s stMa)
                                replaceSt (Name s) stMa syMa = Name $ fromMaybe s (M.lookup s syMa)

expandState :: ASt -> State Abbrv St
expandState (Name s)      = (M.lookup (Name s)) . aStateTable <$> get >>= \m -> case m of
                              (Just x) -> return x
                              Nothing  -> do
                                            v <- M.size . aStateTable <$> get
                                            modify (\a -> a{aStateTable = M.insert (Name s) v (aStateTable a)})
                                            return v
expandState s@(Func n args) = aStateTable <$> get >>= \st ->
                                                              case M.lookup s st of
                                                                Just i  -> return i
                                                                Nothing -> expandMacro n args

nextName :: State Abbrv ASt
nextName = do
  nst <- M.size <$> aStateTable <$> get
  modify (\s -> s{aStateTable = M.insert (Name (show nst)) nst (aStateTable s) })
  return (Name (show nst))

expandClause :: ASt -> (ASym, [Act], ASt) -> State Abbrv ASt
expandClause st (sym, [], ast) = return ast
expandClause st (Read v, (Print sy:Move d:acts), ast) = do
                                                           syms <- map fst <$> M.toList <$> aSymbolTable <$> get
                                                           s'   <- lookupSymbol sy
                                                           nst <- nextName
                                                           st' <- expandState st
                                                           mapM_ (\s1 -> do
                                                                     s  <- lookupSymbol s1
                                                                     nst <- nextName
                                                                     nxt' <- expandClause nst (Any, map (f v s1) acts, ast) >>= expandState
                                                                     insertClause (st', s) (Action (if v == sy then s else s') d, nxt')) syms
                                                           return st
                                                           where
                                                             f a b (Print s) = if s == a then Print b else Print s
                                                             f _ _ a = a
expandClause st (sym, (Print s:Move d:acts), ast) = do
                                syms <- expandSymbol sym 
                                s'   <- lookupSymbol s
                                nst <- nextName
                                nxt  <- expandClause nst (Any , acts, ast)
                                st' <- expandState st
                                nxt' <- expandState nxt
                                mapM_ (\s -> insertClause (st', s) (Action s' d, nxt')) syms
                                return st
expandClause st (sym, (Print s:acts), ast) = expandClause st (sym, Print s: Move N:acts, ast)
expandClause st (Read v, (Move d:acts), ast) = expandClause st (Read v, (Print v: Move d: acts), ast)
expandClause st (sym, (Move d:acts), ast) = do
                                syms <- expandSymbol sym
                                nst <- nextName
                                nxt <- expandClause nst (Any, acts, ast) 
                                nxt' <- expandState nxt
                                st' <- expandState st
                                mapM_ (\s -> insertClause (st', s) (Action s d, nxt')) syms
                                return st


expandSymbol :: ASym -> State Abbrv [Symbol]
expandSymbol (Sym s) = return <$> lookupSymbol s
expandSymbol Any     = map snd . M.toList . aSymbolTable <$> get
expandSymbol (Not s) = lookupSymbol s >>= \sv -> filter (/= sv) <$> expandSymbol Any
expandSymbol None    = return [0]

expandRule :: ASt -> [(ASym, [Act], ASt)] -> State Abbrv ASt
expandRule ast clauses = do
                          forM_ clauses (expandClause ast)
                          return ast

removeDups :: Ord a => [(a,b)] -> [(a,b)]
removeDups = rD . sortOn fst
  where
    rD [] = []
    rD [a] = [a]
    rD (x:y:xs) = if fst x == fst y then rD (x:xs) else x:y:rD xs

expand :: Abbrv -> M.Map ASt [(ASym, [Act], ASt)] -> TransitionTable
expand a@Abbrv {aSymbolTable = syt, aStateTable = stt, aMacroTable = mt} tt =
    TransTable (invert syt') (invert stt') (res)
  where
    (_, Abbrv {aSymbolTable=syt',aStateTable=stt', result = res}) = runState (forM_ (M.toList tt) (uncurry expandRule)) a
