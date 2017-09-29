{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Macro where
import qualified Data.IntMap            as IM
import qualified Data.Map               as M

import           Machine                hiding (states)

import           Control.Monad.RWS.Lazy hiding (Any, asks, get, gets, local,
                                         modify)
import           Data.Label             (get, mkLabels)
import           Data.Label.Monadic
import           Data.List
import           Data.Maybe

data ASym   = Sym String | Not String | Read String | Any | None | AnyAndNone
              deriving (Eq, Ord)

instance Show ASym where
  show (Sym str)  = str
  show (Not str)  = "Not " ++ str
  show (Read str) = "Not " ++ str
  show Any        = "Any"
  show None       = "None"
  show AnyAndNone = "AnyAndNone"

data AState a = Func a [Type] | Name a
              deriving (Eq, Ord, Functor)

type ASt = AState String

instance Show (AState String) where
  show (Func n args) = n ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (Name n)      = n

data Act    = Print String | Move Dir

instance Show Act where
  show (Print " ") = "E"
  show (Print s)   = "P " ++ s
  show (Move d)    = show d

type ARow = ((ASt, ASym), ([Act], ASt))
type Row = ((St, Symbol),(Action Symbol, St))

data Type = Sy String | St ASt
            deriving (Eq, Ord)

instance Show Type where
  show (Sy s) = s
  show (St s) = show s

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

data Env = Env { _macroMap  :: M.Map (String, [ShallowType]) ([Type], [(ASym, [Act], ASt)])
               , _symbolMap :: M.Map String Int
               , _substSymbol  :: String -> String
               , _substState   :: ASt -> ASt
               , _readSymbol :: String
               }


env mm symm = Env mm symm id id undefined

data Status  = Status { _states       :: M.Map ASt Int
                      , _currentState :: String
                      , _counter      :: Int
                      }
  deriving (Show)

emptyStatus :: Status
emptyStatus = Status M.empty "" 0

$(mkLabels [''Env, ''Status])

instance Show Env where
  show e = show (get macroMap e) ++ "\n" ++ show (get symbolMap e)

newtype Expander a = Expander {unExpander :: RWS Env [Row] Status a}
  deriving (Functor, Applicative, Monad, MonadRWS Env [Row] Status, MonadReader Env, MonadState Status, MonadWriter [Row])

invert :: M.Map a Int -> IM.IntMap a
invert = IM.fromList . map (\(a,b) -> (b,a)). M.toList

type Sub a = (String -> String) -> (ASt -> ASt) -> a -> a

subState :: ASt -> Expander ASt
subState (Func n args) = do
  sy <- asks substSymbol
  st <- asks substState

  return $ Func n (map (subType sy st) args)
subState s = do
  st <- asks substState
  return $ st s

subType :: Sub Type
subType sy st (Sy s) = Sy $ sy s
subType sy st (St s) = St $ st s

subSymbol :: ASym -> Expander ASym
subSymbol (Sym s) = do
  sy <- asks substSymbol
  return $ Sym $ sy s
subSymbol (Not s) = do
  sy <- asks substSymbol
  return $ Not $ sy s
subSymbol (Read s) = do
  sy <- asks substSymbol
  return $ Read $ sy s
subSymbol s = return s

subAct :: Sub Act
subAct sy st (Print s) = Print $ sy s
subAct _  _   a        = a

symbols :: Expander [String]
symbols = M.keys <$> asks symbolMap

expand :: [ARow] -> Expander ()
expand = mapM_ (\row -> resetCounters row >> expandRow row)
  where
    resetCounters ((ast,_),_) = do
      modify currentState (const $ show ast)
      --modify counter (const 0)

insertState :: ASt -> Expander Int
insertState st = do
  n <- M.size <$> gets states
  modify states (M.insert st n)
  return n

expandRow :: ARow -> Expander ()
expandRow ((ast, asym),(acts,k)) = do
  let (x:xs) = padActs acts
  st <- lookupState ast
  expandSymbol asym $ \s -> local readSymbol (const s) $ do
    k' <- lookupState k
    lastk <- foldM (expandAct Nothing) k' (reverse xs)
    expandAct (Just st) lastk x
    return ()
  return ()

lookupState st = do
  st' <- subState st
  stM <- gets states
  case M.lookup st' stM of
    (Just x) -> return x
    Nothing  -> expandState st'

lookupSymbol sym = do
  subSym <- asks substSymbol
  symM <- asks symbolMap
  case M.lookup (subSym sym) symM of
    (Just x) -> return x
    Nothing  -> asks symbolMap >>= \sm -> error ("This shouldn't happen" ++ show sym ++ show (subSym sym))

expandSymbol :: ASym -> (String -> Expander ()) -> Expander ()
expandSymbol AnyAndNone k = symbols >>= mapM_ k
expandSymbol Any k = filter (" "/=) <$> symbols >>= mapM_ k
expandSymbol (Not s) k = filter (s/=) <$> symbols >>= mapM_ k
expandSymbol None k = k " "
expandSymbol (Sym s) k = k s
expandSymbol (Read v) k = symbols >>= mapM_ (\s -> local substSymbol (. (\x -> if x == v then s else x)) $ k s)

padActs []                   = []
padActs (Print x: Move d:xs) = (Just x ,      d): padActs xs
padActs (Print x:xs)         = (Just x ,      N): padActs xs
padActs (Move d: xs)         = (Nothing,      d): padActs xs

newState = do
  s <- M.size <$> gets states
  cS <- gets currentState
  count <- gets counter
  modify counter (+1)
  modify states (M.insert (Name $ cS ++ "." ++  show count) s)
  return s

expandAct Nothing k a = do
  st <- newState
  expandSymbol AnyAndNone (\s -> local readSymbol (const s) $ void (expandAct (Just st) k a ))
  return st
expandAct (Just st) k (Just x, d) = do
  x' <- lookupSymbol x
  sym <- lookupSymbol =<< asks readSymbol
  tell [((st,sym),(Action x' d, k))]
  return st
expandAct (Just st) k (Nothing, d) = do
  rS <- asks readSymbol
  expandAct (Just st) k (Just rS, d)

partitionType :: [Type] -> ([String],[ASt])
partitionType []      = ([], [])
partitionType (t:ts)  = case t of
                                Sy s -> (s:ls,rs)
                                St s -> (ls,s:rs)
  where
    (ls,rs) = partitionType ts
expandState :: ASt -> Expander Int
expandState ast@(Func str args) = do
  mm <- asks macroMap
  case M.lookup (str, map SType args) mm of
    (Just (mapping,rows)) -> do
      let (syms1, sts1) = partitionType args
      let (syms2, sts2) = partitionType mapping
      let symM = M.fromList $ zip syms2 syms1
      let stM = M.fromList $ zip sts2 sts1
      st <- insertState ast
      local substSymbol (const (\k -> M.findWithDefault k k symM)) $
        local substState (const (\k -> M.findWithDefault k k stM )) $ expand $ map (\(a,b,c) -> ((ast,a),(b,c))) rows
      return st
    Nothing  -> error "undefined m-function"
expandState _ = error "Insert a more informative error message here"

prettyPrintRows :: Env -> Status -> [Row]-> [(String, String, String, String)]
prettyPrintRows env status = map (\((a,b),(c,d)) -> (show a ++ ppSt a, show b ++ bracket (ppSy b), ppAct c, show d ++ ppSt d))
  where
  syms = invert . get symbolMap $ env
  sts = invert . get states $ status
  ppSt s = bracket . show . fromMaybe (error $ "undefined state: " ++ show s ++ " " ++ show sts) . flip IM.lookup sts $ s
  ppSy s = fromMaybe (error $ "undefined symbol: " ++ show s) . flip IM.lookup syms $ s
  ppAct (Action 0 dir)= "E " ++ " " ++ show dir
  ppAct (Action sy dir)= "P " ++ ppSy sy ++ " " ++ show dir
  bracket str = " (" ++ str ++ ") "
