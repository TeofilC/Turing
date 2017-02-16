module Machine where

import qualified Data.Map as M

type St  = Int -- St stand for state it uniquely denotes a state in a turing machine
type Symbol = Int

data Config = Config { curState :: St
                     , states :: M.Map (St, Symbol) (Action Symbol, St)
                     , ltape :: [Symbol]
                     , rtape :: [Symbol]
                     , cur   :: Symbol
                     }
              deriving (Show)

data Dir  = L | R | N
            deriving Show
data Action s = Action s Dir
              deriving (Show)


move :: Dir -> Config -> Config
move L c@Config {ltape = (l:ls), rtape = rs    , cur = cur} = c {ltape = ls, rtape = cur:rs, cur = l}
move L c@Config {ltape = []    , rtape = rs    , cur = cur} = c {ltape = [], rtape = cur:rs, cur = 0}
move R c@Config {ltape = ls    , rtape = (r:rs), cur = cur} = c {ltape = cur:ls, rtape = rs, cur = r}
move R c@Config {ltape = ls    , rtape = []    , cur = cur} = c {ltape = cur:ls, rtape = [], cur = 0}
move N c = c

applyAction :: Action Symbol -> Config -> Config
applyAction (Action sym d) c@Config {} = move d $ c {cur = sym}


step :: Config -> Maybe Config
step c@Config {states = states, curState = curState, cur = cur} = (\(a,s) -> applyAction a c {curState = s}) <$> M.lookup (curState, cur) states

stepAll :: Config -> Maybe Config
stepAll c = step c >>= stepAll

stepN :: Int -> Config -> Maybe Config
stepN 0 c = Just c
stepN n c = step c >>= stepN (n-1)

tapes :: Config -> [Symbol]
tapes Config {ltape=l, rtape=r, cur=c} = reverse l ++ [c] ++ r

standardDesc :: Config -> String
standardDesc Config{states = s} = take 10000 $ foldl f "" (M.toList s)
  where
    f a ((st,re),(Action pr d, nxt)) = a ++ showSt st ++ showSy re ++ showSy pr ++ show d ++ showSt nxt ++ ";"
    showSt = reverse . showSt'
    showSt' 0 = "D"
    showSt' n = 'A':showSt' (n-1)
    showSy = reverse . showSy'
    showSy' 0 = "D"
    showSy' n = 'C':showSy' (n-1)
