{-# LANGUAGE QuasiQuotes #-}
module Examples.Basic where
import Text.RawString.QQ

basic :: String
basic = [r|B:
None P 0; B
1 R,R,P 0; B
0 R,R,P 1; B
|]
