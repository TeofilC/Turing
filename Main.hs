import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core

import System.Environment

main :: IO ()
main = getArgs >>= \args ->
                     let port = read (head args) in warp port App
