import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Yesod.Static

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let port = read (head args)
  static@(Static settings) <- static "static"
  warp port $ App static
