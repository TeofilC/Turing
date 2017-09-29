module Main where
import Data.IORef
import GHCJS.DOM
import GHCJS.DOM.Types
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Document
import qualified Gen as G
import Widget
import Handler

main = do
  doc <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  setTitle doc "Turing Expander"
  hasContent <- getElementById doc "content"
  case hasContent of
               Just _ -> return ()
               Nothing -> G.attach app body
  (Just content) <- fmap (uncheckedCastTo HTMLElement) <$> getElementById doc "content"
  currentDrag <- newIORef undefined
  machineRef <- newIORef undefined
  registerHandlers machineRef currentDrag content
  return ()
