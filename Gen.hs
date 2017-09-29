{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Gen  where
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.String
import           GHCJS.DOM
import qualified GHCJS.DOM.Document         as Doc
import           GHCJS.DOM.DocumentFragment
import           GHCJS.DOM.Element
import           GHCJS.DOM.Node
import           GHCJS.DOM.NodeList
import           GHCJS.DOM.ParentNode       (querySelectorAll)
import           GHCJS.DOM.Types


newtype Html a = Html {unHtml :: ReaderT Node IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Node)

instance a ~ () => IsString (Html a) where
  fromString = text


parent :: Html Node
parent = ask

text :: String -> Html ()
text str = parent >>= flip setTextContent (Just str)

element :: (IsNode b) => (JSVal -> b) -> JSString -> Html a -> Html b
element typ tag children = do
  doc <- currentDocumentUnchecked
  t <- uncheckedCastTo typ <$> Doc.createElement doc tag
  _ <- flip appendChild t =<< parent
  _ <- local (const $ uncheckedCastTo Node t) children
  return t

clone :: (IsNode a, MonadIO m) => a -> m Node
clone node = cloneNode node True

toFragment :: MonadIO m => Html a -> m DocumentFragment
toFragment (Html r)= do
  df <- uncheckedCastTo Node <$> newDocumentFragment
  _ <- liftIO $ runReaderT r df
  return $ uncheckedCastTo DocumentFragment df

prependChild :: (MonadIO m, IsNode self, IsNode node) => self -> node -> m Node
prependChild parent child = getFirstChild parent >>= insertBefore parent child


fromNodeList :: (MonadIO m) => NodeList -> m [HTMLElement]
fromNodeList nl = do
  l <- getLength nl
  if l > 0 then mapM (fmap (uncheckedCastTo HTMLElement . fJ) . item nl) [0..l-1] else return []
  where
    fJ (Just x) = x
    fJ Nothing  = error "out of bound NodeList"

getChildren :: (MonadIO m, IsNode a) => a -> m [HTMLElement]
getChildren node = getChildNodes node >>= fromNodeList

queryAll :: (MonadIO m, IsParentNode self, ToJSString selector) => self -> selector -> m [HTMLElement]
queryAll e s =  querySelectorAll e s >>= fromNodeList

deleteChildren :: (MonadIO m, IsNode a) => a -> m ()
deleteChildren node = do
  children <- getChildren node
  mapM_ (removeChild node) children
  return ()

attach :: (MonadIO m, IsNode node) => Html a -> node -> m ()
attach html p = toFragment html >>= prependChild p >> return ()

infixl 5 =.
(=.) :: JSString -> JSString -> Html ()
(=.) attr val = do
  elem <- uncheckedCastTo HTMLElement <$> parent
  liftIO $ setAttribute elem attr val
  return ()

p :: Html a -> Html HTMLParagraphElement
p = element HTMLParagraphElement "p"

div :: Html a -> Html HTMLDivElement
div = element HTMLDivElement "div"

table :: Html a -> Html HTMLTableElement
table = element HTMLTableElement "table"

tbody :: Html a -> Html HTMLTableSectionElement
tbody = element HTMLTableSectionElement "tbody"

thead :: Html a -> Html HTMLTableSectionElement
thead = element HTMLTableSectionElement "thead"

tfoot :: Html a -> Html HTMLTableSectionElement
tfoot = element HTMLTableSectionElement "tfoot"

tr :: Html a -> Html HTMLTableRowElement
tr = element HTMLTableRowElement "tr"

th :: Html a -> Html HTMLTableCellElement
th = element HTMLTableCellElement "th"

td :: Html a -> Html HTMLTableCellElement
td = element HTMLTableCellElement "td"

span :: Html a -> Html HTMLSpanElement
span = element HTMLSpanElement "span"

button :: Html a -> Html HTMLButtonElement
button = element HTMLButtonElement "button"

i :: Html a -> Html HTMLElement
i = element HTMLElement "i"

select = element HTMLSelectElement "select"
option = element HTMLElement "option"

h5 = element HTMLElement "h5"
