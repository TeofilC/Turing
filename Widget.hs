{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Widget where
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.IntMap                   as IM
import           Data.List                     (groupBy)
import           Gen
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM
import           GHCJS.DOM.GlobalEventHandlers hiding (focus, select)
import           GHCJS.DOM.HTMLElement         (focus, getIsContentEditable,
                                                setContentEditable,
                                                setDraggable)
import           GHCJS.DOM.Node
import           GHCJS.DOM.NodeList
import           GHCJS.DOM.Types
import           Machine
import           Prelude                       hiding (div, lookup, span)

dragHandle :: Html ()
dragHandle = span (do
  "class" =. "draghandle fa fa-bars"
  text ""
  ) >> return ()

deleteButton = do
  button $ do
    "type" =. "button"
    "class" =. "btn btn-default btn-sm"
    "data-button" =. "delete"
    "aria-label" =. "delete"
    i $ do
      text ""
      "aria-hidden" =. "true"
      "class" =. "fa fa-close"

expandButton = do
   button $ do
    "type" =. "button"
    "class" =. "btn btn-default"
    "data-button" =. "expand"
    "aria-label" =. "Expand"
    span $ do
      text ""
      "aria-hidden" =. "true"
      "class" =. "glyphicon glyphicon-arrow-right"
    span "Expand"

exampleChooser = select $ do
  "class" =. "custom-select"
  "id" =. "exampleChooser"
  option ("value" =. "none" >> "Pick Example")
  option ("value" =. "basic" >> "Simple")
  option ("value" =. "sqrt" >> "sqrt(2)/2")
  -- option ("value" =. "turing" >> "Universal Turing Machine") -- TODO

editable :: Html ()
editable = do
  obj <- uncheckedCastTo HTMLElement <$> parent
  liftIO $ print "hello"
  _ <- liftIO $ on obj click $ do
    isEdit <- getIsContentEditable obj
    unless isEdit $ setContentEditable obj ("true" :: JSString) >> focus obj
  _ <- liftIO $ on obj blur $ do
    setContentEditable obj ("false" :: JSString)
  return ()

mconfig isEditable (mconf, rows) = tbody $ do
  "data-dragclass" =. "mconfig"
  tb <- uncheckedCastTo HTMLElement <$> parent
  tr $ do
       td $ do
         "class" =. "mconfig"
         "rowspan" =. (toJSString $ show $ length rows + 1)
         div $ do
           "class" =. "flex"
           when isEditable dragHandle
           div $ text mconf >> when isEditable ("class" =. "editable fill data") >> unless isEditable ("class" =. "data")
       return ()
  mapM_ (row isEditable) rows
  when isEditable (tr (td ("colspan" =. "6" >> "data-button" =. "addrow" >> "add row")) >> return ())

row isEditable (symbol, operation, finalmconf) = tr $ do
  "data-dragclass" =. "row"
  td $ text symbol >> when isEditable ("class" =. "editable")
  td $ text operation  >> when isEditable ("class" =. "editable")
  td $ text finalmconf >> when isEditable ("class" =. "editable")
  when isEditable $ void (td dragHandle)
  when isEditable $ void (td deleteButton)

eR = [("B", "None", "N", "B")]
emptyRow     t = row t ("None", "N", "B")
emptyMConfig t = mconfig t ("B", [("None", "N", "B")])

header = thead $ tr $ do
            th "mconfig."
            th "read symbol"
            th "operations"
            th "final mconfig."

outputWidget rows = table $ do
  "class" =. "table table-bordered table-hover table-sm"
  header
  mapM (mconfig False) groupped
  where
    groupped = map (\((a,b,c,d):xs) -> (a,(b,c,d):map notfst xs)) $ groupBy (\(a,_,_,_) (b,_,_,_) -> a == b) $ rows
    notfst (_,b,c,d) = (b,c,d)

inputWidget rows = table $ do
        "class" =. "table table-bordered table-sm"
        header
        tfoot $ tr (td $ "data-button" =. "addmconfig" >> "colspan" =. "6" >> "add mconfig.")
        mapM (mconfig True) groupped
          where
            groupped = map (\((a,b,c,d):xs) -> (a,reverse $ (b,c,d):map notfst xs)) $ groupBy (\(a,_,_,_) (b,_,_,_) -> a == b) $ rows
            notfst (_,b,c,d) = (b,c,d)

runModal = do
  div $ do
    "class" =. "modal"
    "id" =. "runModal"
    div $ do
      "class" =. "modal-dialog modal-lg"
      div $ do
        "class" =. "modal-content"
        div $ do
          "class" =. "modal-header"
          h5 $ do
            "class" =. "modal-title"
            "Run"
          button $ do
            "type" =. "button"
            "class" =. "close"
            "data-dismiss" =. "modal"
            "aria-label" =. "Close"
            span $ do
              "aria-hidden" =. "true"
              "×"
        div $ do
          "class" =. "modal-body"
          "id" =. "modalBody"
          "Hello World"
        div $ do
          "class" =. "modal-footer"
          button $ do
            "type" =. "button"
            "class" =. "btn"
            "data-button" =. "step1"
            "Step 1"

runWidget = do
  div $ do
    "class" =. "card-header flex"
    "Run"
  div $ do
      "id" =. "runBody"
  div $ do
    "class" =. "card-footer"
    button $ do
      "type" =. "button"
      "class" =. "btn"
      "data-button" =. "step1"
      "Step 1"
    button $ do
      "type" =. "button"
      "class" =. "btn"
      "data-button" =. "step1000"
      "Step 1000"

configWidget (Config {..}) sy st = div $ do
  span (text $ "Current m-config: " ++ show (st IM.!  curState))
  table $ do
    "class" =. "table table-bordered table-sm scroll monospace"
    tr $ do
      mapM col (reverse ltape)
      td $ do
        "class" =. "table-info"
        text $ if cur == 0 then " " else sy IM.! cur
      mapM col rtape
   where
    col sym = td $ (text $ if sym == 0 then " " else sy IM.! sym)

app = do
  div $ do
    "id" =. "content"
    "class" =. "container-fluid"
    div $ do
      "class" =. "row"
      div $ do
        "class" =. "col-md"
        runWidget
    div $ do
      "class" =. "row"
      div $ do
        "class" =. "col-md"
        div $ do
          "class" =. "card"
          div $ do
            "class" =. "card-header flex"
            "Input"
            span ("class" =. "fill" >> "")
            exampleChooser
          div $ do
            "id" =. "input"
            inputWidget eR
      div $ do
        "class" =. "col-md"
        div $ do
          "class" =. "card"
          div $ do
            "class" =. "card-header flex"
            "Output"
          div $ do
            "id" =. "output"
            outputWidget eR
