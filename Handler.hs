{-# LANGUAGE ScopedTypeVariables #-}
module Handler where
import           Control.Concurrent             (forkIO)
import           Control.Monad
import           Control.Monad.RWS.Lazy         (ask, execRWS)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans
import           Data.Either
import           Data.IORef
import           Data.Label                     (get)
import           Data.List                      (intercalate)
import qualified Data.Map                       as M
import           Data.Maybe
import qualified Examples                       as Ex
import           Gen                            (attach, deleteChildren,
                                                 getChildren, queryAll,
                                                 toFragment)
import           GHCJS.DOM
import           GHCJS.DOM.ChildNode
import           GHCJS.DOM.DataTransfer
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM
import           GHCJS.DOM.GlobalEventHandlers  hiding (error, focus)
import           GHCJS.DOM.HTMLElement          hiding (blur, click)
import           GHCJS.DOM.HTMLSelectElement    (getValue)
import           GHCJS.DOM.MouseEvent
import           GHCJS.DOM.Node
import           GHCJS.DOM.NodeList             (getLength)
import           GHCJS.DOM.NonElementParentNode
import           GHCJS.DOM.ParentNode           hiding (getChildren)
import           GHCJS.DOM.Types
import           Machine                        hiding (states)
import           Macro
import           Parser
import           Prelude                        hiding (drop)
import           Widget

-- TODO: Refactor this file

data HandlerEnv = HandlerEnv {}

type Handler e t a = ReaderT HandlerEnv (EventM e t) a

onClickEditable target = do
  isEdit <- getIsContentEditable target
  unless isEdit $ setContentEditable target "true" >> focus target

onBlurEditable machineRef target = do
  setContentEditable target "false"
  onChange machineRef

setDragTargetDraggable target b = do
  dragTarget <- uncheckedCastTo HTMLElement <$> closestUnchecked target "[data-dragclass]"
  setDraggable dragTarget b
  return ()

onClickAddMconfigButton machineRef target = do
  table <- closestUnchecked target "table"
  mconf <- liftIO $ toFragment (emptyMConfig True)
  appendChild table mconf
  onChange machineRef
  return ()

onClickAddRowButton machineRef target = do
  tbody <- closestUnchecked target "tbody"
  mconf <- querySelectorUnchecked tbody ".mconfig"
  parent <- getParentElementUnchecked target
  r <- liftIO $ toFragment (emptyRow True)
  insertBefore tbody r (Just parent)
  updateMconfig tbody mconf
  onChange machineRef
  return ()

stepBy machineRef n = do
  liftIO $ print "steppp"
  doc <- currentDocumentUnchecked
  modal <- getElementByIdUnchecked doc "runBody"
  deleteChildren modal
  (sy, st, conf) <- liftIO $ readIORef machineRef
  let conf' = case stepN n conf of
        (Just x) -> x
        Nothing  -> error $ "error running Turing machine at mconf:" ++ show (curState conf)
  attach (configWidget conf' sy st) modal
  liftIO $ writeIORef machineRef (sy, st, conf')

updateMconfig tbody mconf = do
  childLen <- getChildNodes tbody >>= getLength
  if childLen <= 2 then
    liftIO(print "bbop") >> remove tbody
   else setAttribute mconf "rowspan" (toJSString $ show $ childLen - 1)

onSelectExample machineRef target = lift $ do
  doc <- currentDocumentUnchecked
  val <- getValue target
  let src = case val of
        "basic" -> Ex.basic
        "sqrt"  -> Ex.sq
  case parseAbbrv src of
    (Right (env,status,rows)) -> do
      input <- getElementByIdUnchecked doc "input"
      deleteChildren input
      attach (inputWidget . reverse $ map showRow rows) input
      onChange machineRef
    (Left er) -> error $ show er
  where
    showRow ((a,b),(c,d)) = (show a, show b, showActs c, show d)
    showActs = intercalate ", ". map show

onChange machineRef = do
  doc <- currentDocumentUnchecked
  input <- getElementByIdUnchecked doc "input"
  output <- getElementByIdUnchecked doc "output"
  (env,status, rows) <- extract input
  let (st,result) = execRWS (unExpander $ expand rows) env status
  deleteChildren output
  attach (outputWidget . prettyPrintRows env st . M.toList . M.fromList $ result) output
  liftIO $ writeIORef machineRef (invert (get symbolMap env), invert (get states st), Config 0 (M.fromList result) [] [] 0)
  return ()
  where forkIO_ a = void (forkIO a)

onClickDeleteButton machineRef target = do
  tbody <- closestUnchecked target "tbody"
  mconf <- querySelectorUnchecked tbody ".mconfig"
  row <- closestUnchecked target "tr"
  remove row
  updateMconfig tbody mconf
  onChange machineRef
  return ()

onDragStart currentDragTarget target = do
  lift $ writeIORef currentDragTarget target
  (Just dt) <- event >>= getDataTransfer
  (iHtml :: JSString)<- getInnerHTML target
  setData dt "text/html" iHtml
  setDropEffect dt "move"
  return ()

onDragEnter currentDragTarget target = do
  cdt <- lift $ readIORef currentDragTarget
  (Just dragclass :: Maybe String) <-lift $ getAttribute cdt "data-dragclass"
  hasActualDragTarget <- closest target ("[data-dragclass='" ++ dragclass ++ "']")
  case hasActualDragTarget of
    Just actualDragTarget -> do
      prevented <- defaultPrevented
      (preventDefault >> lift (print "dragover"))
    Nothing -> return ()

onDrop machineRef currentDragTarget target = do
  cdt <- lift $ readIORef currentDragTarget
  lift $ setDraggable cdt False
  (Just dragclass :: Maybe String) <-lift $ getAttribute cdt "data-dragclass"
  hasActualDragTarget <- closest target ("[data-dragclass='" ++ dragclass ++ "']")
  case hasActualDragTarget of
    Just actualDragTarget -> do
      prevented <- defaultPrevented
      preventDefault
      cdtHTML :: JSString <- getInnerHTML cdt
      targetHTML :: JSString <- getInnerHTML actualDragTarget
      setInnerHTML cdt targetHTML
      setInnerHTML actualDragTarget cdtHTML
      onChange machineRef
      lift $ print "drop"
    Nothing -> return ()

registerHandlers machineRef currentDragTarget content = do
  doc <- currentDocumentUnchecked
  _ <- on content click $ do
    (Just target) <- fmap (uncheckedCastTo HTMLElement) <$> eventTarget
    isEditable <- matches target ".editable"
    isAddRowButton <- matches target "[data-button='addrow']"
    isExpandButton <- matches target "[data-button='expand']"
    isAddMconfigButton <- matches target "[data-button='addmconfig']"
    isStep1Button <- matches target "[data-button='step1']"
    isStep1000Button <- matches target "[data-button='step1000']"
    when isEditable $ onClickEditable target
    when isAddRowButton $ onClickAddRowButton machineRef target
    when isAddMconfigButton $ onClickAddMconfigButton machineRef target
    when isStep1Button $ stepBy machineRef 1
    when isStep1000Button $ stepBy machineRef 1000
    closest target "[data-button='delete']" >>= maybe (return ()) (onClickDeleteButton machineRef)
    --when isExpandButton $ onClickExpandButton
  l <- newListener $ do
    (Just target) <- fmap (uncheckedCastTo HTMLElement) <$> eventTarget
    isEditable <- matches target "[class='editable']"
    when isEditable $ onBlurEditable machineRef target
  addListener content blur l True
  _ <- on content mouseOver $ do
    (Just target) <- fmap (uncheckedCastTo HTMLElement) <$> eventTarget
    isDragHandle <- matches target ".draghandle"
    when isDragHandle $ setDragTargetDraggable target True
  _ <- on content mouseOut $ do
    (Just target) <- fmap (uncheckedCastTo HTMLElement) <$> eventTarget
    isDragHandle <- matches target ".draghandle"
    when isDragHandle $ setDragTargetDraggable target False
    return ()
  _ <- on content dragStart $ do
    (Just target) <- fmap (uncheckedCastTo HTMLElement) <$> eventTarget
    onDragStart currentDragTarget target
  _ <- on content dragEnter $ do
    (Just target) <- fmap (uncheckedCastTo HTMLElement) <$> eventTarget
    (tag :: String)<- getTagName target
    onDragEnter currentDragTarget target
  _ <- on content dragOver $ do
    (Just target) <- fmap (uncheckedCastTo HTMLElement) <$> eventTarget
    (tag :: String)<- getTagName target
    onDragEnter currentDragTarget target
  _ <- on content drop $ do
    (Just target) <- fmap (uncheckedCastTo HTMLElement) <$> eventTarget
    onDrop machineRef currentDragTarget target
  exampleChooser <- uncheckedCastTo HTMLSelectElement <$> getElementByIdUnchecked doc "exampleChooser"
  _ <- on exampleChooser change $ do
    (Just target) <- fmap (uncheckedCastTo HTMLSelectElement) <$> eventTarget
    onSelectExample machineRef target
  onChange machineRef 
  return ()



-- extract :: MonadIO m => HTMLElement -> m (Env, Status, [ARow])
extract elem = do
  tbodies <- queryAll elem "#input tbody"
  (\(a,b,c) -> (a,b,reverse c)) . scanSymbols . rulesToAbbrv <$> mapM extractMconfig tbodies

--extractMconfig :: MonadIO m => HTMLElement -> m (ASt, [(ASym, [Act], ASt)])
extractMconfig elem = do
  mconfElem <- querySelectorUnchecked elem ".mconfig .data"
  (mconf :: String) <- getInnerHTML mconfElem
  let mconfP = either (error . show) id $ parseState mconf
  rows <- queryAll elem "tr[data-dragclass='row']"
  rows' <- mapM extractRow rows
  return (mconfP, rows')

--extractRow :: MonadIO m => HTMLElement -> m (ASym, [Act], ASt)
extractRow elem = do
  (sym:acts:finmconf:_) <- mapM getInnerHTML =<< getChildren elem
  let r = do
        symP <- parseSymbolExpr sym
        actsP <- parseActions acts
        finmconfP <- parseState finmconf
        return (symP, actsP,finmconfP)
  case r of
    (Left err)  -> error $ show err
    (Right row) -> return row
