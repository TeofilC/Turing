{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Home where
import Data.Text hiding (length, reverse)

import Foundation
import Parser
import Macro
import Machine
import Yesod.Core
import Yesod.Form
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.IntMap ((!))



instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

machineAForm :: AForm Handler Textarea
machineAForm = areq textareaField (bfs ("Code" :: Text)) Nothing

machineForm = renderBootstrap3 BootstrapBasicForm machineAForm

getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost machineForm
  defaultLayout $ do
    setTitle "Teo's Turing Translator"
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    mmsg <- getMessage
    [whamlet|
        $maybe msg <- mmsg 
          <p> Your message was: #{msg}
        <form role=form method=post action=@{TuringR} enctype=#{enctype}>
           ^{widget}
           <button type="submit" .btn .btn-default> Submit
    |]

transTableW :: TransitionTable -> Widget
transTableW TransTable {symbolTable= syt, stateTable=stt, transTable =tt} = [whamlet|
                                                                                    <table .table .table-bordered >
                                                                                       $forall r <- rule <$> rearrange (M.toList tt)
                                                                                         ^{r}
                                                                                    |]
  where
    rearrange :: [((St, Symbol),(Action Symbol, St))] -> [(St, [(Symbol, Action Symbol, St)])]
    rearrange = flip rearrange' []
    rearrange' [] rs = rs
    rearrange' (((st1,sy), (a,b)):xs) [] = rearrange' xs [(st1,[(sy,a,b)])]
    rearrange' (((st1,sy), (a,b)):xs) ((st2, cls):rs) = if st1 == st2 then rearrange' xs ((st2,(sy,a,b):cls):rs) else rearrange' xs ((st1,[(sy,a,b)]):(st2,cls):rs)
    rule   (st, clauses)           = [whamlet|
                                             <tr>
                                               <td rowspan=#{show $ (length clauses) + 1}> #{show $ stt ! st}
                                             $forall cl <- clause <$> clauses
                                                ^{cl}
                                             |]
    clause (sy1, Action sy2 d, st) = [whamlet|
                                            <tr>
                                              <td> #{syt ! sy1}
                                              <td> #{syt ! sy2}
                                              <td> #{show d}
                                              <td> #{show $ stt ! st}
                                            |]

joinTapes :: Config -> [Symbol]
joinTapes Config {ltape = l, rtape=r, cur = c} = reverse l ++ [c] ++ r
      
configW :: IM.IntMap String -> Config -> Widget
configW s c = [whamlet|
                    <div style="overflow:auto">
                      <table .table .table-bordered >
                        <thead>
                          <tr>
                           $forall t <- joinTapes c
                             <td> #{s ! t}
|]

postTuringR :: Handler Html
postTuringR = do
     ((result, widget), enctype) <- runFormPost machineForm
     case result of
       FormSuccess mach -> defaultLayout $ do
         addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"
         addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
         addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
         let res = parseAbbrv . unpack . unTextarea $ mach
         case res of
           (Right a) -> (case stepN 100 (Config 0 (transTable v) [] [3] 3) of
                            (Just x) -> configW (symbolTable v) x
                            Nothing -> [whamlet|<p> something went wrong :(|]) >> (transTableW v)
                            where
                              v = uncurry expand a
           (Left  v) -> [whamlet|<p> #{show v}]
         )
       _ -> defaultLayout [whamlet|<p>error|]
