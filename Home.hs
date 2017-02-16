{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Home where
import Data.Text hiding (length, reverse)

import Foundation
import Parser
import Macro
import Machine
import Yesod.Core
import Yesod.Static
import Yesod.Form
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3
import Text.Blaze.Internal (preEscapedString)
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
    addStylesheet $ StaticR css_pygment_css
    mmsg <- getMessage
    syntax <- liftIO $ readFile "syntax.html"
    [whamlet|
        <div .panel .panel-default>
          <div .panel-heading>
            <a role="button" data-toggle="collapse" aria-expanded="true" href="#syntax"> Info
          <div .panel-body #syntax >
            <h2> Usage
            Enter a description of an abbreviated Turing machine into the textarea entitled Code and press submit. You will be redirected to a page which will show the tape after 100 iterations and a table showing the full Turing machine state table. The syntax is described in EBNF as
            : #{preEscapedString syntax}
            Note that my syntax diverges slightly from that employed by Turing. Instead of using German gothic characters for m-configuration and skeletal table names, I use string of uppercase letters and digts. Furthermore symbols are restricted to strings of lowercase letters and digits. I add AnyAndNone, which as one could guess is equivalent to saying all symbols including the blank symbol.
           And here are a few examples
           <h3> Turing's first example of a machine
           <div .well>
             <p>
               B: <br>
               None P 0;     B<br>
               1    R,R,P 1; B<br>
               0    R,R,P 0; B<br>

           <h3> A machine which prints 0,1,2 to the tape then replaces the first occurence of 1 with "found"
           <div .well>
             <p>
                B:<br>
                AnyAndNone R,R,R,P 0, R, P 1, R, P 2, R, R; F(C,D,1)<br>
                <br>
                C:<br>
                Any P found; D<br>
                <br>
                D:<br>
                Any N; D<br>
                None P notfound; D<br>
                <br>
                F(B,C,a):<br>
                e L; F1(C,B,a)<br>
                Not e L; F(C,B,a)<br>
                None L; F(C,B,a)<br>
                <br>
                F1(B,C,a):<br>
                a N; C<br>
                Not a R; F1(B,C,a)<br>
                None R; F2(B,C,a)<br>
                <br>
                F2(B,C,a):<br>
                a N; C<br>
                Not a R; F1(B,C,a)<br>
                None N; B<br>

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

configW :: IM.IntMap String -> Config -> Widget
configW s c = [whamlet|
                    <div style="overflow:auto">
                      <table .table .table-bordered >
                        <thead>
                          <tr>
                           $forall t <- tapes c
                             <td> #{s ! t}
                    <div style="overflow:auto">
                      <p> #{standardDesc c}
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
