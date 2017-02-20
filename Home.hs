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
import Data.Maybe



instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

machineAForm :: AForm Handler Textarea
machineAForm = fromJust <$> aopt textareaField (FieldSettings (SomeMessage ("Code" :: Text)) Nothing (Just "code") Nothing []) Nothing

machineForm = renderBootstrap3 BootstrapBasicForm machineAForm

getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost machineForm
  defaultLayout $ do
    setTitle "Teo's Turing Translator"
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"
    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
    addScript $ StaticR js_codemirror_js
    addScript $ StaticR js_simple_js
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    addStylesheet $ StaticR css_pygment_css
    addStylesheet $ StaticR css_codemirror_css
    mmsg <- getMessage
    syntax <- liftIO $ readFile "syntax.html"
    toWidget [julius|
                    CodeMirror.defineSimpleMode("tturing",{start: [
                    {regex: /None|Read|L|R|N|AnyAndNone|Any|Not/, token: "keyword"},
                    {regex: /[a-z0-9][a-z0-9]*/, token: "variable-3"},
                    {regex: /[A-Z][A-Z0-9]*/, token: "variable-2"},
]});
                    var myCodeMirror = CodeMirror.fromTextArea(document.getElementById("code"), {
                    lineNumbers: true,
                    smartIndent: false,
                    autofocus: true,
                    mode       : "tturing"
                    });
|]
    [whamlet|
        <form role=form method=post action=@{TuringR} enctype=#{enctype}>
           ^{widget}
           <button type="submit" .btn .btn-default> Submit
        <div .panel .panel-default>
          <div .panel-heading>
            <a role="button" data-toggle="collapse" aria-expanded="true" href="#syntax"> Info
          <div .panel-body #syntax >
            <h2> Usage
            <p> Enter a description of an abbreviated Turing machine into the textarea entitled Code and press submit. You will be redirected to a page which will show the tape after 100 iterations and a table showing the expanded state transition table. The syntax is described in EBNF as
            : #{preEscapedString syntax}
            <p> Note that my syntax diverges slightly from that employed by Turing in the following ways:
            <ul>
              <li> M-configuration and skeleton table names are denoted by a sequence of uppercase characters and digits (but cannot begin with a digit), as opposed to german gothic characters, eg, A, ABCD, PR1, PR2RC4, ...
              <li> Symbols must only consist of a sequence of lowercase characteers and digits, eg, 0, 1, 1b2, abc, ...
              <li> Each clause of a given m-configuration or skeletal table must have at least one behaviour even if it is merely "N" which does not move the tape, ie, "Any B" or "Any ; B" is not valid but "Any N; B" is valid
              <li> AnyAndNone denotes any non-empty character and the empty character
              <li> Turing occasionally uses syntax such as in his defintion of C1, ie, C1(E) β PE(E,β), beta in this case does not denote itself but rather acts as a variable. In my syntax this can be written as "C1(E): Read b N; PE(E,b)"

           And here are a few examples
           <h3> Turing's first example of a machine
           <div .well>
             <p>
               B: <br>
               None P 0;     B<br>
               1    R,R,P 0; B<br>
               0    R,R,P 1; B<br>

           <h3> A machine which prints 0,1,2 to the tape then replaces the first occurence of 1 with "found"
           <div .well>
             <p>
                B:<br>
                AnyAndNone R,P 0, R, P 1, R, P 2, R, R; F(D,C,1)<br>
                <br>
                C:<br>
                Any P found; D<br>
                <br>
                D:<br>
                Any N; D<br>
                None P notfound; D<br>
                <br>
                F(B,C,a):<br>
                e L; F1(B,C,a)<br>
                Not e L; F(B,C,a)<br>
                None L; F(B,C,a)<br>
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
    |]

transTableW :: TransitionTable -> Widget
transTableW TransTable {symbolTable= syt, stateTable=stt, transTable =tt} = [whamlet|
                                                                                    <table .table .table-bordered >
                                                                                       <tr>
                                                                                         <td> M-config
                                                                                         <td> Read symbol
                                                                                         <td> Printed symbol
                                                                                         <td> Movement
                                                                                         <td> Next m-config
                                                                                       $forall r <- rule <$> reverse (rearrange (M.toList tt))
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
                                               <td rowspan=#{show $ (length clauses) + 1}> #{show st} (#{show $ stt ! st})
                                             $forall cl <- clause <$> clauses
                                                ^{cl}
                                             |]
    clause (sy1, Action sy2 d, st) = [whamlet|
                                            <tr>
                                              <td> #{syt ! sy1}
                                              <td> #{syt ! sy2}
                                              <td> #{show d}
                                              <td> #{show st} (#{show $ stt ! st})
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
           (Right a) -> (case stepN 100 (Config 0 (transTable v) [3,3] [] 0) of
                            (Just x) -> configW (symbolTable v) x
                            Nothing -> [whamlet|<p> something went wrong :(|]) >> (transTableW v)
                            where
                              v = uncurry expand a
           (Left  v) -> [whamlet|<p> #{show v}]
         )
       _ -> defaultLayout [whamlet|<p>error|]
