import GHCJS.DOM( runWebGUI, webViewGetDomDocument )
import GHCJS.DOM.EventM( uiWhich, EventM, newListener, addListener, target )
import GHCJS.DOM.Document( getBody, getElementById, createElement, createTextNode, Document )
import GHCJS.DOM.Element( keyUp )
import GHCJS.DOM.Node( appendChild )
import GHCJS.DOM.HTMLInputElement( HTMLInputElement, getValue )
import GHCJS.DOM.KeyboardEvent( KeyboardEvent )
import GHCJS.DOM.Types( toJSString )

onKeyUp :: Document -> EventM HTMLInputElement KeyboardEvent ()
onKeyUp doc = do
  key <- uiWhich
  if (key == 13)
    then do
      Just t <- target
      Just value <- getValue t
      Just append <- getElementById doc "report"
      Just newLine <- createElement doc (Just "p")
      text <- createTextNode doc (value :: [Char])
      appendChild newLine text
      appendChild append (Just newLine)
      return ()
    else do
      return ()
    

interface view = do
  Just doc <- webViewGetDomDocument view
  Just body <- getBody doc
  Just input <- getElementById doc "input"
  listener <- newListener (onKeyUp doc)
  addListener input keyUp listener False
  
main = runWebGUI interface
