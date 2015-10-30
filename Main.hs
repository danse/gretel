import GHCJS.DOM( runWebGUI, webViewGetDomDocument )
import GHCJS.DOM.EventM( uiWhich, EventM, newListener, addListener, target )
import GHCJS.DOM.Document( getBody, getElementById, createElement, createTextNode, Document )
import GHCJS.DOM.Element( keyUp )
import GHCJS.DOM.Node( appendChild )
import GHCJS.DOM.HTMLInputElement( HTMLInputElement, getValue )
import GHCJS.DOM.KeyboardEvent( KeyboardEvent )
import GHCJS.DOM.Types( toJSString, Window, IsDocument, Node )
import Control.Monad.IO.Class

appendLine :: (GHCJS.DOM.Types.IsDocument d, Control.Monad.IO.Class.MonadIO m) => d -> [Char] -> m (Maybe GHCJS.DOM.Types.Node)
appendLine doc value = do
  Just append <- getElementById doc "report"
  Just newLine <- createElement doc (Just "p")
  text <- createTextNode doc (value :: [Char])
  appendChild newLine text
  appendChild append (Just newLine)

onKeyUp :: Document -> EventM HTMLInputElement KeyboardEvent ()
onKeyUp doc = do
  key <- uiWhich
  Just t <- target
  Just value <- getValue t
  if (key == 13)
    then
    do
      appendLine doc value
      return ()
    else return ()
    
interface :: GHCJS.DOM.Types.Window -> IO ()
interface view = do
  Just doc <- webViewGetDomDocument view
  Just body <- getBody doc
  Just input <- getElementById doc "input"
  listener <- newListener (onKeyUp doc)
  addListener input keyUp listener False
  
main = runWebGUI interface
