import GHCJS.DOM( runWebGUI, webViewGetDomDocument )
import GHCJS.DOM.EventM( uiWhich, EventM, newListener, addListener, target )
import GHCJS.DOM.Document( getBody, getElementById, getElementsByTagName, createElement, createTextNode, Document )
import GHCJS.DOM.Element( keyUp )
import GHCJS.DOM.Node( appendChild, getFirstChild, insertBefore )
import GHCJS.DOM.HTMLInputElement( HTMLInputElement, getValue, setValue )
import GHCJS.DOM.KeyboardEvent( KeyboardEvent )
import GHCJS.DOM.Types( toJSString, Window, IsDocument, Node, Element )
import Control.Monad.IO.Class( MonadIO )

prependChild parent child = do
  f <- getFirstChild parent
  insertBefore parent child f

initPage :: Document -> IO ()
initPage doc = do
  Just body <- getBody doc
  Just input <- createElement doc (Just "input")
  Just report <- createElement doc (Just "div")
  appendChild body (Just input)
  appendChild body (Just report)
  listener <- newListener (onKeyUp doc report)
  addListener input keyUp listener False

appendLine :: MonadIO m => Document -> Element -> [Char] -> m (Maybe Node)
appendLine doc report value = do
  Just newLine <- createElement doc (Just "p")
  text <- createTextNode doc (value :: [Char])
  appendChild newLine text
  prependChild report (Just newLine)

onKeyUp :: Document -> Element -> EventM HTMLInputElement KeyboardEvent ()
onKeyUp doc report = do
  key <- uiWhich
  Just t <- target
  Just value <- getValue t
  if (key == 13)
    then
    do
      appendLine doc report value
      setValue t (Just "")
      return ()
    else return ()
    
interface :: GHCJS.DOM.Types.Window -> IO ()
interface view = do
  Just doc <- webViewGetDomDocument view
  initPage doc
  
main = runWebGUI interface
