import GHCJS.DOM( runWebGUI, webViewGetDomDocument )
import GHCJS.DOM.EventM( uiWhich, EventM, newListener, addListener, target )
import GHCJS.DOM.Document( getBody, getElementById, getElementsByTagName, createElement, createTextNode, Document )
import GHCJS.DOM.Element( keyUp )
import GHCJS.DOM.Node( appendChild, getFirstChild, insertBefore )
import GHCJS.DOM.HTMLInputElement( HTMLInputElement, getValue, setValue )
import GHCJS.DOM.KeyboardEvent( KeyboardEvent )
import GHCJS.DOM.Types( toJSString, Window, IsDocument, Node, Element )
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.Trans.Class( lift )
import Data.Functor( fmap )
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Control.Event.Handler( Handler, newAddHandler )
import GHCJS.DOM.EventTargetClosures( SaferEventListener )

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
  (addHandler, fire) <- newAddHandler
  let onKeyUp :: EventM t KeyboardEvent ()
      onKeyUp = do
        Just t <- target
        Just value <- getValue t
        key <- uiWhich
        if (key == 13)
          then
          do
            lift $ fire value
            setValue t (Just "")
            return ()
          else
          return ()

  listener <- newListener onKeyUp
  addListener input keyUp listener False
  let handler :: Handler [Char]
      handler = appendLine doc report
  let networkDescription :: MomentIO ()
      networkDescription = do
        esubmit <- fromAddHandler $ addHandler
        reactimate $ fmap handler esubmit
  network <- compile networkDescription
  actuate network


appendLine :: MonadIO m => Document -> Element -> [Char] -> m ()
appendLine doc report value = do
  Just newLine <- createElement doc (Just "p")
  text <- createTextNode doc (value :: [Char])
  appendChild newLine text
  prependChild report (Just newLine)
  return ()

interface :: GHCJS.DOM.Types.Window -> IO ()
interface view = do
  Just doc <- webViewGetDomDocument view
  initPage doc
  
main = runWebGUI interface
