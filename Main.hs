{-# LANGUAGE JavaScriptFFI #-}

import GHCJS.DOM( runWebGUI, webViewGetDomDocument )
import GHCJS.DOM.EventM( uiWhich, EventM, newListener, addListener, target )
import GHCJS.DOM.Document( getBody, getElementById, getElementsByTagName, createElement, createTextNode, Document )
import GHCJS.DOM.Element( keyUp, setClassName )
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
import GHCJS.DOM.Window( getLocalStorage )
import GHCJS.DOM.Storage( setItem, getItem, Storage )
import Data.List( groupBy )
import Control.Concurrent
import Control.Monad( forever )
import GHCJS.Types( JSVal )
import GHCJS.Prim( fromJSString )

foreign import javascript unsafe "Date.now()+''" dateNow :: IO (JSVal)

prependChild parent child = do
  f <- getFirstChild parent
  insertBefore parent child f

serializeRecords :: Maybe [Char] -> [Char] -> [Char]
serializeRecords records newRecord = 
  maybe newRecord (++ [separator] ++ clean) records
  where separator = ','
        clean = filter (/= separator) newRecord

parseRecords :: Maybe [Char] -> [[Char]]
parseRecords = (map (filter (/= ','))) . (maybe [] (groupBy (\x y -> y /= ',')))
  
-- i don't get all these concerns about simultaneity. ideally i would
-- like two simultaneous events to just happen one after the other, in
-- any order. here i will just pick one and drop the other
getFirst = unionWith $ \ x y -> x

initPage :: Document -> Storage -> IO ()
initPage doc storage = do
  Just body <- getBody doc
  Just wrapper <- createElement doc (Just "div")
  setClassName wrapper "wrapper"
  Just container <- createElement doc (Just "div")
  setClassName container "container"
  Just input <- createElement doc (Just "input")
  Just report <- createElement doc (Just "div")
  appendChild body (Just wrapper)
  appendChild wrapper (Just container)
  appendChild container (Just input)
  appendChild container (Just report)
  (addEnterHandler, fireEnter) <- newAddHandler
  (addStoredHandler, fireStored) <- newAddHandler
  (addTimeHandler, fireTime) <- newAddHandler
  let onKeyUp :: EventM [Char] KeyboardEvent ()
      onKeyUp = do
        Just t <- target
        Just value <- getValue t
        key <- uiWhich
        if (key == 13)
          then
          do
            lift $ fireEnter value
            setValue t (Just "")
            return ()
          else
          return ()
  threadId <- forkIO $ forever $ do
    threadDelay (60 * (1000 * 1000)) -- one minute in microseconds
    milliseconds <- dateNow
    fireTime (read (fromJSString milliseconds) :: Integer)
  listener <- newListener onKeyUp
  records <- getItem storage "records"
  addListener input keyUp listener False
  let showRecord :: Handler [Char]
      showRecord value = do
        appendLine doc report value
  let storeRecord :: Handler [Char]
      storeRecord value = do
        records <- getItem storage "records"
        setItem storage "records" (serializeRecords records value)
  let networkDescription :: MomentIO ()
      networkDescription = do
        enter <- fromAddHandler $ addEnterHandler
        stored <- fromAddHandler $ addStoredHandler
        milliseconds <- fromAddHandler $ addTimeHandler
        reactimate $ fmap print milliseconds
        reactimate $ fmap storeRecord enter
        reactimate $ fmap showRecord (getFirst enter stored)
  network <- compile networkDescription
  actuate network
  sequence (fmap fireStored (parseRecords records))
  return ()


appendLine :: MonadIO m => Document -> Element -> [Char] -> m ()
appendLine doc report value = do
  Just newLine <- createElement doc (Just "p")
  text <- createTextNode doc (value :: [Char])
  appendChild newLine text
  prependChild report (Just newLine)
  return ()

interface :: GHCJS.DOM.Types.Window -> IO ()
interface win = do
  Just doc <- webViewGetDomDocument win
  Just storage <- getLocalStorage win
  initPage doc storage
  
main = runWebGUI interface
