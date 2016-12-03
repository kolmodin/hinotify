{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.INotify
-- Copyright   :  (c) Lennart Kolmodin 2006-2012
-- License     :  BSD3
-- Maintainer  :  kolmodin@gmail.com
-- Stability   :  experimental
-- Portability :  hc portable, linux only
--
-- A Haskell binding to INotify.
-- See <http://www.kernel.org/pub/linux/kernel/people/rml/inotify/> and @man
-- inotify@.
--
-- Use 'initINotify' to get a 'INotify', then use 'addWatch' to
-- add a watch on a file or directory. Select which events you're interested
-- in with 'EventVariety', which corresponds to the 'Event' events.
-- 
-- Use 'removeWatch' once you don't want to watch a file any more.
--
-----------------------------------------------------------------------------

module System.INotify
    ( initINotify
    , killINotify
    , withINotify
    , addWatch
    , removeWatch
    , INotify
    , WatchDescriptor
    , Event(..)
    , EventVariety(..)
    , Cookie
    ) where

#include "sys/inotify.h"

import Prelude hiding (init)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception as E hiding (mask)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign.C
import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable
import System.IO
import System.IO.Error

import System.Posix.ByteString.FilePath
import System.Posix.Files.ByteString

import GHC.IO.FD as FD (mkFD)
import GHC.IO.Handle.FD (mkHandleFromFD)
import GHC.IO.Device (IODeviceType(Stream))

import System.INotify.Masks

type FD = CInt
type WD = CInt
type Masks = CUInt

type EventMap = Map WD (Event -> IO ())
type WDEvent = (WD, Event)

data INotify = INotify Handle FD (MVar EventMap) (Async ()) (Async ())
data WatchDescriptor = WatchDescriptor INotify WD deriving Eq

instance Eq INotify where
  (INotify _ fd1 _ _ _) == (INotify _ fd2 _ _ _) = fd1 == fd2

newtype Cookie = Cookie CUInt deriving (Eq,Ord)

data FDEvent = FDEvent WD Masks CUInt{-Cookie-} (Maybe RawFilePath) deriving (Eq, Show)

data Event =
    -- | A file was accessed. @Accessed isDirectory file@
      Accessed
        { isDirectory :: Bool
        , maybeFilePath :: Maybe RawFilePath
        }
    -- | A file was modified. @Modified isDirectory file@
    | Modified
        { isDirectory :: Bool
        , maybeFilePath :: Maybe RawFilePath
        }
    -- | A files attributes where changed. @Attributes isDirectory file@
    | Attributes
        { isDirectory :: Bool
        , maybeFilePath :: Maybe RawFilePath
        }
    -- | A file was closed. @Closed isDirectory file wasWriteable@
    | Closed
        { isDirectory :: Bool
        , maybeFilePath :: Maybe RawFilePath
        , wasWriteable :: Bool
        }
    -- | A file was opened. @Opened isDirectory maybeFilePath@
    | Opened
        { isDirectory :: Bool
        , maybeFilePath :: Maybe RawFilePath
        }
    -- | A file was moved away from the watched dir. @MovedFrom isDirectory from cookie@
    | MovedOut
        { isDirectory :: Bool
        , filePath :: RawFilePath
        , moveCookie :: Cookie
        }
    -- | A file was moved into the watched dir. @MovedTo isDirectory to cookie@
    | MovedIn
        { isDirectory :: Bool
        , filePath :: RawFilePath
        , moveCookie :: Cookie
        }
    -- | The watched file was moved. @MovedSelf isDirectory@
    | MovedSelf
        { isDirectory :: Bool
        }
    -- | A file was created. @Created isDirectory file@
    | Created
        { isDirectory :: Bool
        , filePath :: RawFilePath
        }
    -- | A file was deleted. @Deleted isDirectory file@
    | Deleted
        { isDirectory :: Bool
        , filePath :: RawFilePath
        }
    -- | The file watched was deleted.
    | DeletedSelf
    -- | The file watched was unmounted.
    | Unmounted
    -- | The queue overflowed.
    | QOverflow
    | Ignored
    | Unknown FDEvent
    deriving (Eq, Show)

data EventVariety
    = Access
    | Modify
    | Attrib
    | Close
    | CloseWrite
    | CloseNoWrite
    | Open
    | Move
    | MoveIn
    | MoveOut
    | MoveSelf
    | Create
    | Delete
    | DeleteSelf
    | OnlyDir
    | NoSymlink
    | MaskAdd
    | OneShot
    | AllEvents
    deriving Eq

instance Show INotify where
    show (INotify _ fd _ _ _) =
        showString "<inotify fd=" . 
        shows fd $ ">"

instance Show WatchDescriptor where
    show (WatchDescriptor _ wd) = showString "<wd=" . shows wd $ ">"

instance Show Cookie where
    show (Cookie c) = showString "<cookie " . shows c $ ">"

initINotify :: IO INotify
initINotify = do
    fdint <- throwErrnoIfMinus1 "initINotify" c_inotify_init
    (fd,fd_type) <- FD.mkFD fdint ReadMode (Just (Stream,0,0))
            False{-is_socket-}
            False{-is_nonblock-}
    h <- mkHandleFromFD fd fd_type
           (showString "<inotify handle, fd=" . shows fd $ ">")
           ReadMode
           True  -- make non-blocking.  Otherwise reading uses select(), which
                 -- can fail when there are >=1024 FDs
           Nothing -- no encoding, so binary
    em <- newMVar Map.empty
    (tid1, tid2) <- inotify_start_thread h em
    return (INotify h fdint em tid1 tid2)

addWatch :: INotify -> [EventVariety] -> RawFilePath -> (Event -> IO ()) -> IO WatchDescriptor
addWatch inotify@(INotify _ fd em _ _) masks fp cb = do
    catch_IO (void $
              (if (NoSymlink `elem` masks) then getSymbolicLinkStatus else getFileStatus)
              fp) $ \_ ->
        ioError $ mkIOError doesNotExistErrorType
             "can't watch what isn't there!"
             Nothing
             (Just (show fp))
    let mask = joinMasks (map eventVarietyToMask masks)
    wd <- withFilePath fp $ \fp_c ->
            throwErrnoIfMinus1 "addWatch" $
              c_inotify_add_watch (fromIntegral fd) fp_c mask
    let event = \e -> ignore_failure $ do
            case e of
              -- if the event is Ignored then we know for sure that
              -- this is the last event on that WatchDescriptor
              Ignored -> rm_watch inotify wd
              _       -> return ()
            cb e
    modifyMVar_ em $ \em' -> return (Map.insertWith (liftM2 (>>)) wd event em')
    return (WatchDescriptor inotify wd)
    where
    -- catch_IO is same as catchIOError from base >= 4.5.0.0
    catch_IO :: IO a -> (IOError -> IO a) -> IO a
    catch_IO = E.catch
    eventVarietyToMask ev =
        case ev of
            Access -> inAccess
            Modify -> inModify
            Attrib -> inAttrib
            Close -> inClose
            CloseWrite -> inCloseWrite
            CloseNoWrite -> inCloseNowrite
            Open -> inOpen
            Move -> inMove
            MoveIn -> inMovedTo
            MoveOut -> inMovedFrom
            MoveSelf -> inMoveSelf
            Create -> inCreate
            Delete -> inDelete
            DeleteSelf-> inDeleteSelf
            OnlyDir -> inOnlydir
            NoSymlink -> inDontFollow
            MaskAdd -> inMaskAdd
            OneShot -> inOneshot
            AllEvents -> inAllEvents

    ignore_failure :: IO () -> IO ()
    ignore_failure action = action `E.catch` ignore
      where
      ignore :: SomeException -> IO ()
      ignore e
        | Just ThreadKilled{} <- fromException e = throwIO e
        | otherwise = return ()

removeWatch :: WatchDescriptor -> IO ()
removeWatch (WatchDescriptor (INotify _ fd _ _ _) wd) = do
    _ <- throwErrnoIfMinus1 "removeWatch" $
      c_inotify_rm_watch (fromIntegral fd) wd
    return ()

rm_watch :: INotify -> WD -> IO ()
rm_watch (INotify _ _ em _ _) wd =
    modifyMVar_ em (return . Map.delete wd)

read_events :: Handle -> IO [WDEvent]
read_events h = 
    let maxRead = 16385 in
    allocaBytes maxRead $ \buffer -> do
        _ <- hWaitForInput h (-1)  -- wait forever
        r <- hGetBufNonBlocking h buffer maxRead
        read_events' buffer r
    where
    read_events' :: Ptr a -> Int -> IO [WDEvent]
    read_events' _ r |  r <= 0 = return []
    read_events' ptr r = do
        wd     <- (#peek struct inotify_event, wd)     ptr :: IO CInt
        mask   <- (#peek struct inotify_event, mask)   ptr :: IO CUInt
        cookie <- (#peek struct inotify_event, cookie) ptr :: IO CUInt
        len    <- (#peek struct inotify_event, len)    ptr :: IO CUInt
        nameM  <- if len == 0
                    then return Nothing
                    else do
                        fmap Just $ peekFilePath ((#ptr struct inotify_event, name) ptr)
        let event_size = (#size struct inotify_event) + (fromIntegral len) 
            event = cEvent2Haskell (FDEvent wd mask cookie nameM)
        rest <- read_events' (ptr `plusPtr` event_size) (r - event_size)
        return (event:rest)
    cEvent2Haskell :: FDEvent 
               -> WDEvent
    cEvent2Haskell fdevent@(FDEvent wd mask cookie nameM)
        = (wd, event)
        where
        event
            | isSet inAccess     = Accessed isDir nameM
            | isSet inModify     = Modified isDir nameM
            | isSet inAttrib     = Attributes isDir nameM
            | isSet inClose      = Closed isDir nameM (isSet inCloseWrite)
            | isSet inOpen       = Opened isDir nameM
            | isSet inMovedFrom  = MovedOut isDir name (Cookie cookie)
            | isSet inMovedTo    = MovedIn isDir name (Cookie cookie)
            | isSet inMoveSelf   = MovedSelf isDir
            | isSet inCreate     = Created isDir name
            | isSet inDelete     = Deleted isDir name
            | isSet inDeleteSelf = DeletedSelf
            | isSet inUnmount    = Unmounted
            | isSet inQOverflow  = QOverflow
            | isSet inIgnored    = Ignored
            | otherwise          = Unknown fdevent
        isDir = isSet inIsdir
        isSet bits = maskIsSet bits mask
        name = fromJust nameM

inotify_start_thread :: Handle -> MVar EventMap -> IO (Async (), Async ())
inotify_start_thread h em = do
    chan_events <- newChan
    tid1 <- async (logFailure "dispatcher" (dispatcher chan_events))
    tid2 <- async (logFailure "start_thread" (start_thread chan_events))
    return (tid1,tid2)
    where
    start_thread :: Chan [WDEvent] -> IO ()
    start_thread chan_events = do
        events <- read_events h
        writeChan chan_events events
        start_thread chan_events
    dispatcher :: Chan [WDEvent] -> IO ()
    dispatcher chan_events = do
        events <- readChan chan_events
        mapM_ runHandler events
        dispatcher chan_events
    runHandler :: WDEvent -> IO ()
    runHandler (_,  e@QOverflow) = do -- send overflows to all handlers
        handlers <- readMVar em
        mapM_ ($ e) (Map.elems handlers)
    runHandler (wd, event) = do 
        handlers <- readMVar em
        let handlerM = Map.lookup wd handlers
        case handlerM of
          Nothing -> putStrLn "runHandler: couldn't find handler" -- impossible?
          Just handler -> handler event

    logFailure name io = io `E.catch` \e ->
       case e of
         _ | Just ThreadKilled{} <- fromException e -> return ()
           | otherwise -> hPutStrLn stderr (name ++ " dying: " ++ show e)

killINotify :: INotify -> IO ()
killINotify (INotify h _ _ tid1 tid2) =
    do cancelWait tid1
       cancelWait tid2
       hClose h

cancelWait :: Async a -> IO ()
##if MIN_VERSION_async(2,1,1)
cancelWait = cancel
##else
cancelWait a = do cancel a; void $ waitCatch a
##endif

withINotify :: (INotify -> IO a) -> IO a
withINotify = bracket initINotify killINotify

foreign import ccall unsafe "sys/inotify.h inotify_init" c_inotify_init :: IO CInt
foreign import ccall unsafe "sys/inotify.h inotify_add_watch" c_inotify_add_watch :: CInt -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "sys/inotify.h inotify_rm_watch" c_inotify_rm_watch :: CInt -> CInt -> IO CInt
