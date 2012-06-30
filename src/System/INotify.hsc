{-# Language ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.INotify
-- Copyright   :  (c) Lennart Kolmodin 2006
-- License     :  BSD3
-- Maintainer  :  kolmodin@dtek.chalmers.se
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
    , watch
    , removeWatch
    , INotify
    , WatchDescriptor
    , Event(..)
    , EventVariety(..)
    , Cookie
    ) where

#include "sys/inotify.h"

import Prelude hiding (init, catch)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar ()
import Control.Exception (bracket, catch, SomeException)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Directory
import System.IO
import System.IO.Error hiding (catch)
#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Handle.FD (fdToHandle')
import GHC.IO.Device (IODeviceType(Stream))
#else
import GHC.Handle
import System.Posix.Internals
#endif

import System.INotify.Masks

type FD = CInt
type WD = CInt
type Masks = CUInt

type WatchMap = Map WD WatchDescriptor
type WDEvent = (WD, Event)

data INotify = INotify {
     inHandle   :: Handle
   , inFD       :: FD
   , inWatchMap :: (MVar WatchMap)
   , inTid      :: ThreadId
 }

data WatchDescriptor = WatchDescriptor {
    wdInotify :: INotify
  , wdCInt    :: WD
  , wdChan    :: (Chan Event)
  , wdMasks   :: [EventVariety]
  } deriving Eq

instance Eq INotify where
  in1 == in2 = inFD in1 == inFD in2

newtype Cookie = Cookie CUInt deriving (Eq,Ord)

data FDEvent = FDEvent WD Masks CUInt{-Cookie-} (Maybe String) deriving (Eq, Show)

data Event =
    -- | A file was accessed. @Accessed isDirectory file@
      Accessed
        { isDirectory :: Bool
        , maybeFilePath :: Maybe FilePath
        }
    -- | A file was modified. @Modified isDirectory file@
    | Modified
        { isDirectory :: Bool
        , maybeFilePath :: Maybe FilePath
        }
    -- | A files attributes where changed. @Attributes isDirectory file@
    | Attributes
        { isDirectory :: Bool
        , maybeFilePath :: Maybe FilePath
        }
    -- | A file was closed. @Closed isDirectory file wasWriteable@
    | Closed
        { isDirectory :: Bool
        , maybeFilePath :: Maybe FilePath
        , wasWriteable :: Bool
        }
    -- | A file was opened. @Opened isDirectory maybeFilePath@
    | Opened
        { isDirectory :: Bool
        , maybeFilePath :: Maybe FilePath
        }
    -- | A file was moved away from the watched dir. @MovedFrom isDirectory from cookie@
    | MovedOut
        { isDirectory :: Bool
        , filePath :: FilePath
        , moveCookie :: Cookie
        }
    -- | A file was moved into the watched dir. @MovedTo isDirectory to cookie@
    | MovedIn
        { isDirectory :: Bool
        , filePath :: FilePath
        , moveCookie :: Cookie
        }
    -- | The watched file was moved. @MovedSelf isDirectory@
    | MovedSelf
        { isDirectory :: Bool
        }
    -- | A file was created. @Created isDirectory file@
    | Created
        { isDirectory :: Bool
        , filePath :: FilePath
        }
    -- | A file was deleted. @Deleted isDirectory file@
    | Deleted
        { isDirectory :: Bool
        , filePath :: FilePath
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
    show inotify =
        showString "<inotify fd=" . 
        shows (inFD inotify) $ ">"

instance Show WatchDescriptor where
    show wd = showString "<wd=" . shows (wdCInt wd) $ ">"

instance Show Cookie where
    show (Cookie c) = showString "<cookie " . shows c $ ">"

initINotify :: IO INotify
initINotify = do
    fd <- throwErrnoIfMinus1 "initINotify" c_inotify_init
    let desc = showString "<inotify handle, fd=" . shows fd $ ">"
#if __GLASGOW_HASKELL__ < 608
    h <-  openFd (fromIntegral fd) (Just Stream) False{-is_socket-} desc ReadMode True{-binary-}
#else
    h <-  fdToHandle' (fromIntegral fd) (Just Stream) False{-is_socket-} desc ReadMode True{-binary-}
#endif
    wm <- newMVar Map.empty
    tid1 <- inotify_start_thread h wm
    return (INotify h fd wm tid1)

addWatch :: INotify -> [EventVariety] -> FilePath -> (Event -> IO ()) -> IO WatchDescriptor
addWatch inotify masks fp cb = do
  (wd,chan) <- watch inotify masks fp
  _<- forkIO $ do
        ev <- readChan chan
        cb ev `catch` \(_::SomeException) -> return ()
  return wd

watch :: INotify -> [EventVariety] -> FilePath -> IO (WatchDescriptor, Chan Event)
watch inotify masks fp = do
    chan <- newChan
    is_dir <- doesDirectoryExist fp
    when (not is_dir) $ do
        file_exist <- doesFileExist fp
        when (not file_exist) $ do
            -- it's not a directory, and not a file...
            -- it doesn't exist
            ioError $ mkIOError doesNotExistErrorType
                                "can't watch what isn't there"
                                Nothing 
                                (Just fp)
    let mask = joinMasks (map eventVarietyToMask masks)
    wdInt <- withCString fp $ \fp_c ->
              throwErrnoIfMinus1 "addWatch" $
                c_inotify_add_watch (fromIntegral $ inFD inotify) fp_c mask
    let wd = (WatchDescriptor inotify wdInt chan masks)
    modifyMVar_ (inWatchMap inotify) $ \wm -> return (Map.insert wdInt wd wm)
    return (wd, chan)
    where
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

removeWatch :: WatchDescriptor -> IO ()
removeWatch wd = do
    _ <- throwErrnoIfMinus1 "removeWatch" $
      c_inotify_rm_watch (fromIntegral $ inFD $ wdInotify wd) (wdCInt wd)
    return ()

rm_watch :: WatchDescriptor -> IO ()
rm_watch wd =
    modifyMVar_ (inWatchMap $ wdInotify wd) (return . Map.delete (wdCInt wd))

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
                    else fmap Just $ peekCString ((#ptr struct inotify_event, name) ptr)
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
       
inotify_start_thread :: Handle -> MVar WatchMap -> IO ThreadId
inotify_start_thread h watchMap = forkIO $
    read_events h >>= mapM_ chanDispatch
  where
    writeEvent wd = writeChan (wdChan wd)

    chanDispatch :: WDEvent -> IO ()
    chanDispatch (_,  e@QOverflow) = do -- send overflows to all handlers
        wMap <- readMVar watchMap
        flip mapM_ (Map.elems wMap) $ \wd -> writeEvent wd e
    chanDispatch (wdInt, event) = do 
        wMap <- readMVar watchMap
        case Map.lookup wdInt wMap of
          Nothing -> putStrLn "chanDispatch: couldn't find watcher" -- impossible?
          Just wd -> handle wd event
      where
        handle wd e = do
            when (OneShot `elem` wdMasks wd) $
              rm_watch wd
            case e of
              -- if the event is Ignored then we know for sure that
              -- this is the last event on that WatchDescriptor
              Ignored -> rm_watch wd
              _       -> return ()
            writeEvent wd e

killINotify :: INotify -> IO ()
killINotify inotify = killThread (inTid inotify) >> hClose (inHandle inotify)

withINotify :: (INotify -> IO a) -> IO a
withINotify = bracket initINotify killINotify
        
foreign import ccall unsafe "sys/inotify.h inotify_init" c_inotify_init :: IO CInt
foreign import ccall unsafe "sys/inotify.h inotify_add_watch" c_inotify_add_watch :: CInt -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "sys/inotify.h inotify_rm_watch" c_inotify_rm_watch :: CInt -> CInt -> IO CInt
