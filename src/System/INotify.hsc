-----------------------------------------------------------------------------
-- |
-- Module      :  System.INotify
-- Copyright   :  (c) Lennart Kolmodin 2006
-- License     :  GPL
-- Maintainer  :  kolmodin@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  hc portable, linux only
--
-- A Haskell binding to INotify.
-- See <http://www.kernel.org/pub/linux/kernel/people/rml/inotify/> and @man
-- inotify@.
--
-- Use 'inotify_init' to get a 'INotify', then use 'inotify_add_watch' to
-- add a watch on a file or directory. Select which events you're interested
-- in with 'EventVariety', which corresponds to the 'Event' events.
-- 
-- Use 'inotify_rm_watch' once you don't want to watch a file any more.
--
-----------------------------------------------------------------------------

module System.INotify
    ( init
    , addWatch
    , removeWatch
    , INotify
    , WatchDescriptor
    , Event(..)
    , EventVariety(..)
    , Cookie
    ) where

#include "inotify.h"

import Prelude hiding (init)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Handle
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Directory
import System.IO
import System.IO.Error
import System.Posix.Internals

import System.INotify.Masks

type FD = CInt
type WD = CInt
type Masks = CUInt

type EventMap = Map WD (Event -> IO ())
type WDEvent = (WD, Event)

data INotify = INotify Handle FD (MVar EventMap)
data WatchDescriptor = WatchDescriptor Handle WD deriving Eq

newtype Cookie = Cookie CUInt deriving (Eq,Ord)

data FDEvent = FDEvent WD Masks CUInt{-Cookie-} (Maybe String) deriving Show

data Event =
    -- | A file was accessed. @Accessed isDirectory file@
      Accessed
        { isDirectory :: Bool
        , maybeFilePath :: Maybe FilePath
        }
    -- | A file was modified. @Modified isDiroctory file@
    | Modified
        { isDirectory :: Bool
        , maybeFilePath :: Maybe FilePath
        }
    -- | A files attributes where changed. @Attributes isDirectory file@
    | Attributes
        { isDirectory :: Bool
        , maybeFilePath :: Maybe FilePath
        }
    -- | A file was closed. @Closed isDirectory wasWritable file@
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
    -- | A file was moved away from the watched dir. @MovedFrom isDirectory from@
    | MovedOut
        { isDirectory :: Bool
        , filePath :: FilePath
        , cookie :: Cookie
        }
    -- | A file was moved into the watched dir. @MovedTo isDirectory to@
    | MovedIn
        { isDirectory :: Bool
        , filePath :: FilePath
        , cookie :: Cookie
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
        { isDirecotry :: Bool
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
    deriving Show

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
    show (INotify _ fd _) =
        showString "<inotify fd=" . 
        shows fd $ ">"

instance Show WatchDescriptor where
    show (WatchDescriptor _ wd) = showString "<wd=" . shows wd $ ">"

instance Show Cookie where
    show (Cookie c) = showString "<cookie " . shows c $ ">"

init :: IO INotify
init = do
    fd <- throwErrnoIfMinus1 "INotify.init" c_inotify_init
    em <- newMVar Map.empty
    let desc = showString "<inotify handle, fd=" . shows fd $ ">"
#if __GLASGOW_HASKELL__ < 608
    h <-  openFd (fromIntegral fd) (Just Stream) False{-is_socket-} desc ReadMode True{-binary-}
#else
    h <-  fdToHandle' (fromIntegral fd) (Just Stream) False{-is_socket-} desc ReadMode True{-binary-}
#endif
    -- h <- fdToHandle fd
    inotify_start_thread h em
    return (INotify h fd em)

addWatch :: INotify -> [EventVariety] -> FilePath -> (Event -> IO ()) -> IO WatchDescriptor
addWatch inotify@(INotify h fd em) masks fp cb = do
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
    em' <- takeMVar em
    wd <- withCString fp $ \fp_c ->
	    throwErrnoIfMinus1 "addWatch" $
              c_inotify_add_watch (fromIntegral fd) fp_c mask
    let event = \e -> do
            when (OneShot `elem` masks) $
              rm_watch inotify wd
            case e of
              -- if the event is Ignored then we know for sure that
              -- this is the last event on that WatchDescriptor
              Ignored -> rm_watch inotify wd
              _       -> return ()
            cb e
    putMVar em (Map.insert wd event em')
    return (WatchDescriptor h wd)
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

removeWatch :: INotify -> WatchDescriptor -> IO ()
removeWatch (INotify _ fd _) (WatchDescriptor _ wd) = do
    throwErrnoIfMinus1 "removeWatch" $
      c_inotify_rm_watch (fromIntegral fd) wd
    return ()

rm_watch :: INotify -> WD -> IO ()
rm_watch (INotify _ _ em) wd =
    modifyMVar_ em (return . Map.delete wd)

read_events :: Handle -> IO [WDEvent]
read_events h = 
    let maxRead = 16385 in
    allocaBytes maxRead $ \buffer -> do
        hWaitForInput h (-1)  -- wait forever
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
       
inotify_start_thread :: Handle -> MVar EventMap -> IO ()
inotify_start_thread h em = do
    chan_events <- newChan
    forkIO (dispatcher chan_events)
    forkIO (start_thread chan_events)
    return ()
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
        flip mapM_ (Map.elems handlers) $ \handler ->
            catch (handler e) (\_ -> return ()) -- supress errors
    runHandler (wd, event) = do 
        handlers <- readMVar em
        let handlerM = Map.lookup wd handlers
        case handlerM of
          Nothing -> putStrLn "runHandler: couldn't find handler" -- impossible?
          Just handler -> catch (handler event) (\_ -> return ())
        
foreign import ccall unsafe "inotify-syscalls.h inotify_init" c_inotify_init :: IO CInt
foreign import ccall unsafe "inotify-syscalls.h inotify_add_watch" c_inotify_add_watch :: CInt -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "inotify-syscalls.h inotify_rm_watch" c_inotify_rm_watch :: CInt -> CInt -> IO CInt
