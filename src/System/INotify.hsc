-----------------------------------------------------------------------------
-- |
-- Module      :  System.INotify
-- Copyright   :  (c) Lennart Kolmodin 2006
-- License     :  GPL
-- Maintainer  :  kolmodin@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  hc portable, x86 linux only
--
-- A Haskell binding to INotify.
-- See <http://www.kernel.org/pub/linux/kernel/people/rml/inotify/>.
--
-----------------------------------------------------------------------------

module System.INotify
    ( inotify_init
    , inotify_add_watch
    , inotify_rm_watch
    , INotify
    , WatchDescriptor
    , Event(..)
    , EventVariety(..)
    ) where

#include "inotify.h"

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
import System.IO
import System.Posix.Internals
import Prelude hiding (lookup)

import System.INotify.Masks

type FD = CInt
type WD = CInt
type Cookie = CUInt
type Masks = CUInt

type WDEventQueue = [(Cookie, FDEvent -> Event)]
type EventMap = Map WD (Event -> IO ())
type WDEvent = (WD, Event)

data INotify = INotify Handle FD (MVar EventMap)
data WatchDescriptor = WatchDescriptor Handle WD deriving Eq

data FDEvent = FDEvent WD Masks Cookie (Maybe String) deriving Show

data Event = 
    -- | A file was accessed. @Accessed isDirectory file@
      Accessed 
        Bool
        (Maybe FilePath)
    -- | A file was modified. @Modified isDiroctory file@
    | Modified    Bool (Maybe FilePath)
    -- | A files attributs where changed. @Attributes isDirectory file@
    | Attributes  Bool (Maybe FilePath)
    -- | A file was closed. @Closed isDirectory wasWritable file@
    | Closed
        Bool
        Bool
        (Maybe FilePath)
    -- | A file was opened. @Opened isDirectory maybeFilePath@
    | Opened
        Bool
        (Maybe FilePath)
    -- | A file was moved. @Moved isDirectory from to@
    | Moved Bool FilePath FilePath
    -- | A file was created. @Created isDirectory file@
    | Created Bool FilePath
    -- | A file was deleted. @Deleted isDirectory file@
    | Deleted Bool FilePath
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
    | Close Bool{-write-}
    | Open
    | Move
    | Create
    | Delete Bool{-self-}
    --    | OnlyDir
    --    | NoSymlink
    --    | MaskAdd
    | OneShot
    deriving Eq

instance Show INotify where
    show (INotify _ fd _) =
        showString "<inotify fd=" . 
        shows fd $ ">"

instance Show WatchDescriptor where
    show (WatchDescriptor _ wd) = showString "<wd=" . shows wd $ ">"

inotify_init :: IO INotify
inotify_init = do
    fd <- c_inotify_init
    em <- newMVar Map.empty
    let desc = showString "<inotify handle, fd=" . shows fd $ ">"
    h <- openFd (fromIntegral fd) (Just Stream) False{-is_socket-} desc ReadMode True{-binary-}
    inotify_start_thread h em
    return (INotify h fd em)

inotify_add_watch :: INotify -> [EventVariety] -> FilePath -> (Event -> IO ()) -> IO WatchDescriptor
inotify_add_watch (INotify h fd em) masks fp cb = do
    let mask = joinMasks (map eventVarietyToMask masks)
    em' <- takeMVar em
    wd <- withCString fp $ \fp_c ->
              c_inotify_add_watch (fromIntegral fd) fp_c mask
    let event = \e -> do
            when (OneShot `elem` masks) $
              modifyMVar_ em (return . Map.delete wd)
            cb e
    putMVar em (Map.insert wd event em')
    return (WatchDescriptor h wd)
    where
    eventVarietyToMask ev =
        case ev of
            Access -> inAccess
            Modify -> inModify
            Attrib -> inAttrib
            Close True -> inCloseWrite
            Close False -> inCloseNowrite
            Open -> inOpen
            Move -> inMove
            Create -> inCreate
            Delete True -> inDeleteSelf
            Delete False -> inDelete
            OneShot -> inOneshot
            

inotify_rm_watch :: INotify -> WatchDescriptor -> IO ()
inotify_rm_watch (INotify _ fd em) (WatchDescriptor _ wd) = do
    c_inotify_rm_watch (fromIntegral fd) wd
    modifyMVar_ em (return . Map.delete wd)

read_events :: Handle -> WDEventQueue -> IO ([WDEvent], WDEventQueue)
read_events h wdEventQueue = 
    let maxRead = 16385 in
    allocaBytes maxRead $ \buffer -> do
        hWaitForInput h (-1)  -- wait forever
        r <- hGetBufNonBlocking h buffer maxRead
        read_events' buffer r wdEventQueue
    where
    read_events' :: Ptr a -> Int -> WDEventQueue -> IO ([WDEvent], WDEventQueue)
    read_events' _ r q |  r <= 0 = return ([], q)
    read_events' ptr r queue = do
        wd     <- (#peek struct inotify_event, wd)     ptr :: IO CInt
        mask   <- (#peek struct inotify_event, mask)   ptr :: IO CUInt
        cookie <- (#peek struct inotify_event, cookie) ptr :: IO CUInt
        len    <- (#peek struct inotify_event, len)    ptr :: IO CUInt
        nameM  <- if len == 0
                    then return Nothing
                    else fmap Just $ peekCString ((#ptr struct inotify_event, name) ptr)
        let event_size = (#size struct inotify_event) + (fromIntegral len) 
            event = (FDEvent wd mask cookie nameM)
            (eventM,queue') = connectEvents queue event
        (rest, queue'') <- read_events' (ptr `plusPtr` event_size) (r - event_size) queue'
        return . flip (,) queue'' $ maybe rest (:rest) eventM
    connectEvents :: WDEventQueue
                  -> FDEvent 
                  -> (Maybe WDEvent, WDEventQueue)
    connectEvents queue fdevent@(FDEvent wd mask cookie nameM)
        | isSet inAccess = only $ Accessed (isSet inIsdir) nameM
        | isSet inModify = only $ Modified (isSet inIsdir) nameM
        | isSet inAttrib = only $ Attributes (isSet inIsdir) nameM
        | isSet inClose  = only $ Closed (isSet inIsdir) (isSet inCloseWrite) nameM
        | isSet inOpen   = only $ Opened (isSet inIsdir) nameM
        | isSet inMovedFrom = intoQ $ \(FDEvent _ _ _ (Just nameTo)) ->
            -- the parameter is the other event with the same cookie
            Moved (isSet inIsdir) name nameTo
        | isSet inMovedTo = joinWithQ
        | isSet inCreate = only $ Created (isSet inIsdir) name
        | isSet inDelete = only $ Deleted (isSet inIsdir) name
        | isSet inDeleteSelf = only DeletedSelf
        | isSet inUnmount = only Unmounted
        | isSet inQOverflow = only QOverflow
        | isSet inIgnored = only Ignored
        | otherwise = only $ Unknown fdevent
        where
        only :: Event -> (Maybe WDEvent, WDEventQueue)
        only e = (Just $ (wd, e), queue)
        intoQ :: (FDEvent -> Event) -> (Maybe WDEvent, WDEventQueue)
        intoQ eWait = (Nothing, (cookie, eWait):queue)
        joinWithQ :: (Maybe WDEvent, WDEventQueue)
        joinWithQ =
            let (Just e, queue') = lookupAndDelete cookie queue
            in (Just (wd, e fdevent), queue')
        isSet bits = maskIsSet bits mask
        name = fromJust nameM
        lookupAndDelete _ [] = (Nothing, [])
        lookupAndDelete k' ((x@(k,v)):xs)
            | k == k' = (Just v, xs)
            | otherwise = let (v',xs') = lookupAndDelete k' xs in (v', x:xs')
        
       
inotify_start_thread :: Handle -> MVar EventMap -> IO ()
inotify_start_thread h em = do
    forkIO start_thread
    return ()
    where
    start_thread :: IO ()
    start_thread = do
        let loop queue = do
            (events, queue') <- read_events h queue
            mapM_ runHandler events
            loop queue'
        loop []
    runHandler :: WDEvent -> IO ()
    runHandler (wd, event) = do 
        handlers <- readMVar em
        let handlerM = Map.lookup wd handlers
        case handlerM of
          Nothing -> putStrLn "runHandler: couldn't find handler" -- impossible?
                                                                  -- no.  qoverflow has wd=-1
          Just handler -> handler event
        

-- TODO:
-- Until I get the compilation right, this is a workaround.
-- The preferred way is to used the commented out code, but I can't get it
-- to link.
-- Loading package HINotify-0.1 ... linking ... ghc-6.4.1: /usr/local/lib/HINotify-0.1/ghc-6.4.1/HSHINotify-0.1.o: unknown symbol `inotify_rm_watch'


{-
foreign import ccall unsafe "inotify-syscalls.h inotify_init" c_inotify_init :: IO CInt
foreign import ccall unsafe "inotify-syscalls.h inotify_add_watch" c_inotify_add_watch :: CInt -> CString -> CUInt -> IO CInt
foreign import ccall unsafe "inotify-syscalls.h inotify_rm_watch" c_inotify_rm_watch :: CInt -> CInt -> IO CInt
-}

c_inotify_init :: IO CInt
c_inotify_init = syscall1 __NR_inotify_init

c_inotify_add_watch :: CInt -> CString -> CUInt -> IO CInt
c_inotify_add_watch = syscall4 __NR_inotify_add_watch

c_inotify_rm_watch :: CInt -> CInt -> IO CInt
c_inotify_rm_watch = syscall3 __NR_inotify_rm_watch

__NR_inotify_init      = 291
__NR_inotify_add_watch = 292
__NR_inotify_rm_watch  = 293

foreign import ccall "syscall" syscall1 :: CInt -> IO CInt
foreign import ccall "syscall" syscall3 :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "syscall" syscall4 :: CInt -> CInt -> CString -> CUInt -> IO CInt
