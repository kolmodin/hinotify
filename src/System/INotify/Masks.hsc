module System.INotify.Masks
    ( inAccess
    , inModify
    , inAttrib
    , inCloseWrite
    , inCloseNowrite
    , inOpen
    , inMovedFrom
    , inMovedTo
    , inMoveSelf
    , inCreate
    , inDelete
    , inDeleteSelf
    , inUnmount
    , inQOverflow
    , inIgnored
    , inClose
    , inMove
    , inOnlydir
    , inDontFollow
    , inMaskAdd
    , inIsdir
    , inOneshot
    , inAllEvents
    , maskIsSet
    , joinMasks
    , Mask
    ) where

import Data.Bits
import Data.Maybe
import Foreign.C.Types

#include "sys/inotify.h"

data Mask
    = UserSpace CUInt
    | Extra     CUInt
    | Helper    CUInt
    | Special   CUInt
    | All       CUInt
    deriving (Eq,Ord)

maskIsSet :: Mask -> CUInt -> Bool
maskIsSet mask cuint =
    value mask .&. cuint > 0
    
value :: Mask -> CUInt
value (UserSpace i) = i
value (Extra i) = i
value (Helper i) = i
value (Special i) = i
value (All i) = i

instance Show Mask where
    show mask =
        fromJust $ lookup mask [ 
            (inAccess, "IN_ACCESS"),
            (inModify, "IN_MODIFY"),
            (inAttrib, "IN_ATTRIB"),
            (inClose,  "IN_CLOSE"),
            (inCloseWrite, "IN_CLOSE_WRITE"),
            (inCloseNowrite, "IN_CLOSE_NOWRITE"),
            (inOpen, "IN_OPEN"),
            (inMove, "IN_MOVE"),
            (inMovedFrom, "IN_MOVED_FROM"),
            (inMovedTo, "IN_MOVED_TO"),
            (inMoveSelf, "IN_MOVE_SELF"),
            (inCreate, "IN_CREATE"),
            (inDelete, "IN_DELETE"),
            (inDeleteSelf, "IN_DELETE_SELF"),
            (inUnmount, "IN_UNMOUNT"),
            (inQOverflow, "IN_Q_OVERFLOW"),
            (inIgnored, "IN_IGNORED"),
            (inClose, "IN_CLOSE"),
            (inIsdir, "IN_ISDIR"),
            (inOneshot, "IN_ONESHOT")]

joinMasks :: [Mask] -> CUInt
joinMasks = foldr (.|.) 0 . map value

#enum Mask, UserSpace, IN_ACCESS, IN_MODIFY, IN_ATTRIB, IN_CLOSE_WRITE
#enum Mask, UserSpace, IN_CLOSE_NOWRITE, IN_OPEN, IN_MOVED_FROM, IN_MOVED_TO
#enum Mask, UserSpace, IN_CREATE, IN_DELETE, IN_DELETE_SELF, IN_MOVE_SELF

#enum Mask, Extra, IN_UNMOUNT, IN_Q_OVERFLOW, IN_IGNORED

#enum Mask, Helper, IN_CLOSE, IN_MOVE

#enum Mask, Special, IN_ONLYDIR, IN_DONT_FOLLOW, IN_MASK_ADD, IN_ISDIR
#enum Mask, Special, IN_ONESHOT

#enum Mask, All, IN_ALL_EVENTS
