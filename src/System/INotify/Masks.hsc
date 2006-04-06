module System.INotify.Masks
    ( inAccess
    , inModify
    , inAttrib
    , inCloseWrite
    , inCloseNowrite
    , inOpen
    , inMovedFrom
    , inMovedTo
    , inCreate
    , inDelete
    , inDeleteSelf
    , inUnmount
    , inQOverflow
    , inIgnored
    , inClose
    , inMove
    , inIsdir
    , inOneshot
    , maskIsSet
    , Mask
    ) where

import Data.Bits
import Data.Maybe
import Foreign.C.Types

#include "inotify.h"

data Mask
    = UserSpace CUInt
    | Extra     CUInt
    | Helper    CUInt
    | Special   CUInt
    deriving Eq

maskIsSet :: Mask -> CUInt -> Bool
maskIsSet mask cuint =
    value mask .&. cuint > 0
    where
    value (UserSpace i) = i
    value (Extra i) = i
    value (Helper i) = i
    value (Special i) = i

instance Show Mask where
    show mask =
        fromJust $ lookup mask [ 
            (inAccess, "IN_ACCESS"),
            (inModify, "IN_MODIFY"),
            (inAttrib, "IN_ATTRIB"),
            (inCloseWrite, "IN_CLOSE_WRITE"),
            (inCloseNowrite, "IN_CLOSE_NOWRITE"),
            (inOpen, "IN_OPEN"),
            (inMovedFrom, "IN_MOVED_FROM"),
            (inMovedTo, "IN_MOVED_TO"),
            (inCreate, "IN_CREATE"),
            (inDelete, "IN_DELETE"),
            (inDeleteSelf, "IN_DELETE_SELF"),
            (inUnmount, "IN_UNMOUNT"),
            (inQOverflow, "IN_Q_OVERFLOW"),
            (inIgnored, "IN_IGNORED"),
            (inClose, "IN_CLOSE"),
            (inMove, "IN_MOVE"),
            (inIsdir, "IN_ISDIR"),
            (inOneshot, "IN_ONESHOT")]

#enum Mask, UserSpace, IN_ACCESS, IN_MODIFY, IN_ATTRIB, IN_CLOSE_WRITE
#enum Mask, UserSpace, IN_CLOSE_NOWRITE, IN_OPEN, IN_MOVED_FROM, IN_MOVED_TO
#enum Mask, UserSpace, IN_CREATE, IN_DELETE, IN_DELETE_SELF

#enum Mask, Extra, IN_UNMOUNT, IN_Q_OVERFLOW, IN_IGNORED

#enum Mask, Helper, IN_CLOSE, IN_MOVE

#enum Mask, Special, IN_ISDIR, IN_ONESHOT

{-
/* the following are legal, implemented events that user-space can watch for */
#define IN_ACCESS               0x00000001      /* File was accessed */
#define IN_MODIFY               0x00000002      /* File was modified */
#define IN_ATTRIB               0x00000004      /* Metadata changed */
#define IN_CLOSE_WRITE          0x00000008      /* Writtable file was closed */
#define IN_CLOSE_NOWRITE        0x00000010      /* Unwrittable file closed */
#define IN_OPEN                 0x00000020      /* File was opened */
#define IN_MOVED_FROM           0x00000040      /* File was moved from X */
#define IN_MOVED_TO             0x00000080      /* File was moved to Y */
#define IN_CREATE               0x00000100      /* Subfile was created */
#define IN_DELETE               0x00000200      /* Subfile was deleted */
#define IN_DELETE_SELF          0x00000400      /* Self was deleted */

/* the following are legal events.  they are sent as needed to any watch */
#define IN_UNMOUNT              0x00002000      /* Backing fs was unmounted */
#define IN_Q_OVERFLOW           0x00004000      /* Event queued overflowed */
#define IN_IGNORED              0x00008000      /* File was ignored */

/* helper events */
#define IN_CLOSE                (IN_CLOSE_WRITE | IN_CLOSE_NOWRITE) /* close */
#define IN_MOVE                 (IN_MOVED_FROM | IN_MOVED_TO) /* moves */

/* special flags */
#define IN_ISDIR                0x40000000      /* event occurred against dir */
#define IN_ONESHOT              0x80000000      /* only send event once */
-}
