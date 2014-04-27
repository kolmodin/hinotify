hinotify: inotify for Haskell
=============================

About
-----

hinotify, a library to [inotify] which has been part of the Linux kernel
since 2.6.13.

inotify provides file system event notification, simply add a watcher to
a file or directory and get an event when it is accessed or modified.

This module is named `hinotify`.

See example code in the `examples` directory, distributed with the source
code.

[inotify]: http://www.kernel.org/pub/linux/kernel/people/rml/inotify/

News
----

**hinotify 0.3.7**

* Bug fix: When registerering a new watch on a path which is already watched,
  don't overwrite the event listener from the previous watch.

**hinotify 0.3.2**

* Make each `WatchDescriptor` contain its `INotify`. Changes to the function types:

>      -removeWatch :: INotify -> WatchDescriptor -> IO ()
>      +removeWatch :: WatchDescriptor -> IO ()

* Fix typo in declaration of `Deleted` in `data Event`;

>      - { isDirecotry :: Bool
>      + { isDirectory :: Bool

**hinotify 0.3.1**

* Use `inotify.h` from `glibc` rather than from the linux headers, as
      recommended upstream.

**hinotify 0.3**

* Compiles with GHC 6.12, GHC 6.10.4, GHC 6.8.2 and GHC 6.6.1

**hinotify 0.2**

* Updates to the API
    - Function names is now in semiCamelCase
    - Restructure event parameters to make it more consistent
* Small test suit in `tests/`
* Compiles with GHC 6.8.2 and GHC 6.6.1
* Requires Cabal 1.2

**hinotify 0.1**
:   Initial release

API
---

The API basically consists of:

```haskell
initINotify :: IO INotify
addWatch :: INotify
         -> [EventVariety]   -- different events to listen on
         -> FilePath         -- file/directory to watch
         -> (Event -> IO ()) -- event handler
         -> IO WatchDescriptor
removeWatch :: WatchDescriptor -> IO ()
```

A sample program:

```haskell
import System.Directory
import System.IO

import System.INotify

main :: IO ()
main = do
  inotify <- initINotify
  print inotify
  home <- getHomeDirectory
  wd <- addWatch
          inotify
          [Open,Close,Access,Modify,Move]
          home
          print
  print wd
  putStrLn "Listens to your home directory. Hit enter to terminate."
  getLine
  removeWatch wd
```

Download
--------

The code is available via the [homepage], and via darcs:

    git clone https://github.com/kolmodin/hinotify.git

The [API] is available online.

I'm most grateful for feedback on the API, and what else you might have to
suggest.

Author
------

Lennart Kolmodin

`kolmodin at gmail.com`

Legal
-----

This software is released under a BSD-style license. See LICENSE for
more details.

Copyright &copy; 2007-2012 Lennart Kolmodin

[homepage]: https://github.com/kolmodin/hinotify.git

[API]: http://hackage.haskell.org/packages/archive/hinotify/latest/doc/html/System-INotify.html
