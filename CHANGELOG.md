hinotify
======

hinotify-0.3.10
---------------

- Allow async-2.2

hinotify-0.3.9
--------------

Patches contributed by Simon Marlow marlowsd@gmail.com

- Don't run callbacks in `mask_`.

  It prevented the callback threads from receiving StackOverflow, amongst other things.

- Synchronous `killThread`.

  `killThread` will now wait for the callback dispatcher threads to finish.

- Bug fixes

  https://github.com/kolmodin/hinotify/pull/23

hinotify-0.3.8
--------------

- Use file system encoding for file names.

  When run in a locale like LANG=C, this ensures that the filename is encoded
  as a filename, so that arbitrary bytes in it will round-trip correctly,
  rather than being stripped out.

  Fixes https://github.com/kolmodin/hinotify/issues/13

  Patch contributed by Joey Hess joeyh@joeyh.name
