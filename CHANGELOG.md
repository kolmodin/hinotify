hinotify
======

hinotify-0.3.8
--------------

- Use file system encoding for file names.

  When run in a locale like LANG=C, this ensures that the filename is encoded
  as a filename, so that arbitrary bytes in it will round-trip correctly,
  rather than being stripped out.

  Fixes https://github.com/kolmodin/hinotify/issues/13

  Patch contributed by Joey Hess joeyh@joeyh.name
