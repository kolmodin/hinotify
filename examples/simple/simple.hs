module Main where

import qualified Data.ByteString.Char8         as C
import           System.Directory
import           System.IO

import           System.INotify

main :: IO ()
main = do
    inotify <- initINotify
    print inotify
    home <- getHomeDirectory
    wd <- addWatch inotify [Open,Close,Access,Modify,Move] (C.pack home) print
    print wd
    putStrLn "Listens to your home directory. Hit enter to terminate."
    getLine
    removeWatch wd
