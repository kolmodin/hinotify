module Main where

import System.Directory
import System.IO

import System.INotify as INotify

main :: IO ()
main = do
    inotify <- INotify.init
    print inotify
    home <- getHomeDirectory
    wd <- addWatch inotify [Open,Close,Access,Modify,Move] home print
    print wd
    putStrLn "Listens to your home directory. Hit enter to terminate."
    getLine
    removeWatch inotify wd
