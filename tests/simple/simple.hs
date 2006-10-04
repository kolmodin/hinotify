module Main where

import System.Directory
import System.IO

import System.INotify
main :: IO ()
main = do
    inotify <- inotify_init
    print inotify
    home <- getHomeDirectory
    wd <- inotify_add_watch inotify [Open,Close,Access,Modify,Move] home print
    print wd
    putStrLn "Listens to your home directory. Hit enter to terminate."
    getLine
    inotify_rm_watch inotify wd
