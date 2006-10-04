{- 
Duncan Coutts 2006

Doesn't compile with gtk2hs 0.9.10, you'll need the darcs version.

-}

import qualified Data.Map as Map
import System.Directory

import Data.IORef
import Control.Monad (liftM)
import Ix (inRange)

import System.INotify

import Graphics.UI.Gtk
import Graphics.UI.Gtk.TreeList.Types (TypedTreeModelClass)
import Graphics.UI.Gtk.TreeList.TreeModel (TreeModelFlags(TreeModelListOnly))
import Graphics.UI.Gtk.TreeList.CustomStore
import Graphics.UI.Gtk.TreeList.TreeIter

import Control.Concurrent

instance TypedTreeModelClass (CustomTreeModel a)

dirModelNew :: FilePath -> IO (CustomTreeModel () FilePath)
dirModelNew path = do
  
  dirContents <- getDirectoryContents path
  
  rows <- newIORef (Map.fromList (zip dirContents (repeat ())))

  model <- customTreeModelNew () CustomTreeModelImplementation {
      customTreeModelGetFlags      = return [TreeModelListOnly],
      customTreeModelGetIter       = \[n] -> return (Just (TreeIter 0 (fromIntegral n) 0 0)),
      customTreeModelGetPath       = \(TreeIter _ n _ _) -> return [fromIntegral n],
      customTreeModelGetRow        = \(TreeIter _ n _ _) ->
                                     readIORef rows >>= \rows -> 
                                     if inRange (0, Map.size rows - 1) (fromIntegral n)
                                       then return (fst $ Map.elemAt (fromIntegral n) rows)
                                       else fail "DirModel.getRow: iter does not refer to a valid entry",

      customTreeModelIterNext      = \(TreeIter _ n _ _) ->
                                     readIORef rows >>= \rows ->
                                        if n >= fromIntegral (Map.size rows) - 1
                                          then return Nothing
                                          else return (Just (TreeIter 0 (n+1) 0 0)),
      customTreeModelIterChildren  = \_ -> return Nothing,
      customTreeModelIterHasChild  = \_ -> return False,
      customTreeModelIterNChildren = \index -> readIORef rows >>= \rows ->
                                           case index of
                                             Nothing -> return $! Map.size rows
                                             _       -> return 0,
      customTreeModelIterNthChild  = \index n -> case index of
                                               Nothing -> return (Just (TreeIter 0 (fromIntegral n) 0 0))
                                               _       -> return Nothing,
      customTreeModelIterParent    = \_ -> return Nothing,
      customTreeModelRefNode       = \_ -> return (),
      customTreeModelUnrefNode     = \_ -> return ()
    }

  notify <- inotify_init
  watch <- inotify_add_watch notify [Move, Create, Delete] path $ \event -> 
    let add file = do 
          index <- atomicModifyIORef rows (\map ->
                     let map' = Map.insert file () map
                      in (map', Map.findIndex file map'))
          treeModelRowInserted model [index] (TreeIter 0 (fromIntegral index) 0 0)
        remove file = do
          index <- atomicModifyIORef rows (\map ->
                     let map' = Map.delete file map
                      in (map', Map.findIndex file map))
          treeModelRowDeleted model [index]

     in case event of
          MovedIn  _ _ file -> add file
          MovedOut _ _ file -> remove file
          Created  _ file -> add file
          Deleted  _ file -> remove file
          _ -> putStrLn $ "other event: " ++ show event

  -- TODO: on destroy model (inotify_rm_watch watch)
  
  return model

main = do
  initGUI
  win <- windowNew
  win `onDestroy` mainQuit

  model <- dirModelNew "/home/kolmodin/Desktop"

  tv <- treeViewNewWithModel model
  win `containerAdd` tv

  tvc <- treeViewColumnNew
  treeViewAppendColumn tv tvc

  text <- cellRendererTextNew
  cellLayoutPackStart tvc text True
  cellLayoutSetAttributes tvc text model
    (\file -> [cellText := file])

  widgetShowAll win
  timeoutAddFull (yield >> return True) priorityDefaultIdle 50
  mainGUI
