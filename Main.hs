import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Prelude hiding (FilePath)
import System.Directory
import System.Posix.Directory
import System.Posix.Files
import System.Path
import System.Directory
import System.IO

import DepGraph.CPPIncludes
import DepGraph.DepGraph
import DepGraph.DepMain

import RandomStuff hiding (getDependencies)

----------------------------------------------------------------

normalisePath :: AbsFile -> AbsFile
normalisePath path = (foldl (</>) (asAbsDir "/") (consumeDots elts)) </> fromJust file
    where
      (elts, file) = splitPath path
      consumeDots [] = []
      consumeDots xs@(x:[]) = xs
      consumeDots (x:y:xs) =
          if getPathString y == ".."
          then xs
          else x : consumeDots (y:xs)

followSymlinks :: Int -> AbsFile -> IO AbsFile
followSymlinks 0 path = error $ "too many symbolic links:" ++ getPathString path
followSymlinks n path = do
  exists <- fileExist $ getPathString path
  (if exists
   then do
     info <- getSymbolicLinkStatus $ getPathString path
     (if isSymbolicLink info
      then do
        path' <- readSymbolicLink $ getPathString path
        case mkPathAbsOrRel path' of
          Left p  -> followSymlinks (n - 1) p
          Right p -> followSymlinks (n - 1) (dropFileName path </> p)
      else return path)
   else error $ "no such path: " ++ getPathString path)

bestModule :: RelFile -> [Module] -> Maybe Module
bestModule path ms = listToMaybe .
                     map fst .
                     filter ((> 0) . snd) .
                     zip ms $
                     map score ms
    where
      score m = if moduleName m `isPrefixOf` getPathString path
                then length $ moduleName m
                else 0

-- The public interface to each module is symlinked into the include
-- directory.  We need to follow these symlinks.
includeToModule :: AbsDir -> String -> IO (Maybe Module)
includeToModule location nm = do
  let p = location </> asRelDir "include" </> asRelFile nm
  exist <- fileExist (getPathString p)
  if exist
     then do
       path <- followSymlinks 10 p
       let relPath = makeRelative location (normalisePath path)
       ms <- modules
       return . bestModule relPath $ ms
     else return Nothing

getDependencies :: AbsDir -> Module -> AbsFile -> IO [Dependency Module]
getDependencies location from path =
    withFile (getPathString path) ReadMode $ \h -> do
      txt <- hGetContentsStrict h
      let includes = filter (not . null) . nub . filterIncludes $ txt
      (map (Dependency from) . catMaybes) <$> mapM (includeToModule location) includes

-- FIXME: should shell out a 'find'
modules :: IO [Module]
modules = return ms
    where
      ms = map (Module . ("lib/" ++))
           [ "activate"
           , "commands"
           , "datastruct"
           , "display"
           , "filters"
           , "format_pool"
           , "freeseg"
           , "locking"
           , "mirror"
           , "mm"
           , "snapshot"
           , "unknown"
           , "zero"
           , "cache"
           , "config"
           , "device"
           , "error"
           , "format1"
           , "format_text"
           , "label"
           , "log"
           , "metadata"
           , "misc"
           , "report"
           , "striped"
           , "uuid"
           ]

graph :: AbsDir -> IO [Dependency Module]
graph location = do
  ms <- modules
  concat <$> (forM ms $ \m -> do
                exist <- fileExist $ moduleName m
                (if exist 
                 then do
                   files <- findSource (moduleName m)
                   (filter (not . selfRef) . nub . sort . concat) <$> 
                     mapM (getDependencies location m) (map (makeAbsolute location . asRelFile) files)
                 else return [])
             )
    
main :: IO ()
main = do
  pwd <- asAbsDir <$> getCurrentDirectory
  graphMain $ Project { projLocation = pwd
                      , projItems    = modules
                      , projGraph    = graph pwd
                      }

