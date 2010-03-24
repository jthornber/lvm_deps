module RandomStuff
    ( hGetContentsStrict
    , dropPrefix
    , Dep (..)
    , getDependencies
    , Module (..)
    , ModuleDep
    , toModuleDeps
    , selfDep
    , findSource
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import qualified Data.Map as M
import System.Exit
import System.FilePath.Posix
import System.Process
import System.IO

import DepGraph.CPPIncludes
import DepGraph.DepGraph

----------------------------------------------------------------
-- IO
hGetContentsStrict h = do
  txt <- BS.hGetContents h
  return $ BS.unpack txt

dropPrefix :: FilePath -> FilePath -> FilePath
dropPrefix prefix p
    | prefix `isPrefixOf` p = drop (length prefix) p
    | otherwise             = p

----------------------------------------------------------------
-- File dependencies
data Dep = Dep String String

getDependencies :: FilePath -> FilePath -> IO [Dep]
getDependencies location path =
    withFile path ReadMode $ \h -> do
      txt <- hGetContentsStrict h
      let includes = filter (not . null) . nub . map takeDirectory . filterIncludes $ txt
          p        = dropPrefix location path
      return . map (Dep p) $ filter (not . boring) includes

    where
      boring txt = any (`isPrefixOf` txt) stopList
      stopList = [ "..", "boost", "TIBMsg", "SessionLayer", "AnsiPage"
                 , "Common", "Config", "Logger", "netinet", "sys" ]

----------------------------------------------------------------
-- Module dependencies
data Module = Module {moduleName :: String} deriving (Show, Eq, Ord)

instance GraphItem Module where
    displayName = moduleName
    fromDisplay = Module

type ModuleDep = Dependency Module

toModuleDeps :: [Module] -> [Dep] -> [Dependency Module]
toModuleDeps mods = map toModDep
    where
      toModDep (Dep from to) = Dependency (toModule from) (toModule to)

      toModule path = fst $ foldr (bestModule path) (Module path, 0) mods

      bestModule path m2@(Module p) acc@(m, len)
                 | p `isPrefixOf` path && length p > len = (m2, length p)
                 | otherwise                             = acc

selfDep :: Dependency Module -> Bool
selfDep (Dependency f t) = f == t

----------------------------------------------------------------
-- FIXME: allow the caller to specify the file extensions to search
-- for.
findSource :: FilePath -> IO [String]
findSource path = do
  (eCode, out, err) <- readProcessWithExitCode "find" args ""
  case eCode of
    ExitSuccess -> return $ lines out
    ExitFailure txt -> error $ "find failed: " ++ show txt

    where
      args = path : words "-name *.hpp -o -name *.cpp -o -name *.h -o -name *.c"

