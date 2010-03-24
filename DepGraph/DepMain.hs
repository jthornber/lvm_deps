module DepGraph.DepMain
    ( Project (..)
    , graphMain
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List
import System.Console.GetOpt
import System.FilePath.Posix
import System.Environment
import System.Path

import DepGraph.DepGraph
import DepGraph.CPPIncludes

----------------------------------------------------------------
-- Every project will want to define what nodes are possible, and
-- what the dependencies are between nodes.
data Project g = Project { projLocation :: AbsDir
                         , projItems :: IO [g]
                         , projGraph :: IO [Dependency g]
                         }

data Flag = Help
          | Reachable String
          | Cycles
          | All
            deriving Show

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Usage info"
          , Option ['a'] ["all"] (NoArg All) "Show all dependencies"
          , Option ['r'] ["reachable"] (ReqArg Reachable "NODE") "Graph all nodes reachable from a given start point"
          , Option ['c'] ["cycles"] (NoArg Cycles) "Show strongly connected sub graphs"
          ]

usage :: (GraphItem g) => Project g -> IO ()
usage proj = do
  putStrLn $ usageInfo header options
  putStrLn "Valid nodes:"
  items <- projItems proj
  forM_ items $ putStrLn . ("    " ++) . displayName
    where
      header = "Usage: deps <OPTION>"

getOpts :: (GraphItem g) => Project g -> [String] -> IO [Flag]
getOpts proj argv =
    case getOpt Permute options argv of
      (o, n, []) -> return o
      (_, _, errs) -> usage proj >> return []

graphMain :: (GraphItem g, Ord g) => Project g -> IO ()
graphMain proj = do
  args   <- getArgs
  if length args /= 1
   then usage proj
   else do
    [flag] <- getOpts proj args

    case flag of
      Help        -> usage proj
      All         -> emit graphAll
      Cycles      -> emit graphCycles
      Reachable m -> emit (graphReachable . fromDisplay $ m)

    where
      emit printer = projGraph proj >>= putStr . renderGraph printer

----------------------------------------------------------------