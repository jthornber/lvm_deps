module DepGraph.DepGraph
    ( GraphItem (..)
    , Dependency (..)
    , selfRef
    , graphAll
    , graphCycles
    , graphReachable
    , renderGraph
    ) where

import Data.Function
import qualified Data.Graph as G
import Data.List
import Data.Maybe
import Data.Ord

----------------------------------------------------------------

class GraphItem g where
    displayName :: g -> String
    fromDisplay :: String -> g

data Dependency g = Dependency { depFrom, depTo :: g }

instance (GraphItem g, Eq g) => Eq (Dependency g) where
    d1 == d2 = (toPair d1) == (toPair d2)

instance (GraphItem g, Ord g) => Ord (Dependency g) where
    compare d1 d2 = compare (toPair d1) (toPair d2)

toPair :: Dependency g -> (g, g)
toPair g = (depFrom g, depTo g)

selfRef :: (GraphItem g, Eq g) => Dependency g -> Bool
selfRef g = depFrom g == depTo g

----------------------------------------------------------------

reachableGraph :: (GraphItem g, Ord g) => g -> [Dependency g] -> [g]
reachableGraph m ds = reachable
    where
      reachable = map lookupMod . G.reachable g . fromJust . lookupK $ m

      lookupMod v = let (m, _, _) = lookupV v in m

      (g, lookupV, lookupK) = G.graphFromEdges . nodeList $ ds

nodeList :: (GraphItem g, Ord g) => [Dependency g] -> [(g, g, [g])]
nodeList = map mkNode .
           groupBy (on (==) depFrom) .
           sortBy (comparing depFrom)
    where
      mkNode [] = error "no modules"
      mkNode (d:ds) = (depFrom d, depFrom d, map depTo (d:ds))

stronglyConnected :: (GraphItem g, Ord g) => [Dependency g] -> [[g]]
stronglyConnected ds = filter (not . null) . map expand $ sccs
    where
      expand (G.CyclicSCC vs) = vs
      expand _                = []

      sccs = G.stronglyConnComp nList
      nList = nodeList ds

subGraph :: (GraphItem g, Eq g) => [g] -> [Dependency g] -> [Dependency g]
subGraph ms = filter (\d -> depFrom d `elem` ms)

strictSubGraph :: (GraphItem g, Eq g) => [g] -> [Dependency g] -> [Dependency g]
strictSubGraph ms = filter (\d -> depFrom d `elem` ms && depTo d `elem` ms)

graphAll :: (GraphItem g) => [Dependency g] -> [Dependency g]
graphAll = id

graphCycles :: (GraphItem g, Ord g) => [Dependency g] ->[Dependency g]
graphCycles mdeps = concatMap (\ds -> strictSubGraph ds mdeps) (stronglyConnected mdeps)

graphReachable :: (GraphItem g, Ord g) => g -> [Dependency g] -> [Dependency g]
graphReachable m mdeps = subGraph (reachableGraph m mdeps) mdeps

renderGraph :: (GraphItem g) => ([Dependency g] -> [Dependency g]) -> [Dependency g] -> String
renderGraph fn deps = unlines $ "digraph dependencies {" : map ppDep (fn deps) ++ ["}"]
    where
      indent = replicate 8 ' '
      ppDep dep = indent ++ "\"" ++ displayName (depFrom dep) ++ "\" -> \"" ++ displayName (depTo dep) ++ "\""


----------------------------------------------------------------