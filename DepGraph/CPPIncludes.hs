module DepGraph.CPPIncludes
    ( filterIncludes
    ) where

import Text.Regex.Posix

----------------------------------------------------------------
-- Routines concerned with extracting the dependencies between C pre
-- processor files (ie. via #include).

trim :: String -> String
trim = reverse . tail . reverse . tail

filterIncludes :: String -> [String]
filterIncludes = map trim . map getBetweenQuotes . filter isInclude . lines
    where
      isInclude = (=~ "#include ")

      getBetweenQuotes :: String -> String
      getBetweenQuotes = (=~ "[\"<].*[\">]")

----------------------------------------------------------------