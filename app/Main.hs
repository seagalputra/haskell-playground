module Main where

import           Lib

main = interact wordCount
  where wordCount input = show (length (lines input)) ++ "\n"
