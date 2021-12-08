module Main where

import Advent.Prelude

import qualified Advent.Day1 as Day1
import System.Environment

read :: Read a => String -> a
read = fromMaybe (error "Could not read") . readMaybe

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["Day1", part, input] -> readFile input >>= Day1.run (read part)
    _ -> error "Invalid input"
