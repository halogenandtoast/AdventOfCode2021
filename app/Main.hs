module Main where

import Advent.Prelude

import Advent.Day1 qualified as Day1
import Advent.Day2 qualified as Day2
import System.Environment

read :: Read a => String -> a
read = fromMaybe (error "Could not read") . readMaybe

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["Day1", part, input] -> readFile input >>= Day1.run (read part)
    ["Day2", part, input] -> readFile input >>= Day2.run (read part)
    _ -> error "Invalid input"
