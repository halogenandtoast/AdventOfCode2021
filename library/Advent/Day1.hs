module Advent.Day1 where

import Advent.Parser
import Advent.Prelude

parseInput :: String -> Either ParseError [Int]
parseInput = doParse (manyTill (decimal <* try newline) eof)

solvePart1 :: [Int] -> IO ()
solvePart1 xs = print $ length $ filter (== True) $ zipWith (<) xs (drop 1 xs)

solvePart2 :: [Int] -> IO ()
solvePart2 xs = solvePart1 $ map sum (threes xs)
 where
  threes :: [a] -> [[a]]
  threes [x, y, z] = [[x, y, z]]
  threes (x : y : z : zs) = [x, y, z] : threes (y : z : zs)
  threes _ = []

run :: Part -> String -> IO ()
run part raw = do
  let input = fromRight (error "Invalid Input") (parseInput raw)
  case part of
    Part1 -> solvePart1 input
    Part2 -> solvePart2 input
