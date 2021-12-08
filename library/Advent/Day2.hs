module Advent.Day2 where

import Advent.Parser
import Advent.Prelude hiding (Down)

data Direction = Forward Int | Down Int | Up Int
type Position = (Int, Int)
type Position2 = (Int, Int, Int)

move :: Position -> Direction -> Position
move (x, y) = \case
  Forward n -> (x + n, y)
  Down n -> (x, y + n)
  Up n -> (x, y - n)

move2 :: Position2 -> Direction -> Position2
move2 (x, y, aim) = \case
  Forward n -> (x + n, y + aim * n, aim)
  Down n -> (x, y, aim + n)
  Up n -> (x, y, aim - n)

toPosition :: Position2 -> Position
toPosition (x, y, _) = (x, y)

direction :: Parser Direction
direction = do
  f <- (string "forward" $> Forward) <|> (string "down" $> Down) <|> (string "up" $> Up)
  _ <- space
  f <$> decimal

solvePart1 :: [Direction] -> IO ()
solvePart1 xs = print $ uncurry (*) $ foldl' move (0, 0) xs

solvePart2 :: [Direction] -> IO ()
solvePart2 xs = print $ uncurry (*) $ toPosition $ foldl' move2 (0, 0, 0) xs

run :: Part -> String -> IO ()
run part raw = do
  let input = parseInput direction raw
  case part of
    Part1 -> solvePart1 input
    Part2 -> solvePart2 input
