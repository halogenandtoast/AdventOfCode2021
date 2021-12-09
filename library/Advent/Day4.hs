module Advent.Day4 where

import Advent.Parser
import Advent.Prelude hiding (many)
import Data.List (partition)

solvePart1 :: ([Int], [Board]) -> IO ()
solvePart1 (xs, bs) = print $ go xs bs
 where
  go (x : xs') bs' = case call x bs' of
    ([], bs'') -> go xs' bs''
    ([y], _) -> sum (unmarked y) * x
    (_, _) -> error "invalid input"
  go _ _ = error "invalid input"

solvePart2 :: ([Int], [Board]) -> IO ()
solvePart2 (xs, bs) = print $ go xs bs
 where
  go (x : xs') bs' = case call x bs' of
    ([], []) -> error "invalid input"
    ([y], []) -> sum (unmarked y) * x
    (_, bs'') -> go xs' bs''
  go _ _ = error "invalid input"

data Cell = Uncalled Int | Called
  deriving stock (Eq, Show)

newtype Row = Row {unCols :: [Cell]}
  deriving newtype (Show)

newtype Board = Board {unRows :: [Row]}
  deriving newtype (Show)

unmarked :: Board -> [Int]
unmarked b = mapMaybe unmarkedVal (concatMap unCols (unRows b))
 where
  unmarkedVal Called = Nothing
  unmarkedVal (Uncalled x) = Just x

numbersAndBoards :: Parser ([Int], [Board])
numbersAndBoards = do
  digits <- decimal `sepBy` char ','
  _ <- newline
  _ <- newline
  boards <- manyTill board (try eof)
  pure (digits, boards)

board :: Parser Board
board = Board <$> manyTill row (try (void newline <|> eof))

row :: Parser Row
row = Row <$> manyTill (spaces *> (Uncalled <$> decimal)) (try (void newline <|> eof))

fill :: Int -> Board -> Board
fill n b = Board $ map (Row . map fillCell . unCols) (unRows b)
 where
  fillCell (Uncalled n') | n == n' = Called
  fillCell other = other

call :: Int -> [Board] -> ([Board], [Board])
call n bs = let bs' = map (fill n) bs in partition won bs'

won :: Board -> Bool
won b = any rowWins x || any rowWins y
 where
  x = map unCols (unRows b)
  y = transpose x
  rowWins = all (== Called)

run :: Part -> String -> IO ()
run part raw = do
  let input = either (error . show) id $ parse numbersAndBoards "input" raw
  case part of
    Part1 -> solvePart1 input
    Part2 -> solvePart2 input
