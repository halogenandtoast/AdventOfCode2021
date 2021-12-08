module Advent.Day3 where

import Advent.Parser
import Advent.Prelude hiding (Down, product)
import Data.List

data BitCriteria = MostCommon | LeastCommon

data Bit = One | Zero
  deriving stock (Eq, Show)

bitNot :: Bit -> Bit
bitNot One = Zero
bitNot Zero = One

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One = 1

bitsToInt :: [Bit] -> Int
bitsToInt bs = go 0 bs
 where
  go n [] = n
  go n (x : xs) = go ((n * 2) + bitToInt x) xs

bit :: Parser Bit
bit = (char '0' $> Zero) <|> (char '1' $> One)

bits :: Parser [Bit]
bits = many1 bit

mostCommonBit :: [Bit] -> Bit
mostCommonBit list = case compare zeroCounts oneCounts of
  EQ -> One
  GT -> Zero
  LT -> One
 where
  oneCounts = count (== One) list
  zeroCounts = count (== Zero) list

solve :: [[[Bit]] -> [Bit]] -> [[Bit]] -> IO ()
solve ops = print . product . fmap bitsToInt . sequence ops

solvePart1 :: [[Bit]] -> IO ()
solvePart1 = solve [getGamma, getLemma]

solvePart2 :: [[Bit]] -> IO ()
solvePart2 = solve [getO2, getCo2]

getGamma :: [[Bit]] -> [Bit]
getGamma = map mostCommonBit . transpose

getLemma :: [[Bit]] -> [Bit]
getLemma = map (bitNot . mostCommonBit) . transpose

getO2 :: [[Bit]] -> [Bit]
getO2 = filterByCriteria MostCommon

getCo2 :: [[Bit]] -> [Bit]
getCo2 = filterByCriteria LeastCommon

filterByCriteria :: BitCriteria -> [[Bit]] -> [Bit]
filterByCriteria c bs = go 0 bs
 where
  go _ [] = error "invalid input"
  go _ [x] = x
  go n xs = go (n + 1) (filter (\x -> x !! n == mask xs !! n) xs)
  mask = case c of
    MostCommon -> getGamma
    LeastCommon -> getLemma

run :: Part -> String -> IO ()
run part raw = do
  let input = parseInput bits raw
  case part of
    Part1 -> solvePart1 input
    Part2 -> solvePart2 input
