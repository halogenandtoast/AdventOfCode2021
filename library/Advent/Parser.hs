module Advent.Parser (module X, module Advent.Parser) where

import Advent.Prelude

import Data.Char (digitToInt)
import Text.Parsec as X
import Text.Parsec.String as X

doParse :: Parser a -> String -> Either ParseError a
doParse p i = parse p "input" i

decimal :: Parser Int
decimal = do
  digits <- many1 baseDigit
  let n = foldl' (\x d -> base * x + digitToInt d) 0 digits
  seq n (pure n)
 where
  base = 10
  baseDigit = digit
