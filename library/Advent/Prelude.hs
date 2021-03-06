module Advent.Prelude (module X, module Advent.Prelude) where

import Relude as X hiding ((<|>))

data Part = Part1 | Part2
  deriving stock (Read)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
