module Advent.Prelude (module X, module Advent.Prelude) where

import Relude as X hiding ((<|>))

data Part = Part1 | Part2
  deriving stock (Read)
