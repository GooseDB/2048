module Utils where

import Data.List (intersperse)

size :: Int
size = 4

intersperseAndEncircle :: a -> [a] -> [a]
intersperseAndEncircle a as = a : intersperse a as <> [a]
