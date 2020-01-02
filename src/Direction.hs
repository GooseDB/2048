module Direction where

import Data.Char (toLower)
import Data.List (find)
import Prelude hiding (Either(..))

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq)

readDirection :: Char -> Maybe Direction
readDirection = recognize . toLower
  where
    recognize raw =
      fmap snd . find ((raw==) . fst) $
      [ ('w', Up)
      , ('s', Down)
      , ('a', Left)
      , ('d', Right)
      ]
