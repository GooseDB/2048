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

readDirection :: String -> Maybe Direction
readDirection = recognize . map toLower
  where
    recognize raw =
      fmap snd . find (elem raw . fst) $
      [ (["w", "up"], Up)
      , (["s", "down"], Down)
      , (["a", "left"], Left)
      , (["d", "right"], Right)
      ]
