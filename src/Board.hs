module Board where

import Data.List (findIndices, intercalate, transpose)
import Data.List.Split (chunksOf)
import Prelude hiding (Either(..))

import Cell (Cell(..), addEmpty, mergeLine, removeEmpty)
import Direction (Direction(..))
import Utils (intersperseAndEncircle, size)

newtype Board =
  Board [[Cell]]
  deriving (Eq)

instance Show Board where
  show (Board lines) =
    unlines .
    intersperseAndEncircle horizontalSeparator .
    map (concat . intersperseAndEncircle "|" . map show) $
    lines

emptyBoard :: Board
emptyBoard = Board . replicate size . replicate size $ Empty

horizontalSeparator :: String
horizontalSeparator = intercalate "---" . replicate (size + 1) . pure $ '+'

left :: Board -> Board
left (Board lines) = Board $ map (addEmpty . mergeLine . removeEmpty) lines

right :: Board -> Board
right = reverseBoard . left . reverseBoard

up :: Board -> Board
up = transposeBoard . left . transposeBoard

down :: Board -> Board
down = transposeBoard . right . transposeBoard

makeMerge :: Direction -> Board -> Board
makeMerge Up = up
makeMerge Down = down
makeMerge Left = left
makeMerge Right = right

reverseBoard :: Board -> Board
reverseBoard (Board lines) = Board $ map reverse lines

transposeBoard :: Board -> Board
transposeBoard (Board lines) = Board $ transpose lines

putCell :: Board -> Cell -> Int -> Board
putCell (Board lines) cell pos =
  Board . chunksOf size $ take pos concated <> [cell] <> drop (pos + 1) concated
  where
    concated = concat lines

genNewCell :: Board -> Int -> Board
genNewCell board@(Board lines) gen = putCell board cell pos
  where
    cell
      | gen `mod` 3 == 0 = Number 1
      | otherwise = Number 0
    pos = emptyCells !! (gen `mod` length emptyCells)
    emptyCells = findIndices (== Empty) . concat $ lines

genBoard :: Int -> Board
genBoard gen = foldl put emptyBoard $ zip positions cells
  where
    put board (position, cell) = putCell board cell position
    cells
      | even gen = map Number [0, 0]
      | otherwise = pure $ Number 1
    positions = map (flip mod size . div gen) $ iterate (100 *) 100

gameOver :: Board -> Bool
gameOver board = all (== board) $ [up, down, left, right] <*> [board]
