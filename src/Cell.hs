module Cell where

import Utils (size)

data Cell
  = Empty
  | Number Int
  deriving (Eq)

instance Show Cell where
  show (Number n) = [' ', (map (head . show) [1 .. 9] ++ ['A' ..]) !! n, ' ']
  show Empty = "   "

removeEmpty :: [Cell] -> [Cell]
removeEmpty = filter (/= Empty)

addEmpty :: [Cell] -> [Cell]
addEmpty line = line <> replicate (size - length line) Empty

mergeLine :: [Cell] -> [Cell]
mergeLine line =
  let merged = singleMerge line
   in if merged == line
        then line
        else mergeLine merged

singleMerge :: [Cell] -> [Cell]
singleMerge [] = []
singleMerge [a] = [a]
singleMerge (a:b:rest) =
  if a == b
    then merge a b : rest
    else a : singleMerge (b : rest)

merge :: Cell -> Cell -> Cell
merge (Number a) (Number b)
  | a == b = Number $ a + 1
  | otherwise = error "impossible"
merge Empty Empty = Empty
merge _ _ = error "impossible"
