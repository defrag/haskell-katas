module Maze where

import Data.List
import Data.Maybe

data Cell =
    Start
    | Exit
    | Empty 
    | Wall deriving (Eq, Show)

data Move = 
    East
  | South
  | West
  | North deriving (Eq, Show)

data Point = Point Int Int deriving (Eq, Show)
data Path = X | O deriving (Eq, Show)
data Tile = Tile Cell Point deriving (Eq, Show)

type Solution = [[Path]]
type CoordinatedMaze = [Tile]
type Maze = [[Cell]]

solve :: Maze -> Maybe Solution
solve maze = findStart maze >>= findExitPath >>= format
  where 
    format :: [Point] -> Maybe Solution
    format path = Just (formatResult (mazeSize maze) path)
    findExitPath :: Point -> Maybe [Point]
    findExitPath startingPoint = case go startingPoint [] [] of
      [] -> Nothing
      r@_ -> Just r
    go :: Point -> [Point] -> [Tile] -> [Point]  
    go cursor path visited = case tileAtPoint maze cursor of
      Just (Tile Exit p ) -> p : path
      Just t@(Tile _ p) -> case nextTile maze p visited of 
        Just (Tile _ np) -> go np (p : path) (t : visited) 
        Nothing -> case path of
          [] -> []
          (x:xs) -> go x xs (t : visited)
      Nothing -> []

mazeSize :: Maze -> (Int, Int)
mazeSize maze = (length maze, length $ head maze)

translateMove :: Point -> Move -> Point
translateMove (Point x y) move = case move of
  East -> Point (x + 1) y
  South -> Point x (y - 1)
  West -> Point (x - 1) y
  North -> Point x (y + 1)

allMoves :: Point -> [Point]
allMoves point = map (translateMove point) [East, South, West, North]

availableMoves :: Maze -> Point -> [Point]
availableMoves maze point = filter isValid (allMoves point)
  where 
    size = mazeSize maze
    isValid :: Point -> Bool
    isValid (Point x y) = x >= 0 && (x <= (fst size) -1) && y <= 0 && (y >= (*) (snd size) (-1) + 1)

tileAtPoint :: Maze -> Point -> Maybe Tile
tileAtPoint maze point = case tilesFiltered of 
  [] -> Nothing
  (x:_) -> Just x
  where
    isTileAtPoint :: Point -> Tile -> Bool 
    isTileAtPoint p (Tile _ p') = p == p'
    tilesFiltered :: [Tile]
    tilesFiltered = filter (isTileAtPoint point) (toTiles maze)

tilePoint :: Tile -> Point
tilePoint (Tile _ p) = p

toTiles :: Maze -> CoordinatedMaze
toTiles maze = do
  (y, row) <- zip [0..] maze 
  (x, cell) <- zip [0..] row
  return $ Tile cell (Point x $ (*) y (-1))

nextTile :: Maze -> Point -> [Tile] -> Maybe Tile
nextTile maze cursor visited = case sequence tiles of
  Just (x:_) -> Just x 
  _ -> Nothing
  where
    tiles = filter canStepOn $ map (tileAtPoint maze) (availableMoves maze cursor)
    canStepOn :: Maybe Tile -> Bool
    canStepOn (Just t@(Tile Empty _)) = False == elem t visited
    canStepOn (Just t@(Tile Exit _)) =  False == elem t visited
    canStepOn _ = False

findStart :: Maze -> Maybe Point
findStart maze = case filtered of 
  [] -> Nothing
  (x:_) -> Just (tilePoint x)
  where
    filtered = filter isStarting tiles
    tiles = toTiles maze
    isStarting :: Tile -> Bool
    isStarting (Tile Start _) = True
    isStarting (Tile _ _) = False

formatResult :: (Int, Int) -> [Point] -> Solution 
formatResult size points = [[format x y | x <- [0..(fst size) - 1]] | y <- [0..(snd size) - 1]]
  where
    format x y = if elem (Point x ((*) (y) (-1))) points then X else O

maze3x3 = [
    [Start, Empty, Wall], 
    [Wall, Empty, Wall], 
    [Wall, Empty, Exit]
  ]  
maze4x4 = [
    [Wall, Start, Empty, Wall], 
    [Wall, Wall, Empty, Empty], 
    [Wall, Empty, Empty, Wall], 
    [Wall, Wall, Empty, Exit]
  ]
maze3x3Revisited = [
    [Start, Empty, Empty], 
    [Wall, Empty, Wall], 
    [Exit, Empty, Wall]
  ]    
mazeWithNoExit = [
    [Start, Empty, Wall, Wall], 
    [Wall, Wall, Empty, Empty], 
    [Wall, Empty, Empty, Wall], 
    [Wall, Wall, Empty, Exit]
  ]

  