module Bowling where

data Game = Game {
  rolls :: [Int]
  } deriving (Eq, Show)

emptyGame :: Game
emptyGame = Game []

score :: Game -> Int
score (Game []) = 0
score (Game [x]) = x
score (Game [x,y]) = x + y 
score (Game [x,y,z]) = x + y + z
score (Game (x:y:z:xs)) 
  | x == 10 = 10 + y + z + score (Game (y:z:xs))
  | x + y == 10 = 10 + z + score (Game (z:xs))
  | otherwise = x + y + score (Game (z:xs))

roll :: Game -> Int -> Game
roll game pins = Game {rolls = (rolls game) ++ [pins] }

rollMany :: Game -> Int -> Int -> Game
rollMany game times pins = foldl (\x y -> roll x (score y)) game (map (\s -> Game [s]) (replicate times pins))