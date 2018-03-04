module AlphabetCipher where

import Data.List
import Data.Maybe

type Message = String
type Keyword = String

data Cell = Cell Point Char deriving (Eq, Show)
data Point = Point Char Char deriving (Eq, Show)
type Board = [Cell]
type Lookup = (Board -> Char -> Char -> Char)

encode :: Keyword -> Message -> Message
encode keyword message = execute keyword message encodeChar

decode :: Keyword -> Message -> Message
decode keyword message = execute keyword message decodeChar

execute :: Keyword -> Message -> Lookup -> Message      
execute keyword message lookup = reverse $ go (repeatUntil keyword (length message)) message []
  where
    board = buildBoard
    go k m acc = case k of 
      [] -> acc
      (x:sx) -> go (tail k) (tail m) $ (lookup board (head k) (head m)) : acc

repeatUntil :: String -> Int -> String
repeatUntil source l = go source source l
  where 
    go source acc l
      | length acc >= l = take l acc
      | otherwise = go source (concat $ replicate 2 acc) l
    
encodeChar :: Lookup
encodeChar board x y = head $ map cellValue $ filter (pointed x y) board
  where
    pointed :: Char -> Char -> Cell -> Bool
    pointed x y (Cell (Point x' y') _) = x == x' && y == y'
    cellValue :: Cell -> Char
    cellValue (Cell _ v) = v

decodeChar :: Lookup
decodeChar board key value = head $ map decodeValue $ filter matches board
  where
    matches :: Cell -> Bool
    matches (Cell (Point x _) v) = key == x && v == value
    decodeValue :: Cell -> Char
    decodeValue (Cell (Point _ y) _) = y

buildBoard :: Board
buildBoard = do
  x <- alphabet
  y <- alphabet
  return (Cell (Point x y) (putChar (Point x y)))
  where 
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    putChar (Point x y) = 
      let
        split = splitAt (fromJust $ (elemIndex y alphabet)) alphabet
        transformed = snd split ++ fst split
      in
        transformed !! (fromJust $ elemIndex x alphabet)