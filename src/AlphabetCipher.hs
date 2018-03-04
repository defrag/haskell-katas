module AlphabetCipher where

import Data.List
import Data.Maybe

type Message = String
type Keyword = String

data Cell = Cell Char Char Char deriving (Eq, Show)
type Board = [Cell]
type Lookup = Board -> Char -> Char -> Char

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
    pointed x y (Cell x' y' _) = x == x' && y == y'
    cellValue :: Cell -> Char
    cellValue (Cell _ _ v) = v

decodeChar :: Lookup
decodeChar board key value = head $ map decodeValue $ filter matches board
  where
    matches :: Cell -> Bool
    matches (Cell x _ v) = key == x && v == value
    decodeValue :: Cell -> Char
    decodeValue (Cell _ y _) = y

buildBoard :: Board
buildBoard = do
  x <- alphabet
  y <- alphabet
  return (Cell x y (putChar x y))
  where 
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    putChar x y = 
      let
        split = splitAt (fromJust $ (elemIndex y alphabet)) alphabet
        transformed = snd split ++ fst split
      in
        transformed !! (fromJust $ elemIndex x alphabet)