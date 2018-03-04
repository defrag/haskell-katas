module AlphabetCipher where

import Data.List
import Data.Maybe

type Message = String
type Keyword = String

data Cell = Cell Point Char deriving (Eq, Show)
data Point = Point Char Char deriving (Eq, Show)
type Board = [Cell]

encode :: Keyword -> Message -> Message
encode keyword message = reverse $ go (repeatUntil keyword (length message)) message []
  where
    board = buildBoard
    go k m acc = case k of 
      [] -> acc
      (x:sx) -> go (tail k) (tail m) $ (findEncodedValue board (Point (head k) (head m))) : acc

decode :: Keyword -> Message -> Message
decode keyword message = reverse $ go (repeatUntil keyword (length message)) message []
  where
    board = buildBoard
    go k m acc = case k of 
      [] -> acc
      (x:sx) -> go (tail k) (tail m) $ (findDecodedValue board (head k) (head m)) : acc

repeatUntil :: String -> Int -> String
repeatUntil source l = go source source l
  where 
    go source acc l
      | length acc >= l = take l acc
      | otherwise = go source (concat $ replicate 2 acc) l
    
findEncodedValue :: Board -> Point -> Char
findEncodedValue board point = head $ map cellValue $ filter (pointed point) board
  where
    pointed :: Point -> Cell -> Bool
    pointed p (Cell p' _) = p == p'
    cellValue :: Cell -> Char
    cellValue (Cell _ v) = v

findDecodedValue :: Board -> Char -> Char -> Char
findDecodedValue board key value = head $ map decodeValue $ filter matches board
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