module MazeSpec (main, spec) where

import Test.Hspec
import Maze

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

testMaze3x3 = [
    [Start, Empty, Wall], 
    [Wall, Empty, Wall], 
    [Wall, Empty, Exit]
  ] 

testMaze4x4 = [
    [Wall, Start, Wall, Wall], 
    [Wall, Empty, Wall, Wall], 
    [Wall, Empty, Empty, Wall], 
    [Wall, Wall, Exit, Wall]
  ]

mazeWithoutStart = [
    [Empty, Empty, Wall], 
    [Wall, Empty, Wall], 
    [Wall, Empty, Exit]
  ]   

mazeWithoutSolution = [
    [Start, Wall, Exit], 
    [Empty, Empty, Wall], 
    [Empty, Empty, Wall]
  ]   

spec :: Spec
spec = do
  describe "mazeSize" $ do
    it "returns size for a maze" $ do
      mazeSize testMaze3x3 `shouldBe` (3,3)
      mazeSize testMaze4x4 `shouldBe` (4,4)
  
  describe "translateMove" $ do
    it "translates moves based on direction into a point" $ do
      translateMove (Point 0 0) East `shouldBe` (Point 1 0)
      translateMove (Point 0 1) East `shouldBe` (Point 1 1)
      translateMove (Point 0 0) South `shouldBe` (Point 0 (-1))
      translateMove (Point 1 (-1)) South `shouldBe` (Point 1 (-2))
      translateMove (Point 0 0) West `shouldBe` (Point (-1) 0)
      translateMove (Point 1 1) West `shouldBe` (Point 0 1)
      translateMove (Point 0 0) North `shouldBe` (Point 0 1)
      translateMove (Point 2 2) North `shouldBe` (Point 2 3)
      translateMove (Point 2 (-1)) North `shouldBe` (Point 2 0)

  describe "allMoves" $ do
    it "returns moves available at given point" $ do
      allMoves (Point 0 0) `shouldBe` [(Point 1 0), (Point 0 (-1)), (Point (-1) 0), (Point 0 1)]
      allMoves (Point 2 2) `shouldBe` [(Point 3 2), (Point 2 1), (Point 1 2), (Point 2 3)]
  
  describe "availableMoves" $ do
    it "filters moves available for given point in a maze" $ do
      availableMoves testMaze3x3 (Point 0 0) `shouldBe` [(Point 1 0), (Point 0 (-1))]
      availableMoves testMaze3x3 (Point 1 (-1)) `shouldBe` [(Point 2 (-1)), (Point 1 (-2)), (Point 0 (-1)), (Point 1 0)]
      
    it "filters moves exceeding boundaries of maze as well" $ do
      availableMoves testMaze3x3 (Point 2 (-2)) `shouldBe` [(Point 1 (-2)), (Point 2 (-1))]
  
  describe "tileAtPoint" $ do
    it "returns the tile on a given point" $ do
      tileAtPoint testMaze3x3 (Point 0 0) `shouldBe` Just (Tile Start (Point 0 0))
      tileAtPoint testMaze3x3 (Point 1 0) `shouldBe` Just (Tile Empty (Point 1 0))
  
  describe "tilePoint" $ do
    it "returns the point a tile is on" $ do
      tilePoint (Tile Empty  (Point 1 1)) `shouldBe` (Point 1 1)
  
  describe "findStart" $ do
    it "returns the starting point if available" $ do
      findStart testMaze3x3 `shouldBe` Just (Point 0 0)
      findStart testMaze4x4 `shouldBe` Just (Point 1 0)
      findStart mazeWithoutStart `shouldBe` Nothing
  
  describe "nextTile" $ do
    it "returns the tile to step on next" $ do
      nextTile testMaze3x3 (Point 0 0) [] `shouldBe` Just (Tile Empty (Point 1 0))
      nextTile testMaze3x3 (Point 1 0) [] `shouldBe` Just (Tile Empty (Point 1 (-1)))
      nextTile testMaze3x3 (Point 1 (-1)) [] `shouldBe` Just (Tile Empty (Point 1 (-2)))
      nextTile testMaze3x3 (Point 1 (-2)) [] `shouldBe` Just (Tile Exit (Point 2 (-2)))
      nextTile testMaze3x3 (Point (-2) (-2)) [] `shouldBe` Nothing

  describe "formatResult" $ do
    it "takes a tuple of size and points and formats it with X and O" $ do
      formatResult (3, 3) []  `shouldBe` [
          [O, O, O], 
          [O, O, O], 
          [O, O, O]
        ]  
      
      formatResult (3, 3) [(Point 1 (-1)), (Point 1 0), (Point 2 0)]  `shouldBe` [
          [O, X, X], 
          [O, X, O], 
          [O, O, O]
        ]    

    describe "solve" $ do
      it "solves the mazes and produces output matrix" $ do
        solve testMaze3x3  `shouldBe` [
            [X, X, O], 
            [O, X, O], 
            [O, X, X]
          ]
        
        solve testMaze4x4  `shouldBe` [
            [O, X, O, O], 
            [O, X, O, O], 
            [O, X, X, O],
            [O, O, X, O]
          ]  
      it "returns empty list when there is no valid solution for a maze" $ do
        solve mazeWithoutSolution `shouldBe` [
            [O, O, O], 
            [O, O, O], 
            [O, O, O]
          ]
