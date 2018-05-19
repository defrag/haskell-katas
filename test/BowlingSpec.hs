module BowlingSpec (main, spec) where

import Test.Hspec
import Bowling

main :: IO ()
main = hspec spec    

spec :: Spec
spec = do
  describe "roll" $ do
    it "appends rolls with every time its called" $ do
      roll emptyGame 0 `shouldBe` Game [0]
      roll (Game [0]) 1 `shouldBe` Game [0, 1]
      roll (Game [5]) 5  `shouldBe` Game [5, 5]

  describe "bowling game" $ do
    it "returns game with score zero for all rolls that are zero" $ do
      let game = rollMany emptyGame 20 0
      score game `shouldBe` 0

    it "returns game with score 20 for all rolls that are 1" $ do
      let game = rollMany emptyGame 20 1
      score game `shouldBe` 20
    
    it "returns returns valid result for a game with one spare" $ do
      let one = roll emptyGame 5 
      let two = roll one 5
      let three = roll two 3
      let game = rollMany three 17 0
      score game `shouldBe` 16
        
    it "returns returns valid result for a game with one strike" $ do
      let one = roll emptyGame 10
      let two = roll one 3
      let three = roll two 4
      let game = rollMany three 16 0
      score game `shouldBe` 24
    
    it "returns returns valid result for a perfect game" $ do
      let game = rollMany emptyGame 12 10
      score game `shouldBe` 300
            
      
