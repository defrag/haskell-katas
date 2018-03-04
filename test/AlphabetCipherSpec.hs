module AlphabetCipherSpec (main, spec) where

import Test.Hspec
import AlphabetCipher

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "buildBoard" $ do
    it "builds the decoding/encoding board" $ do
      buildBoard !! 0 `shouldBe` Cell (Point 'a' 'a') 'a'
      buildBoard !! 25 `shouldBe` Cell (Point 'a' 'z') 'z'
      buildBoard !! 26 `shouldBe` Cell (Point 'b' 'a') 'b'
      buildBoard !! 51 `shouldBe` Cell (Point 'b' 'z') 'a'
  
  describe "findEncodedValue" $ do
    it "finds a value on intersection of chars on board" $ do
      findEncodedValue buildBoard (Point 'v' 'm') `shouldBe` 'h'
      findEncodedValue buildBoard (Point 'i' 'e') `shouldBe` 'm'
      findEncodedValue buildBoard (Point 'g' 'e') `shouldBe` 'k'
    
  describe "repeatUntil" $ do    
    it "repeats given string until it matches the given length" $ do
      repeatUntil "abc" 6 `shouldBe` "abcabc"
      repeatUntil "abc" 5 `shouldBe` "abcab"
      repeatUntil "abc" 4 `shouldBe` "abca"
      repeatUntil "abc" 2 `shouldBe` "ab"

  describe "encode" $ do
    it "encodes a message" $ do
      encode "vigilance" "meetmeontuesdayeveningatseven" `shouldBe` "hmkbxebpxpmyllyrxiiqtoltfgzzv"
      encode "scones" "meetmebythetree"  `shouldBe` "egsgqwtahuiljgs"
  
  describe "findEncodedValue" $ do
    it "finds a value on intersection of chars on board" $ do
      findDecodedValue buildBoard 'v' 'h' `shouldBe` 'm'

  describe "decode" $ do
    it "decodes a message" $ do
      decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" `shouldBe` "meetmeontuesdayeveningatseven"
      decode "scones" "egsgqwtahuiljgs" `shouldBe` "meetmebythetree"      