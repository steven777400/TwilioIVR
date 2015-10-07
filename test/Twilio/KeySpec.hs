module Twilio.KeySpec where

import Test.Hspec
import Twilio.Key


ks = "0123456789#*"
kss = map (:[]) ks

kk = [K0, K1, K2, K3, K4, K5, K6, K7, K8, K9, KPound, KStar]

spec :: Spec
spec = do
    describe "read" $ do        
        it "read individual keys" $ do                     
            mapM_ (\(x,y) -> read x `shouldBe` y) (zip kss kk)            
        it "reads key list" $ do
            read ks `shouldBe` kk
    describe "show" $ do
        it "shows individual keys" $ do
            mapM_ (\(x,y) -> show x `shouldBe` y) (zip kk kss)            
        it "shows key list" $ do
            show kk `shouldBe` ks
    describe "keyToLetter" $ do
        it "converts alpha keys to letters" $ do
            keyToLetter K3 `shouldBe` "DEF"
            keyToLetter K7 `shouldBe` "PQRS"
        it "converts non-alpha keys to empty list" $ do
            keyToLetter K0 `shouldBe` ""
            keyToLetter K1 `shouldBe` ""
    describe "letterToKey" $ do
        it "converts alpha letters to key" $ do
            letterToKey 'E' `shouldBe` (Just K3)
            letterToKey 'u' `shouldBe` (Just K8)
        it "converts numbers to key" $ do
            letterToKey '1' `shouldBe` (Just K1)
            letterToKey '5' `shouldBe` (Just K5)
        it "converts non-alpha letters to nothing" $ do
            letterToKey '+' `shouldBe` Nothing
