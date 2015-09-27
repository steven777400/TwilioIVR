module Data.TransientStoreSpec where

import Test.Hspec
import Data.TransientStore

import Control.Concurrent
import Data.UUID

spec :: Spec
spec = do
    describe "peek" $ do
        it "returns Nothing if db is empty" $ do
            db <- create 5 :: IO (TransientStore String)            
            peek db nil `shouldReturn` Nothing
        it "returns Nothing if key not in db" $ do
            db <- create 5 :: IO (TransientStore String)
            insert db "hello"
            result <- peek db nil 
            result `shouldBe` Nothing            
        it "returns the value if key in db" $ do
            db <- create 5 :: IO (TransientStore String)
            key <- insert db "hello"
            result <- peek db key
            result `shouldBe` (Just "hello")
        it "returns the value multiple times" $ do
            db <- create 5 :: IO (TransientStore String)
            key <- insert db "hello"
            result <- peek db key
            result `shouldBe` (Just "hello")            
            result2 <- peek db key
            result2 `shouldBe` (Just "hello")            
        it "returns the value if key one of several in db" $ do
            db <- create 5 :: IO (TransientStore String)
            key1 <- insert db "hello"
            key2 <- insert db "different"
            key1 `shouldNotBe` key2
            result1 <- peek db key1
            result1 `shouldBe` (Just "hello")            
            result2 <- peek db key2
            result2 `shouldBe` (Just "different")  
            
    describe "pop" $ do
        it "returns a value once only" $ do
            db <- create 5 :: IO (TransientStore String)
            key <- insert db "hello"
            result <- pop db key
            result `shouldBe` (Just "hello")
            result2 <- pop db key
            result2 `shouldBe` Nothing
            
    describe "cleanIfNeeded" $ do
        it "should not remove a value after a short time" $ do
            db <- create 1 :: IO (TransientStore String)
            key <- insert db "hello"
            threadDelay 500000
            result <- pop db key
            result `shouldBe` (Just "hello")
        it "should remove a value after a long time, but leave shorter values" $ do
            db <- create 1 :: IO (TransientStore String)
            key <- insert db "hello"
            threadDelay 500000
            result <- peek db key
            result `shouldBe` (Just "hello")            
            key2 <- insert db "different"
            threadDelay 900000
            result' <- peek db key
            result' `shouldBe` Nothing
            result2' <- peek db key2
            result2' `shouldBe` (Just "different")
            threadDelay 1000000
            result' <- peek db key
            result' `shouldBe` Nothing
            result2' <- peek db key2
            result2' `shouldBe` Nothing