module Twilio.IVRSpec where

import Test.Hspec

import Twilio.IVR


import Control.Lens
import Control.Lens.Setter

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Text.XML.Light

-- never use for production purposes, NOT TRUE XML EQUIVALENCE
instance Eq Content where
    x == y = (showContent x) == (showContent y)

spec :: Spec
spec = do
    describe "say" $ do        
        it "generates data structure" $ do           
            Left (Request res _) <- resume (say "Test Message")         
            res `shouldBe` (Right (Say "Test Message"))
        it "generates XML" $ do            
            Left (Request res _) <- resume (say "Test Message") 
            renderTwiML res `shouldBe` (Elem $ unode "Say" "Test Message")
    describe "hangup" $ do
        it "generates data structure" $ do           
            Left (Request res _) <- resume hangup
            res `shouldBe` (Right Hangup)
        it "generates XML" $ do            
            Left (Request res _) <- resume hangup
            renderTwiML res `shouldBe` (Elem $ unode "Hangup" ())
    describe "gather" $ do            
        it "generates XML" $ do           
            Left (Request res _) <- resume (gather "Test Message" (numDigits .~ 10))
            renderTwiML res `shouldBe` (Elem $ unode "Gather" (Elem $ unode "Say" "Test Message") &
                add_attrs [
                Attr (unqual "timeout") "5",
                Attr (unqual "finishOnKey") "#",
                Attr (unqual "numDigits") "10",
                Attr (unqual "method") "POST",
                Attr (unqual "action") "" ])
            Left (Request res2 _) <- resume (gather "Test Message" (
                (numDigits .~ 5) . 
                (timeout .~ 10) .
                (finishOnKey .~ Nothing)
                ))
            renderTwiML res2 `shouldBe` (Elem $ unode "Gather" (Elem $ unode "Say" "Test Message") &
                add_attrs [
                Attr (unqual "timeout") "10",
                Attr (unqual "finishOnKey") "",
                Attr (unqual "numDigits") "5",
                Attr (unqual "method") "POST",
                Attr (unqual "action") "" ])                
            
            