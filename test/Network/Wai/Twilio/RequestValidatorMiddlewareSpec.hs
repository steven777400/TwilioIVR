{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Twilio.RequestValidatorMiddlewareSpec where


import Test.Hspec

import Network.Wai.Twilio.RequestValidatorMiddleware

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test


trivialApp :: Application
trivialApp req f =
    f $
      responseLBS
        ok200
        [ ("content-type", "text/plain") ]
        "Trivial App"

-- https://mycompany.com/myapp.php?foo=1&bar=2
req1 = SRequest defaultRequest
    { requestMethod = "POST"
    , rawPathInfo = "/myapp.php"
    , rawQueryString = "?foo=1&bar=2"
    , isSecure = True
    , requestHeaderHost = Just "mycompany.com"
    , requestHeaders = [("X-Twilio-Signature", "RSOYDt4T1cUTdK1PDd93/VVr8B8=")]
    } "Digits=1234&To=+18005551212&From=+14158675309&Caller=+14158675309&CallSid=CA1234567890ABCDE"


spec :: Spec
spec = do
    describe "requestValidator" $ do        
        it "validates correct requests" $ do           
            (SResponse status _ msg) <- runSession (srequest req1) (requestValidator "12345" trivialApp)
            print msg
            status `shouldBe` ok200  



-- unauthorized401





