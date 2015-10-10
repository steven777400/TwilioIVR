{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Twilio.RequestValidatorMiddleware where

import qualified Crypto.MAC.HMAC as HMAC
import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.ByteString.Base64
import Data.List
import Data.Monoid

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.Approot

url :: B.ByteString -> Request -> Builder
url approot r = "http" <>
    (if isSecure r then "s" else "") <>
    "://" <>
    (byteString $ maybe "" id (requestHeaderHost r)) <>
    (byteString approot) <>
    (byteString $ rawPathInfo r) <>
    (byteString $ rawQueryString r)


-- load request bytestring using Wai  strictRequestBody   
postParams :: B.ByteString -> Builder
postParams r = foldl (<>) "" stringified
    where 
        ordered = sortBy (\a b -> compare (fst a) (fst b)) (parseQuery r)
        stringified = map (\(k, mv) -> byteString k <> (byteString $ maybe "" id mv)) ordered
   
    
hmacsha1 = HMAC.hmac SHA1.hash 64    

signature :: B.ByteString -> Builder -> B.ByteString
signature authtoken contents = encode $ hmacsha1 authtoken rbs
    where rbs = BL.toStrict $ toLazyByteString contents

calcSignature :: B.ByteString -> Request -> B.ByteString -> B.ByteString
calcSignature authtoken r rb = let 
    rootUrl = maybe "" id (getApprootMay r)
    url' = url rootUrl r
    requestbody' = case requestMethod r of
            "POST" -> postParams rb
            _ -> ""
    in
    signature authtoken (url' <> requestbody')


verifySignature :: B.ByteString -> Request -> IO Bool
verifySignature authtoken r = 
    case lookup "X-Twilio-Signature" $ requestHeaders r of
        Nothing                 -> return False -- If signature missing from request, must not be valid
        (Just givenSignature)   -> (givenSignature ==) <$> calcSignature authtoken r <$> BL.toStrict <$> strictRequestBody r


-- ISSUES: 
-- 1. strictRequestBody only works once.  Need to do something so down the line they can read again
-- 2. + is not coming through.  Some kind of encoding?

requestValidator :: B.ByteString -> Middleware
requestValidator authtoken app req respond = do
    test <- strictRequestBody req
    vs <- verifySignature authtoken req
    let test' = BL.toStrict test
    case vs of
        False -> respond $ responseLBS unauthorized401 [] $ toLazyByteString $ postParams test'
        True -> app req respond


