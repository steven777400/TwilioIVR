{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Wai.Twilio.RequestValidatorMiddleware
Description : A wai middleware for validating incoming Twilio requests
Copyright   : (c) 2015 Steve Kollmansberger
Maintainer  : steve@kolls.net
Stability   : experimental

See https://www.twilio.com/docs/security for details.  Twilio includes an X-Twilio-Signature
in each request, signing the request using your API auth token. 
-}
module Network.Wai.Twilio.RequestValidatorMiddleware (requestValidator) where

import qualified Crypto.MAC.HMAC as HMAC
import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Data.ByteString.Base64
import Data.List
import Data.Monoid
import Data.IORef

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


verifySignature :: B.ByteString -> B.ByteString -> Request -> IO Bool
verifySignature authtoken body r = 
    case lookup "X-Twilio-Signature" $ requestHeaders r of
        Nothing                 -> return False -- If signature missing from request, must not be valid
        (Just givenSignature)   -> return $ (givenSignature ==) $ calcSignature authtoken r $ body


readRequestBodyAndPushback :: Request -> IO (B.ByteString, IO B.ByteString)
readRequestBodyAndPushback req = do
    -- This loop body and then the ichunks rbody stuff comes from http://hackage.haskell.org/package/wai-extra-3.0.3.2/docs/src/Network-Wai-Middleware-RequestLogger.html
    -- ** BEGIN READ BODY
    let loop front = do
            bs <- requestBody req
            if BC.null bs
                then return $ front []
                else loop $ front . (bs:)
    body <- loop id
    -- ** END READ BODY
    
    -- ** BEGIN BODY PUSHBACK
    ichunks <- newIORef body
    let rbody = atomicModifyIORef ichunks $ \chunks -> case chunks of
            [] -> ([], BC.empty)
            x:y -> (y, x)
    -- ** END BODY PUSHBACK

    return (B.concat body, rbody)



-- | Middleware to validate Twilio request.  Parameterized by API auth token.
--   Returns 401 Unauthorized if the request does not contain a valid signature based on the given auth token.
requestValidator :: 
    B.ByteString    -- ^ AuthToken from your Twilio page
    -> Middleware
requestValidator authtoken app req respond = do
    -- strictRequestBody appears to work only once, so we re-insert result into request when validated.
    (body, rbody) <- readRequestBodyAndPushback req
    vs <- verifySignature authtoken body req    
    case vs of
        False -> respond $ responseLBS unauthorized401 [] "Invalid X-Twilio-Signature"
        True -> app (req {requestBody = rbody}) respond


