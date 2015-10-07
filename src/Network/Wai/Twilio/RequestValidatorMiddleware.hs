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

signature authtoken contents = encode $ hmacsha1 authtoken rbs
    where rbs = BL.toStrict $ toLazyByteString contents


verifySignature :: B.ByteString -> Request -> IO Bool
verifySignature authtoken r = do
    rb <- strictRequestBody r
    let rootUrl = maybe "" id (getApprootMay r)
    let url' = url rootUrl r
    let requestbody' = case requestMethod r of
            methodPost -> postParams (BL.toStrict rb)
            _ -> ""
    let proposedSignature = signature authtoken (url' <> requestbody')
    return $ proposedSignature == "TODO"


