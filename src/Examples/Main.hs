{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses #-}
module Main where

import Control.Applicative

import qualified Examples.EX1 as EX1
import qualified Examples.EX2 as EX2
import qualified Examples.EX3 as EX3

import qualified Data.TransientStore as TS

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai.Handler.FastCGI as FCGI (run)
import Network.Wai.Middleware.Approot
import Network.Wai.Middleware.Routes
import Network.Wai.Twilio.IVR
import Network.Wai.Twilio.RequestValidatorMiddleware

import Twilio.IVR


getTIVRSubRoute :: (Call -> TwilioIVRCoroutine ()) -> MyRoute -> TIVRSubRoute
getTIVRSubRoute entry (MyRoute db) = TIVRSubRoute db entry

example1Route = getTIVRSubRoute EX1.simple
example2Route = getTIVRSubRoute EX2.account
example3Route = getTIVRSubRoute EX3.search

data MyRoute = MyRoute TIVRDB


mkRoute "MyRoute" [parseRoutes|
/ex1 Example1 TIVRSubRoute example1Route
/ex2 Example2 TIVRSubRoute example2Route
/ex3 Example3 TIVRSubRoute example3Route
|]


prepareApp :: IO Application
prepareApp = do
    db <- TS.create 120
    return $ waiApp $ route $ MyRoute db
  

dev :: IO ()
dev = do
    print "http://localhost:8080/"    
    prepareApp >>= Warp.run 8080 
    
main :: IO ()
main = hardcoded "/test/ivr-examples.fcgi" <$> prepareApp >>= FCGI.run   
