{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses #-}
module Main where

import Control.Applicative

import qualified Examples.EX1 as EX1

import qualified Data.TransientStore as TS

import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai.Handler.FastCGI as FCGI (run)
import Network.Wai.Middleware.Approot
import Network.Wai.Middleware.Routes
import Network.Wai.Twilio.IVR

import Twilio.IVR

getTIVRSubRoute :: (Call -> TwilioIVRCoroutine ()) -> MyRoute -> TIVRSubRoute
getTIVRSubRoute entry (MyRoute db) = TIVRSubRoute db entry

example1Route = getTIVRSubRoute EX1.simple

data MyRoute = MyRoute TIVRDB


mkRoute "MyRoute" [parseRoutes|
/ex1 Examples TIVRSubRoute example1Route
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
