{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns, MultiParamTypeClasses, FlexibleInstances #-}
{-|
Module      : Network.Wai.Twilio.IVR
Description : A wai implementation for the Twilio IVR coroutines
Copyright   : (c) 2015 Steve Kollmansberger
Maintainer  : steve@kolls.net
Stability   : experimental

Exposes coroutine-based Twilio IVR from 'Twilio.IVR' as a wai subroute
for web hosting.
-}
module Network.Wai.Twilio.IVR (TIVRDB, TIVRSubRoute(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Coroutine
import qualified Control.Monad.Coroutine.SuspensionFunctors as SF
import Control.Lens.Setter

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as TX
import Data.TransientStore
import Data.UUID

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.Approot
import Network.Wai.Middleware.Routes

import Text.XML.Light
import Twilio.IVR

import Web.PathPieces

instance PathPiece UUID where
    fromPathPiece = fromString.TX.unpack
    toPathPiece = TX.pack.toString

type TIVRDB = TransientStore (String -> TwilioIVRCoroutine ())

-- | The wai subroute definition. Definer must provide a cross-request stateful 'TIVRDB' and the 'root' entry-point for incoming IVR calls.
data TIVRSubRoute = TIVRSubRoute {
    db :: TIVRDB,                           -- ^ This data store must be persisted across requests to provide coroutine resume when Twilio responds
    root :: Call -> TwilioIVRCoroutine ()   -- ^ This function provides the entry point into the IVR when a call is received
    }    

    

mkRouteSub "TIVRSubRoute" "RenderRoute" [parseRoutes| 
 /tivr         New POST
 /tivr/#UUID   Continue POST
|]


asXml :: Element -> HandlerM sub master ()
asXml e = asContent "text/xml" (TX.pack (showTopElement e))

queryLookup :: [(TX.Text, Maybe TX.Text)] -> TX.Text -> String
queryLookup query key = case Prelude.lookup key query of    
    (Just (Just val)) -> TX.unpack val
    _ -> ""

executeTIVRStep :: RenderRoute master => TIVRDB -> TwilioIVRCoroutine () -> HandlerM TIVRSubRoute master [Content]
executeTIVRStep db cont = do
    -- execute the coroutine, generating a result
    result <- liftIO $ resume cont
    -- every result is either a further coroutine (Left), or the final termination (right)
    case result of
        (Left (SF.Request eresp cont')) ->
            -- if the coroutine continues, it might be continuing with a non-interactive entry (like Say)
            -- we can just accumulate these, we don't need to send and redirect back for each one
            -- Or, it might be an interactive entry (like Gather)
            -- in which case, we have to go out for a response
            case eresp of            
            
                (Left iresp) -> do -- interactive entry
                    -- we're going to have to suspend while the request is handled by Twillio
                    -- so place it into the transient store and send the ID for resuming
                    newid <- liftIO $ insert db cont'
                    -- build the URL for the action
                    rootUrl <- getApprootMay <$> request
                    routeUrl <- showRouteSub
                    let url = TX.unpack $ routeUrl (Continue newid)
                    let url' = case rootUrl of
                            Nothing -> url
                            (Just root) -> BSC.unpack root ++ url

                    -- modify the interactive entry's action to point to the new url, and send
                    return [renderTwiML $ Left ((action .~ url') iresp)] 
                    
                (Right resp) -> do -- Non-interactive entry
                    -- recursively continue processing, acquiring further entries leading to either
                    -- termination or another interactive entry
                    reslt <- executeTIVRStep db 
                        (cont' "") -- Note: the continuation requires a parameter, but since it's non-interactive, we provide no digits. 
                                   -- This will be forcably ignored anyways by the wrappers
                    return $ renderTwiML eresp:reslt                
                    
        (Right final) -> return [renderTwiML $ Right Hangup] -- All processing done, make sure system hangs up


    

postNew :: RenderRoute master => HandlerS TIVRSubRoute master
postNew = runHandlerM $ do
    TIVRSubRoute db beginTCR <- sub    
    -- for a new request, load the call params out of the body
    query <- queryLookup <$> parseQueryText <$> rawBody        
    let call = Call (query "CallSid") (query "AccountSid") (query "From")
    -- execute the first step using the specified entry point
    out <- executeTIVRStep db (beginTCR call)        
    asXml $ makeRootTwiML out



postContinue :: RenderRoute master => UUID -> HandlerS TIVRSubRoute master
postContinue id = runHandlerM $ do
  TIVRSubRoute db _  <- sub
  -- we're not going to re-provide the call params (although they should all be there)
  -- instead, we're only going to go for the digits, which is what might have changed
  query <- queryLookup <$> parseQueryText <$> rawBody           
  -- using the id given, extract the coroutine out of the transient store
  (Just cont) <- liftIO $ pop db id 
  -- provide the digits and continue execution
  out <- executeTIVRStep db $ cont (query "Digits")
  asXml $ makeRootTwiML out
