{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
{-|
Module      : Twilio.IVR
Description : A coroutine for interactive monadic Twilio IVR
Copyright   : (c) 2015 Steve Kollmansberger
Maintainer  : steve@kolls.net
Stability   : experimental

IVR with Twilio can be very painful, since every interaction cycle requires a discrete
request/response. This may be implemented using a separate web method for each possible
stage in a conversation. Tracking state during a call is also problematic for the same reason.
This module introduces a coroutine-based monad for Twilio IVR which makes extended
interactions and state maintenance as seamless as console I/O.
-}
module Twilio.IVR (Response (..), IResponse, Call(..), TwilioIVRCoroutine, 
    
    -- * Monadic functions used for controlling IVR
    say, gather, hangup,     
    -- * Lens for gather
    message, timeout, finishOnKey, numDigits, action,
    -- * Lens for call
    callSid, accountSid, Twilio.IVR.from,
    
    -- * Support functions used to generate TwiML, not expected to be called by the user
    renderTwiML, makeRootTwiML) where

import Control.Applicative

import Control.Lens
import Control.Lens.Setter
import Control.Lens.TH

import Control.Monad (void)

import Control.Monad.Coroutine
import qualified Control.Monad.Coroutine.SuspensionFunctors as SF
import Control.Monad.Trans.Class

import Text.XML.Light
import Twilio.Key

-- | A single interactive response entry, that expects the user the give input
data IResponse =         
    Gather { 
        _message        :: String,             
        _timeout        :: Int,             -- ^ Timeout, in seconds, to wait between each digit
        _finishOnKey    :: Maybe Key,       -- ^ Optionally, the key to immediately terminate gathering
        _numDigits      :: Int,             -- ^ Maximum number of digits to gather                
        _action         :: String}
    deriving (Show, Eq)        
makeLenses ''IResponse

defaultGather = Gather "" 5 (Just KPound) 50 ""

-- | A single non-interactive response entry, that does not stop for user input
data Response =                 
    Say { _sayMessage :: String } |     
    Hangup                              
    deriving (Show, Eq)
makeLenses ''Response


-- | Information about the call
data Call = 
    Call { 
        _callSid :: String,         -- ^ The unique Twilio SID        
        _accountSid :: String,      -- ^ Your account SID
        _from :: String }           -- ^ The phone number or identifier of the caller
    deriving (Show)
makeLenses ''Call

-- | The main monadic type for IVR conversations
type TwilioIVRCoroutine a = Coroutine (SF.Request (Either IResponse Response) [Key]) IO a

-- | Speak a message to the user
say :: String -> TwilioIVRCoroutine ()
say msg = 
    void $ -- the "result" here is always the empty string, so discard it
    SF.request $ Right $ Say msg  


-- | Receive keypad entry from the user
gather :: String                    -- ^ A message to 'say' inside the Gather command
    -> (IResponse -> IResponse)     -- ^ A lens to change gather settings.
                                    -- Lens available are:
                                    --
                                    -- [@timeout        .~ (n :: Int)@]  Sets the timeout, in seconds, to wait between each digit
                                    -- [@finishOnKey    .~ (k :: Maybe Key)@]  Optionally, sets the key to immediately terminate gathering
                                    -- [@numDigits      .~ (n :: Int)@]  Sets the maximum number of digits to listen for                                    
    -> TwilioIVRCoroutine [Key]     -- ^ Bind to the keys to receive the caller's keypad entry
gather msg lens = SF.request <$> Left <$> lens $ (message .~ msg ) defaultGather

-- | Terminate the call
hangup :: TwilioIVRCoroutine ()
hangup = void $ SF.request $ Right Hangup




string :: String -> Content
string str = Text $ CData CDataText str Nothing


renderTwiML :: Either IResponse Response -> Content
renderTwiML (Left (Gather{..})) = Elem $ unode "Gather" (renderTwiML (Right $ Say _message)) &
    add_attrs [
        Attr (unqual "timeout") (show _timeout),
        Attr (unqual "finishOnKey") (maybe "" show _finishOnKey),
        Attr (unqual "numDigits") (show _numDigits),
        Attr (unqual "method") "POST",
        Attr (unqual "action") _action ]
renderTwiML (Right (Say msg)) = Elem $ unode "Say" (string msg) 
renderTwiML (Right Hangup) = Elem $ unode "Hangup" ()

makeRootTwiML :: [Content] -> Element
makeRootTwiML = unode "Response"

