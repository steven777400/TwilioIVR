module Examples.EX1 where

import Control.Lens
import Control.Lens.Setter
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)

import Data.List (intersperse)

import Twilio.IVR


simple :: Call -> TwilioIVRCoroutine ()
simple call = do
    say $ "hello to you " ++ (intersperse ' ' $ call ^. Twilio.IVR.from)
    num <- gather "Please enter your five digit sign in code" (numDigits .~ 5)    
    let ok = num == (read "12345")
    say "Please wait"
    say "While we process your request"
    -- We can do some IO, like if you need to read from a database
    lift $ print ok
    if ok then say "You've signed in correctly!" else say "We were unable to verify your account"
