module Examples.EX2 where

import Control.Lens
import Control.Lens.Setter
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)

import Data.List (intersperse)
import Data.Time.LocalTime

import Twilio.IVR

data User = User {
    uid :: String,
    name :: String,
    balance :: Int
    }

users = [
    User "1234" "Joe Smith" 150,
    User "2525" "Jane Doe" 267,
    User "4321" "Linda Doe" 350
    ]
    
-- Can use functions that return values in the monad
-- they do not need to include or terminate in a "gather"
-- they can also be recursive    
signin :: TwilioIVRCoroutine User
signin = do
    eid <- gather "Please enter your account id" (numDigits .~ 4)
    case filter (\u -> read (uid u) == eid) users of
        [] -> do
            say "Sorry, we don't recognize that id, please try again."
            signin
        [u] -> do
            say $ "Welcome " ++ (name u)
            return u

account :: Call -> TwilioIVRCoroutine ()
account call = do
    -- work in the IO monad
    (ZonedTime (LocalTime _ timeOfDay) _) <- lift getZonedTime
    let hours = todHour timeOfDay
    say $ "Good " ++ if (hours < 12) then "morning"
        else if (hours < 20) then "afternoon"
        else "evening"
    user <- signin
    say $ "Your account balance is " ++ (show $ balance user)
    hangup -- Not actually needed
    
    

