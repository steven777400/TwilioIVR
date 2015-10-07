module Examples.EX3 where

import Control.Lens
import Control.Lens.Setter
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)

import Data.List (intersperse, isPrefixOf)
import Data.Time.LocalTime
import Data.Maybe (catMaybes)

import Twilio.Key
import Twilio.IVR

data User = User {
    lastname :: String,
    firstname :: String,
    extension :: Maybe String
    }
    deriving (Show)

users = [
    User "Smith" "Joe" $ Just "7734",
    User "Doe" "Jane" $ Just "1556",
    User "Doe" "Linda" Nothing,
    User "Samson" "Tina" $ Just "3443",
    User "O'Donald" "Jack" $ Just "5432",
    User "Sam-son" "Tony" $ Nothing
    ]

-- | match last name by removing non-keypad characters
matchingUsers :: [User] -> [Key] -> [User]
matchingUsers ux kx = filter (\u -> let lnky = catMaybes $ map letterToKey (lastname u) in
    isPrefixOf kx lnky) ux
    
    
describeUser :: User -> TwilioIVRCoroutine ()
describeUser u = do
    say (firstname u)
    say (lastname u)
    case extension u of
        (Just x) -> say $ "Phone extension " ++ (intersperse ' ' x)
        Nothing -> return ()


search :: Call -> TwilioIVRCoroutine ()
search _ = do
    say "Welcome."
    name <- gather "Please enter the last name of the person you wish to find.  You do not have to enter the entire name. Finish with the pound key."
        ((finishOnKey .~ Just KPound).(timeout .~ 30))
    case matchingUsers users name of
        []  -> say "Sorry, no matching results found."
        [x] -> describeUser x
        xs  -> do
            say $ "We found "++(show $ length xs)++" matching users."  
            mapM_ describeUser xs
    say "Have a nice day."            
