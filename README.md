# Twilio IVR

A fluent Twilio IVR library for Haskell. 

IVR with Twilio can be very painful, since every interaction cycle requires a discrete
request/response. This may be implemented using a separate web method for each possible
stage in a conversation. Tracking state during a call is also problematic for the same reason.
This library introduces a coroutine-based monad for Twilio IVR which makes extended
interactions and state maintenance as seamless as console I/O.

The module requires wai for web services, but can work with any wai handler, such as the Warp server 
or as a FastCGI process on a shared server.

See examples folder.

Note: The Call data and Twilio methods still need to be fleshed out.  They exist in a bare bones or proof of concept form.

```hs
simple :: Call -> TwilioIVRCoroutine ()
simple call = do
    say $ "hello to you " ++ (intersperse ' ' $ call ^. Twilio.IVR.from)
    num <- gather "Please enter your five digit sign in code" (numDigits .~ 5)    
    let ok = num == "12345"
    say "Please wait"
    say "While we process your request"
    -- We can do some IO, like if you need to read from a database
    lift $ print ok
    if ok then say "You've signed in correctly!" else say "We were unable to verify your account"
```    
