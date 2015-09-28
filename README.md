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
