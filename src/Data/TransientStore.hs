{-# LANGUAGE BangPatterns #-}
{-|
Module      : Data.TransientStore
Description : An IO-bound store of values, referenced by a serializable identifier, which expire automatically after a fixed time
Copyright   : (c) 2015 Steve Kollmansberger
Maintainer  : steve@kolls.net
Stability   : experimental

Primarily to support web service based coroutines, where a coroutine (which cannot be serialized)
must be stored while a serializable identifier is sent to the client, which may or may not use it
later to continue the coroutine.  Since some instances will not be resumed, a memory leak ensues
unless old instances are cleared out, hence, the time limited factor.

Instances are guaranteed not to be removed before the specified fixed time, but may linger for some
time after, depending on implementation.
-}
module Data.TransientStore (TransientStore, create, peek, pop, insert, delete) where

import Control.Applicative

import qualified Data.HashMap.Strict as HM
import Data.IORef 
import Data.Time.Clock
import Data.UUID

import System.Random



data ITransientStore a = ITransientStore {
    ttl :: NominalDiffTime,                     -- ^ The minimum length of time a value is guaranteed to survive in the store after insertion
    lastCleanup :: UTCTime,                     -- ^ The last time a cleanup (search for and removal of old values) was run
    store :: !(HM.HashMap UUID (UTCTime, a))    -- ^ The actual store of key/value pairs, together with the time the value was inserted
    }

type TransientStore a = IORef (ITransientStore a)
    
-- | Create a new transient store and embed it in the IO monad.
create :: NominalDiffTime ->    -- ^ The "Time to Live", the minimum length of time a value is guaranteed to survive in the store after insertion
    IO (TransientStore a)
create ttl = do
    now <- getCurrentTime    
    newIORef $ ITransientStore ttl now HM.empty           
    
-- | Retrieve a value from the transient store without removing it.  Instead, most users will call 'pop' to retrieve and remove in one operation.
peek :: TransientStore a            -- ^ The transient store to look in
    -> UUID                         -- ^ The key to look for (as returned by 'insert')
    -> IO (Maybe a)                 -- ^ The value, if it exists and is not expired, or Nothing otherwise
peek tstore id = do        
    -- Check if cleanup is needed, and retrieve value
    now <- getCurrentTime    
    atomicModifyIORef' tstore (\istore -> let cleanedIStore = cleanIfNeeded now istore in 
        (cleanedIStore, snd <$> HM.lookup id (store cleanedIStore)))    

-- | Insert a new value into the store, returning a unique, serializable identifier    
insert :: TransientStore a          -- ^ The transient store to insert into
    -> a                            -- ^ The value to insert
    -> IO UUID                      -- ^ A unique identifier representing the value
insert tstore !value = do
    now <- getCurrentTime    
    newid <- randomIO :: IO UUID    
    atomicModifyIORef' tstore (\istore -> let cleanedIStore = cleanIfNeeded now istore in 
        (cleanedIStore { store = HM.insert newid (now, value) (store cleanedIStore)}, ()))
    return newid
    
-- | Delete a value from the store by key. If the key does not exist or has expired, the command is ignored.
delete :: TransientStore a          -- ^ The transient store to look in
    -> UUID                         -- ^ The key to look for (as returned by 'insert')
    -> IO ()
delete tstore id = do 
    -- Check if cleanup is needed, and remove value
    now <- getCurrentTime       
    atomicModifyIORef' tstore (\istore -> let cleanedIStore = cleanIfNeeded now istore in 
        (cleanedIStore { store = HM.delete id (store cleanedIStore) }, ()))
        
-- | Retrieve a value from the transient store and remove it.
pop :: TransientStore a            -- ^ The transient store to look in
    -> UUID                         -- ^ The key to look for (as returned by 'insert')
    -> IO (Maybe a)                 -- ^ The value, if it exists and is not expired, or Nothing otherwise
pop tstore id = do          
    mvalue <- peek tstore id
    delete tstore id
    return mvalue
    
-- | Based on the ttl and last cleaned time, maybe clean the store, updating the last cleaned time if so    
cleanIfNeeded :: UTCTime -> ITransientStore a -> ITransientStore a
-- we only want to filter if some time has elapsed, otherwise all the operations get really expensive !
-- we'll base it on ttl, so, in worst case, an element could live about 2x ttl
cleanIfNeeded now istore    | addUTCTime (ttl istore) (lastCleanup istore) < now   = 
                                istore {
                                    lastCleanup = now,
                                    store = HM.filter (\(time, _) -> addUTCTime (ttl istore) time > now) (store istore) }
                            | otherwise = istore