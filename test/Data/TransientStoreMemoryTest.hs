-- runhaskell -isrc test/Data/TransientStoreMemoryTest.hs
-- ghc -isrc test/Data/TransientStoreMemoryTest.hs -o memtest
module Main where

import Control.Concurrent
import Control.Parallel.Strategies

import Data.TransientStore


-- adds 10 entries per second, should equalize at 10 entries total since they live 1 second
accumNoPop db = do
        let val = show [1..100000]        
        insert db (val `using` rdeepseq)
        threadDelay 100000
        accumNoPop db


addPop db = do       
        let val = show [1..100000]        
        key <- insert db (val `using` rdeepseq)
        threadDelay 100000
        pop db key
        addPop db 
        
main = do
    putStrLn "Begin"
    db <- create 1 :: IO (TransientStore String)
    forkIO (accumNoPop db)
    forkIO (addPop db)    
    putStrLn "Press key to end"
    getChar
    putStrLn "Ended"