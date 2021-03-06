{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Twilio.Key
Description : Keys used for Twilio phone IVR
Copyright   : (c) 2015 Steve Kollmansberger
Maintainer  : steve@kolls.net
Stability   : experimental

Basic representation, including read and show, for digits (0-9), star, and pound.
-}
module Twilio.Key (Key(..), letterToKey, keyToLetter) where

import Control.Applicative
import Data.Char (toUpper)
import Data.List
import Text.Read (readMaybe)

-- | A physical phone key which can be received from a gather
data Key
  = K0      -- ^ 0
  | K1      -- ^ 1
  | K2      -- ^ 2
  | K3      -- ^ 3
  | K4      -- ^ 4
  | K5      -- ^ 5
  | K6      -- ^ 6
  | K7      -- ^ 7
  | K8      -- ^ 8
  | K9      -- ^ 9
  | KStar   -- ^ \*
  | KPound  -- ^ #
  deriving (Eq)
  
instance Show Key where
    show K0     = "0"
    show K1     = "1"
    show K2     = "2"
    show K3     = "3"
    show K4     = "4"
    show K5     = "5"
    show K6     = "6"
    show K7     = "7"
    show K8     = "8"
    show K9     = "9"
    show KStar  = "*"
    show KPound = "#"
    showList kx xs = concatMap show kx++xs


readKeyList :: String -> [Key]  
readKeyList = map (read.(:[]))   

    
instance Read Key where
    readsPrec _ "0"    = [(K0, "")]
    readsPrec _ "1"    = [(K1, "")]
    readsPrec _ "2"    = [(K2, "")]
    readsPrec _ "3"    = [(K3, "")]
    readsPrec _ "4"    = [(K4, "")]
    readsPrec _ "5"    = [(K5, "")]
    readsPrec _ "6"    = [(K6, "")]
    readsPrec _ "7"    = [(K7, "")]
    readsPrec _ "8"    = [(K8, "")]
    readsPrec _ "9"    = [(K9, "")]
    readsPrec _ "*"    = [(KStar, "")]
    readsPrec _ "#"    = [(KPound, "")]
    readsPrec _ _      = []    
    readList kx        = [(readKeyList kx, [])]


letterMapping = [
    (K2, "ABC"),
    (K3, "DEF"),
    (K4, "GHI"),
    (K5, "JKL"),
    (K6, "MNO"),
    (K7, "PQRS"),
    (K8, "TUV"),
    (K9, "WXYZ")]


-- | Given a character, converts it to a key symbol.  Not case sensitive.
letterToKey :: Char -> Maybe Key
letterToKey c = case readMaybe [c] of
    (Just k) -> Just k
    _ -> fst <$> find (\(_, lx) -> elem (toUpper c) lx) letterMapping

-- | Given a Key, returns all letter characters associated with it.
keyToLetter :: Key -> [Char]
keyToLetter k = case find (\(ke, _) -> k == ke) letterMapping of
    (Just (_, lx)) -> lx
    Nothing -> []
