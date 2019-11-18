{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.ReminderBot.HashCode
  ( HashCode(..)
  , hashCode
  , hashCodeInv
  ) where

import Data.Coerce
import Data.Word
import Numeric

newtype HashCode = HashCode Word64 deriving (Eq, Ord, Enum, Num, Real, Integral)

instance Show HashCode where
  showsPrec _ (HashCode n) cs = let hex = showHex n ""
                                in replicate (internalBitSize `div` 4 - length hex) '0' ++ hex ++ cs

instance Read HashCode where
  readsPrec _ s = coerce (readHex s :: [(Word64, String)])

hashCode :: Word64 -> HashCode
hashCode n = HashCode $ n * p

hashCodeInv :: HashCode -> Word64
hashCodeInv (HashCode n) = n * pInv

internalBitSize :: Num a => a
internalBitSize = 64

p :: Word64
p = 11400714819323198549  -- nearest prime of 2 ^ 64 / φ

pInv :: Word64
pInv = 6236490470931210493  -- p * pInv ≡ 1 (mod 2 ^ 64)
