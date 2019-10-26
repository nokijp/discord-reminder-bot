{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.ReminderBot.HashCode
  ( HashCode(..)
  , hashCode
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Binary
import Data.Coerce
import Data.Digest.Pure.SHA
import Numeric

newtype HashCode = HashCode Word32 deriving (Eq, Ord, Enum, Num, Real, Integral)

instance Show HashCode where
  showsPrec _ (HashCode n) cs = let hex = showHex n ""
                                in replicate (internalBitSize `div` 4 - length hex) '0' ++ hex ++ cs

instance Read HashCode where
  readsPrec _ s = coerce (readHex s :: [(Word32, String)])

hashCode :: Binary a => a -> HashCode
hashCode = HashCode . decode . LBS.take (internalBitSize `div` 8) . bytestringDigest . sha256 . encode

internalBitSize :: Num a => a
internalBitSize = 32
