
module Rom16k
  ( Rom16k
  , init
  , read
  ) where

import Prelude hiding (init,read)
import Data.Array (Array,(!),listArray)
import Data.Word (Word8,Word16)

data Rom16k = Rom16k { arr :: Array Word16 Word8 }

init :: [Word8] -> Rom16k
init bytes = if
  | n == size -> Rom16k { arr = listArray (0,size-1) bytes }
  | otherwise -> error $ "Rom16k.init: " <> show n
  where
    n = fromIntegral (length bytes)

read :: Rom16k -> Word16 -> Word8
read Rom16k{arr} n = if
  | inRange n -> arr ! n
  | otherwise -> error $ "Rom16k.read: " <> show n

inRange :: Word16 -> Bool
inRange n = n >= 0 && n < size

size :: Word16
size = 0x4000 -- 16k
