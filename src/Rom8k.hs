
module Rom8k
  ( Rom8k
  , init
  , read
  ) where

import Prelude hiding (init,read)
import Data.Array (Array,(!),listArray)
import Data.Word (Word8,Word16)

data Rom8k = Rom8k { arr :: Array Word16 Word8 }

init :: [Word8] -> Rom8k
init bytes = if
  | n == size -> Rom8k { arr = listArray (0,size-1) bytes }
  | otherwise -> error $ "Rom8k.init: " <> show n
  where
    n = fromIntegral (length bytes)

read :: Rom8k -> Word16 -> Word8
read Rom8k{arr} n = if
  | inRange n -> arr ! n
  | otherwise -> error $ "Rom8k.read: " <> show n

inRange :: Word16 -> Bool
inRange n = n >= 0 && n < size

size :: Word16
size = 0x2000 -- 8k
