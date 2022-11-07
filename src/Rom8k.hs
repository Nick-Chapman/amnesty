
module Rom8k
  ( Rom8k
  , init
  , read
  ) where

import Prelude hiding (init,read)
import Data.Array (Array,(!),listArray)
import Data.Word8 (Word8)

data Rom8k = Rom8k { arr :: Array Int Word8 }

init :: [Word8] -> Rom8k
init bytes = if
  | n == size -> Rom8k { arr = listArray (0,size-1) bytes }
  | otherwise -> error $ "Rom8k.init: " <> show n
  where
    n = length bytes

read :: Rom8k -> Int -> Word8
read Rom8k{arr} n = if
  | inRange n -> arr ! n
  | otherwise -> error $ "Rom8k.read: " <> show n

inRange :: Int -> Bool
inRange n = n >= 0 && n < size

size :: Int
size = 0x2000 -- 8k
