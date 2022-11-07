
module Rom16k
  ( Rom16k
  , init
  , read
  ) where

import Prelude hiding (init,read)
import Data.Array (Array,(!),listArray)
import Data.Word8 (Word8)

data Rom16k = Rom16k { arr :: Array Int Word8 }

init :: [Word8] -> Rom16k
init bytes = if
  | n == size -> Rom16k { arr = listArray (0,size-1) bytes }
  | otherwise -> error $ "Rom16k.init: " <> show n
  where
    n = length bytes

read :: Rom16k -> Int -> Word8
read Rom16k{arr} n = if
  | inRange n -> arr ! n
  | otherwise -> error $ "Rom16k.read: " <> show n

inRange :: Int -> Bool
inRange n = n >= 0 && n < size

size :: Int
size = 0x4000 -- 16k
