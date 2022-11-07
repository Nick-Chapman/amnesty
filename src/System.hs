
module System (Pix,top)
  where

import Data.Word8 (Word8)
import Eff (Phase(..),Eff(..))

top :: Eff p [Pix p]
top = sequence [ pixel (x,y) | x <- [1..4], y <- [1..3] ]

data Pix p = Pix (Byte p, Byte p, Col p)
deriving instance Phase p => Show (Pix p)

pixel :: (Word8,Word8) -> Eff p (Pix p)
pixel (x,y) = do
  let n = y*10 + x
  a <- LitB n
  b <- LookupRom a
  c <- MakeCol b
  x <- LitB x
  y <- LitB y
  pure (Pix (x,y,c))

