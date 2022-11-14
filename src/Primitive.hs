
module Primitive (P1(..),P2(..),evalP1,evalP2) where

import Data.Bits (testBit,shiftL,shiftR,(.&.),(.|.))
import Data.Word (Word8,Word16)
import Types (HiLo(..))

data P1 arg ret where
  MakeAddr :: P1 (HiLo Word8) Word16
  SplitAddr :: P1 Word16 (HiLo Word8)
  MakeByte :: P1 (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) Word8

data P2 arg1 arg2 ret where
  TestBit :: P2 Word8 Word8 Bool
  EqB :: P2 Word8 Word8 Bool
  AddB :: P2 Word8 Word8 Word8
  SubtractB :: P2 Word8 Word8 Word8
  BwAnd :: P2 Word8 Word8 Word8
  BwOr :: P2 Word8 Word8 Word8
  ShiftL :: P2 Word8 Int Word8
  ShiftR :: P2 Word8 Int Word8

deriving instance Show (P1 a b)
deriving instance Show (P2 a b r)

evalP1 :: P1 a r -> a -> r
evalP1 = \case
  MakeAddr -> \HiLo{hi,lo} -> fromIntegral hi `shiftL` 8 .|. fromIntegral lo
  SplitAddr -> \a -> HiLo { hi = fromIntegral a `shiftR` 8, lo = fromIntegral a .&. 0xff }
  MakeByte ->
    \(a,b,c,d,e,f,g,h) ->
      (if a then 128 else 0) .|.
      (if b then  64 else 0) .|.
      (if c then  32 else 0) .|.
      (if d then  16 else 0) .|.
      (if e then   8 else 0) .|.
      (if f then   4 else 0) .|.
      (if g then   2 else 0) .|.
      (if h then   1 else 0) .|.
      0

evalP2 :: P2 a b r -> a -> b -> r
evalP2 = \case
  TestBit -> \v n -> v `testBit` fromIntegral n
  EqB -> (==)
  AddB -> (+)
  SubtractB -> (-)
  BwAnd -> (.&.)
  BwOr -> (.|.)
  ShiftL -> shiftL
  ShiftR -> shiftR
