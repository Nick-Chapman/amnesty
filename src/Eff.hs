
module Eff
  ( Phase(..)
  , Eff(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Word8 (Word8)
import Types (Key,XY,RGB,HiLo)

class ( Show (Byte p)
      ) => Phase p where
  type Byte p

data Eff p x where
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y
  LitB :: Word8 -> Eff p (Byte p)
  AddB :: Byte p -> Byte p -> Eff p (Byte p)


  IsPressed :: Key -> Eff p Bool
  EmitPixel :: XY (Byte p) -> RGB (Byte p) -> Eff p ()
  SetPPUReg1 :: Byte p -> Eff p ()
  GetPPUReg1 :: Eff p (Byte p)
  SetPPUReg2 :: Byte p -> Eff p ()
  GetPPUReg2 :: Eff p (Byte p)

  ReadVmem :: HiLo (Byte p) -> Eff p (Byte p)
  TestBit :: Byte p -> Int -> Eff p Bool -- TODO: kill, subsumed by TestBitB
  TestBitB :: Byte p -> Byte p -> Eff p Bool
  EqB :: Byte p -> Byte p -> Eff p Bool -- TODO: better, return (Bit p)?

  BwAnd :: Byte p -> Byte p -> Eff p (Byte p)
  BwOr :: Byte p -> Byte p -> Eff p (Byte p)
  ShiftL :: Byte p -> Int -> Eff p (Byte p)
  ShiftR :: Byte p -> Int -> Eff p (Byte p)

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind
