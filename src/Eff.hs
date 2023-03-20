
module Eff
  ( Phase(..)
  , Eff(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Word (Word8,Word16)
import Types (Key,XY,HiLo,Reg)

class ( Show (Byte p)
      ) => Phase p where
  type Bit p
  type Byte p
  type Addr p

data Eff p x where
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y
  Assert :: String -> Bit p -> Eff p ()
  If :: Bit p -> Eff p Bool
  Repeat :: Int -> Eff p () -> Eff p ()
  IsPressed :: Key -> Eff p (Bit p)
  EmitPixel :: XY (Byte p) -> Byte p -> Eff p ()
  WriteVmem :: Addr p -> Byte p -> Eff p ()
  ReadVmem :: Addr p -> Eff p (Byte p)
  SetReg :: Reg -> Byte p -> Eff p ()
  GetReg :: Reg -> Eff p (Byte p)
  Bit0 :: Eff p (Bit p)
  Bit1 :: Eff p (Bit p)
  LitB :: Word8 -> Eff p (Byte p)
  LitA :: Word16 -> Eff p (Addr p)
  IteB :: Bit p -> Byte p -> Byte p -> Eff p(Byte p)
  MakeAddr :: HiLo (Byte p) -> Eff p (Addr p)
  SplitAddr :: Addr p -> Eff p (HiLo (Byte p))
  MakeByte :: (Bit p, Bit p, Bit p, Bit p, Bit p, Bit p, Bit p, Bit p) -> Eff p (Byte p)
  ShiftL :: Byte p -> Int -> Eff p (Byte p)
  ShiftR :: Byte p -> Int -> Eff p (Byte p)
  TestBit :: Byte p -> Byte p -> Eff p (Bit p)
  EqB :: Byte p -> Byte p -> Eff p (Bit p)
  AddB :: Byte p -> Byte p -> Eff p (Byte p)
  SubtractB :: Byte p -> Byte p -> Eff p (Byte p)
  BwAnd :: Byte p -> Byte p -> Eff p (Byte p)
  BwOr :: Byte p -> Byte p -> Eff p (Byte p)

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = Ret; (<*>) = ap
instance Monad (Eff p) where (>>=) = Bind
