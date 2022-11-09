
module Eff
  ( Phase(..)
  , Eff(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Word (Word8)
import Types (Key,XY,HiLo,Reg)

class ( Show (Byte p)
      ) => Phase p where
  type Byte p
  -- TODO: type Bit p
  -- TODO: type Addr p

data Eff p x where
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y

  IsPressed :: Key -> Eff p Bool
  EmitPixel :: XY (Byte p) -> Byte p -> Eff p ()

  WriteVmem :: HiLo (Byte p) -> Byte p -> Eff p ()
  ReadVmem :: HiLo (Byte p) -> Eff p (Byte p)
  SetReg :: Reg -> Byte p -> Eff p ()
  GetReg :: Reg -> Eff p (Byte p)

  LitB :: Word8 -> Eff p (Byte p)
  TestBit :: Byte p -> Byte p -> Eff p Bool
  EqB :: Byte p -> Byte p -> Eff p Bool -- TODO: better, return (Bit p)?
  AddB :: Byte p -> Byte p -> Eff p (Byte p)
  BwAnd :: Byte p -> Byte p -> Eff p (Byte p)
  BwOr :: Byte p -> Byte p -> Eff p (Byte p)
  ShiftL :: Byte p -> Int -> Eff p (Byte p)
  ShiftR :: Byte p -> Int -> Eff p (Byte p)

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind
