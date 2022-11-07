
module Eff
  ( Phase(..)
  , Eff(..)
  ) where

import Control.Monad (ap,liftM)
import Data.Word8 (Word8)

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind

-- phase
class ( Show (Byte p)
      , Show (Col p)
      ) => Phase p where
  type Byte p
  type Col p

-- abstract effect type (finite)
data Eff p x where
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y
  IncB :: Byte p -> Eff p (Byte p)
  LitB :: Word8 -> Eff p (Byte p)

  MakeCol :: Byte p -> Eff p (Col p)

  -- some rom containing 256 bytes
  LookupRom :: Byte p -> Eff p (Byte p)

  -- random reg in my dummy ppu
  SetPPUReg1 :: Byte p -> Eff p ()
  GettPPUReg1 :: Eff p (Byte p)
