
module Emu
  ( emulate
  ) where

import Data.Map (Map)
import Data.Word8 (Word8)
import Eff (Phase(..),Eff(..))
import qualified Data.Map as Map (fromList,lookup)

--[emulation]---------------------------------------------------------

data DuringEmulation

instance Phase DuringEmulation where
  type Byte DuringEmulation = Word8
  type Col DuringEmulation = Colour

data Colour = Colour Word8 deriving Show

type Effect a = Eff DuringEmulation a

emulate :: Effect a -> [a]
emulate e0 = outer state0
  where
    outer s = inner s e0 $ \s a -> a : outer s

    inner :: State -> Effect b -> (State -> b -> r) -> r
    inner s e k = case e of
      Ret x -> k s x
      Bind e f -> inner s e $ \s a -> inner s (f a) k
      IncB b -> k s (b+1)
      LitB n -> k s n
      LookupRom b -> do
        let State{rom} = s
        k s (romLookup rom b)
      SetPPUReg1 b ->
        k s { reg1 = b } ()
      GettPPUReg1 -> do
        let State{reg1} = s
        k s reg1
      MakeCol b -> do
        k s (Colour b)


-- concrete state of the entire system; whatever is necessary to emulate
data State = State { reg1 :: Word8, rom :: Rom }

state0 :: State
state0 = State { reg1 = 42, rom = theRom }

-- type for 256 byte rom
data Rom = Rom (Map Word8 Word8)

romLookup :: Rom -> Word8 -> Word8
romLookup (Rom m) k =
  maybe (error (show ("romLookup",k))) id $ Map.lookup k m

theRom :: Rom
theRom = Rom $ Map.fromList [ (b,b) | b <- [0..255] ]
