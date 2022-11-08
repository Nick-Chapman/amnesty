
module Emu
  ( Context, makeContext
  , State , state0, emulate
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (testBit,(.&.),(.|.),shiftR,shiftL)
import Data.Map (Map)
import Data.Word (Word8,Word16)
import Eff (Phase(..),Eff(..))
import GHC.Generics (Generic)
import NesFile (NesFile(..))
import Rom8k (Rom8k)
import Types (Picture(..),XY(..),RGB(..),Keys(..),HiLo(..))
import qualified Data.Map as Map (empty,toList,insert)
import qualified Rom8k (read)

data DuringEmulation
instance Phase DuringEmulation where
  type Byte DuringEmulation = Word8

type Effect a = Eff DuringEmulation a

data Context = Context
  { chr1 :: Rom8k
  }

makeContext :: NesFile -> Context
makeContext nesFile = do
  let NesFile { chrs = [chr1] } = nesFile
  Context { chr1 }

data State = State
  { emitted :: Map (XY Word8) (RGB Word8)
  , reg1 :: Word8
  , reg2 :: Word8
  }
  deriving (Generic,NFData)

state0 :: State
state0 = State
  { emitted = Map.empty
  , reg1 = 100
  , reg2 = 100
  }

emulate :: Effect () -> Context -> Keys -> State -> (Picture,State)
emulate e0 context Keys{pressed} s0 = loop s0 e0 $ \s () -> mkPicture s
  where
    loop :: State -> Effect b -> (State -> b -> r) -> r
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      LitB n -> k s n
      AddB a b -> k s (a+b)
      EmitPixel xy rgb -> do
        let State{emitted} = s
        k s { emitted = Map.insert xy rgb emitted } ()
      IsPressed key -> do
        k s (key `elem` pressed)
      SetPPUReg1 b ->
        k s { reg1 = b } ()
      GetPPUReg1 -> do
        let State{reg1} = s
        k s reg1
      SetPPUReg2 b ->
        k s { reg2 = b } ()
      GetPPUReg2 -> do
        let State{reg2} = s
        k s reg2
      ReadVmem a -> do
        k s (readVmem context a)
      TestBit b n -> do
        k s (b `testBit` n)
      TestBitB b n -> do
        k s (b `testBit` fromIntegral n)
      EqB b1 b2 -> do
        k s (b1 == b2)
      BwAnd b1 b2 -> do
        k s (b1 .&. b2)
      BwOr b1 b2 -> do
        k s (b1 .|. b2)
      ShiftR b n -> do
        k s (b `shiftR` n)
      ShiftL b n -> do
        k s (b `shiftL` n)

readVmem :: Context -> HiLo Word8 -> Word8
readVmem Context{chr1} HiLo{hi,lo} = do
  let a :: Word16 = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
  Rom8k.read chr1 a

mkPicture :: State -> (Picture,State)
mkPicture state@State{emitted} = do
  (picture, state { emitted = Map.empty })
  where
    picture = Pictures
      [ Draw (fmap fromIntegral xy) rgb
      | (xy,rgb) <- Map.toList emitted
      ]
