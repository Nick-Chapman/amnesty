
module Emu
  ( Context, makeContext
  , State , state0, emulate
  ) where

import Data.Bits (testBit,(.&.),(.|.),shiftR,shiftL)
import Data.Map (Map)
import Data.Word (Word8,Word16)
import Eff (Phase(..),Eff(..))
import NesFile (NesFile(..))
import Rom8k (Rom8k)
import Types (Picture(..),XY(..),RGB(..),Keys(..),HiLo(..),Reg(..))
import qualified Data.Map as Map (empty,toList,insert,lookup)
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
  , countEmitted :: Int
  , regs :: Map Reg Word8
  }

state0 :: State
state0 = State
  { emitted = Map.empty
  , countEmitted = 0
  , regs = Map.empty
  }

emulate :: Effect () -> Context -> Keys -> State -> (Picture,State)
emulate e0 context Keys{pressed} s0 = loop s0 e0 $ \s () -> mkPicture s
  where
    loop :: State -> Effect b -> (State -> b -> r) -> r
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k

      IsPressed key -> do
        k s (key `elem` pressed)

      EmitPixel xy col -> do
        let State{emitted,countEmitted=c} = s
        let expectedXY =
              XY { x = fromIntegral (c `mod` 256)
                 , y = fromIntegral (c `div` 256)
                 }
        case xy /= expectedXY of
          True -> error (show ("EmitPixel:c=",c,"xy=",xy,"expected=",expectedXY))
          False ->
            k s { emitted = Map.insert xy (col2rgb col) emitted
                , countEmitted = 1 + c
                } ()

      ReadVmem a -> do
        k s (readVmem context a)
      GetReg r -> do
        let State{regs} = s
        k s (maybe 0 id $ Map.lookup r regs)
      SetReg r b -> do
        let State{regs} = s
        k s { regs = Map.insert r b regs } ()

      LitB n -> do
        k s n
      TestBit b n -> do
        k s (b `testBit` n)
      TestBitB b n -> do
        k s (b `testBit` fromIntegral n)
      EqB b1 b2 -> do
        k s (b1 == b2)
      AddB a b -> do
        k s (a+b)
      BwAnd b1 b2 -> do
        k s (b1 .&. b2)
      BwOr b1 b2 -> do
        k s (b1 .|. b2)
      ShiftL b n -> do
        k s (b `shiftL` n)
      ShiftR b n -> do
        k s (b `shiftR` n)

col2rgb :: Word8 -> RGB Word8
col2rgb = \case
  0 -> RGB { r = 0, g = 0, b = 0 }
  1 -> RGB { r = 255, g = 0, b = 0 }
  2 -> RGB { r = 0, g = 255, b = 0 }
  3 -> RGB { r = 0, g = 0, b = 255 }
  n -> error (show ("col2rgb",n))

readVmem :: Context -> HiLo Word8 -> Word8
readVmem Context{chr1} HiLo{hi,lo} = do
  let a :: Word16 = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
  Rom8k.read chr1 a

mkPicture :: State -> (Picture,State)
mkPicture state@State{emitted,countEmitted=c} = do
  let expected = 256*240
  if c /= expected
    then error (show ("mkPicture,c=",c,"expected=",expected))
    else do
    (picture, state { emitted = Map.empty, countEmitted = 0 })
  where
    picture = Pictures
      [ Draw (fmap fromIntegral xy) rgb
      | (xy,rgb) <- Map.toList emitted
      ]
