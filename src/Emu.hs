
module Emu
  ( Context, makeContext
  , State , state0, emulate
  , FrameHash, Result(..)
  ) where

import Data.Bits (testBit,(.&.),(.|.),shiftR,shiftL)
import Data.Hashable (hash)
import Data.Map (Map)
import Data.Word (Word8,Word16)
import Eff (Phase(..),Eff(..))
import NesFile (NesFile(..))
import Rom8k (Rom8k)
import Text.Printf (printf)
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

data Result = Result
  { state :: State
  , picture :: Picture
  , frameHash :: FrameHash
  , regs :: Map Reg Word8
  , vmemReadCount :: Int
  , vramWriteCount :: Int
  }

data State = State
  { emitted :: Map (XY Word8) (RGB Word8)
  , countEmitted :: Int
  , emittedAcc :: [Word8] -- TODO: type Col to assert the 6bit restriction
  , regs :: Map Reg Word8
  , vmemReadCount :: Int
  , vramWriteCount :: Int
  , vram :: Map Word16 Word8 -- for nametable
  }

state0 :: State
state0 = State
  { emitted = Map.empty
  , countEmitted = 0
  , emittedAcc = []
  , regs = Map.empty
  , vmemReadCount = 0
  , vramWriteCount = 0
  , vram = Map.empty
  }

emulate :: Effect () -> Context -> Keys -> State -> Result
emulate e0 context Keys{pressed} s0 = loop s0 e0 $ \s () -> mkPicture s
  where
    loop :: State -> Effect b -> (State -> b -> r) -> r
    loop s@State{vmemReadCount,vramWriteCount} e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k

      IsPressed key -> do
        k s (key `elem` pressed)

      EmitPixel xy col -> do
        k (emitPixel xy col s) ()

      ReadVmem a -> do
        k s { vmemReadCount = 1 + vmemReadCount } (readVmem context s a)

      WriteVmem HiLo{hi,lo} b -> do
        let State{vram} = s
        let a :: Word16 = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
        k s { vramWriteCount = vramWriteCount + 1
            , vram = Map.insert a b vram
            } ()

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

readVmem :: Context -> State -> HiLo Word8 -> Word8
readVmem Context{chr1} State{vram} HiLo{hi,lo} = do
  let a :: Word16 = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
  if
    | a < 0x2000 -> Rom8k.read chr1 a
    | a < 0x3000 -> maybe 0 id $ Map.lookup a vram
    | True -> error $ printf "readVmem: %x" a

emitPixel :: XY Word8 -> Word8 -> State -> State
emitPixel xy col s = do
  let State{emitted,countEmitted=c,emittedAcc} = s
  let expectedXY =
        XY { x = fromIntegral (c `mod` 256)
           , y = fromIntegral (c `div` 256)
           }
  case xy /= expectedXY of
    True -> error (show ("EmitPixel:c=",c,"xy=",xy,"expected=",expectedXY))
    False ->
      s { emitted = Map.insert xy (col2rgb col) emitted
        , countEmitted = 1 + c
        , emittedAcc = col : emittedAcc
        }

mkPicture :: State -> Result
mkPicture state0@State{emitted,emittedAcc,countEmitted=c,regs
                      ,vmemReadCount,vramWriteCount} = do
  let expected = 256*240
  let n = length emittedAcc
  if (c /= expected) || (n /= expected)
    then error (show ("mkPicture,c/n=",c,n,"expected=",expected))
    else do
    let
      state = state0
        { emitted = Map.empty
        , countEmitted = 0
        , emittedAcc = []
        , vmemReadCount = 0
        , vramWriteCount = 0
        }
      picture = Pictures
        [ Draw (fmap fromIntegral xy) rgb
        | (xy,rgb) <- Map.toList emitted
        ]
      frameHash = makeFrameHash (reverse emittedAcc)
    Result { state, picture, frameHash, regs
           , vmemReadCount, vramWriteCount
           }

data FrameHash = FrameHash Int

makeFrameHash :: [Word8] -> FrameHash
makeFrameHash cols = FrameHash (hash cols)

instance Show FrameHash where
  show (FrameHash n) = printf "[%s]" (take 4 (printf "%xd" n) :: String)
