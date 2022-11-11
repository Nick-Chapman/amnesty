
module Emulate (Effect,emulate) where

import Behaviour (Behaviour(..),Report(..))
import Col6 (Col6,makeCol6)
import Data.Bits (testBit,(.&.),(.|.),shiftR,shiftL)
import Data.Map (Map)
import Data.Word (Word8,Word16)
import Eff (Phase(..),Eff(..))
import Frame (makeFrame)
import NesFile (NesFile(..))
import Rom8k (Rom8k)
import Text.Printf (printf)
import Types (XY(..),Keys(..),HiLo(..),Reg(..))
import qualified Data.Map as Map (empty,insert,lookup,fromList)
import qualified Rom8k (read)

data DuringEmulation
instance Phase DuringEmulation where
  type Bit DuringEmulation = Bool
  type Byte DuringEmulation = Word8

type Effect a = Eff DuringEmulation a

emulate :: NesFile -> Effect () -> Behaviour
emulate nesFile e0 = outer state0
  where
    outer :: State -> Behaviour
    outer s0 = do
      let NesFile { chrs = [chr1] } = nesFile
      Poll $ \keys -> do
        let context = Context { chr1, keys }
        inner context s0 e0 $ \s1 () -> do
          let State{emitted,regs,vmemReadCount,vramWriteCount} = s1
          let state = resetState s1
          let frame = makeFrame emitted
          let report = Report { vmemReadCount, vramWriteCount, regs }
          Render frame report (outer state)

data Context = Context
  { chr1 :: Rom8k
  , keys :: Keys
  }

inner :: Context -> State -> Effect b -> (State -> b -> Behaviour) -> Behaviour
inner c@Context{chr1,keys} s@State{vmemReadCount,vramWriteCount} e k = case e of
  Ret x -> k s x
  Bind e f -> inner c s e $ \s a -> inner c s (f a) k
  If b -> k s b
  Repeat n eff ->
    inner c s (sequence_ [ eff | _ <- [0::Int .. n-1] ]) k
  IsPressed key -> do
    let Keys{pressed} = keys
    k s (key `elem` pressed)
  EmitPixel xy byte -> do
    k (emitPixel xy (makeCol6 byte) s) ()
  ReadVmem a -> do
    --Log (printf "ReadVmem(%07d): %s" vmemReadCount (show a)) $ do
    k s { vmemReadCount = 1 + vmemReadCount } (readVmem chr1 s a)
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
    k s (b `testBit` fromIntegral n)
  EqB b1 b2 -> do
    k s (b1 == b2)
  AddB a b -> do
    k s (a+b)
  SubtractB a b -> do
    k s (a-b)
  BwAnd b1 b2 -> do
    k s (b1 .&. b2)
  BwOr b1 b2 -> do
    k s (b1 .|. b2)
  ShiftL b n -> do
    k s (b `shiftL` n)
  ShiftR b n -> do
    k s (b `shiftR` n)


data State = State
  { emitted :: Map (XY Word8) Col6
  , countEmitted :: Int
  , regs :: Map Reg Word8
  , vmemReadCount :: Int
  , vramWriteCount :: Int
  , vram :: Map Word16 Word8 -- for nametable
  }

state0 :: State
state0 = State
  { emitted = Map.empty
  , countEmitted = 0
  , regs = regs0
  , vmemReadCount = 0
  , vramWriteCount = 0
  , vram = Map.empty
  }

regs0 :: Map Reg Word8
regs0 = Map.fromList
  [ (RegP,1)
  ]

resetState :: State -> State
resetState state = state
  { emitted = Map.empty
  , countEmitted = 0
  , vmemReadCount = 0
  , vramWriteCount = 0
  }

readVmem :: Rom8k -> State -> HiLo Word8 -> Word8
readVmem chr1 State{vram} HiLo{hi,lo} = do
  let a :: Word16 = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
  if
    | a < 0x2000 -> Rom8k.read chr1 a
    | a < 0x3000 -> maybe 0 id $ Map.lookup a vram
    | True -> error $ printf "readVmem: %x" a

emitPixel :: XY Word8 -> Col6 -> State -> State
emitPixel xy col s = do
  let State{emitted,countEmitted=c} = s
  let expectedXY =
        XY { x = fromIntegral (c `mod` 256)
           , y = fromIntegral (c `div` 256)
           }
  case xy /= expectedXY of
    True -> error (show ("EmitPixel:c=",c,"xy=",xy,"expected=",expectedXY))
    False ->
      s { emitted = Map.insert xy col emitted
        , countEmitted = 1 + c
        }
