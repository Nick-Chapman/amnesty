
module Emulate (Effect,emulate) where

import Behaviour (Behaviour(..),Report(..))
import Col6 (Col6,makeCol6)
import Data.Map (Map)
import Data.Word (Word8,Word16)
import Eff (Phase(..),Eff(..))
import Frame (makeFrame)
import NesFile (NesFile(..))
import Rom8k (Rom8k)
import Text.Printf (printf)
import Types (XY(..),Keys(..),Reg(..))
import qualified Data.Map as Map (empty,insert,lookup,fromList)
import qualified Rom8k (read)
import qualified Primitive as Prim

data DuringEmulation
instance Phase DuringEmulation where
  type Bit DuringEmulation = Bool
  type Byte DuringEmulation = Word8
  type Addr DuringEmulation = Word16

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

inner :: forall e. Context -> State -> Effect e -> (State -> e -> Behaviour) -> Behaviour
inner c@Context{chr1,keys} s@State{vmemReadCount,vramWriteCount} e k = case e of

  Ret x -> k s x
  Bind e f -> inner c s e $ \s a -> inner c s (f a) k
  If b -> k s b
  Repeat n eff -> inner c s (sequence_ [ eff | _ <- [0::Int .. n-1] ]) k
  IsPressed key -> k s (key `elem` pressed keys)
  EmitPixel xy byte -> k (emitPixel xy (makeCol6 byte) s) ()

  ReadVmem a -> do
    let v = readVmem chr1 s a
    --Log (printf "ReadVmem(%07d): %s --> %s" vmemReadCount (show a) (show  v)) $ do
    k s { vmemReadCount = 1 + vmemReadCount } v
  WriteVmem a v -> do
    -- TODO: share same MM abstraction for read/write vmem
    let State{vram} = s
    --Log (printf "WriteVmem(%07d): %s := %s" vmemReadCount (show a) (show v)) $ do
    k s { vramWriteCount = vramWriteCount + 1
        , vram = Map.insert a v vram
        } ()

  GetReg r -> do
    let State{regs} = s
    k s (maybe 0 id $ Map.lookup r regs)
  SetReg r b -> do
    let State{regs} = s
    k s { regs = Map.insert r b regs } ()

  Bit0 -> k s False
  Bit1 -> k s True
  LitB n -> k s n
  LitA n -> k s n

  MakeAddr x -> prim1 Prim.MakeAddr x
  SplitAddr x -> prim1 Prim.SplitAddr x
  MakeByte x -> prim1 Prim.MakeByte x

  TestBit x y -> prim2 Prim.TestBit x y
  EqB x y -> prim2 Prim.EqB x y
  AddB x y -> prim2 Prim.AddB x y
  SubtractB x y -> prim2 Prim.SubtractB x y
  BwAnd x y  -> prim2 Prim.BwAnd x y
  BwOr x y -> prim2 Prim.BwOr x y
  ShiftL x y -> prim2 Prim.ShiftL x y
  ShiftR x y -> prim2 Prim.ShiftR x y

  where
    prim1 :: forall x. Prim.P1 x e -> x -> Behaviour
    prim1 p1 x = k s (Prim.evalP1 p1 x)

    prim2 :: forall x y. Prim.P2 x y e -> x -> y -> Behaviour
    prim2 p2 x y = k s (Prim.evalP2 p2 x y)


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

readVmem :: Rom8k -> State -> Word16 -> Word8
readVmem chr1 State{vram} a = do
  if
    | a < 0x2000 -> Rom8k.read chr1 a
    | a < 0x3000 ->
      -- nametable(+atts). TODO: support mirroring & mapping to 2k VRAM
      maybe 0 id $ Map.lookup a vram

    | a < 0x3f00 -> error $ printf "readVmem: %x -- unexpected mirror read" a
    | a < 0x3f1f -> do
      -- palletes: TODO: map to sep 32 byte RAM space
        maybe 0 id $ Map.lookup a vram

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
