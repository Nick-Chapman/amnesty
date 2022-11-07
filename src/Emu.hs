
module Emu
  ( State
  , state0, emulate
  ) where

import Control.DeepSeq (NFData)
import Data.Map (Map)
import Data.Word8 (Word8)
import Eff (Phase(..),Eff(..))
import GHC.Generics (Generic)
import Types (Picture(..),XY(..),RGB(..),Keys(..))
import qualified Data.Map as Map (empty,toList,insert)

data DuringEmulation
instance Phase DuringEmulation where
  type Byte DuringEmulation = Word8

type Effect a = Eff DuringEmulation a

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

emulate :: Keys -> State -> Effect () -> (Picture,State)
emulate Keys{pressed} s0 e0 = loop s0 e0 $ \s () -> mkPicture s
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

mkPicture :: State -> (Picture,State)
mkPicture state@State{emitted} = do
  (picture, state { emitted = Map.empty })
  where
    picture = Pictures
      [ Draw (fmap fromIntegral xy) rgb
      | (xy,rgb) <- Map.toList emitted
      ]
