
module Behaviour (Behaviour(..),Report(..)) where

import Data.Map (Map)
import Types (Keys,Reg)
import Data.Word
import Frame (Frame)

data Behaviour where
  Render :: Frame -> Report -> Behaviour -> Behaviour
  Poll :: (Keys -> Behaviour) -> Behaviour
  Log :: String -> Behaviour -> Behaviour

data Report = Report
  { regs :: Map Reg Word8
  , vmemReadCount :: Int
  , vramWriteCount :: Int
  }
