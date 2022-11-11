
module Types
  ( Picture(..)
  , XY(..)
  , RGB(..)
  , Key(..)
  , Keys(..)
  , HiLo(..)
  , Reg(..)
  ) where

import Data.Set (Set)
import Data.Word8 (Word8)

data Picture where
  Draw :: XY Int -> RGB Word8 -> Picture
  Pictures :: [Picture] -> Picture

data XY a = XY { y :: a, x :: a } deriving (Eq,Ord,Functor) -- sort: y,x
data RGB a = RGB { r :: a, g :: a, b :: a }

data Key -- TODO: rename Buttons, corresponding to NES controller
  = KeyEnter
  | KeyShift
  | KeyZ
  | KeyX
  | KeyN -- select nametable
  | KeyP -- select/flip pat-table
  deriving (Eq,Ord,Enum,Bounded)

instance Show Key where
  show = \case
    KeyEnter -> "E"
    KeyShift -> "S"
    KeyZ -> "z"
    KeyX -> "x"
    KeyN -> "n"
    KeyP -> "p"

allKeys :: [Key]
allKeys = [KeyEnter,KeyShift,KeyZ,KeyX,KeyN,KeyP]

newtype Keys = Keys { pressed :: Set Key }

instance Show Keys where
  show Keys{pressed} =
    concat [ if b then show k else "-" | k <- allKeys , let b = k `elem` pressed ]

instance Show a => Show (XY a) where show XY{x,y} = show (x,y)
instance Show a => Show (RGB a) where show RGB{r,g,b} = "RGB" ++ show (r,g,b)

data HiLo a = HiLo { hi :: a, lo :: a } deriving (Eq,Ord,Functor,Show)

data Reg
  = RegZ | RegX | RegN | RegP -- driven by keye for dev/debug
  | RegScanX | RegScanY
  deriving (Eq,Ord,Show)
