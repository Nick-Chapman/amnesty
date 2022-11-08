
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

data Picture where  -- TODO: move to Emu (for use in UsingSDL)
  Draw :: XY Int -> RGB Word8 -> Picture
  Pictures :: [Picture] -> Picture

data XY a = XY { x :: a, y :: a } deriving (Eq,Ord,Functor)
data RGB a = RGB { r :: a, g :: a, b :: a } -- TODO: move to Emu (for use in UsingSDL)

data Key
  = KeyEnter
  | KeyShift
  | KeyZ
  | KeyX
  deriving (Eq,Ord,Enum,Bounded,Show)

newtype Keys = Keys { pressed :: Set Key }

instance Show a => Show (XY a) where show XY{x,y} = show (x,y)
instance Show a => Show (RGB a) where show RGB{r,g,b} = "RGB" ++ show (r,g,b)

data HiLo a = HiLo { hi :: a, lo :: a } deriving (Eq,Ord,Functor,Show)

data Reg = Reg1 | Reg2
  deriving (Eq,Ord,Show)
