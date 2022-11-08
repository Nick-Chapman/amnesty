
module Types
  ( Picture(..)
  , XY(..)
  , RGB(..)
  , Key(..)
  , Keys(..)
  , HiLo(..)
  , Reg(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Set (Set)
import Data.Word8 (Word8)
import GHC.Generics (Generic)

data Picture where
  Draw :: XY Int -> RGB Word8 -> Picture
  Pictures :: [Picture] -> Picture
  deriving (Generic,NFData)

data XY a = XY { x :: a, y :: a } deriving (Eq,Ord,Generic,NFData,Functor)
data RGB a = RGB { r :: a, g :: a, b :: a } deriving (Generic,NFData)

data Key
  = KeyEnter
  | KeyShift
  | KeyZ
  | KeyX
  deriving (Eq,Ord,Enum,Bounded,Show)

newtype Keys = Keys { pressed :: Set Key }

instance Show a => Show (XY a) where show XY{x,y} = show (x,y)
instance Show a => Show (RGB a) where show RGB{r,g,b} = "RGB" ++ show (r,g,b)

data HiLo a = HiLo { hi :: a, lo :: a } deriving (Eq,Ord,Generic,NFData,Functor,Show)

data Reg = Reg1 | Reg2
  deriving (Eq,Ord,Show,Generic,NFData)
