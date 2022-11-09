
module Col6 (Col6,makeCol6,col2rgb) where

import Data.Hashable (Hashable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Types (RGB(..))

newtype Col6 = Col6 Word8
  deriving (Generic,Hashable)

makeCol6 :: Word8 -> Col6
makeCol6 w = if w >= 64 then error $ printf "makeCol6: %d" w else Col6 w

-- TODO: scavenge from honesty
col2rgb :: Col6 -> RGB Word8
col2rgb (Col6 w) = case w of
  0 -> RGB { r = 0, g = 0, b = 0 }
  1 -> RGB { r = 255, g = 0, b = 0 }
  2 -> RGB { r = 0, g = 255, b = 0 }
  3 -> RGB { r = 0, g = 0, b = 255 }
  n -> error (show ("col2rgb",n))
