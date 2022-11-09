
module Frame
  ( Frame, makeFrame, toPicture
  , toFrameHash, FrameHash
  ) where

import Col6 (Col6,col2rgb)
import Data.Hashable (hash)
import Data.List (sortOn)
import Data.Map (Map)
import Data.Word (Word8)
import Text.Printf (printf)
import Types (XY,Picture(..))
import qualified Data.Map as Map (toList)

data Frame = Frame { m :: Map (XY Word8) Col6, fh :: FrameHash }

makeFrame :: Map (XY Word8) Col6 -> Frame
makeFrame m =
  if size /= expected
  then error $ printf "makeFrame: size:%d, expected:%d" size expected
  else Frame { m, fh = makeFrameHash xs }
  where
    size = length xs
    expected = 256*240
    xs = map snd (sortOn fst (Map.toList m))

toPicture :: Frame -> Picture
toPicture Frame{m} = do
  Pictures
    [ Draw (fmap fromIntegral xy) (col2rgb col)
    | (xy,col) <- Map.toList m
    ]

toFrameHash :: Frame -> FrameHash
toFrameHash Frame {fh} = fh

data FrameHash = FrameHash Int deriving Eq

makeFrameHash :: [Col6] -> FrameHash
makeFrameHash cols = FrameHash (hash cols)

instance Show FrameHash where
  show (FrameHash n) = printf "[%s]" (take 4 (printf "%xd" n) :: String)
