
module PPU (effect) where

import Eff (Eff(..))
import Types (XY(..),RGB(..))

effect :: Eff p ()
effect = do
  drawSquare

drawSquare :: Eff p ()
drawSquare = do
  let XY{x=sizeX,y=sizeY} = squareSize
  posX <- GetPPUReg1
  posY <- GetPPUReg2

  sequence_
    [ do
        xi <- LitB xi
        yi <- LitB yi
        x <- AddB posX xi
        y <- AddB posY yi
        pixel (x,y)
    | xi <- [0..sizeX-1]
    , yi <- [0..sizeY-1]
    , (xi+yi) `mod` 2 == 0
    ]

  where
    pixel (x,y) = do
      let xy = XY { x, y }
      r <- LitB 255
      g <- LitB 255
      b <- LitB 0
      let rgb = RGB { r, g, b }
      EmitPixel xy rgb

    squareSize = XY { x = 10, y = 10 }
