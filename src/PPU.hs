
module PPU (effect) where

import Eff (Eff(..))
import Types (XY(..),RGB(..))

effect :: Eff p ()
effect = do
  drawSquare
  drawTile

drawTile :: forall p. Eff p ()
drawTile = do
  full <- LitB 255
  zero <- LitB 0
  let yellow = RGB { r = full, g = full, b = zero }
  let black = RGB { r = zero, g = zero, b = zero }
  posX <- LitB 50
  posY <- LitB 50

  base <- GetPPUReg1
  let
    testPix :: Int -> Int -> Eff p Bool -- x/y in range 0..7
    testPix x y = do
      offset <- LitB (fromIntegral x)
      a <- AddB base offset
      b <- ReadVmem a
      TestBit b y

  sequence_
    [ do
        on <- testPix xi yi
        xi <- LitB (fromIntegral xi)
        yi <- LitB (fromIntegral yi)
        x <- AddB posX xi
        y <- AddB posY yi

        let xy = XY { x, y }

        let rgb = if on then yellow else black
        EmitPixel xy rgb
    | xi <- [0::Int ..7]
    , yi <- [0::Int ..7]
    ]


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
