
module PPU (effect) where

import Eff (Eff(..),Byte)
import Types (XY(..),HiLo(..),Reg(..))

effect :: Eff p ()
effect = do
  drawPixels

drawPixels :: Eff p ()
drawPixels =
  sequence_
  [ do
      x <- LitB x
      y <- LitB y
      doPix XY {x,y}
      pure ()
  | x <- [0..255]
  , y <- [0..239]
  ]

doPix :: XY (Byte p) -> Eff p ()
doPix xy@XY{x,y} = do
  HiLo{hi=coarseX,lo=fineX} <- splitCourseFine x
  HiLo{hi=coarseY,lo=fineY} <- splitCourseFine y
  let coarse = XY {x = coarseX, y = coarseY}
  let fine = XY {x = fineX, y = fineY}
  pickTileForCoarse coarse >>= \case
    Nothing -> pure ()
    Just (pat,tile) -> do
      plane1 <- getTilePlaneBit pat Plane1 tile fine
      plane2 <- getTilePlaneBit pat Plane2 tile fine
      col <- colourOfPlanes plane1 plane2
      EmitPixel xy col
      pure ()

colourOfPlanes :: Bool -> Bool -> Eff p (Byte p)
colourOfPlanes plane1 plane2 = LitB $
  case (plane1,plane2) of
    (True,True) -> 1
    (True,False) -> 2
    (False,True) -> 3
    (False,False) -> 0

data Plane = Plane1 | Plane2
data Pat = PatL | PatR

getTilePlaneBit :: Pat -> Plane -> Byte p -> XY (Byte p) -> Eff p Bool
getTilePlaneBit pat plane tile fine = do
  tableOffset <- LitB (case pat of PatL -> 0; PatR -> 16)
  planeOffset <- LitB (case plane of Plane1 -> 0; Plane2 -> 8)
  let XY {x = fineX, y = fineY} = fine
  HiLo {hi = tileHi, lo = tileLo} <- nibbles tile
  hi <- BwOr tileHi tableOffset
  lo <- do
    shifted <- tileLo `ShiftL` 4
    n1 <- pure fineY
    n <- BwOr n1 planeOffset
    BwOr shifted n
  byte <- ReadVmem HiLo { hi, lo }
  TestBitB byte fineX

splitCourseFine :: Byte p -> Eff p (HiLo (Byte p))
splitCourseFine b = do
  mask <- LitB 0x7
  lo <- BwAnd b mask -- 0..7
  hi <- ShiftR b 3 -- 0..31
  pure HiLo {hi,lo}

nibbles :: Byte p -> Eff p (HiLo (Byte p))
nibbles b = do
  mask <- LitB 0xF
  lo <- BwAnd b mask -- 0..15
  hi <- ShiftR b 4 -- 0..15
  pure HiLo {hi,lo}


pickTileForCoarse :: XY (Byte p) -> Eff p (Maybe (Pat, Byte p))
pickTileForCoarse coarse = do
  -- at runtime ths would consult the nametable
  let XY{x,y} = coarse
  HiLo{hi=hx,lo=lx} <- nibbles x
  HiLo{hi=hy,lo=ly} <- nibbles y
  zero <- LitB 0
  showLeft <- EqB hx zero

  swapLR <- do
    reg1 <- GetReg Reg1
    TestBit reg1 0

  let pat = if (showLeft == swapLR) then PatL else PatR

  yon <- EqB hy zero
  if not yon then pure Nothing else do
    shifted <- ly `ShiftL` 4
    tile <- BwOr shifted lx
    pure (Just (pat,tile))

{-
_litXY :: XY Word8 -> Eff p (XY (Byte p))
_litXY XY{x,y} = do
  x <- LitB x
  y <- LitB y
  pure XY {x,y}

_eqXY :: XY (Byte p) -> XY (Byte p) -> Eff p Bool
_eqXY XY{x=x1,y=y1} XY{x=x2,y=y2} = do
  b1 <- EqB x1 x2
  b2 <- EqB y1 y2
  pure (b1 && b2)
-}
