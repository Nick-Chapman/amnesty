
module PPU (effect,Mode(..)) where

import Eff (Eff(..),Bit,Byte)
import Types (XY(..),HiLo(..),Reg(..))

data Mode = Mode_CHR | Mode_NameTable

effect :: Mode -> Eff p ()
effect mode = do
  LitB 0 >>= SetReg RegScanY
  sequence_
    [ do
        y <- getAndInc RegScanY
        -- no need to zero X as it initializes to 0, and wraps to 0
        sequence_
          [ do
              x <- getAndInc RegScanX
              doPix mode XY {x,y}
              pure ()
          | _ <- [0::Int ..255]
          ]
    | _ <- [0::Int ..239]
    ]

getAndInc :: Reg -> Eff p (Byte p)
getAndInc r = do
  x <- GetReg r
  one <- LitB 1
  x1 <- AddB x one
  SetReg r x1
  pure x

doPix :: Mode -> XY (Byte p) -> Eff p ()
doPix mode xy@XY{x,y} = do
  HiLo{hi=coarseX,lo=fineX} <- splitCourseFine x
  HiLo{hi=coarseY,lo=fineY} <- splitCourseFine y
  let coarse = XY {x = coarseX, y = coarseY}
  let fine = XY {x = fineX, y = fineY}
  zero <- LitB 0 -- light grey
  pickTileForCoarse mode coarse >>= \case
    Nothing -> do
      EmitPixel xy zero
    --Just (PatR,_) -> do EmitPixel xy zero -- ignore PatR
    Just (pat,tile) -> do
      plane1 <- getTilePlaneBit pat Plane1 tile fine
      plane2 <- getTilePlaneBit pat Plane2 tile fine
      col <- colourOfPlanes plane1 plane2
      EmitPixel xy col

colourOfPlanes :: Bit p -> Bit p -> Eff p (Byte p)
colourOfPlanes plane1 plane2 = do
  b1 <- If plane1
  b2 <- If plane2
  LitB $ case (b1,b2) of
    (True,True) -> 6 -- red
    (True,False) -> 44 -- cyan
    (False,True) -> 1 -- blue
    (False,False) -> 63 -- black

data Plane = Plane1 | Plane2
data Pat = PatL | PatR

getTilePlaneBit :: Pat -> Plane -> Byte p -> XY (Byte p) -> Eff p (Bit p)
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
  TestBit byte fineX

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


pickTileForCoarse :: Mode -> XY (Byte p) -> Eff p (Maybe (Pat, Byte p))
pickTileForCoarse = \case
  Mode_CHR -> pickViaCHR
  Mode_NameTable -> pickViaNameTable


pickViaNameTable :: XY (Byte p) -> Eff p (Maybe (Pat, Byte p))
pickViaNameTable coarse = do
  ntHiBase <- LitB 0x20 -- 0x24,0x28,0x2C
  let XY{x,y} = coarse -- both x/y are max 5 bits
  mask <- LitB 7
  yLo3 <- y `BwAnd` mask
  yLo3shifted <- yLo3 `ShiftL` 5
  yHi2 <- y `ShiftR` 3
  hi <- BwOr yHi2 ntHiBase
  lo <- BwOr x yLo3shifted
  tile <- ReadVmem HiLo { hi, lo }
  pure (Just (PatL,tile))


pickViaCHR :: XY (Byte p) -> Eff p (Maybe (Pat, Byte p))
pickViaCHR coarse = do
  let XY{x,y} = coarse
  HiLo{hi=hx,lo=lx} <- nibbles x
  HiLo{hi=hy,lo=ly} <- nibbles y
  zero <- LitB 0
  showLeft <- EqB hx zero >>= If
  swapLR <- do
    reg1 <- GetReg Reg1
    TestBit reg1 zero >>= If
  let pat = if (showLeft /= swapLR) then PatL else PatR
  yon <- EqB hy zero >>= If
  if not yon then pure Nothing else do
    shifted <- ly `ShiftL` 4
    tile <- BwOr shifted lx
    pure (Just (pat,tile))
