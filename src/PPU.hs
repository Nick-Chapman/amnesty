
module PPU (renderEffect,showPatTables) where

import Eff (Eff(..),Bit,Byte)
import Types (XY(..),HiLo(..),Reg(..))

renderEffect :: Eff p ()
renderEffect = do
  LitB 0 >>= SetReg RegScanY
  Repeat 240 $ do
    y <- getAndInc RegScanY
    -- no need to zero X as it initializes to 0, and wraps to 0
    Repeat 256 $ do
      x <- getAndInc RegScanX
      doPix XY {x,y}

getAndInc :: Reg -> Eff p (Byte p)
getAndInc r = do
  x <- GetReg r
  one <- LitB 1
  x1 <- AddB x one
  SetReg r x1
  pure x

data Status = Status
  { nt :: NT
  , pat :: Pat
  }

data NT = NT0 | NT1 | NT2 | NT3 -- which nametable to show
data Pat = PatL | PatR

statusDev :: Eff p Status -- status for dev; driven from keys
statusDev = do
  x <- GetReg RegN
  b1 <- LitB 1 >>= TestBit x >>= If
  b0 <- LitB 0 >>= TestBit x >>= If
  let nt = if b1 then (if b0 then NT3 else NT2) else (if b0 then NT1 else NT0)
  x <- GetReg RegP
  b0 <- LitB 0 >>= TestBit x >>= If
  let pat = if b0 then PatR else PatL
  pure Status { nt, pat }

doPix :: XY (Byte p) -> Eff p ()
doPix xy@XY{x,y} = do
  --EqB x y >>= Assert "eq-x-y" -- TODO: just testin assert works

  HiLo{hi=coarseX,lo=fineX} <- splitCourseFine x
  HiLo{hi=coarseY,lo=fineY} <- splitCourseFine y
  let coarse = XY {x = coarseX, y = coarseY}
  let fine = XY {x = fineX, y = fineY}
  (pat,tile) <- pickViaNameTable coarse
  (b,c) <- getAttributeTableBits coarse
  d <- getTilePlaneBit pat Plane2 tile fine
  e <- getTilePlaneBit pat Plane1 tile fine
  col <- colourOfPlanes (b,c) (d,e)
  EmitPixel xy col

splitCourseFine :: Byte p -> Eff p (HiLo (Byte p))
splitCourseFine b = do
  mask <- LitB 0x7
  lo <- BwAnd b mask -- 0..7
  hi <- ShiftR b 3 -- 0..31
  pure HiLo {hi,lo}

pickViaNameTable :: XY (Byte p) -> Eff p (Pat, Byte p)
pickViaNameTable coarse = do
  Status{nt,pat} <- statusDev
  ntHiBase <- ntHiFromStatus nt
  let XY{x,y} = coarse -- both x/y are max 5 bits
  mask <- LitB 7
  yLo3 <- y `BwAnd` mask
  yLo3shifted <- yLo3 `ShiftL` 5
  yHi2 <- y `ShiftR` 3
  hi <- BwOr yHi2 ntHiBase
  lo <- BwOr x yLo3shifted
  a <- MakeAddr HiLo { hi, lo }
  tile <- ReadVmem a
  pure (pat,tile)

getAttributeTableBits :: XY (Byte p) -> Eff p (Bit p, Bit p)
getAttributeTableBits coarse = do
  let XY{x,y} = coarse -- both x/y are max 5 bits
  (x432,x1) <- split x
  (y432,y1) <- split y
  Status{nt} <- statusDev
  ntHiBase <- ntHiFromStatus nt
  hi <- do
    three <- LitB 3
    BwOr ntHiBase three
  lo <- do
    y432LLL <- ShiftL y432 3
    xy <- BwOr x432 y432LLL
    base <- LitB 0xc0
    BwOr base xy
  a <- MakeAddr HiLo { hi, lo }
  v <- ReadVmem a
  bx1 <- If x1
  by1 <- If y1
  let (p,q) =
        if by1
        then (if bx1 then (7,6) else (5,4))
        else (if bx1 then (3,2) else (1,0))
  b <- LitB p >>= TestBit v
  c <- LitB q >>= TestBit v
  pure (b,c)
  where
    split :: Byte p -> Eff p (Byte p, Bit p)
    split v = do
      vRR <- ShiftR v 2
      v1 <- LitB 1 >>= TestBit v
      pure (vRR,v1)

data Plane = Plane1 | Plane2

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
  a <- MakeAddr HiLo { hi, lo }
  v <- ReadVmem a
  seven <- LitB 7
  fineXflipped <- seven `SubtractB` fineX -- [0..7] --> [7..0]
  TestBit v fineXflipped

colourOfPlanes :: (Bit p,Bit p) -> (Bit p,Bit p) -> Eff p (Byte p)
colourOfPlanes (b,c) (d,e) = do
  z <- Bit0
  let a = z -- Background
  hi <- LitB 0x3f
  lo <- MakeByte (z,z,z,a,b,c,d,e)
  a <- MakeAddr HiLo { hi, lo }
  ReadVmem a

ntHiFromStatus :: NT -> Eff p (Byte p)
ntHiFromStatus nt = LitB $ case nt of
  NT0 -> 0x20
  NT1 -> 0x24
  NT2 -> 0x28
  NT3 -> 0x2C

nibbles :: Byte p -> Eff p (HiLo (Byte p))
nibbles b = do
  mask <- LitB 0xF
  lo <- BwAnd b mask -- 0..15
  hi <- ShiftR b 4 -- 0..15
  pure HiLo {hi,lo}


----------------------------------------------------------------------

showPatTables :: Eff p ()
showPatTables = do
  LitB 0 >>= SetReg RegScanY
  Repeat 240 $ do
    y <- getAndInc RegScanY
    Repeat 256 $ do
      x <- getAndInc RegScanX
      let xy = XY {x,y}
      HiLo{hi=coarseX,lo=fineX} <- splitCourseFine x
      HiLo{hi=coarseY,lo=fineY} <- splitCourseFine y
      let fine = XY {x = fineX, y = fineY}
      HiLo{hi=hx,lo=lx} <- nibbles coarseX
      HiLo{hi=hy,lo=ly} <- nibbles coarseY
      zero <- LitB 0
      showLeft <- EqB hx zero >>= If
      let pat = if showLeft then PatL else PatR
      yon <- EqB hy zero >>= If
      if not yon then EmitPixel xy zero else do
        shifted <- ly `ShiftL` 4
        tile <- BwOr shifted lx
        d <- getTilePlaneBit pat Plane2 tile fine >>= If
        e <- getTilePlaneBit pat Plane1 tile fine >>= If
        col <- LitB $
          case (d,e) of
            (False,False) -> 63
            (False,True) -> 1
            (True,False) -> 44
            (True,True) -> 6
        EmitPixel xy col
