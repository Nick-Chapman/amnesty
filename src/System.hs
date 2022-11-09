
module System (top) where

import Data.Bits ((.&.),(.|.),shiftL,shiftR)
import Data.Word (Word16)
import Eff (Eff(..),Byte)
import Types (Key(..),Reg(..),HiLo(..))
import qualified PPU (effect,Mode(..))

top :: Eff p ()
top = do
  doKeyX -- Reg1
  doKeyZ -- Reg2
  setupNameTable
  mode <- selectMode
  PPU.effect mode

setupNameTable :: Eff p () -- to show CHR(PatL/PatR)
setupNameTable =
  sequence_
  [ do
      a <- litHL (0x2000 + n)
      b <- LitB tile
      WriteVmem a b
  | lo <- [0..15]
  , hi <- [0..15]
  , let tile = hi `shiftL` 4 .|. lo
  , let n = fromIntegral hi `shiftL` 5 .|. fromIntegral lo
  ]

litHL :: Word16 -> Eff p (HiLo (Byte p))
litHL w16 = do
  hi <- LitB (fromIntegral (w16 `shiftR` 8))
  lo <- LitB (fromIntegral (w16 .&. 255))
  pure HiLo {hi,lo}

selectMode :: Eff p PPU.Mode
selectMode = do
  zero <- LitB 0
  byte <- GetReg Reg2
  bool <- TestBit byte zero >>= If
  pure (if bool == False then PPU.Mode_CHR else PPU.Mode_NameTable)

doKeyX :: Eff p ()
doKeyX = do
  b <- IsPressed KeyX
  if not b then pure () else do
    incrementR Reg1

doKeyZ :: Eff p ()
doKeyZ = do
  b <- IsPressed KeyZ
  if not b then pure () else do
    incrementR Reg2

incrementR :: Reg -> Eff p ()
incrementR reg = do
  v <- GetReg reg
  one <- LitB 1
  v' <- AddB v one
  SetReg reg v'
