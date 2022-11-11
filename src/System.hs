
module System (showCHR,dk50,dk400) where

import Data.Bits ((.&.),shiftR)
import Data.Word (Word16)
import Data.Word (Word8)
import Eff (Eff(..),Byte)
import Types (Key(..),Reg(..),HiLo(..))
import qualified NameTableTestData (dk50,dk400)
import qualified PPU (effect,Mode(..))

dk50 :: Eff p ()
dk50 = do
  setupNameTableTD NameTableTestData.dk50
  PPU.effect PPU.Mode_NameTable

dk400 :: Eff p ()
dk400 = do
  setupNameTableTD NameTableTestData.dk400
  PPU.effect PPU.Mode_NameTable

setupNameTableTD :: [Word8] -> Eff p ()
setupNameTableTD testData = do
  sequence_
    [ do
        a <- litHL (0x2000 + fromIntegral i)
        b <- LitB b
        WriteVmem a b
    | (i,b) <- zip [0::Int ..] testData
    ]

litHL :: Word16 -> Eff p (HiLo (Byte p))
litHL w16 = do
  hi <- LitB (fromIntegral (w16 `shiftR` 8))
  lo <- LitB (fromIntegral (w16 .&. 255))
  pure HiLo {hi,lo}


showCHR :: Eff p () -- show the tiles in PatL/PatR
showCHR = do
  doKeys -- X swaps left/right (just to demonstate some interactive control)
  PPU.effect PPU.Mode_CHR

doKeys :: Eff p ()
doKeys = do
  doKeyX -- Reg1
  doKeyZ -- Reg2

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
