
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
  doKeys
  setupNameTableTD NameTableTestData.dk50
  PPU.effect PPU.Mode_NameTable

dk400 :: Eff p ()
dk400 = do
  doKeys
  setupNameTableTD NameTableTestData.dk400
  PPU.effect PPU.Mode_NameTable

showCHR :: Eff p () -- show the tiles in PatL/PatR
showCHR = do
  doKeys
  PPU.effect PPU.Mode_CHR

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



doKeys :: Eff p ()
doKeys = do
  doKey KeyX RegX
  doKey KeyZ RegZ
  doKey KeyN RegN
  doKey KeyP RegP

doKey :: Key -> Reg -> Eff p ()
doKey key reg = do
  b <- IsPressed key
  if not b then pure () else do
    incrementReg reg

incrementReg :: Reg -> Eff p ()
incrementReg reg = do
  v <- GetReg reg
  one <- LitB 1
  v' <- AddB v one
  SetReg reg v'
