
module System (showCHR,dk50,dk400) where

import Data.Bits ((.&.),shiftR)
import Data.Map (Map)
import Data.Word (Word16)
import Data.Word (Word8)
import Eff (Eff(..),Byte)
import Types (Key(..),Reg(..),HiLo(..))
import qualified Data.Map as Map (fromList,toList)
import qualified NameTableTestData (dk50,dk400)
import qualified PPU (effect,Mode(..))

palDataX4 :: Map Int Word8 -- dummy colour data for showing CHR
palDataX4 = Map.fromList
  [ (0, 63) , (1, 1) , (2, 44) , (3, 6)
  , (4, 63) , (5, 1) , (6, 44) , (7, 6)
  , (8, 63) , (9, 1) , (10, 44) , (11, 6)
  , (12, 63) , (13, 1) , (14, 44) , (15, 6)
  ]

_dk50pal :: Map Int Word8
_dk50pal = Map.fromList [(0,0x0f),(1,0x2c),(2,0x38),(3,0x12),(4,0x0f),(5,0x27),(6,0x27),(7,0x27),(8,0x0f),(9,0x30),(10,0x30),(11,0x30),(12,0x0f),(17,0x25)]

_dk400pal :: Map Int Word8
_dk400pal = Map.fromList [(0,0x0f),(1,0x15),(2,0x2c),(3,0x12),(4,0x0f),(5,0x27),(6,0x02),(7,0x17),(8,0x0f),(9,0x30),(10,0x36),(11,0x06),(12,0x0f),(13,0x30),(14,0x2c),(15,0x24),(17,0x02),(18,0x36),(19,0x16),(21,0x30),(22,0x27),(23,0x24),(25,0x16),(26,0x30),(27,0x37),(29,0x06),(30,0x27),(31,0x02)]


dk50 :: Eff p ()
dk50 = do
  doKeys
  setupPalData _dk50pal -- palDataX4
  setupNameTableTD NameTableTestData.dk50
  PPU.effect PPU.Mode_NameTable

dk400 :: Eff p ()
dk400 = do
  doKeys
  setupNameTableTD NameTableTestData.dk400
  setupPalData _dk400pal
  PPU.effect PPU.Mode_NameTable

showCHR :: Eff p () -- show the tiles in PatL/PatR
showCHR = do
  doKeys
  setupPalData palDataX4
  PPU.effect PPU.Mode_CHR


setupPalData :: Map Int Word8 -> Eff p ()
setupPalData xs = do
  sequence_
    [ do
        v <- LitB v
        a <- litHL (0x3f00 + fromIntegral a)
        WriteVmem a v
    |
      (a,v) <- Map.toList xs
    ]

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
  doKey KeyY RegY
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
