
module System (top) where

import Eff (Eff(..))
import Types (Key(..),Reg(..))
import qualified PPU (effect)

top :: Eff p ()
top = do
  doKeyX
  doKeyZ
  PPU.effect

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

