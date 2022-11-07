
module System (top) where

import Eff (Eff(..))
import qualified PPU (effect)
import Types (Key(..))

top :: Eff p ()
top = do
  PPU.effect
  doKeyX
  doKeyZ

doKeyX :: Eff p ()
doKeyX = do
  b <- IsPressed KeyX
  if not b then pure () else do
    incrementReg1
    pure ()

doKeyZ :: Eff p ()
doKeyZ = do
  b <- IsPressed KeyZ
  if not b then pure () else do
    incrementReg2
    pure ()

incrementReg1 :: Eff p ()
incrementReg1 = do
  v <- GetPPUReg1
  one <- LitB 1
  v' <- AddB v one
  SetPPUReg1 v'

incrementReg2 :: Eff p ()
incrementReg2 = do
  v <- GetPPUReg2
  one <- LitB 1
  v' <- AddB v one
  -- TODO: need mod by screen-height of 240
  SetPPUReg2 v'
