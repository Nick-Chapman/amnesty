
module Interaction -- TODO: Interaction == Emu?
  ( State
  , state0
  , runForOneFrame
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Types (Keys,Picture)
import qualified Emu (State,emulate,state0)
import qualified System (top)

data State = State
  { emuState :: Emu.State
  }
  deriving (Generic,NFData)

state0 :: State
state0 = State { emuState = Emu.state0 }

runForOneFrame :: State -> Keys -> (Picture,State)
runForOneFrame state@State{emuState} keys = do
  let (picture,emuState') = Emu.emulate keys emuState the_effect
  (picture,state { emuState = emuState' })
  where
    the_effect = System.top
