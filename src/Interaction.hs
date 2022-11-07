
module Interaction
  ( State
  , initState
  , runForOneFrame
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Types (Picture(..),XY(..),RGB(..),Key(..),Keys(..))

data State = State
  { squarePos :: XY Int
  }
  deriving (Generic,NFData)

initState :: State
initState = State { squarePos = XY { x = 100, y = 100 } }

runForOneFrame :: State -> Keys -> (Picture,State)
runForOneFrame state keys = do
  let picture = mkPicture state
  let state' = updateState keys state
  (picture,state')

updateState :: Keys -> State -> State
updateState Keys{pressed} s@State{squarePos} = do
  let func1 = if KeyX `elem` pressed then incX else id
  let func2 = if KeyZ `elem` pressed then incY else id
  s { squarePos = (func1 . func2) squarePos }
  where
    incX XY{x,y} = XY { x = x+1, y }
    incY XY{x,y} = XY { x, y = y+1 }

mkPicture :: State -> Picture
mkPicture State{squarePos} = do
  let XY{x=maxX,y=maxY} = screenSize
  let XY{x=sizeX,y=sizeY} = squareSize
  let XY{x=posX,y=posY} = squarePos
  Pictures
    [ Draw XY { x = x `mod` maxX, y = y `mod` maxY } RGB { r = 255, g = 255, b = 0 }
    | x <- [posX..posX+sizeX]
    , y <- [posY..posY+sizeY]
    , (x+y) `mod` 2 == 0
    ]
  where
    squareSize = XY { x = 10, y = 10 }
    screenSize = XY { x = 256, y = 240 }
