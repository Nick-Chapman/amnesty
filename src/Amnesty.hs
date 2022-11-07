
module Amnesty (main) where

import Eff (Phase(..))
import Emu (emulate)
import System (Pix)
import qualified System (top)
import qualified UsingSDL (main)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  let _ = playSys
  UsingSDL.main
  pure ()

playSys :: IO ()
playSys = do
  let xs = emulate System.top
  sequence_ (map display (take 3 xs))
  pure ()

display :: Phase p => [Pix p] -> IO ()
display line = do
  print ("display",line)