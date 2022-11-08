
module Amnesty (main) where

import qualified Emu (makeContext)
import qualified NesFile (load)
import qualified UsingSDL (main)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  let filename = "carts/smb.nes"
  nesFile <- NesFile.load filename
  print nesFile
  let context = Emu.makeContext nesFile
  UsingSDL.main context
  pure ()
