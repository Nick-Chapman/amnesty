
module Amnesty (main) where

import qualified UsingSDL (main)
import qualified NesFile (load)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  let filename = "carts/smb.nes"
  nesFile <- NesFile.load filename
  print nesFile
  let _ = UsingSDL.main
  pure ()
