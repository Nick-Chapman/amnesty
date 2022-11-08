
module Amnesty (main) where

import System.Environment (getArgs)
import qualified Emu (makeContext)
import qualified NesFile (load)
import qualified UsingSDL (main)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  args <- getArgs
  let config = parseCommandLine args
  run config

data Config = Config { path :: FilePath }

config0 :: Config
config0 = Config { path = "carts/smb.nes" }

parseCommandLine :: [String] -> Config
parseCommandLine = loop config0
  where
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "smb":xs -> loop acc { path = "carts/smb.nes" } xs
      "dk":xs -> loop acc { path = "carts/dk.nes" } xs
      path:xs -> loop acc { path } xs

run :: Config -> IO ()
run Config{path} = do
  nesFile <- NesFile.load path
  print nesFile
  let context = Emu.makeContext nesFile
  UsingSDL.main context
  pure ()
