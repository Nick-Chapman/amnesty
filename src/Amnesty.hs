
module Amnesty (main) where

import System.Environment (getArgs)
import qualified Emu (makeContext)
import qualified NesFile (load)
import qualified UsingSDL (nopic1,main)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  args <- getArgs
  let config = parseCommandLine args
  run config

data Config = Config
  { path :: FilePath
  , pic :: Bool
  }

config0 :: Config
config0 = Config
  { path = "carts/smb.nes"
  , pic = True
  }

parseCommandLine :: [String] -> Config
parseCommandLine = loop config0
  where
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "smb":xs -> loop acc { path = "carts/smb.nes" } xs
      "dk":xs -> loop acc { path = "carts/dk.nes" } xs
      "-nopic":xs -> loop acc { pic = False } xs
      path:xs -> loop acc { path } xs

run :: Config -> IO ()
run Config{path,pic} = do
  nesFile <- NesFile.load path
  let _ = print nesFile
  let context = Emu.makeContext nesFile
  if
    | not pic -> UsingSDL.nopic1 context
    | otherwise -> UsingSDL.main context
  pure ()
