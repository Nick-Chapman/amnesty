
module Amnesty (main) where

import System.Environment (getArgs)
import qualified NesFile (load)
import qualified UsingSDL (nopic,main)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  args <- getArgs
  let config = parseCommandLine args
  run config

data Config = Config
  { path :: FilePath
  , mode :: Mode
  }

config0 :: Config
config0 = Config
  { path = "carts/smb.nes"
  , mode = SDL
  }

data Mode = SDL | NoPic1 | NoPic

parseCommandLine :: [String] -> Config
parseCommandLine = loop config0
  where
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "smb":xs -> loop acc { path = "carts/smb.nes" } xs
      "dk":xs -> loop acc { path = "carts/dk.nes" } xs
      "-nopic1":xs -> loop acc { mode = NoPic1 } xs
      "-nopic":xs -> loop acc { mode = NoPic } xs
      path:xs -> loop acc { path } xs

run :: Config -> IO ()
run Config{path,mode} = do
  nesFile <- NesFile.load path
  let _ = print nesFile
  case mode of
    NoPic1 -> UsingSDL.nopic False (Just 1) nesFile
    NoPic -> UsingSDL.nopic True Nothing nesFile
    SDL -> UsingSDL.main nesFile
  pure ()
