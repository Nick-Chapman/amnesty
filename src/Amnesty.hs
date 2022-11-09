
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
  , maxFrame :: Maybe Int
  }

config0 :: Config
config0 = Config
  { path = "carts/smb.nes"
  , mode = SDL
  , maxFrame = Nothing
  }

data Mode = SDL | NoPic | Regression

parseCommandLine :: [String] -> Config
parseCommandLine = loop config0
  where
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "smb":xs -> loop acc { path = "carts/smb.nes" } xs
      "dk":xs -> loop acc { path = "carts/dk.nes" } xs
      "-regression":xs -> loop acc { mode = Regression } xs
      "-nopic":xs -> loop acc { mode = NoPic } xs
      "-max":n:xs -> loop acc { maxFrame = Just (read n) } xs
      path:xs -> loop acc { path } xs

run :: Config -> IO ()
run Config{path,mode,maxFrame} = do
  nesFile <- NesFile.load path
  let _ = print nesFile
  case mode of
    Regression -> UsingSDL.nopic False (Just 1) nesFile
    NoPic -> UsingSDL.nopic True maxFrame nesFile
    SDL -> UsingSDL.main nesFile
  pure ()
