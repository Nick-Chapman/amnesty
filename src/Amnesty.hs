
module Amnesty (main) where

import Eff (Eff)
import System.Environment (getArgs)
import qualified NesFile (load)
import qualified System (showCHR,dk50,dk400)
import qualified UsingSDL (runTerm,runSDL)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  args <- getArgs
  let config = parseCommandLine args
  run config

data Config = Config
  { path :: FilePath
  , verb :: Bool
  , mode :: Mode
  , maxFrame :: Maybe Int
  , effect :: forall p. Eff p ()
  }

config0 :: Config
config0 = Config
  { path = "carts/smb.nes"
  , verb = False
  , mode = SDL
  , maxFrame = Nothing
  , effect = System.showCHR
  }

data Mode = SDL | NoPic | Regression

parseCommandLine :: [String] -> Config
parseCommandLine = loop config0
  where
    loop :: Config -> [String] -> Config
    loop acc = \case
      [] -> acc
      "nopic":xs -> loop acc { mode = NoPic } xs
      "verb":xs -> loop acc { verb = True } xs

      "smb":xs -> loop acc { path = smbPath } xs
      "dk":xs -> loop acc { path = dkPath } xs
      "dk50":xs -> loop acc { path = dkPath, effect = System.dk50 } xs
      "dk400":xs -> loop acc { path = dkPath, effect = System.dk400 } xs

      "-max":n:xs -> loop acc { maxFrame = Just (read n) } xs
      "-reg":xs -> loop acc { mode = Regression } xs

      path:xs -> loop acc { path } xs

    smbPath = "carts/smb.nes"
    dkPath = "carts/dk.nes"

run :: Config -> IO ()
run Config{path,verb,mode,maxFrame,effect} = do
  nesFile <- NesFile.load path
  let _ = print nesFile
  case mode of
    Regression ->
      UsingSDL.runTerm verb (Just 1) nesFile effect
    NoPic ->
      UsingSDL.runTerm verb maxFrame nesFile effect
    SDL ->
      UsingSDL.runSDL verb nesFile effect
  pure ()
