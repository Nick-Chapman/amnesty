
module Amnesty (main) where

import Eff (Eff)
import System.Environment (getArgs)
import NesFile (NesFile)
import qualified C (dump)
import qualified FastEm (compile)
import qualified NesFile (load)
import qualified System (showCHR,dk50,dk400)
import qualified UsingSDL (Config(..),runTerm,runSDL)

main :: IO ()
main = do
  --putStrLn "*amnesty*"
  args <- getArgs
  let config = parseCommandLine args
  run config

data Config = Config
  { path :: FilePath
  , verb :: Bool
  , fast :: Bool
  , dump :: Bool
  , mode :: Mode
  , maxFrame :: Maybe Int
  , effect :: forall p. Eff p ()
  }

config0 :: Config
config0 = Config
  { path = "carts/smb.nes"
  , verb = False
  , fast = True
  , dump = False
  , mode = SDL
  , maxFrame = Nothing
  , effect = System.showCHR
  }

data Mode = SDL | NoPic | CDump

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
      "-reg":xs -> loop acc { mode = NoPic, maxFrame = Just 1 } xs
      "slow":xs -> loop acc { fast = False } xs
      "old-dump":xs -> loop acc { dump = True, maxFrame = Just 1 } xs
      "dump":xs -> loop acc { mode = CDump } xs

      path:xs -> loop acc { path } xs

    smbPath = "carts/smb.nes"
    dkPath = "carts/dk.nes"

run :: Config -> IO ()
run Config{path,verb,fast,dump,mode,maxFrame,effect} = do
  nesFile <- NesFile.load path
  let _ = print nesFile
  case mode of
    NoPic -> UsingSDL.runTerm conf maxFrame nesFile effect
    SDL ->   UsingSDL.runSDL  conf          nesFile effect
    CDump -> cdump                          nesFile effect
  where
    conf = UsingSDL.Config { verb, fast, dump }


cdump :: NesFile -> (forall p. Eff p ()) -> IO ()
cdump _nesFile eff = do -- TODO: use nesFile
  let code = FastEm.compile eff
  putStrLn (C.dump code)
