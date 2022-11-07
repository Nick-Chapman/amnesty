
module Amnesty (main) where

import qualified UsingSDL (main)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  UsingSDL.main
  pure ()
