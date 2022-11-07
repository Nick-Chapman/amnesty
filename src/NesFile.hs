
module NesFile
  ( NesFile(..)
  , load
  ) where

import Control.Monad (when)
import Data.Bits (testBit)
import Data.Word8 (Word8)
import Rom16k (Rom16k)
import Rom8k (Rom8k)
import Text.Printf (printf)
import qualified Data.ByteString as BS (readFile,unpack)
import qualified Data.Char as Char (chr)
import qualified Rom16k (init)
import qualified Rom8k (init)

headerSize :: Int
headerSize = 16

prgSize :: Int
prgSize = 0x4000 --16k

patSize :: Int
patSize = 0x1000 --2k (One PAT of 256 tiles)

chrSize :: Int
chrSize = 2 * patSize --4k

data NesFile = NesFile -- TODO: move elsewhere?
  { path :: FilePath
  , header :: [Word8]
  , prgs :: [Rom16k]
  , chrs :: [Rom8k]
  , ntm :: NametableMirroring
  }

data NametableMirroring
  = NTM_Horizontal -- vertical arrangement, iNes, Flags6, bit0 = 0   (Ice)
  | NTM_Vertical   -- horizontal arrangement, iNes, Flags6, bit0 = 1 (SMB)
  deriving Show

instance Show NesFile where
  show NesFile{path,chrs,prgs,ntm} =
    --"NesFile: " ++ (unwords $ map show header)
    printf "NesFile(%s): #chrs=%d, #prgs=%d, ntm=%s"
    path (length chrs) (length prgs) (show ntm)

bytesToString :: [Word8] -> String
bytesToString = map (Char.chr . fromIntegral)

load :: String -> IO NesFile
load path = do
    byteString <- BS.readFile path
    let bs = BS.unpack byteString
    when (length bs < headerSize) $ error "header failure, too short"
    when (bytesToString (take 3 bs) /= "NES") $ error "header failure, missing NES tag"
    let header = take headerSize bs
    let x = fromIntegral (bs !! 4)
    let y = fromIntegral (bs !! 5)
    let ntm = if (bs !! 6) `testBit` 0 then NTM_Horizontal else NTM_Vertical
    when (length bs /= headerSize + (x * prgSize) + (y * chrSize)) $ error "bad file size"
    let
      prgs =
        [ Rom16k.init $ take prgSize $ drop (headerSize + i * prgSize) bs
        | i <- [0..x-1]
        ]
    let
      chrs =
        [ Rom8k.init $ take chrSize $ drop (headerSize + x * prgSize + i * 2 * patSize) bs
        | i <- [0..y-1]
        ]
    return $ NesFile { path, header, prgs,  chrs, ntm }
