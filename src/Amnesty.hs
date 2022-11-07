module Amnesty (main) where

import Control.Monad (ap,liftM)
import Data.Word8 (Word8)
import Data.Map (Map)
import qualified Data.Map as Map (fromList,lookup)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  top1

top1 :: IO ()
top1 = do
  let xs = emu sys
  sequence_ (map display (take 3 xs))
  pure ()

display :: [Pix DuringEmulation] -> IO ()
display line = do
  print ("display",line)

--[emulation]---------------------------------------------------------

data DuringEmulation

instance Phase DuringEmulation where
  type Byte DuringEmulation = Word8
  type Col DuringEmulation = Colour

data Colour = Colour Word8 deriving Show

type Effect a = Eff DuringEmulation a

emu :: Effect a -> [a]
emu e0 = outer state0
  where
    outer s = inner s e0 $ \s a -> a : outer s

    inner :: State -> Effect b -> (State -> b -> r) -> r
    inner s e k = case e of
      Ret x -> k s x
      Bind e f -> inner s e $ \s a -> inner s (f a) k
      IncB b -> k s (b+1)
      LitB n -> k s n
      LookupRom b -> do
        let State{rom} = s
        k s (romLookup rom b)
      SetPPUReg1 b ->
        k s { reg1 = b } ()
      GettPPUReg1 -> do
        let State{reg1} = s
        k s reg1
      MakeCol b -> do
        k s (Colour b)


-- concrete state of the entire system; whatever is necessary to emulate
data State = State { reg1 :: Word8, rom :: Rom }

state0 :: State
state0 = State { reg1 = 42, rom = theRom }

-- type for 256 byte rom
data Rom = Rom (Map Word8 Word8)

romLookup :: Rom -> Word8 -> Word8
romLookup (Rom m) k =
  maybe (error (show ("romLookup",k))) id $ Map.lookup k m

theRom :: Rom
theRom = Rom $ Map.fromList [ (b,b) | b <- [0..255] ]

--[system]------------------------------------------------------------

sys :: Eff p [Pix p]
sys = sequence [ pixel (x,y) | x <- [1..4], y <- [1..3] ]

data Pix p = Pix (Byte p, Byte p, Col p)
deriving instance Phase p => Show (Pix p)

pixel :: (Word8,Word8) -> Eff p (Pix p)
pixel (x,y) = do
  let n = y*10 + x
  a <- LitB n
  b <- LookupRom a
  c <- MakeCol b
  x <- LitB x
  y <- LitB y
  pure (Pix (x,y,c))


--[effect]------------------------------------------------------------

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind

-- phase
class ( Show (Byte p)
      , Show (Col p)
      ) => Phase p where
  type Byte p
  type Col p

-- abstract effect type (finite)
data Eff p x where
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y
  IncB :: Byte p -> Eff p (Byte p)
  LitB :: Word8 -> Eff p (Byte p)

  MakeCol :: Byte p -> Eff p (Col p)

  -- some rom containing 256 bytes
  LookupRom :: Byte p -> Eff p (Byte p)

  -- random reg in my dummy ppu
  SetPPUReg1 :: Byte p -> Eff p ()
  GettPPUReg1 :: Eff p (Byte p)
