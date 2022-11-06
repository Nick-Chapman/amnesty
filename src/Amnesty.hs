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
  let state0 = State { reg1 = 42, rom = theRom }
  let s0 = state0
  let (s1,v1) = emulate s0 system
  display v1
  let (_,v2) = emulate s1 system
  display v2

display :: Phase p => ScanLine p -> IO ()
display line = do
  print ("display",line)

--[emulation]---------------------------------------------------------

data DuringEmulation

instance Phase DuringEmulation where
  type Byte DuringEmulation = Word8

type Effect a = Eff DuringEmulation a

emulate :: State -> Effect a -> Res a
emulate s0 e0 = loop s0 e0 $ \s a -> (s,a)
  where
    loop :: State -> Effect a -> (State -> a -> Res r) -> Res r
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      IncB b -> k s (b+1)
      LookupRom b -> do
        let State{rom} = s
        k s (romLookup rom b)
      SetPPUReg1 b ->
        k s { reg1 = b } ()
      GettPPUReg1 -> do
        let State{reg1} = s
        k s reg1

type Res r = (State,r)

-- concrete state of the entire system; whatever is necessary to emulate
data State = State { reg1 :: Word8, rom :: Rom }

-- type for 256 byte rom
data Rom = Rom (Map Word8 Word8)

romLookup :: Rom -> Word8 -> Word8
romLookup (Rom m) k =
  maybe (error (show ("romLookup",k))) id $ Map.lookup k m

theRom :: Rom
theRom = Rom m
  where m = Map.fromList [ (b,b) | b <- [0..255] ]

--[system]------------------------------------------------------------

-- TODO: abstract effect type (infinite/clocked)
-- data ClockedEff a
--   ClockedEff [Eff a]

system :: Eff p (ScanLine p)
system = ppuLine

data ScanLine p = ScanLine [Byte p]
deriving instance Phase p => Show (ScanLine p)

ppuLine :: Eff p (ScanLine p)
ppuLine = do
  bs <- sequence (replicate width ppuNext)
  pure (ScanLine bs)
    where width = 10

ppuNext :: Eff p (Byte p)
ppuNext = do
  a <- GettPPUReg1
  a' <- IncB a
  SetPPUReg1 a'
  d <- ppuMM_lookup a
  pure d

-- The MM is the lookup function!
-- does address decoding and then looks in some rom (or ram!)
ppuMM_lookup :: Byte p -> Eff p (Byte p)
ppuMM_lookup b = do
  LookupRom b

--[effect]------------------------------------------------------------

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind

-- phase
class Show (Byte p) => Phase p where
  type Byte p

-- abstract effect type (finite)
data Eff p x where
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y
  IncB :: Byte p -> Eff p (Byte p)

  -- some rom containing 256 bytes
  LookupRom :: Byte p -> Eff p (Byte p)

  -- random reg in my dummy ppu
  SetPPUReg1 :: Byte p -> Eff p ()
  GettPPUReg1 :: Eff p (Byte p)
