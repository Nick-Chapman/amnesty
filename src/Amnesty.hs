module Amnesty (main) where

import Control.Monad (ap,liftM)

import Data.Word8 (Word8)

main :: IO ()
main = do
  putStrLn "*amnesty*"
  top1

top1 :: IO ()
top1 = do
  let s0 = state0
  let (s1,v1) = emulate s0 system
  display v1
  let (_,v2) = emulate s1 system
  display v2

display :: ScanLine DuringInterpretation -> IO ()
display line = do
  print ("display",line)

--emulateClockedEff :: ClockedEff a -> [a]
--emulateClockedEff = undefined

--data ClockedEff a -- abstract effect type (infinite/clocked)
--  ClockedEff [Eff a]

----------------------------------------------------------------------
-- emulate with concrete types and state

data DuringInterpretation

instance Phase DuringInterpretation where
  type Byte DuringInterpretation = Word8

type Effect a = Eff DuringInterpretation a

emulate :: State -> Effect a -> Res a -- Action!
emulate s0 e0 = loop s0 e0 $ \s a -> (s,a)
  where
    loop :: State -> Effect a -> (State -> a -> Res r) -> Res r
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      IncB b -> k s (b+1)
      LookupPat1 b -> undefined b
      SetPPUReg1 b -> undefined b
      GettPPUReg1 -> do
        let State{reg1} = s
        k s reg1

type Res r = (State,r)

-- concrete state of the entire system; whatever is necessary to emulate
data State = State { reg1 :: Word8 }

state0 :: State
state0 = undefined

----------------------------------------------------------------------

system :: Eff p (ScanLine p)
system = ppuLine


width,_height :: Int
(width,_height) = (10,3)

data ScanLine p = ScanLine [Byte p]

deriving instance Phase p => Show (ScanLine p)


ppuLine :: Eff p (ScanLine p)
ppuLine = do
  bs <- sequence (replicate width ppuNext)
  pure (ScanLine bs)

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
  LookupPat1 b


----------------------------------------------------------------------

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind

-- abstract effects
class Show (Byte p) => Phase p where
  type Byte p

data Eff p x where -- abstract effect type (finite)
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y
  --Log :: String -> Eff p x

  --LitB :: Word8 -> Eff p (Byte p)

  IncB :: Byte p -> Eff p (Byte p)

  LookupPat1 :: Byte p -> Eff p (Byte p)

  -- random reg in my dummy cpu
  SetPPUReg1 :: Byte p -> Eff p ()
  GettPPUReg1 :: Eff p (Byte p)
