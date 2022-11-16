
module FastEm (emulate) where

import Behaviour (Behaviour(..),Report(..))
import Col6 (Col6,makeCol6)
import Data.Map (Map)
import Data.Word (Word8,Word16)
import Eff (Phase(..),Eff(..))
import Frame (makeFrame)
import NesFile (NesFile(..))
import Rom8k (Rom8k)
import Text.Printf (printf)
import Types (Key,XY(..),Keys(..),Reg(..),HiLo(..))
import qualified Data.Map as Map (empty,insert,lookup,fromList)
import qualified Primitive as Prim
import qualified Rom8k (read)

import Data.Dynamic (Typeable,Dynamic,toDyn,fromDynamic)

emulate :: Bool -> NesFile -> Effect () -> Behaviour -- compile;(dump);execute
emulate dump nesFile eff = do
  let code = compile eff
  if | dump -> Log (show code) $ do execute nesFile code
     | otherwise -> execute nesFile code

doConstFolding :: Bool
doConstFolding = True

----------------------------------------------------------------------
-- compile

data DuringCompilation
instance Phase DuringCompilation where
  type Bit DuringCompilation = E1
  type Byte DuringCompilation = E8
  type Addr DuringCompilation = E16

type E1 = Exp Bool
type E8 = Exp Word8
type E16 = Exp Word16

type Effect a = Eff DuringCompilation a

compile :: Effect () -> Code
compile e = comp CState { u = 1 } e $ \CState{} () -> Stop

data CState = CState { u :: Int }

comp :: forall e. CState -> Effect e -> (CState -> e -> Code) -> Code
comp s e k = case e of

  Ret x -> k s x
  Bind e f -> comp s e $ \s a -> comp s (f a) k
  Assert mes b -> Do (A_Assert b mes) $ k s ()
  If b -> CodeIf b (k s True) (k s False)
  Repeat n e -> Do (A_Repeat n (comp s e k)) $ Stop
  IsPressed key -> k s (E_IsPressed key)
  EmitPixel xy v -> Do (A_EmitPixel xy v) $ k s ()
  ReadVmem a -> share "m" k s (E_ReadVmem a)
  WriteVmem a v -> Do (A_WriteMem a v) $ k s ()
  GetReg r -> share "r" k s (E_GetReg r)
  SetReg r v -> Do (A_SetReg r v) $ k s ()
  Bit0 -> k s (E_Const False)
  Bit1 -> k s (E_Const True)
  LitB x -> k s (E_Const x)
  LitA x -> k s (E_Const x)
  IteB i t e -> k s (E_IteB i t e)
  MakeAddr x -> k s (makeUnary Prim.MakeAddr (E_HL x))
  SplitAddr x -> undefined x
  MakeByte x -> k s (makeUnary Prim.MakeByte (E_B8 x))
  TestBit x y -> prim2 Prim.TestBit x y
  EqB x y -> prim2 Prim.EqB x y
  AddB x y -> prim2 Prim.AddB x y
  SubtractB x y -> prim2 Prim.SubtractB x y
  BwAnd x y  -> prim2 Prim.BwAnd x y
  BwOr x y -> prim2 Prim.BwOr x y
  ShiftL x y -> prim2 Prim.ShiftL x (E_Const y)
  ShiftR x y -> prim2 Prim.ShiftR x (E_Const y)

  where
    prim2 :: (Show x, Show y) => Prim.P2 x y r -> Exp r ~ e => Exp x -> Exp y -> Code
    prim2 p2 x y = k s (makeBinary p2 x y)


makeUnary :: (Show x) => Prim.P1 x r -> Exp x -> Exp r
makeUnary p1 x = case x of
  E_Const x | doConstFolding -> E_Const (Prim.evalP1 p1 x)
  _ -> Unary p1 x

makeBinary :: (Show x, Show y) => Prim.P2 x y r -> Exp x -> Exp y -> Exp r
makeBinary p2 x y = case (x,y) of
  (E_Const x,E_Const y) | doConstFolding -> E_Const (Prim.evalP2 p2 x y)
  _ -> Binary p2 x y


share :: (Show a, Typeable a) => String -> (CState -> Exp a -> Code) -> CState -> Exp a -> Code
share tag k s thing =
  genId s tag $ \s x -> Do (A_Let x thing) $ k s (E_Var x)

genId :: CState -> String -> (CState -> Identifier a -> r) -> r
genId s@CState{u} tag k = do
  k s { u = u + 1 } (Identifier tag u)

----------------------------------------------------------------------
-- code

data Code
  = Do Act Code
  | CodeIf (Exp Bool) Code Code
  | Stop

data Act
  = A_Repeat Int Code
  | A_SetReg Reg E8
  | A_WriteMem E16 E8
  | A_Assert E1 String
  | A_EmitPixel (XY E8) E8
  | forall a. (Typeable a, Show a) => A_Let (Identifier a) (Exp a)

data Exp a where
  E_Const :: a -> Exp a
  E_IsPressed :: Key -> E1
  E_GetReg :: Reg -> E8
  E_ReadVmem :: E16 -> E8
  Unary :: Show x => Prim.P1 x r -> Exp x -> Exp r
  Binary :: (Show x, Show y) => Prim.P2 x y r -> Exp x -> Exp y -> Exp r
  E_IteB :: E1 -> E8 -> E8 -> E8
  E_B8 :: Tup8 (Exp Bool) -> Exp (Tup8 Bool)
  E_HL :: HiLo (Exp Word8) -> Exp (HiLo Word8)
  E_Var :: Typeable a => Identifier a -> Exp a

data Identifier a where
  Identifier :: String -> Int -> Identifier a

instance Show (Identifier a) where
  show (Identifier tag i) = tag ++ show i

type Tup8 x = (x, x, x, x, x, x, x, x)

----------------------------------------------------------------------
-- pp

instance Show Code where show = unlines . pretty 0

pretty :: Int -> Code -> [String]
pretty i = \case
  Stop -> ["Stop"]
  Do act code -> prettyAct i act ++ pretty i code
  CodeIf cond c1 c2 -> concat
    [ [tab i "if (" ++ show cond ++ ") {"]
    , pretty (i+2) c1
    , [tab i "} else {"]
    , pretty (i+2) c2
    , [tab i "}"]
    ]

prettyAct :: Int -> Act -> [String]
prettyAct i = \case
  A_Repeat n c -> concat
    [ [tab i "repeat (" ++ show n ++ ") {"]
    , pretty (i+2) c
    , [tab i "}"]
    ]
  A_SetReg r v -> [tab i $ "SetReg" ++ show (r,v)]
  A_WriteMem a v -> [tab i $ "writeMem" ++ show (a,v)]
  A_Assert b m -> [tab i $ "Assert" ++ show (b,m)]
  A_EmitPixel xy col -> [tab i $ "EmitPixel" ++ show (xy,col)]
  A_Let x e -> [tab i $ "Let" ++ show (x,e)]

tab :: Int -> String -> String
tab n s = take n (repeat ' ') ++ s

--deriving instance Show a => Show (Exp a)
instance Show a => Show (Exp a) where
  show = \case
    E_Const x -> show x
    E_IsPressed key -> show key
    E_GetReg reg -> show reg
    E_ReadVmem a -> "ReadVmem:(" ++ show a ++ ")"
    Unary p1 x -> show p1 ++ "(" ++ show x ++ ")"
    Binary p2 x y -> show p2 ++ show (x,y)
    E_IteB i t e -> "Ite" ++ show (i,t,e)
    E_B8 v -> show v
    E_HL v -> show v
    E_Var x -> show x

----------------------------------------------------------------------
-- execute

execute :: NesFile -> Code -> Behaviour
execute nesFile code = do
  let NesFile { chrs = [chr1] } = nesFile
  let
    loop :: State -> Behaviour
    loop s = do
      Poll $ \keys -> do
        let q = emptyB
        let d = Context { chr1, keys }
        runCode q d s code $ \s -> do
          let State{emitted,regs,vramWriteCount} = s
          let frame = makeFrame emitted
          let report = Report { vmemReadCount = 999, vramWriteCount, regs }
          let s1 = resetState s
          Render frame report (loop s1)

  --Log (show code) $ do -- dump code for inspection
  loop state0

runCode :: Bindings ->Context -> State -> Code -> (State -> Behaviour) -> Behaviour
runCode q d s code k = case code of
  Do act code -> do
    --Log (show act) $ do
    runAct q d s act $ \q s -> do
      runCode q d s code k
  CodeIf i t e -> runCode q d s (if eval q d s i then t else e) k
  Stop -> k s

runAct :: Bindings -> Context -> State -> Act -> (Bindings -> State -> Behaviour) -> Behaviour
runAct q d s@State{regs,vram,vramWriteCount} act k = case act of
  A_Repeat n code -> loop n s
    where loop n s = if n == 0 then k q s else runCode q d s code $ loop (n-1)
  A_SetReg r v -> do
    let v' = eval q d s v
    --Log (show("SetReg",r,v')) $ do
    k q s { regs = Map.insert r v' regs }
  A_WriteMem a v ->
    k q s { vramWriteCount = vramWriteCount + 1
          , vram = Map.insert (eval q d s a) (eval q d s v) vram
          }
  A_Assert b mes -> if not (eval q d s b) then error mes else k q s
  A_EmitPixel xy v -> do
    let xy' = fmap (eval q d s) xy
    let v' = eval q d s v
    --Log (show("EmitPixel",xy',v')) $ do
    k q (emitPixel xy' (makeCol6 v') s)
  A_Let x e -> do
    let v = eval q d s e
    k (extendB q x v) s

eval :: Bindings -> Context -> State -> Exp a -> a
eval q d@Context{chr1,keys=Keys{pressed}} s@State{regs} = \case
  E_Const x -> x
  E_IsPressed key -> key `elem` pressed
  E_GetReg r -> maybe 0 id $ Map.lookup r regs
  E_ReadVmem a -> readVmem chr1 s (eval q d s a)
  Unary p1 x -> Prim.evalP1 p1 (eval q d s x)
  Binary p2 x y -> Prim.evalP2 p2 (eval q d s x) (eval q d s y)
  E_IteB i t e -> eval q d s (if (eval q d s i) then t else e)
  E_B8 x -> fmapTup8 (eval q d s) x
  --E_HL HiLo{hi,lo}  -> HiLo {hi = eval q d s hi, lo = eval q d s lo }
  E_HL x -> fmap (eval q d s) x
  E_Var x -> maybe (error (show ("E_Var",x))) id $ lookupB q x

fmapTup8 :: (Exp a -> a) -> Tup8 (Exp a) -> Tup8 a
fmapTup8 x (a,b,c,d,e,f,g,h) = (x a, x b, x c, x d, x e, x f, x g, x h)

----------------------------------------------------------------------
-- Hetrogenous Env

data Bindings = Bindings (Map (String,Int) Dynamic)

emptyB :: Bindings
emptyB = Bindings Map.empty

lookupB :: Typeable a => Bindings -> Identifier a -> Maybe a
lookupB (Bindings m) (Identifier s u) = Map.lookup (s,u) m >>= fromDynamic

extendB :: Typeable a => Bindings -> Identifier a -> a -> Bindings
extendB (Bindings m) (Identifier s u) x = Bindings (Map.insert (s,u) (toDyn x) m)

----------------------------------------------------------------------
-- state copied from Emulate -- TODO: dedup!

data State = State
  { emitted :: Map (XY Word8) Col6
  , countEmitted :: Int
  , regs :: Map Reg Word8
--  , vmemReadCount :: Int -- TODO: make this work when wrap mem-read in let-binding
  , vramWriteCount :: Int
  , vram :: Map Word16 Word8
  }

state0 :: State
state0 = State
  { emitted = Map.empty
  , countEmitted = 0
  , regs = regs0
--  , vmemReadCount = 0
  , vramWriteCount = 0
  , vram = Map.empty
  }

regs0 :: Map Reg Word8
regs0 = Map.fromList
  [ (RegP,1)
  ]


emitPixel :: XY Word8 -> Col6 -> State -> State
emitPixel xy col s = do
  let State{emitted,countEmitted=c} = s
  let expectedXY =
        XY { x = fromIntegral (c `mod` 256)
           , y = fromIntegral (c `div` 256)
           }
  case xy /= expectedXY of -- TODO: reinstate
    True -> error (show ("EmitPixel:c=",c,"xy=",xy,"expected=",expectedXY))
    False ->
      s { emitted = Map.insert xy col emitted
        , countEmitted = 1 + c
        }

resetState :: State -> State
resetState state = state
  { emitted = Map.empty
  , countEmitted = 0
--  , vmemReadCount = 0
  , vramWriteCount = 0
  }

readVmem :: Rom8k -> State -> Word16 -> Word8
readVmem chr1 State{vram} a = do
  if
    | a < 0x2000 -> Rom8k.read chr1 a
    | a < 0x3000 ->
      -- nametable(+atts). TODO: support mirroring & mapping to 2k VRAM
      maybe 0 id $ Map.lookup a vram

    | a < 0x3f00 -> error $ printf "readVmem: %x -- unexpected mirror read" a
    | a < 0x3f1f -> do
      -- palletes: TODO: map to sep 32 byte RAM space
        maybe 0 id $ Map.lookup a vram

    | True -> error $ printf "readVmem: %x" a

data Context = Context
  { chr1 :: Rom8k
  , keys :: Keys
  }
