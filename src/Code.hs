
module Code (Code(..),Prog(..),Act(..),Exp(..),Identifier(..)) where

import Data.Dynamic (Typeable)
import Data.Word (Word8,Word16)
import Rom8k (Rom8k)
import Types (Key,XY(..),Reg(..),HiLo(..))
import qualified Primitive as Prim

type E1 = Exp Bool
type E8 = Exp Word8
type E16 = Exp Word16

data Code = Code { prog :: Prog, chr1 :: Rom8k }

data Prog
  = Do Act Prog
  | ProgIf (Exp Bool) Prog Prog
  | Stop

data Act
  = A_Repeat Int Prog
  | A_SetReg Reg E8
  | A_WriteMem E16 E8
  | A_Assert E1 String
  | A_EmitPixel (XY E8) E8
  | forall a. (Typeable a, Show a) => A_Let (Identifier a) (Exp a)

data Exp a where
  E_Const :: Show a => a -> Exp a
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

instance Show Code where show Code{prog} = show prog

instance Show Prog where show = unlines . pretty 0

pretty :: Int -> Prog -> [String]
pretty i = \case
  Stop -> ["Stop"]
  Do act code -> prettyAct i act ++ pretty i code
  ProgIf cond c1 c2 -> concat
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
