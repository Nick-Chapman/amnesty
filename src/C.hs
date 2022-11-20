
module C (dump) where

import Code (Code(..),Act(..),Exp(..),Identifier(..))
import Data.Word (Word8,Word16)
import Types (Reg,HiLo(..))
import Primitive (P1(..),P2(..))
import Data.List (intercalate)

dump :: Code -> String
dump code = do
  let c = cofCodeTop code
  show c

----------------------------------------------------------------------

cofCodeTop :: Code -> CFile
cofCodeTop code = CFile [FunDef fd]
  where
    fd = CFunDef { typ, name, body }
    typ = void
    name = CName "main"
    body = Block (cofCode code)

cofCode :: Code -> [CStat]
cofCode = \case
  Stop -> undefined []
  Do act code -> cofAct act : cofCode code
  CodeIf cond c1 c2 -> undefined cond c1 c2

cofAct :: Act -> CStat
cofAct = \case
  A_Repeat n c -> Repeat (LitI n) (Block (cofCode c))
  A_SetReg r v -> Expression $ Assign (nameOfReg r) (cofExp v)
  A_WriteMem a v -> Expression $ Call (CName "write_mem") [cofExp a, cofExp v]
  A_Assert b m -> Expression $ Call (CName "my_assert") [cofExp b, LitS m]
  A_EmitPixel xy col -> undefined xy col
  A_Let x e -> Declare (typeOfIdent x) (nameOfIdent x) (cofExp e)

nameOfReg :: Reg -> CName
nameOfReg reg = CName ("reg_" ++ show reg)

typeOfIdent :: Identifier a -> CType
typeOfIdent _ = CType "something"

nameOfIdent :: Identifier a -> CName
nameOfIdent x = CName (show x)

void :: CType
void = CType "void"

cofExp :: Exp a -> CExp
cofExp = \case
  E_Const x -> see x
  E_IsPressed key -> Call (CName "is_pressed") [see key]
  E_GetReg reg -> see reg
  E_ReadVmem a -> Call (CName "read_mem") [cofExp a]
  Unary p1 x -> cofPrim1 p1 (cofExp x)
  Binary p2 x y -> cofPrim2 p2 (cofExp x) (cofExp y)
  E_IteB i t e -> IteOp (cofExp i) (cofExp t) (cofExp e)
  E_B8 v -> see v -- TODO: hacky hack
  E_HL HiLo{hi,lo} -> Call (CName "hilo") [cofExp hi,cofExp lo]
  E_Var x -> see x

cofPrim1 :: P1 a r -> CExp -> CExp
cofPrim1 = \case
  MakeAddr -> id -- ??? \HiLo{hi,lo} -> Call (CName "make_addr") [cofExp hi,cofExp lo]
  SplitAddr -> undefined
  MakeByte -> id


cofPrim2 :: P2 a b r -> CExp -> CExp -> CExp
cofPrim2 = \case
  TestBit -> undefined
  EqB -> BinOp "=="
  AddB -> BinOp "+"
  SubtractB -> undefined
  BwAnd -> BinOp "&"
  BwOr -> BinOp "|"
  ShiftL -> BinOp "<<"
  ShiftR -> BinOp ">>"

see :: Show a => a -> CExp -- TODO: temp hack
--see a = LitS (show a)
see a = Ident (CName (show a))


----------------------------------------------------------------------

data CFile = CFile [CTop]
--  deriving Show

data CTop
  = Include String
  | FunDef CFunDef
  | FunDec CFunDec
  | ArrDef CArrDef

data CFunDef = CFunDef
  { typ :: CType
  , name :: CName
  , body :: CStat
  }

data CFunDec = CFunDec
  { typ :: CType
  , name :: CName
  }

data CArrDef = CArrDef
  { typ :: CType
  , name :: CName
  , size :: CExp
  , init :: [CExp]
  }

data CType = CType String
data CName = CName String

data CStat
  = Printf String
  | Block [CStat]
  | Return CExp
  | Expression CExp
  | Comment String
  | Declare CType CName CExp
  | Die
  | Repeat CExp CStat
  | If CExp CStat CStat
  | Switch CExp [(Int,CStat)]

data CExp
  = LitI Int
  | LitS String
  | LitA Word16
  | LitB Word8
  | Ident CName
  | Call CName [CExp]
  | Assign CName CExp
  | UnOp String CExp
  | BinOp String CExp CExp
  | IteOp CExp CExp CExp
  | CastOp CType CExp

----------------------------------------------------------------------


instance Show CFile where
  show (CFile tops) = unlines (map show tops)

instance Show CTop where
  show = \case
    Include what -> unwords ["#include",what]
    FunDef def -> show def
    FunDec dec -> show dec
    ArrDef x -> show x

instance Show CFunDef where
  show CFunDef{typ,name,body} =
    show $ vert [ lay (unwords [show typ, show name, "()"])
                , layCStat body]

instance Show CFunDec where
  show CFunDec{typ,name} =
    show $ lay (unwords [show typ, show name, "();"])

instance Show CArrDef where
  show CArrDef{typ,name,size,init} =
    show $ lay (unwords ([show typ, show name, "[", show size, "] = {"]
                         ++ [intercalate "," (map show init)]
                         ++ ["};"]
                        ))

instance Show CType where show (CType s) = s

instance Show CName where show (CName s) = s

instance Show CStat where show = show . layCStat

layCStat :: CStat -> Lay
layCStat = \case
  Printf mes -> lay (unwords ["printf","(",dq++mes++dq,")",";"]) where dq = "\""
  Block xs -> brace (tab (vert [ layCStat x | x <- xs ]))
  Return e -> lay ("return " ++ show e ++ ";")
  Expression e -> lay (show e ++ ";")
  Comment s -> lay ("// " ++ s)
  Die -> lay "die;"
  Declare ty v e -> lay (unwords [show ty, show v, "=", show e, ";"])
  If i t e ->
    vert [ lay ("if (" ++ show i ++ ")")
         , layCStat t
         , lay "else"
         , layCStat e
         ]
  Repeat n b ->
    vert [ lay ("repeat (" ++ show n ++ ")") -- TODO: C doesn't actually have a repeat!
         , layCStat b
         ]
  Switch exp branches ->
    vert [ lay ("switch (" ++ show exp ++ ") {")
         , tab (vert ([ vert [ lay ("case " ++ show v ++ " :")
                             , layCStat p
                             ]
                      | (v,p) <- branches
                      ] ++ [ lay ("default: { printf(\"unexpected switch value: %d\\n\","
                                  ++ show exp ++ "); die; }")]))
         , lay "}"
         ]

instance Show CExp where
  show = \case
    LitI n -> show n
    LitS s -> show s
    LitB b -> "0x" ++ show b
    LitA a -> "0x" ++ show a
    Ident s -> show s
    Call f args -> unwords [show f,"(",intercalate "," [ show e | e <- args],")"]
    Assign name exp -> unwords [show name, "=", show exp]
    UnOp name exp -> unwords ["(",name,show exp,")"]
    BinOp name e1 e2 -> unwords ["(",show e1,name,show e2,")"]
    IteOp e1 e2 e3 -> unwords ["(",show e1,"?",show e2,":",show e3,")"]
    CastOp ty exp -> unwords ["(","(",show ty,")",show exp,")"]

brace :: Lay -> Lay
brace x = vert [ lay "{", tab x, lay "}" ]

----------------------------------------------------------------------

data Lay = Lay { unLay :: [String] }

instance Show Lay where show = Prelude.unlines . unLay

lay :: String -> Lay
lay s = Lay [s]

vert :: [Lay] -> Lay
vert = Lay . concat . map unLay

tab :: Lay -> Lay
tab (Lay lines) = Lay $ [ " " ++ line | line <- lines ]
