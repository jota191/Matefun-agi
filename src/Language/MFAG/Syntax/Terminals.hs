module Language.MFAG.Syntax.Terminals where

import Language.MFAG.Utils

import Data.Set as S
import Data.List as L

-- | Constructors, each one has an identifier
newtype Constructor
  = C String -- T.Text {- TODO: is this a good choice? -}
  deriving (Eq, Read, Show, Ord)

-- | Enumeration, set of strings
type EnumConsts = S.Set Constructor


data Sort = Z | R | Enum | Tuple [Sort] | List Sort
          deriving (Eq, Ord, Read)
instance Show Sort where
  show Z = "Z"
  show R = "R"
  show Enum = "" -- info got from semantics
  show (Tuple t) = wrapParen . intercalate ", " . L.map show $ t
  show (List s) = show s ++ "*"
-- | variables..
type NVar  = String

-- | Set of Variables
type NVars = [NVar]

-- | Function names
type NFun  = String

-- | Binary Operators
data BOp   = Plus | Minus | Times | Exp | Div | Cons
           deriving (Ord, Eq, Read)
instance Show BOp where
  show Plus  = "+"
  show Minus = "-"
  show Times = "*"
  show Exp   = "^"
  show Div   = "/"
  show Cons  = ":"

-- | Equation operators
data EOp   = GEq | Eq
           deriving (Ord, Eq, Show, Read)
-- | values
data Val
  = ValZ    Integer
  | ValR    Double
  | ValC    Constructor
  | ValSec  [Val]
  | ValTupl [Val]
  deriving (Show, Eq, Read)

arity :: Val -> Int
arity (ValTupl t) = Prelude.length t
arity _           = 1

ith :: Val -> Int -> Val
ith (ValTupl t) i = t !! i
ith _ _ = error "type error, index to nontuple arg"

untup (ValTupl t) = t
untup t           = [t]



printVal :: Val -> String
printVal (ValZ i) = show i
printVal (ValR d) = show d
printVal (ValC (C name)) = name
printVal (ValSec vs) = (L.intercalate ":" $ L.map printVal vs) ++ "[]"
printVal (ValTupl t) = '(':(L.intercalate "," $ L.map printVal t) ++ ")"
