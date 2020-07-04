module Language.MFAG.Syntax.Terminals where


import Data.Set as S
import Data.List as L

-- | Constructors, each one has an identifier
newtype Constructor
  = C String -- T.Text {- TODO: is this a good choice? -}
  deriving (Eq, Read, Show, Ord)

-- | Enumeration, set of strings
type EnumConsts = S.Set Constructor


data Sort = Z | R | Enum |  Tuple [Sort] | List Sort
          deriving (Eq, Ord, Show, Read)


-- | variables..
type NVar  = String

-- | Set of Variables
type NVars = [NVar]

-- | Function names
type NFun  = String

-- | Binary Operators
data BOp   = Plus | Minus | Times | Exp | Div
           deriving (Ord, Eq, Show, Read)

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

untup (ValTupl t) = t
untup t           = [t]


-- Val implements Num interface
instance Num Val where
  (ValZ a) + (ValZ b) = ValZ $ a + b
  (ValR a) + (ValR b) = ValR $ a + b
  (ValR a) + (ValZ b) = ValR $ a + fromInteger b
  (ValZ a) + (ValR b) = ValR $ fromInteger a + b
  
  (ValZ a) * (ValZ b) = ValZ $ a * b
  (ValR a) * (ValR b) = ValR $ a * b
  (ValR a) * (ValZ b) = ValR $ a * fromInteger b
  (ValZ a) * (ValR b) = ValR $ fromInteger a * b

  abs (ValR a) = ValR $ abs a
  abs (ValZ a) = ValZ $ abs a

  fromInteger  = ValZ

  negate (ValR a) = ValR $ negate a
  negate (ValZ a) = ValZ $ negate a

  signum (ValR a) = ValR $ signum a
  signum (ValZ a) = ValZ $ signum a

-- Val implements Fractional interface
instance Fractional Val where
  (ValR a) / (ValR b) = ValR $ a / b
  (ValR a) / (ValZ b) = ValR $ a / fromInteger b
  (ValZ a) / (ValR b) = ValR $ fromInteger a / b
  (ValZ a) / (ValZ b) = case (mod a b) of
                          0 -> ValZ $ a `div` b
                          _ -> ValR $ fromInteger a / fromInteger b
  recip (ValR a) = ValR (1 / a)
  recip (ValZ 1) = ValZ 1
  recip (ValZ a) = ValR (1 / fromInteger a)


printVal :: Val -> String
printVal (ValZ i) = show i
printVal (ValR d) = show d
printVal (ValC (C name)) = name
printVal (ValSec vs) = (L.intercalate ":" $ L.map printVal vs) ++ "[]"
printVal (ValTupl t) = '(':(L.intercalate "," $ L.map printVal t) ++ ")"
