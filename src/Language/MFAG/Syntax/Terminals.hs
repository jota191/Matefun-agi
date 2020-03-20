module Language.MFAG.Syntax.Terminals where

import Data.Text as T
import Data.Set as S

-- | Constructors, each one has an identifier
newtype Constructor
  = C T.Text {- TODO: is this a good choice? -}
  deriving (Eq, Read, Show, Ord)

-- | Enumeration, set of strings
type EnumConsts = S.Set Constructor



-- | variables..
type NVar  = String

-- | Set of Variables
type NVars = [NVar]

-- | Function names
type NFun  = String

-- | Unary operators
type UOp   = String

-- | Binary Operators
type BOp   = String


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
