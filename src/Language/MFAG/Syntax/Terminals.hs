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
