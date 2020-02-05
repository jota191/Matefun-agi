module Language.MFAG.Syntax.Terminals where

import Data.Text as T
import Data.Set as S

-- | Enumeration, set of strings
type EnumConsts = S.Set T.Text {- TODO: is this a good choice? -}

-- | variables..
type NVar  = String
type NVars = [NVar]
type NFun  = String

type UOp   = String
type BOp   = String
