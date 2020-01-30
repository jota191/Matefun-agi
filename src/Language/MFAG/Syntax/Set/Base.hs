{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Language.MFAG.Syntax.Set.Base where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Data.Set as S
import Data.Text as T

-- | Set production
$(addNont "Set")

-- | Real and integers, base sets
$(addProd "Real" ''Nt_Set [("real_t", Ter ''())])
$(addProd "Inte" ''Nt_Set [("inte_t", Ter ''())])

-- | List set, (lists at matefun level, nothing to do with lists at
--   Haskell level)
$(addProd "Sec" ''Nt_Set [("sec_c", NonTer ''Nt_Set)])

-- naming conventions:
-- prod_c is the name of an unique chi
-- prod_l, prod_r are names for chi if there are exactly two
-- prod_l, prod_c, prod_r if 3... 


-- | Enumeration, set od strings
type EnumConsts = S.Set T.Text {- TODO: is this a good choice? -}
$(addProd "Enum" ''Nt_Set [("enum_c", Ter ''EnumConsts)])

-- | Cartesian product non terminal
$(addNont "Cart")

-- | Cartesian product production
$(addProd "Cart" ''Nt_Set [("cart_c", NonTer ''Nt_Cart)])
$(addProd "Cart_Nil" ''Nt_Cart [("cart_nil_t", Ter ''())])
$(addProd "Cart_Cons" ''Nt_Cart [("cart_hd_t", NonTer ''Nt_Set),
                                 ("cart_tl_t", NonTer ''Nt_Cart)])
