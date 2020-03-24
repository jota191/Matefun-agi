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

module Language.MFAG.Semantics.Env where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

-- import Language.MFAG.Syntax.Exp.Base
import Language.MFAG.Syntax.Exp.Core as C
import Language.MFAG.Syntax.Terminals

import Data.Map as M
import Data.Maybe (fromJust)

-- only base types
--import Language.MFAG.Syntax.Set.Base as Set

-- | function environment type
type TGamma = [C.FDef] -- TODO: Refine

-- | lookup a function by name in a gamma environment
lookupFun ::  NFun -> TGamma -> Maybe C.FDef
lookupFun f []
  = Nothing
lookupFun f (fdef@(C.FDef f' _ _) : fdefs)
  | f == f'   = Just fdef
  | otherwise = lookupFun f fdefs

-- by now, environments are only values, no fun env
type Env = M.Map NVar Val
