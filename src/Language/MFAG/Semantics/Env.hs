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
import Language.MFAG.Syntax.Exp.Core
import Language.MFAG.Syntax.Terminals

import Data.Map as M
import Data.Maybe (fromJust)

-- | function environment type
type TGamma = [FDef]

-- | lookup a function by name in a gamma environment
lookupFun ::  NFun -> TGamma -> Maybe FDef
lookupFun f []
  = Nothing
lookupFun f (fdef@(FDef f' _ _) : fdefs)
  | f == f'   = Just fdef
  | otherwise = lookupFun f fdefs

-- by now, environments are only values, no fun env
type Env = M.Map NVar Val
