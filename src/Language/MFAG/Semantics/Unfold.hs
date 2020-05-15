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

module Language.MFAG.Semantics.Unfold where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals
import Language.MFAG.Syntax.Set.Base     as Set

import Language.MFAG.Syntax.Exp.Core
import Language.MFAG.Syntax.Exp.Base

import Language.MFAG.Semantics.Env
import Data.Maybe (fromJust)



type Error = String

asp_Unfold =
  (synmodM sidExpC p_App $ do
     gamm <- at lhs igamma
     nfun <- ter ch_app_f
     arg <- at ch_app_e sidExpC
     case lookupFun nfun gamm of
       Nothing -> error "name not defined"
       Just (FDef _ sig ecu) -> return (AppU ecu arg)) .+:
  asp_sid_Core

asp_unfold_aux1 = asp_iGamma .:+: asp_Unfold

asp_unfold_All = asp_sid_Cond .:+: asp_unfold_aux1
