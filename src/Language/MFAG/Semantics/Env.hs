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

import Language.MFAG.Syntax.Exp.Base
import Language.MFAG.Syntax.Exp.Core
import Language.MFAG.Syntax.Terminals
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH


import Data.Map as M
import Data.Maybe (fromJust)

-- | function environment type
type TGamma = [FDef]


-- | function environment
$(attLabels [("igamma", ''TGamma)])


asp_iGamma = copyAll igamma
copyAll igamma =
  (inh igamma p_OpInf ch_op_inf_l $ at lhs igamma) .+:
  (inh igamma p_OpInf ch_op_inf_r $ at lhs igamma) .+:
  (inh igamma p_OpPre ch_op_pre_e $ at lhs igamma) .+:
  (inh igamma p_App ch_app_e $ at lhs igamma) .+:
  (inh igamma p_AppU ch_appu_ecu $ at lhs igamma) .+:
  (inh igamma p_AppU ch_appu_e $ at lhs igamma) .+:
  (inh igamma p_ExpGIf ch_expGIf_e $ at lhs igamma) .+:
  (inh igamma p_ExpGIf ch_expGIf_cond $ at lhs igamma) .+:
  (inh igamma p_ExpGIf ch_expGIf_tail $ at lhs igamma) .+:
  (inh igamma p_ExpGOr ch_expGOr_e $ at lhs igamma) .+:
  (inh igamma p_Ecu ch_ecu_r $ at lhs igamma) .+:
  (inh igamma p_EProd ch_eprod_e $ at lhs igamma) .+:
  (inh igamma p_TCons ch_tuple_h $ at lhs igamma) .+:
  (inh igamma p_TCons ch_tuple_t $ at lhs igamma) .+:
  (inh igamma p_TSing ch_tuple_s $ at lhs igamma) .+:
  copyAtChi igamma ch_equa_r .+:
  copyAtChi igamma ch_equa_l .+:
  copyAtChi igamma ch_and_r .+:
  copyAtChi igamma ch_and_l .+:
  copyAtChi igamma ch_neg_e .+:
  emptyAspect



-- | lookup a function by name in a gamma environment
lookupFun ::  NFun -> TGamma -> Maybe FDef
lookupFun f []
  = Nothing
lookupFun f (fdef@(FDef f' _ _) : fdefs)
  | f == f'   = Just fdef
  | otherwise = lookupFun f fdefs

-- by now, environments are only values, no fun env
type Env = M.Map NVar Val

-- eval attribute
$(attLabels [("ienv", ''Env)])

asp_env = copyAll ienv
  -- =   (inh ienv p_OpInf ch_op_inf_l $ at lhs ienv)
  -- .+: (inh ienv p_OpInf ch_op_inf_r $ at lhs ienv)
  -- .+: (inh ienv p_OpPre ch_op_pre_e $ at lhs ienv)
  -- .+: (inh ienv p_App   ch_app_e    $ at lhs ienv)
  -- .+: emptyAspect
