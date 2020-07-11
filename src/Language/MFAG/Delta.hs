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
{-# LANGUAGE RankNTypes #-}


module Language.MFAG.Delta where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.AST
import Language.MFAG.Syntax.Terminals
import Language.MFAG.SemFuncs
import Language.MFAG.Utils

import Control.Applicative
import Prelude hiding (exp)
import qualified Data.Map as M

type DeltaT = M.Map NVar FDef

-- Δ attribute, i.e. function environment
$(attLabels [("idelta" , ''DeltaT)])

asp_idelta = AspAll
  asp_idelta_Set
  asp_idelta_Sig
  asp_idelta_Exp
  asp_idelta_ExpG
  asp_idelta_Cond
  asp_idelta_Ecu
  asp_idelta_FDef
  asp_idelta_Tuple

asp_idelta_Set   = undefined
asp_idelta_Sig   = undefined

asp_idelta_Exp   =
  idelta `copyAtChi` ch_op_inf_l .+:
  idelta `copyAtChi` ch_op_inf_r .+:
  idelta `copyAtChi` ch_app_e    .+:
  idelta `copyAtChi` ch_eprod_e  .+:
  idelta `copyAtChi` ch_index_e  .+:
  emptyAspect

asp_idelta_ExpG  =
  idelta `copyAtChi` ch_expGIf_e    .+:
  idelta `copyAtChi` ch_expGIf_cond .+:
  idelta `copyAtChi` ch_expGIf_tail .+:
  idelta `copyAtChi` ch_expGOr_e    .+:
  emptyAspect

asp_idelta_Cond  =
  idelta `copyAtChi` ch_equa_l .+:
  idelta `copyAtChi` ch_equa_r .+:
  idelta `copyAtChi` ch_and_l  .+:
  idelta `copyAtChi` ch_and_l  .+:
  emptyAspect

asp_idelta_Ecu   =
  idelta `copyAtChi` ch_ecu_r .+:
  emptyAspect
  
asp_idelta_FDef  =
  idelta `copyAtChi` ch_fun_sig .+:
  idelta `copyAtChi` ch_fun_body .+:
  emptyAspect

asp_idelta_Tuple =
  idelta `copyAtChi` ch_tuple_h .+:
  idelta `copyAtChi` ch_tuple_t .+:
  idelta `copyAtChi` ch_tuple_s .+:
  emptyAspect

