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

import Language.MFAG.Syntax.Exp.Core     as C
import Language.MFAG.Syntax.Exp.Unfolded as U
import Language.MFAG.Syntax.Exp.Base

import Language.MFAG.Semantics.Env

-- | function environment
$(attLabels [("igamma", ''TGamma)])

asp_iGamma
    = (inh igamma p_OpInf  ch_op_inf_l
         $ at lhs igamma
      )
  .+: (inh igamma p_OpInf  ch_op_inf_r
         $ at lhs igamma
      )
  .+: (inh igamma p_OpPre  ch_op_pre_e
         $ at lhs igamma
      )
  .+: (inh igamma p_App    ch_app_e
         $ at lhs igamma
      )
  .+: (inh igamma p_ExpGIf ch_expGIf_e
         $ at lhs igamma
      )
  .+: (inh igamma p_ExpGIf ch_expGIf_cond
         $ at lhs igamma
      )
  .+: (inh igamma p_ExpGIf ch_expGIf_tail
         $ at lhs igamma
      )
  .+: (inh igamma p_ExpGOr ch_expGOr_e
         $ at lhs igamma
      )
  .+: (inh igamma p_Ecu    ch_ecu_r
         $ at lhs igamma
      )
  .+: emptyAspect

type Error = String
-- type UnfoldedR = Either U.Exp Error

$(attLabels [("sUnfold", ''U.Exp)])

asp_sUnfold
  = (synmodM sidExpU p_App
       $ do env <- at lhs igamma
            return $ U.Var ""
    )
  .+: asp_sid_ExpCasU
