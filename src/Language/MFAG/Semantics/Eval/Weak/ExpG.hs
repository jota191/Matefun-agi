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

module Language.MFAG.Semantics.Eval.Weak.ExpG where

import Language.MFAG.Semantics.Eval.Weak.Exp
import Language.MFAG.Syntax.Exp.Base
import Language.MFAG.Syntax.Terminals

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Data.Map as M

asp_env_ExpG
  =   (inh ienv p_ExpGIf ch_expGIf_e    $ at lhs ienv)
  .+: (inh ienv p_ExpGIf ch_expGIf_cond $ at lhs ienv)
  .+: (inh ienv p_ExpGIf ch_expGIf_tail $ at lhs ienv)
  .+: (inh ienv p_ExpGOr ch_expGOr_e    $ at lhs ienv)
  .+: emptyAspect

asp_eval_ExpG
  =   (syn seval p_ExpGIf $ at ch_expGIf_e seval)
  .+: (syn seval p_ExpGOr $ at ch_expGOr_e seval)
  .+: (syn ienv p_Top $ undefined) -- TODO: why this is needed? -only one-
  .+: emptyAspect

eval_ExpG e
  = sem_ExpG (asp_eval_Exp .:+: asp_env_Exp .:+: asp_eval_ExpG .:+: asp_env_ExpG)
    e initialAtt #. seval
  where initialAtt = (ienv =. M.empty *. emptyAtt)

e2 = ExpGOr (OpInf e1 "*" e1)
