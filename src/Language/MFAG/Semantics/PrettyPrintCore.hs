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

module Language.MFAG.Semantics.Test where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals

import Language.MFAG.Syntax.Exp.Core
import Language.MFAG.Syntax.Exp.Base


-- | pretty print
$(attLabels [("spp", ''String)])


asp_pp =
  =   (syn spp p_Lit
         $ show <$> ter ch_lit_t
      )
  .+: (syn spp p_Var
         $ ter ch_var_t
      )
  .+: (syn spp p_OpInf
         $ do lpp <- at ch_op_inf_l spp
              rpp <- at ch_op_inf_r spp
              op  <- ter ch_op_inf_op
              return $ "(" ++ lpp ++ " " ++ op ++ " " ++ rpp ++ ")"
      )
  .+: (syn spp p_OpPre
         $ do epp <- at ch_op_pre_e spp
              op  <- ter ch_op_pre_op
              return $ "(" op ++ " " ++ epp ++ ")"
      )
  .+: (syn spp p_App
         $ do name <- ter ch_app_f
              arg  <- 
      )
  .+: (syn spp p_AppU
         $ 
      )
  .+: emptyAspect
