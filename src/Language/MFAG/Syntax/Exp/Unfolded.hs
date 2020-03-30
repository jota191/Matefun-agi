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

module Language.MFAG.Syntax.Exp.Unfolded where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals
import Language.MFAG.Syntax.Exp.Base

import Language.MFAG.Syntax.Set.Base as Set

-- | Unfolded application
$(addProd "AppU"   ''Nt_Exp [("appu_ecu",   NonTer ''Nt_Ecu),
                             ("appu_e",     NonTer ''Nt_Exp)])

-- base expression syntax generation
-- base types syntax generation
$(closeNTs [''Nt_Set, ''Nt_Cart ,''Nt_Sig])

-- base expression syntax generation
$(closeNTs [''Nt_Exp, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu])

$(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
              ''Nt_Exp,  ''Nt_ExpG,
              ''Nt_Cond, ''Nt_Ecu])

$(attLabels [("sidExpU", ''Exp)])
$(attLabels [("sidEcu", ''Ecu)])
$(attLabels [("sidExpG", ''ExpG)])
$(attLabels [("sidCond", ''Cond)])

-- | identity for unfolded expressions
asp_sid_ExpCasU
  =   (syn sidExpU p_Lit
         $ Lit <$> ter ch_lit_t
      )
  .+: (syn sidExpU p_Var
         $ Var <$> ter ch_var_t
      )
  .+: (syn sidExpU p_OpInf
         $ OpInf <$> at  ch_op_inf_l sidExpU
                 <*> ter ch_op_inf_op
                 <*> at  ch_op_inf_r sidExpU
      )
  .+: (syn sidExpU p_OpPre
         $ OpPre <$> ter ch_op_pre_op
                 <*> at ch_op_pre_e sidExpU
      )
  .+: (syn sidExpU p_App
         $ App   <$> ter ch_app_f
                 <*> at ch_app_e sidExpU
      )
  .+: emptyAspect

asp_sid_ExpU
  = (syn sidExpU p_AppU
        $ AppU   <$> at ch_appu_ecu sidEcu
                  <*> at ch_appu_e sidExpU
     )
  .+: asp_sid_ExpCasU

-- | identity for Ecu
asp_sid_Ecu
  = (syn sidEcu p_Ecu
       $ Ecu <$> ter ch_ecu_l
             <*> at ch_ecu_r sidExpG
    )
  .+: emptyAspect

-- | Identity for ExpG
asp_sidExpG
  =   (syn sidExpG p_ExpGIf
         $ ExpGIf <$> at ch_expGIf_e sidExpU
                  <*> return Top
                  <*> at ch_expGIf_tail sidExpG
      )
  .+: (syn sidExpG p_ExpGOr
         $ ExpGOr <$> at ch_expGOr_e sidExpU
      )
  .+: emptyAspect

-- | complete identity
asp_sid_U
  =    asp_sid_Ecu
  .:+: asp_sid_ExpU
  .:+: asp_sidExpG
