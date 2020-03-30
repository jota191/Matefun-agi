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

module Language.MFAG.Syntax.Exp.Core where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals
import Language.MFAG.Syntax.Exp.Base

import Language.MFAG.Syntax.Set.Base as Set


-- base expression syntax generation
-- base types syntax generation TODO: no se si va aca
$(closeNTs [''Nt_Set, ''Nt_Cart ,''Nt_Sig])

-- base expression syntax generation
$(closeNTs [''Nt_Exp, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef])

$(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
              ''Nt_Exp,  ''Nt_ExpG,
              ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef])


-- | Identity attribute
$(attLabels [("sidExpC", ''Exp)])
$(attLabels [("sidEcu", ''Ecu)])
$(attLabels [("sidExpG", ''ExpG)])
$(attLabels [("sidCond", ''Cond)])

asp_dummy = singAsp $ syn sidCond p_Top $ undefined

-- | identity aspect for core expressions
asp_sid_Exp
  =   (syn sidExpC p_Lit
         $ Lit <$> ter ch_lit_t
      )
  .+: (syn sidExpC p_Var
         $ Var <$> ter ch_var_t
      )
  .+: (syn sidExpC p_OpInf
         $ OpInf <$> at  ch_op_inf_l sidExpC
                 <*> ter ch_op_inf_op
                 <*> at  ch_op_inf_r sidExpC
      )
  .+: (syn sidExpC p_OpPre
         $ OpPre <$> ter ch_op_pre_op
                 <*> at ch_op_pre_e sidExpC
      )
  .+: (syn sidExpC p_App
         $ App   <$> ter ch_app_f
                 <*> at ch_app_e sidExpC
      )
  .+: (syn sidExpC p_AppU
         $ AppU   <$> at ch_appu_ecu sidEcu
                  <*> at ch_appu_e sidExpC
      )
  .+: emptyAspect


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
         $ ExpGIf <$> at ch_expGIf_e sidExpC
                  <*> return Top
                  <*> at ch_expGIf_tail sidExpG
      )
  .+: (syn sidExpG p_ExpGOr
         $ ExpGOr <$> at ch_expGOr_e sidExpC
      )
  .+: emptyAspect

asp_sid_Core
  =    asp_sid_Ecu
  .:+: asp_sid_Exp
  .:+: asp_sidExpG
