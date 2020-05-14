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

-- | Unfolded application
$(addProd "AppU"   ''Nt_Exp [("appu_ecu",   NonTer ''Nt_Ecu),
                             ("appu_e",     NonTer ''Nt_Exp)])


-- base expression syntax generation
$(closeNTs [''Nt_Exp, ''Nt_Sig, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef, ''Nt_Tuple])

$(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
              ''Nt_Exp,  ''Nt_ExpG,
              ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef , ''Nt_Tuple
             ])

-- | Identity attribute
$(attLabels [("sidExpC",  ''Exp)])
$(attLabels [("sidEcu",   ''Ecu)])
$(attLabels [("sidExpG",  ''ExpG)])
$(attLabels [("sidCond",  ''Cond)])
$(attLabels [("sidTuple", ''Tuple)])

asp_sid_Cond =
  syn sidCond p_Top (return Top)
  .+:
  (syn sidCond p_Equa
    $ Equa <$> at ch_equa_l sidExpC
           <*> ter ch_equa_op
           <*> at ch_equa_r sidExpC
  )
  .+:
  (syn sidCond p_And
    $ And <$> at ch_and_l sidCond <*> at ch_and_r sidCond
  )
  .+:
  (syn sidCond p_Neg
    $ Neg <$> at ch_neg_e sidCond
  )
  .+:
  emptyAspect
  
-- | identity aspect for core expressions
asp_sid_Exp
  =
  (syn sidExpC p_Lit
    $ Lit <$> ter ch_lit_t
  )
  .+:
  (syn sidExpC p_Var
    $ Var <$> ter ch_var_t
  )
  .+:
  (syn sidExpC p_OpInf
    $ OpInf <$> at  ch_op_inf_l sidExpC
    <*> ter ch_op_inf_op
    <*> at  ch_op_inf_r sidExpC
  )
  .+:
  (syn sidExpC p_OpPre
    $ OpPre <$> ter ch_op_pre_op
    <*> at ch_op_pre_e sidExpC
  )
  .+:
  (syn sidExpC p_App
    $ App   <$> ter ch_app_f
    <*> at ch_app_e sidExpC
  )
  .+:
  (syn sidExpC p_AppU
    $ AppU   <$> at ch_appu_ecu sidEcu
    <*> at ch_appu_e sidExpC
  )
  .+:
  (syn sidExpC p_EProd
    $ EProd <$> at ch_eprod_e sidTuple
  )
  .+:
  emptyAspect



-- | identity for Ecu
asp_sid_Ecu
  = (syn sidEcu p_Ecu
       $ Ecu <$> ter ch_ecu_l
             <*> at ch_ecu_r sidExpG
    )
  .+: emptyAspect

-- | Identity for ExpG
asp_sid_ExpG
  =   (syn sidExpG p_ExpGIf
         $ ExpGIf <$> at ch_expGIf_e sidExpC
                  <*> return Top
                  <*> at ch_expGIf_tail sidExpG
      )
  .+: (syn sidExpG p_ExpGOr
         $ ExpGOr <$> at ch_expGOr_e sidExpC
      )
  .+: emptyAspect

asp_sid_Tuple
  =   (syn sidTuple p_TCons
         $ TCons <$> at ch_tuple_h sidExpC <*> at ch_tuple_t sidTuple
      )
  .+: (syn sidTuple p_TSing
         $ TSing <$> at ch_tuple_s sidExpC
      )
  .+: emptyAspect

asp_sid_aux
  =    asp_sid_Exp
  .:+: asp_sid_ExpG
  .:+: asp_sid_Tuple

asp_sid_Core
  =    asp_sid_Ecu
  .:+: asp_sid_aux

