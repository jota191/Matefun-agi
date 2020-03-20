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

$(addProd "AppU"   ''Nt_Exp [("app_ecu",     NonTer ''Nt_Ecu),
                               ("app_e",     NonTer ''Nt_Exp)])

-- base expression syntax generation
-- base types syntax generation
$(closeNTs [''Nt_Set, ''Nt_Cart ,''Nt_Sig])

-- base expression syntax generation
$(closeNTs [''Nt_Exp, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef])

$(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
              ''Nt_Exp,  ''Nt_ExpG,
              ''Nt_Cond, ''Nt_Ecu,  ''Nt_FDef])

$(attLabels [("sidExpU", ''Exp)])
$(attLabels [("sidEcu", ''Ecu)])
$(attLabels [("sidExpG", ''ExpG)])
$(attLabels [("sidCond", ''Cond)])

-- | identity for unfolded expressions
asp_sid_ExpU
  =   (syn sidExpU p_Lit $ Lit <$> ter ch_lit_t)
  .+: (syn sidExpU p_Var $ Var <$> ter ch_var_t)
  .+: (syn sidExpU p_OpInf $ OpInf <$> at ch_op_inf_l sidExpU
                         <*> ter ch_op_inf_op <*> at ch_op_inf_r sidExpU)
  .+: (syn sidExpU p_OpPre $ OpPre <$> ter ch_op_pre_op
                         <*> at ch_op_pre_e sidExpU)
  .+: (syn sidExpU p_AppU $ AppU   <$> at ch_app_ecu sidEcu <*> at ch_app_e sidExpU)
  .+: emptyAspect

-- | identity for Ecu
asp_sid_Ecu
  = (syn sidEcu p_Ecu $ Ecu <$> ter ch_ecu_l <*> at ch_ecu_r sidExpG)
  .+: emptyAspect

-- | Identity for ExpG
asp_sidExpG
  =   (syn sidExpG p_ExpGIf
       $ ExpGIf <$> at ch_expGIf_e sidExpU <*> return Top <*> at ch_expGIf_tail sidExpG)
  .+: (syn sidExpG p_ExpGOr
       $ ExpGOr <$> at ch_expGOr_e sidExpU)
  .+: emptyAspect




-- | tests
testIdU :: Exp -> Exp
testIdU e
  = sem_Exp (asp_sid_Ecu .:+: asp_sid_ExpU
             .:+: asp_sidExpG .:+: dummy) e emptyAtt #. sidExpU 
  where dummy = singAsp $ syn sidCond p_Top $ undefined


e 1 = Var "x"
e 2 = Lit (ValZ 23)
e 3 = OpInf (e 1) "+" (e 2)
e 4 = OpInf (e 2) "+" (e 2)
e 5 = OpPre "-" $ OpInf (e 4) "+" (e 2)

test_id = [e i == testIdU (e i) | i <- [1..5]]
