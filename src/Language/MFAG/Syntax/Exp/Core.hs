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

$(addProd "App"   ''Nt_Exp [("app_f",     Ter    ''NFun),
                            ("app_e",     NonTer ''Nt_Exp)])

-- base expression syntax generation
-- base types syntax generation
$(closeNTs [''Nt_Set, ''Nt_Cart ,''Nt_Sig])

-- base expression syntax generation
$(closeNTs [''Nt_Exp, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef])

$(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
              ''Nt_Exp,  ''Nt_ExpG,
              ''Nt_Cond, ''Nt_Ecu,  ''Nt_FDef])

$(attLabels [("sidExp", ''Exp)])


-- | identity for core expressions
sid_Exp_Core
  =   (syn sidExp p_Lit $ Lit <$> ter ch_lit_t)
  .+: (syn sidExp p_Var $ Var <$> ter ch_var_t)
  .+: (syn sidExp p_OpInf $ OpInf <$> at ch_op_inf_l sidExp
                         <*> ter ch_op_inf_op <*> at ch_op_inf_l sidExp)
  .+: (syn sidExp p_OpPre $ OpPre <$> ter ch_op_pre_op
                         <*> at ch_op_pre_e sidExp)
  .+: (syn sidExp p_App   $ App   <$> ter ch_app_f <*> at ch_app_e sidExp)
  .+: emptyAspect
