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

module Language.MFAG.Semantics.Eval.Weak where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

-- only base syntax
import Language.MFAG.Syntax.Exp.Base
import Language.MFAG.Syntax.Terminals
import Data.Map as M


-- only base types
import Language.MFAG.Syntax.Set.Base as Set

-- base expression syntax generation
-- base types syntax generation
$(closeNTs [''Nt_Set, ''Nt_Cart ,''Nt_Sig])

-- base expression syntax generation
$(closeNTs [''Nt_Exp, ''Nt_Val, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef])

$(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
              ''Nt_Exp,  ''Nt_Val,  ''Nt_ExpG,
              ''Nt_Cond, ''Nt_Ecu,  ''Nt_FDef])

--  First, let us define a simple evaluation, partial since terms
-- could be ill formed

-- evaluation attribute
$(attLabels [("seval", ''Val)])

-- by now, environments are only values, no fun env
type Env = M.Map NVar Val

-- eval attribute
$(attLabels [("ienv", ''Val)])


---------identity-------------------------------
$(attLabels [("idVal", ''Val)])
asp_idVal
  =   (syn idVal p_ValZ (ValZ <$> ter ch_valZ_t))
  .+: (syn idVal p_ValR (ValR <$> ter ch_valR_t))
  .+: emptyAspect
-------------------------------------------------

asp_eval_Exp
  =   (syn seval p_Lit $ at ch_lit_c idVal)
  .+: emptyAspect
