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

module Language.MFAG.Syntax.Exp.Base where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.Syntax.Terminals

import Language.MFAG.Syntax.Set.Base as Set


-- Definition of signatures, type of functions
$(addNont "Sig")
$(addProd "Sig" ''Nt_Sig [("dom", NonTer ''Set.Nt_Set),
                          ("cod", NonTer ''Set.Nt_Set)])


{- TODO: no se si va aca, pero es un buen test -}
-- $(closeNTs [''Nt_Set, ''Nt_Cart ,''Nt_Sig])
--  Obs: The datatypes are defined here, in this module, this will be
-- useful..

  
-- -- | List of variables
-- $(addNont "NVars")
-- since they are a list of terminals, to make a grammar is too much
-- bureaucracy, I'll keep it simple. NVars is defined in Terminals.hs

-- values

data Val
  = ValZ    Integer
  | ValR    Double
  | ValC    Constructor
  | ValSec  [Val]
  | ValTupl [Val]
  deriving (Show, Eq, Read)

arity :: Val -> Int
arity (ValTupl t) = length t
arity _           = 1

untup (ValTupl t) = t
untup t           = [t]

-- $(addNont "Val")
-- $(addProd "ValZ" ''Nt_Val [("valZ_t", Ter ''Integer)])
-- $(addProd "ValR" ''Nt_Val [("valR_t", Ter ''Double)])
-- $(addProd "ValC" ''Nt_Val [("valC_t", Ter ''Constructor)])
-- $(addProd "ValC2" ''Nt_Val [("valC2_t", Ter ''Constructor)])

-- TODO: more values, for now we are ok
$(addNont "Exp")
$(addProd "Lit"   ''Nt_Exp [("lit_t",     Ter    ''Val)])
$(addProd "Var"   ''Nt_Exp [("var_t",     Ter    ''NVar)])
$(addProd "OpInf" ''Nt_Exp [("op_inf_l",  NonTer ''Nt_Exp),
                            ("op_inf_op", Ter    ''BOp),
                            ("op_inf_r",  NonTer ''Nt_Exp)])
$(addProd "OpPre" ''Nt_Exp [("op_pre_op", Ter    ''UOp),
                            ("op_pre_e",  NonTer ''Nt_Exp)])
$(addProd "App"   ''Nt_Exp [("app_f",     Ter    ''NFun),
                            ("app_e",     NonTer ''Nt_Exp)])


$(addNont "Cond")
$(addProd "Top" ''Nt_Cond [])

-- function body definiitons (cases)
$(addNont "ExpG")
$(addProd "ExpGIf" ''Nt_ExpG [("expGIf_e", NonTer ''Nt_Exp),
                              ("expGIf_cond", NonTer ''Nt_Cond),
                              ("expGIf_tail", NonTer ''Nt_ExpG)])
$(addProd "ExpGOr" ''Nt_ExpG [("expGOr_e", NonTer ''Nt_Exp)])

-- algorithm definitions
$(addNont "Ecu")
$(addProd "Ecu" ''Nt_Ecu [("ecu_l", Ter ''NVars),
                          ("ecu_r", NonTer ''Nt_ExpG)])

-- function definitons
$(addNont "FDef")
$(addProd "FDef" ''Nt_FDef [("nfun", Ter ''NFun),
                            ("fun_sig",  NonTer ''Nt_Sig),
                            ("fun_body", NonTer ''Nt_Ecu)])



-- base expression syntax generation
-- base types syntax generation
$(closeNTs [''Nt_Set, ''Nt_Cart ,''Nt_Sig])

-- base expression syntax generation
$(closeNTs [''Nt_Exp, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef])

$(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
              ''Nt_Exp,  ''Nt_ExpG,
              ''Nt_Cond, ''Nt_Ecu,  ''Nt_FDef])

