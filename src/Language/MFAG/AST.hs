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
{-# LANGUAGE RankNTypes #-}

module Language.MFAG.AST where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Data.GenRec -- should be exported by AAG, perhaps?

import Language.MFAG.Syntax.Terminals

import Data.Singletons

--import Data.Singletons
-- cond

$(addNont "Exp")
$(addProd "Lit"   ''Nt_Exp [("lit_t",     Ter    ''Val)])
$(addProd "Var"   ''Nt_Exp [("var_t",     Ter    ''NVar)])
$(addProd "OpInf" ''Nt_Exp [("op_inf_l",  NonTer ''Nt_Exp),
                            ("op_inf_op", Ter    ''BOp),
                            ("op_inf_r",  NonTer ''Nt_Exp)])

$(addNont "Cond")
$(addProd "Top" ''Nt_Cond [])
$(addProd "Equa" ''Nt_Cond [("equa_l", NonTer ''Nt_Exp),
                            ("equa_op", Ter   ''EOp),
                            ("equa_r", NonTer ''Nt_Exp)])
$(addProd "And" ''Nt_Cond [("and_l", NonTer ''Nt_Cond),
                           ("and_r", NonTer ''Nt_Cond)])
$(addProd "Neg" ''Nt_Cond [("neg_e", NonTer ''Nt_Cond)])

$(addNont "Set")
$(addProd "Set" ''Nt_Set [("sort", Ter ''Sort),
                          ("xs", Ter ''NVars),
                          ("refinement", NonTer ''Nt_Cond)])

-- Definition of signatures, type of functions
$(addNont "Sig")
$(addProd "Sig" ''Nt_Sig [("dom", NonTer ''Nt_Set),
                          ("cod", NonTer ''Nt_Set)])


-- | Application, that is, an abstract call to a function.
$(addProd "App"   ''Nt_Exp [("app_f",     Ter    ''NFun),
                            ("app_e",     NonTer ''Nt_Exp)])

$(addNont "Tuple")
$(addProd "TCons" ''Nt_Tuple [("tuple_h", NonTer ''Nt_Exp),
                              ("tuple_t", NonTer ''Nt_Tuple)])
$(addProd "TSing" ''Nt_Tuple [("tuple_s", NonTer ''Nt_Exp)])

$(addProd "EProd" ''Nt_Exp [("eprod_e",     NonTer ''Nt_Tuple)])

$(addProd "Index" ''Nt_Exp [("index_e", NonTer ''Nt_Exp),
                            ("index_i", Ter  ''Int)])

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
$(closeNTs [''Nt_Set, ''Nt_Exp, ''Nt_Sig,
            ''Nt_ExpG, ''Nt_Cond,
            ''Nt_Ecu, ''Nt_FDef,
            ''Nt_Tuple])
-- $(mkSemFuncs [''Nt_Set, ''Nt_Exp, ''Nt_Sig,
--               ''Nt_ExpG, ''Nt_Cond,
--               ''Nt_Ecu, ''Nt_FDef, ''Nt_Tuple])

