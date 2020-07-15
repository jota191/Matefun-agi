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
{-# LANGUAGE ScopedTypeVariables #-}

module Language.MFAG.SemFuncs where

import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH

import Language.MFAG.AST
import Language.MFAG.Syntax.Terminals
import Prelude hiding (exp)

--import Data.Singletons


-- $(mkSemFuncs [''Nt_Set, ''Nt_Exp, ''Nt_Sig,
--               ''Nt_ExpG, ''Nt_Cond,
--               ''Nt_Ecu, ''Nt_FDef,

data AspAll set sig exp expg cond ecu fdef tuple =
  AspAll  {set   :: set,
           sig   :: sig,
           exp   :: exp,
           expg  :: expg,
           cond  :: cond,
           ecu   :: ecu,
           fdef  :: fdef,
           tuple :: tuple}

(.:+:.)
  (AspAll set  sig  exp  expg  cond  ecu  fdef  tuple)
  (AspAll set' sig' exp' expg' cond' ecu' fdef' tuple')
  = AspAll (set   .:+: set'  )
           (sig   .:+: sig'  )
           (exp   .:+: exp'  )
           (expg  .:+: expg' )
           (cond  .:+: cond' )
           (ecu   .:+: ecu'  )
           (fdef  .:+: fdef' )
           (tuple .:+: tuple')

sem_Set asp (Set sort xs refinement) =
  ((knitAspect p_Set) $ set asp)
    (((.*.) (((.=.) ch_sort) (sem_Lit sort)))
       (((.*.) (((.=.) ch_xs) (sem_Lit xs)))
          (((.*.) (((.=.) ch_refinement) ((sem_Cond asp) refinement)))
             emptyGenRec)))


-- sem_Exp (asp :: AspAll (CAspect '[] set)
--           (CAspect '[] sig) (CAspect '[] r1)
--           (CAspect '[] expg) (CAspect '[] cond)
--           (CAspect '[] ecu) (CAspect '[] fdef) (CAspect '[] r2))
--   (Lit lit_t) =
--   ((knitAspect p_Lit) $ exp asp)
--     (((.*.) (((.=.) ch_lit_t) (sem_Lit lit_t))) emptyGenRec)
sem_Exp asp (Lit lit_t) =
  ((knitAspect p_Lit) $ exp asp)
    (((.*.) (((.=.) ch_lit_t) (sem_Lit lit_t))) emptyGenRec)
sem_Exp asp (Var var_t) =
  ((knitAspect p_Var) $ exp asp)
    (((.*.) (((.=.) ch_var_t) (sem_Lit var_t))) emptyGenRec)
sem_Exp asp (OpInf op_inf_l op_inf_op op_inf_r) =
  ((knitAspect p_OpInf) $ exp asp)
    (((.*.) (((.=.) ch_op_inf_l) ((sem_Exp asp) op_inf_l)))
       (((.*.) (((.=.) ch_op_inf_op) (sem_Lit op_inf_op)))
          (((.*.) (((.=.) ch_op_inf_r) ((sem_Exp asp) op_inf_r))) emptyGenRec)))
sem_Exp asp (App app_f app_e) =
  ((knitAspect p_App) $ exp asp)
    (((.*.) (((.=.) ch_app_f) (sem_Lit app_f)))
       (((.*.) (((.=.) ch_app_e) ((sem_Exp asp) app_e))) emptyGenRec))
sem_Exp asp (EProd eprod_e) =
  ((knitAspect p_EProd) $ exp asp)
    (((.*.) (((.=.) ch_eprod_e) ((sem_Tuple asp) eprod_e))) emptyGenRec)
sem_Exp asp (Index index_e index_i)
      = ((knitAspect p_Index) $ exp asp)
          (((.*.) (((.=.) ch_index_e) ((sem_Exp asp) index_e)))
             (((.*.) (((.=.) ch_index_i) (sem_Lit index_i))) emptyGenRec))

sem_Sig asp (Sig dom cod) =
  ((knitAspect p_Sig) $ sig asp)
    (((.*.) (((.=.) ch_dom) ((sem_Set asp) dom)))
       (((.*.) (((.=.) ch_cod) ((sem_Set asp) cod))) emptyGenRec))

sem_ExpG asp (ExpGIf expGIf_e expGIf_cond expGIf_tail) =
  ((knitAspect p_ExpGIf) $ expg asp)
    (((.*.) (((.=.) ch_expGIf_e) ((sem_Exp asp) expGIf_e)))
       (((.*.) (((.=.) ch_expGIf_cond) ((sem_Cond asp) expGIf_cond)))
          (((.*.) (((.=.) ch_expGIf_tail) ((sem_ExpG asp) expGIf_tail)))
             emptyGenRec)))
sem_ExpG asp (ExpGOr expGOr_e) =
  ((knitAspect p_ExpGOr) $ expg asp)
    (((.*.) (((.=.) ch_expGOr_e) ((sem_Exp asp) expGOr_e))) emptyGenRec)

sem_Cond asp Top = ((knitAspect p_Top) $ cond asp) emptyGenRec
sem_Cond asp (Equa equa_l equa_op equa_r) =
  ((knitAspect p_Equa) $ cond asp)
    (((.*.) (((.=.) ch_equa_l) ((sem_Exp asp) equa_l)))
       (((.*.) (((.=.) ch_equa_op) (sem_Lit equa_op)))
          (((.*.) (((.=.) ch_equa_r) ((sem_Exp asp) equa_r))) emptyGenRec)))
sem_Cond asp (And and_l and_r) =
  ((knitAspect p_And) $ cond asp)
    (((.*.) (((.=.) ch_and_l) ((sem_Cond asp) and_l)))
       (((.*.) (((.=.) ch_and_r) ((sem_Cond asp) and_r))) emptyGenRec))
sem_Cond asp (Neg neg_e) =
  ((knitAspect p_Neg) $ cond asp)
    (((.*.) (((.=.) ch_neg_e) ((sem_Cond asp) neg_e))) emptyGenRec)

sem_Ecu asp (Ecu ecu_l ecu_r) =
  ((knitAspect p_Ecu) $ ecu asp)
    (((.*.) (((.=.) ch_ecu_l) (sem_Lit ecu_l)))
       (((.*.) (((.=.) ch_ecu_r) ((sem_ExpG asp) ecu_r))) emptyGenRec))

sem_FDef asp (FDef nfun fun_sig fun_body) =
  ((knitAspect p_FDef) $ fdef asp)
    (((.*.) (((.=.) ch_nfun) (sem_Lit nfun)))
       (((.*.) (((.=.) ch_fun_sig) ((sem_Sig asp) fun_sig)))
          (((.*.) (((.=.) ch_fun_body) ((sem_Ecu asp) fun_body))) emptyGenRec)))

sem_Tuple asp (TCons tuple_h tuple_t) =
  ((knitAspect p_TCons) $ tuple asp)
    (((.*.) (((.=.) ch_tuple_h) ((sem_Exp asp) tuple_h)))
       (((.*.) (((.=.) ch_tuple_t) ((sem_Tuple asp) tuple_t))) emptyGenRec))
sem_Tuple asp (TSing tuple_s) =
  ((knitAspect p_TSing) $ tuple asp)
    (((.*.) (((.=.) ch_tuple_s) ((sem_Exp asp) tuple_s))) emptyGenRec)
