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

-- import Language.Grammars.AspectAG
-- import Language.Grammars.AspectAG.RecordInstances
-- import Data.GenRec
-- import Language.Grammars.AspectAG.TH
-- import Data.Proxy

import Language.MFAG.Syntax.Terminals
import Language.MFAG.Syntax.Exp.Base
import Prelude hiding (exp)
import Language.MFAG.Syntax.Set.Base as Set
-- import Data.Singletons
-- import Data.Singletons.TH
-- import Data.Singletons.TypeLits
-- import Data.Singletons.Prelude.Ord
-- import Data.Singletons.Prelude.Eq
-- import Data.Singletons.Prelude.Either

-- import Prelude hiding (exp)
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.TH
import Language.Grammars.AspectAG.RecordInstances

import Data.Type.Require hiding (emptyCtx)

import Data.GenRec hiding (Label)
import Data.GenRec.Label

import Data.Kind
import Data.Proxy
import GHC.TypeLits

import Data.Maybe
import Data.Type.Equality
import Control.Monad.Reader

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Ord
import Data.Singletons.Prelude.Eq
import Data.Singletons.Prelude.Either
import Data.Singletons.CustomStar
import Data.Singletons.Decide
import Data.Singletons.Prelude.List


-- base expression syntax generation
$(closeNTs [''Nt_Exp, ''Nt_Sig, ''Nt_ExpG, ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef, ''Nt_Tuple])

-- $(mkSemFuncs [''Nt_Set,  ''Nt_Cart, ''Nt_Sig,
--               ''Nt_Exp,  ''Nt_ExpG,
--               ''Nt_Cond, ''Nt_Ecu, ''Nt_FDef , ''Nt_Tuple
--              ])


-- | Identity attribute
$(attLabels [("sidExpC" , ''Exp)  ])
$(attLabels [("sidEcu"  , ''Ecu)  ])
$(attLabels [("sidExpG" , ''ExpG) ])
$(attLabels [("sidCond" , ''Cond) ])
$(attLabels [("sidTuple", ''Tuple)])
$(attLabels [("sidFDef" , ''FDef) ])
$(attLabels [("sidSig"  , ''Sig) ])
-- emptyAspect =
--   emptyAspectC (p_And .:. p_App .:. p_EProd .:. p_Ecu .:. p_Equa
--                 .:. p_ExpGIf .:. p_ExpGOr .:. p_Lit .:. p_Neg .:.
--                 p_OpInf .:. p_TCons .:. p_TSing .:. p_Top .:. p_Var .:. p_FDef
--                 .:. p_Sig .:. eL) Proxy

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
  (syn sidExpC p_App
    $ App   <$> ter ch_app_f
            <*> at ch_app_e sidExpC
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
         $ ExpGIf <$> at ch_expGIf_e    sidExpC
                  <*> at ch_expGIf_cond sidCond
                  <*> at ch_expGIf_tail sidExpG
      )
  .+: 
  (syn sidExpG p_ExpGOr
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

asp_sid_FDef = singAsp (syn sidFDef p_FDef
  $ FDef <$> ter ch_nfun <*> at ch_fun_sig sidSig <*> at ch_fun_body sidEcu)

--asp_sid_Core = asp_sid_Core' .:+: asp_sid_Core''

asp_sid_Core' =
       asp_sid_Ecu
  .:+: asp_sid_Cond
  .:+: asp_sid_Exp

asp_sid_Core'' =
       asp_sid_ExpG
  .:+: asp_sid_Tuple
  .:+: asp_sid_FDef


asp_sid_Sig = singAsp $ syn sidSig p_Sig (return Sig)

-- idE :: Exp -> Exp
-- idE e = sem_Exp (asp_sid_Exp .:+: asp_sid_Tuple) e emptyAtt #. sidExpC

-- idFun :: FDef -> FDef
-- idFun f = sem_FDef asp_sid_Core f emptyAtt #. sidFDef

idFun f = sem_FDef aspAll f emptyAtt #. sidFDef


data AspCore sig exp expg cond ecu fdef tuple =
  AspCore {sig   :: sig,
           exp   :: exp,
           expg  :: expg,
           cond  :: cond,
           ecu   :: ecu,
           fdef  :: fdef,
           tuple :: tuple}
aspAll = AspCore
  asp_sid_Sig
  asp_sid_Exp
  asp_sid_ExpG
  asp_sid_Cond
  asp_sid_Ecu
  asp_sid_FDef
  asp_sid_Tuple

--semfuncs
-- sem_Set asp Real = ((knitAspect p_Real) $ set asp) emptyGenRec
-- sem_Set asp Inte = ((knitAspect p_Inte) $ set asp) emptyGenRec
-- sem_Set asp (Sec sec_c)
--   = ((knitAspect p_Sec) $ set asp)
--     (((.*.) (((.=.) ch_sec_c) ((sem_Set asp) sec_c))) emptyGenRec)
-- sem_Set asp (Enum enum_c)
--   = ((knitAspect p_Enum) $ set asp)
--     (((.*.) (((.=.) ch_enum_c) (sem_Lit enum_c))) emptyGenRec)
-- sem_Set asp (Cart cart_c)
--   = ((knitAspect p_Cart) $ set asp)
--     (((.*.) (((.=.) ch_cart_c) ((sem_Cart asp) cart_c))) emptyGenRec)
-- sem_Cart asp (Cart_Nil cart_nil_t)
--   = ((knitAspect p_Cart_Nil) $ cart asp)
--     (((.*.) (((.=.) ch_cart_nil_t) (sem_Lit cart_nil_t))) emptyGenRec)
-- sem_Cart asp (Cart_Cons cart_hd_t cart_tl_t)
--   = ((knitAspect p_Cart_Cons) $ cart asp)
--     (((.*.) (((.=.) ch_cart_hd_t) ((sem_Set asp) cart_hd_t)))
--       (((.*.) (((.=.) ch_cart_tl_t) ((sem_Cart asp) cart_tl_t)))
--         emptyGenRec))
sem_Sig asp Sig = ((knitAspect p_Sig) $ sig asp) emptyGenRec
sem_Exp asp (Lit lit_t)
  = ((knitAspect p_Lit) $ exp asp)
    (((.*.) (((.=.) ch_lit_t) (sem_Lit lit_t))) emptyGenRec)
sem_Exp asp (Var var_t)
  = ((knitAspect p_Var) $ exp asp)
    (((.*.) (((.=.) ch_var_t) (sem_Lit var_t))) emptyGenRec)
sem_Exp asp (OpInf op_inf_l op_inf_op op_inf_r)
  = ((knitAspect p_OpInf) $ exp asp)
    (((.*.) (((.=.) ch_op_inf_l) ((sem_Exp asp) op_inf_l)))
      (((.*.) (((.=.) ch_op_inf_op) (sem_Lit op_inf_op)))
        (((.*.) (((.=.) ch_op_inf_r) ((sem_Exp asp) op_inf_r)))
          emptyGenRec)))
sem_Exp asp (App app_f app_e)
  = ((knitAspect p_App) $ exp asp)
    (((.*.) (((.=.) ch_app_f) (sem_Lit app_f)))
      (((.*.) (((.=.) ch_app_e) ((sem_Exp asp) app_e))) emptyGenRec))
sem_Exp asp (EProd eprod_e)
  = ((knitAspect p_EProd) $ exp asp)
    (((.*.) (((.=.) ch_eprod_e) ((sem_Tuple asp) eprod_e)))
      emptyGenRec)
sem_ExpG asp (ExpGIf expGIf_e expGIf_cond expGIf_tail)
  = ((knitAspect p_ExpGIf) $ expg asp)
    (((.*.) (((.=.) ch_expGIf_e) ((sem_Exp asp) expGIf_e)))
      (((.*.) (((.=.) ch_expGIf_cond) ((sem_Cond asp) expGIf_cond)))
        (((.*.) (((.=.) ch_expGIf_tail) ((sem_ExpG asp) expGIf_tail)))
          emptyGenRec)))
sem_ExpG asp (ExpGOr expGOr_e)
  = ((knitAspect p_ExpGOr) $ expg asp)
    (((.*.) (((.=.) ch_expGOr_e) ((sem_Exp asp) expGOr_e)))
      emptyGenRec)
sem_Cond asp Top = ((knitAspect p_Top) $ cond asp) emptyGenRec
sem_Cond asp (Equa equa_l equa_op equa_r)
  = ((knitAspect p_Equa) $ cond asp)
    (((.*.) (((.=.) ch_equa_l) ((sem_Exp asp) equa_l)))
      (((.*.) (((.=.) ch_equa_op) (sem_Lit equa_op)))
        (((.*.) (((.=.) ch_equa_r) ((sem_Exp asp) equa_r))) emptyGenRec)))
sem_Cond asp (And and_l and_r)
  = ((knitAspect p_And) $ cond asp)
    (((.*.) (((.=.) ch_and_l) ((sem_Cond asp) and_l)))
      (((.*.) (((.=.) ch_and_r) ((sem_Cond asp) and_r))) emptyGenRec))
sem_Cond asp (Neg neg_e)
  = ((knitAspect p_Neg) $ cond asp)
    (((.*.) (((.=.) ch_neg_e) ((sem_Cond asp) neg_e))) emptyGenRec)
sem_Ecu asp (Ecu ecu_l ecu_r)
  = ((knitAspect p_Ecu) $ ecu asp)
    (((.*.) (((.=.) ch_ecu_l) (sem_Lit ecu_l)))
      (((.*.) (((.=.) ch_ecu_r) ((sem_ExpG asp) ecu_r))) emptyGenRec))
sem_FDef asp (FDef nfun fun_sig fun_body)
  = ((knitAspect p_FDef) $ fdef asp)
    (((.*.) (((.=.) ch_nfun) (sem_Lit nfun)))
      (((.*.) (((.=.) ch_fun_sig) ((sem_Sig asp) fun_sig)))
        (((.*.) (((.=.) ch_fun_body) ((sem_Ecu asp) fun_body)))
          emptyGenRec)))
sem_Tuple asp (TCons tuple_h tuple_t)
  = ((knitAspect p_TCons) $ tuple asp)
    (((.*.) (((.=.) ch_tuple_h) ((sem_Exp asp) tuple_h)))
      (((.*.) (((.=.) ch_tuple_t) ((sem_Tuple asp) tuple_t)))
        emptyGenRec))
sem_Tuple asp (TSing tuple_s)
  = ((knitAspect p_TSing) $ tuple asp)
    (((.*.) (((.=.) ch_tuple_s) ((sem_Exp asp) tuple_s))) emptyGenRec)
