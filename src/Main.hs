module Main where

import Language.MFAG.Syntax.Terminals
import Language.MFAG.Syntax.Set.Base
import Language.MFAG.Syntax.Exp.Core
import Language.MFAG.Syntax.Exp.Base
import Language.MFAG.Semantics.Env
import Language.MFAG.Semantics.Unfold
import Language.MFAG.Semantics.Test


main :: IO ()
main = print $ t 1 

