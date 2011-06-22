module MF.Languages.While.Analyses.LifeVariableAnalysis where

import qualified Data.Set as S
import qualified MF.Analyses.LifeVariableAnalysis as LVA
import MF.Core
import MF.Flowable
import MF.Languages.While.Util
import MF.Languages.While.AST
import MF.Languages.While.Flow as Flow


type PropertySpace = LVA.PropertySpace Identifier

--solve :: Flowable p b => Labelled p -> ValueMap PropertySpace
solve = LVA.solve generate kill
    where
        kill (Assignment c exp)     = S.singleton c
        kill (Expression  exp)      = S.empty
        kill (Flow.Skip)            = S.empty

        generate (Assignment c exp) = fv exp
        generate (Expression exp)   = fv exp
        generate (Flow.Skip)        = S.empty