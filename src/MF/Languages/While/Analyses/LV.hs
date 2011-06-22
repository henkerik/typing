{-# LANGUAGE TypeSynonymInstances #-}

module MF.Languages.While.Analyses.LV where

import qualified Data.Set as S

import MF.Core as C
import MF.Languages.While.Util
import MF.Languages.While.AST
import MF.Languages.While.Flow as Flow
import MF.Flowable

import Debug.Trace

type PropertySpace = S.Set Identifier

solve p = C.solve (transfer . simplify) S.empty bottom Backward p
    where
        transfer node input         = (input `S.difference` kill node) `S.union` generate node 
        bottom                      = S.empty
                                   
        kill (Assignment c exp)     = S.singleton c
        kill (Expression  exp)      = S.empty
        kill (Flow.Skip)            = S.empty

        generate (Assignment c exp) = fv exp
        generate (Expression exp)   = fv exp
        generate (Flow.Skip)        = S.empty
        
        
instance Lattice PropertySpace where
    join     = S.union
    (<:)     = S.isSubsetOf