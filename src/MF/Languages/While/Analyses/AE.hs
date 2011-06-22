{-# LANGUAGE TypeSynonymInstances #-}

module MF.Languages.While.Analyses.AE where

import qualified Data.Set as S
--import qualified MF.Analyses.AE as AE
import MF.Core as C
import MF.Languages.While.Util
import MF.Languages.While.AST
import MF.Languages.While.Flow as Flow
import MF.Flowable

import Debug.Trace

type PropertySpace = S.Set Expression

--solve :: Program Statement -> ValueMap PropertySpace
solve p = C.solve transfer S.empty bottom Forward p
    where
        transfer node input         = (input `S.difference` kill node) `S.union` generate node 
        bottom                      = S.fromList $ expressions p
                                   
        kill (Assignment c exp)     = let x = S.fromList [e | e <- expressions p, c `S.member` fv e] in x
        kill (Flow.Skip)            = S.empty
        kill (Expression exp)       = S.empty

        generate (Assignment c exp) = let x = S.fromList [e | e <- expressions exp, not (c `S.member` fv e )] in x
        generate (Flow.Skip)        = S.empty
        generate (Expression exp)   = let x = S.fromList $ expressions exp in x
        
        
instance Lattice PropertySpace where
    join     = S.intersection
    (<:) l r = S.isSubsetOf r l