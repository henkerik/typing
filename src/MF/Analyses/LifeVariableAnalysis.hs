{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MF.Analyses.LifeVariableAnalysis where

import qualified Data.Set as S
import           MF.Core as C

solve generate kill = C.solve transfer S.empty S.empty Backward
    where
        transfer statement input = (input `S.difference` kill statement) `S.union` generate statement

type PropertySpace a = S.Set a

instance Ord a => Lattice (PropertySpace a) where
    join     = S.union
    (<:)     = S.isSubsetOf