{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MF.Analyses.AE where

import qualified Data.Set as S
import           MF.Core as C

solve bottom generate kill = C.solve transfer S.empty bottom Forward
    where
        transfer statement input = (input `S.difference` kill statement) `S.union` generate statement

type PropertySpace a = S.Set a

instance Ord a => Lattice (PropertySpace a) where
    join     = S.intersection
    (<:) l r = S.isSubsetOf r l