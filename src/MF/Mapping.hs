{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module MF.Mapping where
    
import MF.Lattice
import Data.Set as S
import Data.Map as M

{-    
    
-- Independent attributes
type c :-> l = M.Map c l
instance (Lattice l, Ord c) => Lattice (c :-> l) where
    join = M.unionWith join
    (<:) = M.isSubmapOfBy (<:)


-- Relational
type c :=> l = Set (Map c l)
instance (Lattice l, Ord c, Ord l) => Lattice (c :=> l) where
    join = S.union
    (<:) = S.isSubsetOf
    
    -}