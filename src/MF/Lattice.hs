{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module MF.Lattice where
    



class Lattice a m | a -> m where
    join   :: a -> a -> m a
    (<:)   :: a -> a -> Bool
    
    
    {-
class Mapping m where
    remove :: k -> m k e -> m k e
    insert :: k -> e -> m k e -> m k e
    
    
    
instance Mapping (k :-> e) where
    remove = M.delete 
    insert = M.insert
    
    -}
    

