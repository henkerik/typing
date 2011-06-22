module MF.Utils where
    
import Data.IntMap as IM
import Data.Maybe

mergeWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
mergeWith f left = IM.mapWithKey (\k -> f $ func $ IM.lookup k left)
    where
        func a = case a of 
                     Just t  -> t
                     Nothing -> error "Couldn't merge"
