module MF.CoreM (Direction (..), ValueMap, solve) where

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.IntSet as IS
import qualified Data.Set    as S
import           Data.Maybe
import           Prelude hiding (lookup, init, all)

import           MF.Flowable
import           MF.Context
import           MF.Lattice
import           MF.Utils

data Direction = Forward | Backward
type ValueMap property = IM.IntMap property


-- | Helper function to lookup a value in a ValueMap when given a label
lookup :: Label -> IM.IntMap a -> a
lookup l m = case IM.lookup l m of 
                 Just a  -> a
                 Nothing -> error $ "Looking for non existing label: " ++ show l
                 
-- | Helper function to reverse the flow in case of a backwards analysis
reverseFlow :: Flow -> Flow
reverseFlow = map (\(l, l') -> (l', l))


-- | Helper function to apply the transfer function to all values in a ValueMap
--all :: (Flowable node, Lattice l m) => (Block node -> l -> m l) -> node -> ValueMap l -> m (ValueMap l)
all transfer program valueMap = do result <- sequence . map (uncurry out) . IM.toList . mergeWith transfer (blocks program) $ valueMap
                                   return $ IM.fromList result

out :: Monad m => a -> m b -> m (a, b)
out a b = do b' <- b
             return (a, b')


-- | 'solve' implements the worklist algorithm to compute the MFP solution as described by NNH, page 75
solve :: (Flowable n, Lattice l m, Monad m) => (Block n -> l -> m l) -> l -> l -> Direction -> n -> m (ValueMap l)
solve transfer extremalValue bottom direction p = solve' p initialValueMap worklist
    where                
        -- Step 1. Initialization
        worklist       = case direction of 
                            Forward  -> flow p
                            Backward -> reverseFlow . flow $ p
                
        extremalLabels = case direction of 
                            Forward  -> [init p]
                            Backward -> final p
                
        initialValueMap = IM.fromList $ map initialize $ labels p
        
            where
                initialize l = if l `elem` extremalLabels then (l, extremalValue) else (l, bottom)
                
        -- Step 2. Fix point iteration
--        solve' :: (Flowable n, Lattice l m, Monad m) => n -> ValueMap l -> Flow -> m (ValueMap l)
        solve' p valueMap []                            = all transfer p valueMap --do return valueMap -- all transfer p valueMap -- Step 3. From context to effect values
        solve' p valueMap w@((start, end):worklistTail) = do let context  = lookup start valueMap
                                                                 previous = lookup end valueMap  
                                                                 
                                                             effect <- transfer (lookup start $ blocks p) context
                                                             joined <- join effect previous
                                                             
                                                             let newWorklist = worklistTail ++ [(l', l'') | (l', l'') <- worklist, l' == end] 
                                                                 newValueMap = IM.insert end joined valueMap
                                                                 
                                                             case not (effect <: previous) of
                                                                 True  -> solve' p newValueMap newWorklist 
                                                                 False -> solve' p valueMap worklistTail
