module MF.Core where

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.IntSet as IS
import qualified Data.Set    as S
import           Data.Maybe
import           Prelude hiding (lookup, init, all)

import           MF.Flowable
import           MF.Context
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
all :: (Flowable node, Lattice l) => (Block node -> l -> l) -> node -> ValueMap l -> ValueMap l
all transfer program = mergeWith transfer (blocks program)


-- | 'solve' implements the worklist algorithm to compute the MFP solution as described by NNH, page 75
solve :: (Flowable n, Lattice l) => (Block n -> l -> l) -> l -> l -> Direction -> n -> ValueMap l
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
        solve' p valueMap []                            = all transfer p valueMap -- Step 3. From context to effect values
        solve' p valueMap w@((start, end):worklistTail) = if not (effect <: previous) then solve' p newValueMap newWorklist else solve' p valueMap worklistTail
            where
                context  = lookup start valueMap
                previous = lookup end valueMap                             
                effect = transfer (lookup start $ blocks p) context
                 
                newWorklist = worklistTail ++ [(l', l'') | (l', l'') <- worklist, l' == end] 
                newValueMap = IM.adjust (join effect) end valueMap