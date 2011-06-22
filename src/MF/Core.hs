{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
module MF.Core where

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.IntSet as IS
import qualified Data.Set    as S
import           Data.Maybe
import           Prelude hiding (lookup, init, all)
import           MF.Flowable

import Debug.Trace

data Direction = Forward | Backward
type ValueMap property = IM.IntMap property

type c :-> l = M.Map c l
type Stack = [Int]

instance (Lattice l, Ord c) => Lattice (c :-> l) where
    join = M.unionWith join
    (<:) = M.isSubmapOfBy (<:)


class Context a where
    lift :: (Lattice l, Lattice (a :-> l)) => (Block node -> l -> l) -> Block node -> (a :-> l) -> (a :-> l)
    
instance Context Label where
    lift transfer stmt = M.map (transfer stmt)

instance Context Stack where
    lift transfer stmt@(Entry        _) = M.map (transfer stmt)
    lift transfer stmt@(Exit         _) = M.map (transfer stmt)               
    lift transfer stmt@(Call   lc lr _) = M.map (transfer stmt) . M.mapKeysWith join (bound lc) 
    lift transfer stmt@(Return lc lr _) = M.map (transfer stmt) . M.mapKeys tail . M.filterWithKey (\k v -> not (null k) && head k == lc)
    lift transfer stmt@(Normal       _) = M.map (transfer stmt)
                                        
bound l = take 3 . (:) l

{-
instance Context Stack where
    lift transfer stmt@(B Entry  _            ) = M.map (transfer stmt)
    lift transfer stmt@(B Exit   _            ) = M.map (transfer stmt)               
    lift transfer stmt@(B Call   (L [lc,lr] _)) = M.map (transfer stmt) . M.mapKeys (lc:) 
    lift transfer stmt@(B Return (L [lc,lr] _)) = M.map (transfer stmt) . M.mapKeys tail . M.filterWithKey (\k v -> not (null k) && head k == lc)
    lift transfer stmt@(B _      _            ) = M.map (transfer stmt)
-}

lookupValue :: Label -> ValueMap l -> l
lookupValue l m = case IM.lookup l m of
                      Just v  -> v
                      Nothing -> error $ "Failed to lookup: " ++ show l

type Worklist = Flow

--blockAt :: (Lattice l, Flowable stmt) => Labelled stmt phatom -> Label -> ((Block stmt -> l -> l) -> l -> l)
--blockAt f label = fromJust $ IM.lookup label (blocks f)
lookup :: Label -> IM.IntMap a -> a
lookup l m = case IM.lookup l m of 
                 Just a  -> a
                 Nothing -> error $ "Looking for non existing label: " ++ show l

reverseWorklist :: Worklist -> Worklist
reverseWorklist = map (\(l, l') -> (l', l))

--all :: (Flowable a, Lattice l) => (F a -> l -> l) -> F a -> ValueMap l -> ValueMap l
all transferMap = IM.mapWithKey (\l -> lookup l transferMap)

   
--solve :: (Flowable a, Lattice l) => (Block a -> l -> l) -> l -> l -> Direction -> Labelled a phatom -> ValueMap l
--solve :: (Show l, Flowable n, Lattice l) => (Labelled n -> l -> l) -> l -> l -> Direction -> n -> ValueMap l
solve transfer extremalValue bottom direction p = solve' p initialValueMap worklist
    where                
        -- Step 1. Initialization
        worklist       = case direction of 
                            Forward  -> flow p
                            Backward -> reverseWorklist . flow $ p
                
        extremalLabels = case direction of 
                            Forward  -> [init p]
                            Backward -> final p
                
        initialValueMap = IM.fromList $ map initialize $ labels p
            where
                initialize l = if l `elem` extremalLabels then (l, extremalValue) else (l, bottom)
                
        
        
        
        -- Step 2. Fix point iteration
        solve' p valueMap []                            = trace ("[] :: " ++ show valueMap) $ valueMap -- all transferMap valueMap -- Step 3. From entry to exit values
        solve' p valueMap w@((start, end):worklistTail) = trace (show w ++ " :: " ++ show valueMap) $ if not (effect <: previous) then solve' p newValueMap newWorklist else solve' p valueMap worklistTail
            where
                context  = lookup start valueMap
                previous = lookup end valueMap
                
--                effect = previous
                --transfer = lookup start transferMap 
                
                -- effect   = transfer context
                --effect   = (blockAt p start) transfer context -- transfer context
                
                --continuation :: Block n
                --(B continuation) = lookup start (blocks p)
                
                ---effect = case lookup start (blocks p) of 
                   --          (B continuation) -> continuation transfer context
                             
                effect = lookup start (blocks p) transfer context
                 
                newWorklist = worklistTail ++ [(l', l'') | (l', l'') <- worklist, l' == end] 
                newValueMap = IM.adjust (join effect) end valueMap