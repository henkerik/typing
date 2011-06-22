{-# LANGUAGE GADTs #-}
module MF.Languages.While.Flow where

import Prelude hiding (init)
import qualified Data.IntMap as IM
import qualified Data.Map as M

import           MF.Flowable hiding (Call, Return, Entry, Exit)
import qualified MF.Flowable as F

import MF.Languages.While.AST hiding (Skip,Call) 
import qualified MF.Languages.While.AST as AST


data Simplified = Assignment Identifier Expression
                | Expression Expression
                | Call [Identifier] [Identifier]
                | Entry [Identifier] [Identifier]
                | Exit [Identifier] [Identifier]
                | Return [Identifier] [Identifier]
                | Skip
                deriving Show
            
          
simplify :: Block Node -> Simplified
simplify (B F.Normal (L _ (Program d s)))       = Skip
simplify (B F.Normal (L _ (DSequence f s)))     = Skip
simplify (B F.Normal (L _ (Assign x a)))        = Assignment x a
simplify (B F.Normal (L _ (AST.Skip)))          = Skip
simplify (B F.Normal (L _ (Sequence f s)))      = Skip
simplify (B F.Normal (L _ (If c t f)))          = Expression c
simplify (B F.Normal (L _ (While c s)))         = Expression c
simplify (B F.Call   (L _ (AST.Call n i o)))    = Call i o
simplify (B F.Return (L _ (AST.Call n i o)))    = Return i o
simplify (B F.Entry  (L _ (Procedure n i o s))) = Entry i o
simplify (B F.Exit   (L _ (Procedure n i o s))) = Exit i o
simplify (B a@(_) b@(_))                        = error $ show a



type Environment = M.Map Identifier (Label, Label)

instance Flowable Node where 
    init        (L [ ]     (Program d s))       = init s
    init        (L []      (DSequence f s))     = init f
    init        (L [ln,lx] (Procedure _ _ _ _)) = ln
    init        (L [l]     (Assign x a))        = l
    init        (L [l]     (AST.Skip))          = l
    init        (L []      (Sequence f s))      = init f
    init        (L [l]     (If c t f))          = l
    init        (L [l]     (While c s))         = l
    init        (L [lc,lr] (AST.Call n i o))    = -lc
                            
    final       (L []      (Program d s))       = final s           
    final       (L []      (DSequence f s))     = final s
    final       (L [ln,lx] (Procedure _ _ _ _)) = [lx]
    final       (L [l]     (Assign x a))        = [l]
    final       (L [l]     (AST.Skip))          = [l]
    final       (L []      (Sequence f s))      = final s
    final       (L [l]     (If c t f))          = final t ++ final f
    final       (L [l]     (While c s))         = [l]
    final       (L [lc,lr] (AST.Call n i o))    = [-lr]
                          
    blocks      (L []      (Program d s))       = blocks d `IM.union` blocks s
    blocks      (L []      (DSequence f s))     = blocks f `IM.union` blocks s
    blocks stmt@(L [ln,lx] (Procedure _ _ _ s)) = IM.fromList [(ln, \f -> f $ B F.Entry stmt), (lx, \f -> f $ B F.Exit stmt)] `IM.union` blocks s
    blocks stmt@(L [l]     (Assign x a))        = IM.fromList [(l, \f -> f $ B F.Normal stmt)]
    blocks stmt@(L [l]     (AST.Skip))          = IM.fromList [(l, \f -> f $ B F.Normal stmt)]
    blocks      (L []      (Sequence f s))      = blocks f `IM.union` blocks s
    blocks stmt@(L [l]     (If c t f))          = IM.fromList [(l, \f -> f $ B F.Normal stmt)] `IM.union` blocks t `IM.union` blocks f
    blocks stmt@(L [l]     (While c s))         = IM.fromList [(l, \f -> f $ B F.Normal stmt)] `IM.union` blocks s
    blocks stmt@(L [lc,lr] (AST.Call n i o))    = IM.fromList [
                                                                  (-lc, \f -> f $ B F.Normal (L [-lc] AST.Skip)), 
                                                                  (lc,  \f -> f $ B F.Call   stmt), 
                                                                  (lr,  \f -> f $ B F.Return stmt), 
                                                                  (-lr, \f -> f $ B F.Normal (L [-lr] AST.Skip))
                                                              ]
                          
    flow        (L []      (DSequence f s))     = flow f ++ flow s
    flow        (L [ln,lx] (Procedure _ _ _ s)) = flow s ++ [(ln, init s)] ++ [(l,lx) | l <- final s]
    flow        (L [l]     (Assign x a))        = []
    flow        (L [l]     (AST.Skip))          = []
    flow        (L []      (Sequence f s))      = [(l, init s) | l <- final f] ++ flow f ++ flow s
    flow        (L [l]     (If c t f))          = [(l, init t), (l, init f)] ++ flow t ++ flow f
    flow        (L [l]     (While c s))         = [(l, init s)] ++ [(l', l) | l' <- final s] ++ flow s
    flow        (L [lc,lr] (AST.Call n i o))    = [] -- Interflow is added in Program 
    flow        (L []      (Program d s))       = flow d ++ flow s ++ interflow (environment d) d ++ interflow (environment d) s
                                                where
                                                   environment :: Labelled Node Declaration -> Environment
                                                   environment (L [l]     (DSequence f s))     = environment f `M.union` environment s
                                                   environment (L [ln,lx] (Procedure n _ _ _)) = M.singleton n (ln, lx)
                                                   
                                                   interflow :: Environment -> Labelled Node ix -> Flow
                                                   interflow env (L [ln,lx] (Procedure _ _ _ s)) = interflow env s
                                                   interflow env (L []      (DSequence f s))     = interflow env f ++ interflow env s
                                                   interflow env (L []      (Sequence f s))      = interflow env f ++ interflow env s
                                                   interflow env (L [l]     (If c t f))          = interflow env t ++ interflow env f
                                                   interflow env (L [l]     (While c s))         = interflow env s
                                                   interflow env (L [l]     (Assign x a))        = []
                                                   interflow env (L [lc,lr] (AST.Call n i o))    = to:from:[(-lc, lc), (lr, -lr), (-lc, -lr)] 
                                                                                                 where
                                                                                                    to   = (lc,le)
                                                                                                    from = (lx,lr)
                                                                                                    (Just (le,lx)) = M.lookup n env 
                                                   interflow env (L _  stmt@(_))                 = error $ "foo"

                                               
                                               
                                               
                                               
{--                                            
                                               
instance Flowable Program where
    init   (L l (Program d s)) = init s
    final  (L l (Program d s)) = final s
    blocks (L l (Program d s)) = blocks d `IM.union` blocks s
    labels (L l (Program d s)) = labels d `IS.union` lables s
    flow   (L l (Program d s)) = 


instance Flowable Declaration where
    init   (L l (Sequence f s))    = init f
    init   (L l (Procedure i o s)) = init 

    blocks (L l (Sequence f s))    = blocks f `union` blocks s
    blocks (L l (Procedure i o s)) = IM.fromList [(ln, Is), (lx, End)] `union` blocks s

--}

{--
skip :: Block
skip = B (\f inp -> f Skip inp)

expression :: Expression -> Block
expression exp = B (\inp -> transfer (Expression exp) inp)

assignment :: Identifier -> Expression -> Block
assignment x a = B (\inp -> transfer (Assignment x a) inp)

instance Transferable Node where
    transfer _ = id
--}
{--
instance Transferable Node where
--    transfer :: (Lattice l) => (Node -> l -> l) -> Node -> l -> l
    transfer f node = f node
--}