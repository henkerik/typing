{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module MF.Flowable where

import qualified Data.IntMap as IM
import Prelude hiding (init)
import Data.List hiding (init)

type Label = Int
type Flow = [(Label, Label)]

class Flowable node where
    init     :: node -> Label
    final    :: node -> [Label]
    blocks   :: node -> IM.IntMap ((Block node -> a) -> a)
    flow     :: node -> Flow
    nodes    :: node -> IM.IntMap node
    labels   :: node -> [Label]
    labels p = (sort . nub) (init p : (concat . map (\(l,l') -> [l, l']) . flow $ p))

--data Continuation node = forall a. C ((Block node -> a) -> a)

--    blocks :: Lattice l => Labelled r ix -> IM.IntMap ((Block r -> l -> l) -> l -> l)

--data Block stmt = forall ix. B Type (Labelled stmt ix)

--type Block node = (Type, node)

-- data Block node = forall l. Lattice l => B { run :: (Labelled node -> l -> l) -> l -> l }

data Block node = Normal node
                | Exit node
                | Entry node
                | Call Label Label node
                | Return Label Label node
                deriving Show
                
toNode :: Block node -> node
toNode (Normal n)       = n
toNode (Exit n)         = n
toNode (Entry n)        = n
toNode (Call lc lr n)   = n
toNode (Return lc lr n) = n


class Lattice a where
    join   :: a -> a -> a
    (<:)   :: a -> a -> Bool