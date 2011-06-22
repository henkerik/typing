{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
module MF.Languages.While.AST where

import qualified Data.IntMap as IM
import Data.Set

import MF.Flowable

type Identifier = String

data Program
data Declaration
data Statement

data Node r ix where
    -- Program
    Program    :: r Declaration -> r Statement                                  -> Node r Program
    
    -- Declarations                                             
    Procedure  :: Identifier    -> [Identifier]  -> [Identifier] -> r Statement -> Node r Declaration
    DSequence  :: r Declaration -> r Declaration                                -> Node r Declaration
                                                                                
    -- Statements                                                               
    Skip       ::                                                                  Node r Statement
    Assign     :: Identifier    -> Expression                                   -> Node r Statement
    While      :: Expression    -> r Statement                                  -> Node r Statement
    If         :: Expression    -> r Statement   -> r Statement                 -> Node r Statement
    Sequence   :: r Statement   -> r Statement                                  -> Node r Statement
    Call       :: Identifier    -> [Identifier]  -> [Identifier]                -> Node r Statement



data Expression = Plus Expression Expression
                | Min Expression Expression
                | Times Expression Expression
                | Greater Expression Expression
                | Var Identifier
                | Const Integer
                deriving (Ord, Eq, Show)
                
      
















                
                
                
                


class Expressions a where
    expressions :: a -> [Expression]

{--instance Expressions a => Expressions (Program a) where
    expressions = IM.fold (++) [] . IM.map expressions . blocks
    
    
    
instance Expressions (Labelled Statement) where
    expressions (L _ stmt) = expressions stmt

instance Expressions Expression where
    expressions e@(Plus l r)    = [e] ++ expressions l ++ expressions r
    expressions e@(Times l r)   = [e] ++ expressions l ++ expressions r
    expressions e@(Greater l r) = expressions l ++ expressions r
    expressions _               = []

instance Expressions a => Expressions (Statement a) where
    expressions (Assign c exp) = expressions exp
    expressions (Sequence f s) = expressions f ++ expressions s
    expressions (While c s)    = expressions c ++ expressions s
    expressions (Skip)         = []



instance Show Expression where
    show (Plus l r)        = show l ++ " + " ++ show r
    show (Times l r)       = show l ++ " * " ++ show r
    show (Greater l r)     = show l ++ " > " ++ show r
    show (Var name)        = name
    show (Const n)         = show n
    
    --}
    
    
    {--
    type Identifier = String

    data Statement =
          Sequence Statement Statement
        | Assign   String Expression
        | Expr Expression

        | If Expression Statement Statement
        | While Expression
        | Break
        | Continue
        | Skip

    data Expression =
          Plus Expression Expression
        | Times Expression Expression
        | GreaterThan Expression Expression
        | Var Identifier
        | Const Integer
        deriving (Eq , Ord)
    --}