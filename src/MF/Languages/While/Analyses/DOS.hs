{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module MF.Languages.While.Analyses.DOS where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Prelude hiding (min)

import MF.Core as C
import MF.Languages.While.Util
import MF.Languages.While.AST hiding (Identifier)
import qualified MF.Languages.While.AST as AST
import MF.Languages.While.Flow as Flow
import MF.Flowable

import Debug.Trace

data Sign = Negative 
          | Zero 
          | Positive
          deriving (Ord, Eq, Show)


--type Mapping = Identifier :-> S.Set Sign

data Identifier = Identifier AST.Identifier
                | Parameter Int
                deriving (Ord,Eq,Show)

type Mapping = M.Map Identifier (S.Set Sign)

solve :: Labelled Node phatom -> ValueMap (Stack :-> (M.Map Identifier (S.Set Sign)))
solve = C.solve (lift $ transfer . simplify) (M.singleton [] M.empty) M.empty Forward
    where
        transfer (Flow.Skip)         mapping = mapping
        transfer (Expression exp)    mapping = mapping
        transfer (Flow.Call is os)   mapping = M.mapKeys (\(Identifier x) -> Parameter  $ fromJust (elemIndex x is)) . M.filterWithKey (\(Identifier x) s -> x `elem` is) $ mapping
        transfer (Flow.Entry is os)  mapping = M.mapKeys (\(Parameter p)  -> Identifier $ is !! p) $ mapping
        transfer (Flow.Exit is os)   mapping = M.mapKeys (\(Identifier x) -> Parameter  $ fromJust (elemIndex x os)) . M.filterWithKey (\(Identifier x) s -> x `elem` os) $ mapping
        transfer (Flow.Return is os) mapping = M.mapKeys (\(Parameter p)  -> Identifier $ os !! p) $ mapping
        transfer (Assignment x a)    mapping = M.insert (Identifier x) (S.fromList $ signs a mapping) mapping
                                             where 
                                               signs :: Expression -> Mapping -> [Sign]
                                               signs (Times left right) mapping = undefined
                                               signs (Plus left right)  mapping = nub $ concat [plus l r | l <- signs left mapping , r <- signs right mapping]
                                                                                where
                                                                                    plus Positive Positive = [Positive]
                                                                                    plus Positive Zero     = [Positive]
                                                                                    plus Positive Negative = [Negative, Zero, Positive]
                                                                                    
                                                                                    plus Zero     Positive = [Positive]
                                                                                    plus Zero     Zero     = [Zero]
                                                                                    plus Zero     Negative = [Negative]
                                                                                    
                                                                                    plus Negative Positive = [Negative, Zero, Positive]
                                                                                    plus Negative Zero     = [Negative]
                                                                                    plus Negative Negative = [Negative]
                                                                                    
                                               signs (Min left right)   mapping = nub $ concat [min l r | l <- signs left mapping , r <- signs right mapping]
                                                                                where
                                                                                    min Positive Positive = [Negative, Zero, Positive]
                                                                                    min Positive Zero     = [Positive]
                                                                                    min Positive Negative = [Positive]
                                                                                    
                                                                                    min Zero     Positive = [Negative]
                                                                                    min Zero     Zero     = [Zero]
                                                                                    min Zero     Negative = [Positive]
                                                                                    
                                                                                    min Negative Positive = [Negative]
                                                                                    min Negative Zero     = [Negative]
                                                                                    min Negative Negative = [Negative, Zero, Positive]
                                                                                    
                                               signs (Var name)         mapping = case M.lookup (Identifier name) mapping of 
                                                                                     Just s  -> S.toList s
                                                                                     Nothing -> [] -- error $ show mapping  -- [Negative, Zero, Positive]
                                               signs (Const value)      mapping = case compare value 0 of
                                                                                     LT      -> [Negative]
                                                                                     EQ      -> [Zero]
                                                                                     GT      -> [Positive]
                                   
-- Keep a sign for every identifier using a context
{--
instance Context Identifier where
    lift transfer @stmt(Assignment x a) = M.insert x (transfer stmt)
    lift transfer @stmt(_)              = M.adjust (transfer stmt)
--} 
   
{--
instance Lattice Mapping where
    join = M.unionWith join
    (<:) = M.isSubmapOfBy (<:)    
--}

instance Lattice (S.Set Sign) where
    join     = S.union
    (<:)     = S.isSubsetOf
    