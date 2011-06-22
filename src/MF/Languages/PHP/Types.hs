module MF.Languages.PHP.Types where

import Data.Set as S
import Data.Map as M
import qualified Debug.Trace as T

import MF.Flowable   
import MF.Core    

-- trace = T.trace
trace _ = id

-------------------------------------------------------------------------------
-- TypeSet 
-------------------------------------------------------------------------------

type TypeSet = Set BaseType

data BaseType = TyInt
              | TyFloat
              | TyBool
              | TyString
              | TyResource
              | TyObject
              | TyAny
              | TyArray BaseType
              deriving (Eq, Ord, Show, Read)                            

class Type t where
    toArray   :: t -> t
    fromArray :: t -> t
    depth     :: t -> Int

instance Type BaseType where
    toArray                = TyArray
    fromArray (TyArray t)  = t
    fromArray _            = error "Expecting TyArray"    
    depth (TyArray t)      = 1 + depth t
    depth _                = 0

instance (Ord t, Type t) => Type (Set t) where
    toArray                = S.map toArray
    fromArray              = S.map fromArray
    depth set | S.null set = 0
              | otherwise  = S.findMax . S.map depth $ set


fromArrayRepeatedly :: (Type t) => Int -> t -> t
fromArrayRepeatedly n t = foldr ($) t $ take n $ repeat fromArray

toArrayRepeatedly :: (Type t) => Int -> t -> t
toArrayRepeatedly n t = foldr ($) t $ take n $ repeat toArray


instance Lattice TypeSet where
    -- This is different than the original widening functions. 
    -- 1. We take the maximum value when we extend the depth function to sets
    -- 2. We always return a array type of depth k when the depth of l `union` r exceeds k. 
    join l r | depth' < k = trace ("Joining")  $ l `S.union` r
             | otherwise  = trace ("Failback") $ S.singleton $ toArrayRepeatedly k TyAny
                          where
                              depth' = depth $ l `S.union` r
                              k      = 3
     
    (<:) = S.isSubsetOf




-------------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------------

data Constraint = Label :<=: Label          -- Dependency
                | Label :==: TypeSet
                deriving (Eq, Ord, Show)



isEquality :: Constraint -> Bool
isEquality (l :==: t) = True
isEquality _          = False

hasLabel :: Label -> Constraint -> Bool
hasLabel label (l :==: t) = l == label
hasLabel label _          = False


resolve :: Set Constraint -> Set Constraint
resolve constraints = trace (show constraints) $ S.fold resolve' S.empty constraints
    where
        resolve' c@(l1 :<=: l2) r = S.insert c r `S.union` S.fold update S.empty constraints
                                  where
                                      update (l2' :==: t) r | l2 == l2' = S.insert (l1 :==: t) r
                                                            | otherwise = r
                                      update c                        r = r
        resolve' c            r = S.insert c r
        
fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f a | a == na   = a
             | otherwise = fixPoint f na
                         where na = f a
                         
resolveType :: Set Constraint -> Label -> TypeSet
resolveType constraints label = trace ("resolving type for: " ++ show label ++ " with " ++ show constraints) $ S.fold join S.empty . S.map toType . S.filter (hasLabel label) . fixPoint resolve $ constraints
    where        
        toType (l :==: t) = t

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

data Identifier = Identifier String
                | Parameter Int
                | ReturnValue
                deriving (Ord,Eq,Show)

type Mapping = Identifier :-> TypeSet
    
updateMapping :: Identifier -> Label -> Int -> Set Constraint -> Mapping -> Mapping
updateMapping identifier label depth constraints mapping = trace ("Processing " ++ (show identifier) ++ "(" ++ show label ++ ") with" ++ show constraints ++ " resulting in: " ++ show effect) $ M.insert identifier effect mapping
    where        
        -- We add to what we already know
        context = case M.lookup identifier mapping of
                      Just t  -> t
                      Nothing -> S.empty
       
        -- We resolve the constraints and join the result with our context value
        effect =  S.fold join context . types . (fixPoint resolve) $ constraints
    
        -- Filter equality constraints and lift there type in case of any arrays
        types = S.map (\(l :==: t) -> toArrayRepeatedly depth t) . S.filter isApplicable
    
    
        isApplicable (l :==: t) = l == label
        isApplicable _          = False
                        