{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
module MF.Languages.Closure.Old where

import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import Data.List
import Data.Char

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import Debug.Trace

import MF.CoreM
import MF.Lattice
import MF.Flowable hiding (Call)

--import qualified Data.Triple as T
fst' :: (a,b,c) -> a
fst' (a,b,c) = a
    
type Variable = String
type Function = String

data Identifier = Variable Variable
                | Function Function
                deriving (Eq, Ord, Show)

data Stmt = Expr Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Sequence Stmt Stmt
          | Skip
          deriving Show

data Expr = Assign Variable Expr
          | Call Identifier [Expr]
          | Closure [Variable] Stmt
          | Var Variable
          | Const Con
          deriving Show

data Con = Bool Bool
         | Int Int
         deriving Show


-------------------------------------------------------------------------------
-- TypeSet 
-------------------------------------------------------------------------------

type TypeVar = Integer


data TypeSet = TSet (S.Set BaseType)
             | TVar TypeVar
             | TFun TypeSet TypeSet
             deriving (Eq,Ord)

data BaseType = TInt
              | TBool
              deriving (Eq, Ord, Show)
              

instance Show TypeSet where
    show (TSet set) = "{ " ++ (concat . intersperse ", " . S.toList . S.map show $ set) ++ " }"
    show (TVar n)   = [chr (fromInteger (n) + 97)]
    show (TFun l r) = show l ++ " -> " ++ show r
    
              
infixr 4 `fn` 
fn :: TypeSet -> TypeSet -> TypeSet 
a `fn` b = TFun a b

tBool = TSet $ S.singleton TBool
tInt  = TSet $ S.singleton TInt


simpleEnv = M.fromList [(Variable "a", tBool), (Function "+", tInt `fn` tBool `fn` tInt)]

test = runInference simpleEnv expr
    where
        expr = Assign "b" (Var "a")
--        expr = Call (Function "+") [(Const (Int 1)),(Const (Bool False))]


type TI a = ErrorT String (WriterT [String] (StateT Integer Identity)) a

runInference :: Env -> Expr -> (TypeSet, Subst, Env)
runInference env expr = case result of 
                            Left m  -> error m
                            Right r -> r -- fst' r
                        where
                            result = trace (concat . intersperse "\n" . snd $ r) $ fst r
                            
                            r = fst $ runIdentity (runStateT (runWriterT (runErrorT (inferExpr env expr))) 0)



type Env = M.Map Identifier TypeSet

{-
function fac ($n)
{
    // start env: [n :: alpha, fac :: [alpha] -> beta]
    
    if ($n == 0) {
        // [n :: Int, fac :: [Int] -> beta]
        return 1;
        // [n :: Int, fac :: [Int] -> beta]
    } else {
        return $n * fac ($n - 1);
        // [n :: Int, fac :: [Int] -> Int]
    }
}
-}
{-
instance Lattice Env where
    join left right = do substs <- sequence . M.toList . M.intersectWith unify left right
                         return $ apply (foldr (@@) nullSubst substs) (M.union left right)
    (<:) left right = M.isSubmapOfBy isMoreInformative
        where
            isMoreInformative (TSet l) (TSet r) = l `S.subsetOf` r
            isMoreInformative (T)
-}    
                
                
inferExpr :: Env -> Expr -> TI (TypeSet, Subst, Env)
inferExpr env (Const (Bool _))      = do debug "Con Bool"
                                         return (tBool, nullSubst, env)
                                    
inferExpr env (Const (Int _))       = do debug "Con Int"
                                         return (tInt,  nullSubst, env)
                                    
inferExpr env (Var name)            = do debug "Var"
                                         envLookup (Variable name) env
                                                                          
inferExpr env (Assign name expr)    = do debug "Assign"
                                         (t, s, env')  <- inferExpr env expr
                                         return $ (t, s, M.insert (Variable name) t env')
                                    
inferExpr env (Call ident params)   = do debug "Call"                                   
                                         (t0, s0, env')  <- envLookup ident env
                                         (t1, s1, env'') <- foldM merge ([], nullSubst, env') params
                                         a <- fresh
                                         let t2 = foldr fn a (reverse t1)
                                         s2 <- unify t0 t2
                                         return $ (apply s2 a, s2 @@ s1 @@ s0, env'')
                                         
                               
                                         
                                          
{-                                    
inferExpr env (Closure params stmt) = do debug "Closure"
                                         t <- inferStmt stmt
                                         return $ (t, id, env) 
-}                                         
                                         
                                         
fresh :: (MonadState Integer m) => m TypeSet
fresh = do n <- get 
           put (n + 1)
           return $ TVar n
              
                                         
merge :: ([TypeSet], Subst, Env) -> Expr -> TI ([TypeSet], Subst, Env)                                         
merge (ts, s, env) expr = do (t, s', env') <- inferExpr env expr
                             return $ (t : ts, s' @@ s, env')                                         
                                         
                                         
envLookup :: Identifier -> Env -> TI (TypeSet, Subst, Env)                                         
envLookup ident env = case M.lookup ident env of 
                          Nothing -> do a <- fresh
                                        return (a, nullSubst, M.insert ident a env)
                          Just t' -> do return (t', nullSubst, env)
                          
                          -- t <- instantiate t'
                            -- return (t, nullSubst)
                          
debug :: (MonadWriter [String] m) => String -> m ()
debug m = do tell [m]

unify :: (MonadWriter [String] m, MonadError String m) => TypeSet -> TypeSet -> m Subst 
unify l r = do debug ("Unifying: " ++ show l ++ " and " ++ show r) 
               unify' l r

unify' :: (MonadWriter [String] m, MonadError String m) => TypeSet -> TypeSet -> m Subst 
unify' (TFun l r) (TFun l' r') = do s1 <- unify l l' 
                                    s2 <- unify (apply s1 r) (apply s1 r') 
                                    return $ s2 @@ s1
unify' (TVar u) t            = varBind u t 
unify' t (TVar u)            = varBind u t 
unify' _ _                   = return nullSubst -- Add coercion checking here



varBind :: (MonadError String m) => TypeVar -> TypeSet -> m Subst
varBind u t | t == TVar u      = return nullSubst 
            | u `elem` tv t    = throwError "occurs check fails"
            | otherwise        = return $ u +-> t
          
          
type Subst = [(TypeVar, TypeSet)] 

nullSubst :: Subst 
nullSubst = []

(+->) :: TypeVar -> TypeSet -> Subst 
u +-> t = [(u, t)]

class Types t where 
    apply :: Subst -> t -> t 
    tv    :: t -> [TypeVar]

instance Types TypeSet where 
    apply s (TVar u)  = case lookup u s of 
                            Just t  -> t
                            Nothing -> TVar u  
    apply s t         = t 

    tv (TVar u)  = [u] 
    tv _         = []

instance Types a => Types [a] where 
    apply s = map (apply s) 
    tv      = nub . concat . map tv

infixr 4 @@ 
(@@) :: Subst -> Subst -> Subst 
s1 @@s2 = [(u , apply s1 t) | (u , t) <- s2] ++ s1


{-
    
inferStmt :: Env -> Stmt -> m (ValueMap Env)
inferStmt env = solve transfer env M.empty Forward  
    where
        transfer Skip                 env = do debug "Skip"
                                               return env
                                               
        transfer (If cond left right) env = do debug "If"
                                               (t1, s1, env') <- inferExpr env cond
                                               s2 <- unify tBool t1
                                               return $ apply (s2 @@ s1) env' 
                                               
        transfer (While cond stmt)    env = do debug "While"
                                               (t1, s1, env') <- inferExpr env cond
                                               s2 <- unify tBool t1
                                               return $ apply (s2 @@ s1) env'
                                               
        transfer (Expr expr)          env = do debug "Expr"
                                               (s, t, env') <- inferExpr env expr
                                               return $ apply s env'
                                               



instance Flowable Stmt where
    init   = 1
    final  = 2
    blocks = IM.fromList [(1, Expr (Var "a"))
                         ,(2, Skip)]
    flow   = [(1,2)]
    labels = [1,2]
        
-}