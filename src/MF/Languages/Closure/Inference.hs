{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module MF.Languages.Closure.Inference where


import MF.Languages.Closure.AG hiding (sequence)

import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import Data.List as L
import Data.Char

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import Debug.Trace

import MF.CoreM
import MF.Lattice
import MF.Flowable hiding (Call, Return)

--import qualified Data.Triple as T
fst' :: (a,b,c) -> a
fst' (a,b,c) = a



-------------------------------------------------------------------------------
-- TypeSet 
-------------------------------------------------------------------------------

type TypeVar = Integer


data TypeSet = TSet (S.Set BaseType)
             | TVar TypeVar
             | TFun TypeSet TypeSet -- We include the function type here, because we know up front that a function is of a function with a specific number of parameters
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




test = runInference simpleEnv expr
    where
        expr = Assign "b" (Var "a")
--        expr = Call (Function "+") [(Const (Int 1)),(Const (Bool False))]


type TI = ErrorT String (WriterT [String] (StateT Integer Identity))

runInference :: Env -> Expr -> (TypeSet, Subst, Env)
runInference env expr = case result of 
                            Left m  -> error m
                            Right r -> r -- fst' r
                        where
                            result = trace (concat . intersperse "\n" . snd $ r) $ fst r
                            
                            r = fst $ runIdentity (runStateT (runWriterT (runErrorT (inferExpr env expr))) 0)



type Env = M.Map Identifier TypeSet



f :: Monad m => a -> m b -> m (a, b)
f a b = do b' <- b
           return (a, b')

instance Lattice Env TI where
    -- We moeten zowel de types samen voegen als substituties uitvoeren die hierbij zijn gevonden op de gehele environment
    join left right = do overlap <- sequence . L.map (uncurry f) . M.toList . M.intersectionWith unify left $ right
                         let substitutions  = foldr (@@) nullSubst . L.map (snd . snd) $ overlap
                         let intersected    = M.map fst . M.fromList $ overlap
                         let remnant        = (left `M.difference` right) `M.union` (right `M.difference` left)
                         let environment    = intersected `M.union` (apply substitutions remnant)
                         return environment
                         
    (<:) = M.isSubmapOfBy isMoreInformative
        where
            isMoreInformative (TSet l)    (TSet r)    = l `S.isSubsetOf` r
            isMoreInformative (TVar n)    _           = True
            isMoreInformative _           (TVar n)    = False
            isMoreInformative (TFun l l') (TFun r r') = isMoreInformative l r || isMoreInformative l' r'


                
                
inferExpr :: Env -> Expr -> TI (TypeSet, Subst, Env)
inferExpr env (LTrue)               = do debug "LTrue"
                                         return (tBool, nullSubst, env)
                                         
inferExpr env (LFalse)              = do debug "LFalse"
                                         return (tBool, nullSubst, env)                                         
                                         
inferExpr env (Integer v)           = do debug "Integer"
                                         return (tInt,  nullSubst, env)
                                  
inferExpr env (Var name)            = do debug "Var"
                                         envLookup (Variable name) env
                                                                          
inferExpr env (Assign name expr)    = do debug "Assign"
                                         (t, s, env')  <- inferExpr env expr
                                         return $ (t, s, M.insert (Variable name) t (apply s env'))
                                    
inferExpr env (Call ident params)   = do debug "Call"                                   
                                         (t0, s0, env')  <- envLookup ident env
                                         (t1, s1, env'') <- foldM merge ([], nullSubst, env') params
                                         a <- fresh
                                         let t2 = foldr fn a (reverse t1)
                                         (t3, s2) <- unify t0 t2
                                         let s = s2 @@ s1 @@ s0
                                         return $ (apply s2 a, s, apply s env'')
                                         
                               
                                         
                                                                             
                                         
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

unify :: (MonadWriter [String] m, MonadError String m) => TypeSet -> TypeSet -> m (TypeSet, Subst)
unify l r = do debug ("Unifying: " ++ show l ++ " and " ++ show r) 
               unify' l r

unify' :: (MonadWriter [String] m, MonadError String m) => TypeSet -> TypeSet -> m (TypeSet, Subst)
unify' (TFun l r) (TFun l' r') = do (t1, s1) <- unify l l' 
                                    (t2, s2) <- unify (apply s1 r) (apply s1 r') 
                                    return (t1 `fn` t2, s2 @@ s1)
unify' (TVar u) t              = do s <- varBind u t 
                                    return (t, s)
unify' t (TVar u)              = do s <- varBind u t
                                    return (t, s)
                                    
unify' (TSet l) (TSet r) | l == r    = do return (TSet $ S.union l r, nullSubst)

unify' (TSet l) (TSet r) | otherwise = do debug (":: Coercion :: " ++ show l ++ " with " ++ show r)
                                          return (TSet $ S.union l r, nullSubst)
                                    
unify' _ _                     = error $ "unknown" --return nullSubst -- Add coercion checking here



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
    apply s (TVar u)   = case lookup u s of 
                            Just t  -> t
                            Nothing -> TVar u
    apply s (TFun l r) = TFun (apply s l) (apply s r)   
    apply s t          = t 

    tv (TVar u)  = [u] 
    tv _         = []

instance Types a => Types [a] where 
    apply s = map (apply s) 
    tv      = nub . concat . map tv
    
instance Types a => Types (M.Map Identifier a) where
    apply s = M.map (apply s)
    tv      = nub . concat . M.elems . M.map tv

infixr 4 @@ 
(@@) :: Subst -> Subst -> Subst 
s1 @@s2 = [(u , apply s1 t) | (u , t) <- s2] ++ s1




    
inferStmt :: TypeSet -> Env -> Stmt -> TI (ValueMap Env)
inferStmt (TVar var) env = solve (transfer . toNode) env M.empty Forward  
    where
        transfer Skip                 env = do debug "Skip"
                                               return env
                                               
        transfer (If cond left right) env = do debug "If"
                                               (t1, s1, env') <- inferExpr env cond
                                               (t2, s2) <- unify tBool t1       -- check for coercions
                                               return $ apply (s2 @@ s1) env' 
                                               
        transfer (While cond stmt)    env = do debug "While"
                                               (t1, s1, env') <- inferExpr env cond
                                               (t2, s2) <- unify tBool t1       -- check for coercions
                                               return $ apply (s2 @@ s1) env'
                                               
        transfer (Expr expr)          env = do debug "Expr"
                                               (t, s, env') <- inferExpr env expr
                                               return $ apply s env'
                                               
        transfer (Return expr)        env = do debug "Return"
                                               (t, s, env') <- inferExpr env expr
                                               let s' = (var +-> t) @@ s
                                               return $ apply s' env'
                                               
        transfer stmt                 env = error $ "Failed to transfer: " ++ show stmt


defaultEnv :: TI Env
defaultEnv = do a1 <- fresh
                a2 <- fresh
                return $ M.fromList [(Function "+",  tInt `fn` tInt `fn` tInt) 
                                    ,(Function "-",  tInt `fn` tInt `fn` tInt) 
                                    ,(Function "%",  tInt `fn` tInt `fn` tInt)
                                    ,(Function "*",  tInt `fn` tInt `fn` tInt) 
                                    ,(Function "==", a1 `fn` a1 `fn` tBool)
                                    ,(Function "id", a2 `fn` a2)
                                    ]

runStmtInference :: Env -> Stmt -> ValueMap Env
runStmtInference env stmt = case result of 
                            Left m  -> error m
                            Right r -> r -- fst' r
                        where
                            result = trace (concat . intersperse "\n" . snd $ r) $ fst r

                            r = fst $ runIdentity (runStateT (runWriterT (runErrorT (inferStmt undefined env stmt))) 0)

    
simpleEnv = M.empty
    
inference = runStmtInference simpleEnv 



inferDeclaration :: Stmt -> TI (ValueMap Env)
inferDeclaration (Declaration name params stmt) = do env  <- defaultEnv
                                                     vars <- sequence $ map (const fresh) params  
                                                     a    <- fresh
                                                     let env'  = M.fromList $ zip (map Variable params) vars
                                                     let env'' = M.singleton (Function name) (foldr fn a vars)
                                                     inferStmt a (env `M.union` env' `M.union` env'') stmt
                                                     
runInferDeclaration decl = case result of 
                               Left m  -> error m
                               Right r -> r -- fst' r
                           where
                               result = trace (concat . intersperse "\n" . snd $ r) $ fst r
                          
                               r = fst $ runIdentity (runStateT (runWriterT (runErrorT (inferDeclaration decl))) 0)                                                               
