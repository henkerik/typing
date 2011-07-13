

-- UUAGC 0.9.36 (src/MF/Languages/Closure/AG.ag)
module MF.Languages.Closure.AG where

{-# LINE 1 "src/MF/Languages/Closure/AG/Base.ag" #-}

import           CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import           CCO.Tree.Parser      (parseTree, app, arg, list, tuple)
import           Control.Applicative  (Applicative ((<*>)), (<$>), pure)
import           Prelude              hiding (sequence, init)
import qualified Data.List as L
{-# LINE 14 "src/MF/Languages/Closure/AG.hs" #-}

{-# LINE 2 "src/MF/Languages/Closure/AG/Flow.ag" #-}

import qualified MF.Flowable as F
import MF.Flowable hiding (Return, Call)
import Data.IntMap as IM
import Data.Map as M
import Data.Maybe
import Control.Applicative hiding (Const)
{-# LINE 24 "src/MF/Languages/Closure/AG.hs" #-}
{-# LINE 6 "src/MF/Languages/Closure/AG.ag" #-}

execute attributes p = wrap_Stmt (sem_Stmt p) attributes
    
defaultAttributes = Inh_Stmt { labels_Inh_Stmt = 0 }
{-# LINE 30 "src/MF/Languages/Closure/AG.hs" #-}

{-# LINE 9 "src/MF/Languages/Closure/AG/Base.ag" #-}

type Variable = String
type Function = String
{-# LINE 36 "src/MF/Languages/Closure/AG.hs" #-}

{-# LINE 52 "src/MF/Languages/Closure/AG/Base.ag" #-}


sequence :: [Stmt] -> Stmt
sequence []     = Skip
sequence [x]    = x
sequence xs     = foldr Sequence (last xs) (L.init xs)

buildProgram                 stmts = sequence stmts
buildWhile cond              stmts = While cond (sequence stmts)
buildIf    cond left right         = If cond (sequence left) (sequence right)
buildDeclaration name params stmts = Declaration name (L.map tail params) (sequence stmts)
buildInteger value                 = Integer (read value)

instance Tree Stmt where
    fromTree = undefined
    toTree = parseTree [ app "Expr"        (Expr <$> arg)
                       , app "If"          (buildIf <$> arg <*> arg <*> arg)
                       , app "While"       (buildWhile <$> arg <*> arg)
                       , app "Declaration" (buildDeclaration <$> arg <*> arg <*> arg)
                       , app "Return"      (Return <$> arg)
                       , app "Program"     (buildProgram <$> arg)
                       ]


instance Tree Expr where
    fromTree = undefined    
    toTree = parseTree [ app "Assign"      (Assign <$> arg <*> arg)
                       , app "Plus"        (buildPlus  <$> arg <*> arg)
                       , app "Minus"       (buildMinus <$> arg <*> arg)
                       , app "Equal"       (buildEqual <$> arg <*> arg)
                       , app "Mod"         (buildMod   <$> arg <*> arg)
                       , app "Times"       (buildTimes <$> arg <*> arg)
                       , app "Var"         (Var <$> arg)
                       , app "Integer"     (buildInteger <$> arg)
                       , app "True"        (pure LTrue)
                       , app "False"       (pure LFalse)
                       , app "Call"        (buildCall <$> arg <*> arg)
                       ]
                       
buildPlus  a b = Call (Function "+")  [a,b]
buildMinus a b = Call (Function "-")  [a,b]
buildEqual a b = Call (Function "==") [a,b]
buildMod   a b = Call (Function "%")  [a,b]
buildTimes a b = Call (Function "*")  [a,b]

buildCall name params = Call (Function name) params

{-# LINE 86 "src/MF/Languages/Closure/AG.hs" #-}

{-# LINE 103 "src/MF/Languages/Closure/AG/Flow.ag" #-}


nextUnique :: Int -> (Int, Int)
nextUnique u = (u+1, u)

declarations = declarations_Syn_Stmt . execute defaultAttributes


instance Flowable Stmt where
    init   p  = case init_Syn_Stmt . execute defaultAttributes $ p of
                    Just l  -> l
                    Nothing -> -1
    final   p = case final_Syn_Stmt . execute defaultAttributes $ p of
                    Just l -> l
                    Nothing -> []
    flow     = flow_Syn_Stmt . execute defaultAttributes
    blocks   = blocks_Syn_Stmt . execute defaultAttributes

{-# LINE 107 "src/MF/Languages/Closure/AG.hs" #-}
-- Con ---------------------------------------------------------
data Con  = Bool (Bool) 
          | Int (Int) 
          deriving ( Show)
-- cata
sem_Con :: Con  ->
           T_Con 
sem_Con (Bool _v )  =
    (sem_Con_Bool _v )
sem_Con (Int _v )  =
    (sem_Con_Int _v )
-- semantic domain
type T_Con  = ( Con )
data Inh_Con  = Inh_Con {}
data Syn_Con  = Syn_Con {self_Syn_Con :: Con }
wrap_Con :: T_Con  ->
            Inh_Con  ->
            Syn_Con 
wrap_Con sem (Inh_Con )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Con _lhsOself ))
sem_Con_Bool :: Bool ->
                T_Con 
sem_Con_Bool v_  =
    (let _lhsOself :: Con 
         _self =
             Bool v_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Con_Int :: Int ->
               T_Con 
sem_Con_Int v_  =
    (let _lhsOself :: Con 
         _self =
             Int v_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Expr --------------------------------------------------------
data Expr  = Assign (Variable) (Expr ) 
           | Call (Identifier ) (ExprList ) 
           | Integer (Int) 
           | LFalse 
           | LTrue 
           | Var (Variable) 
           deriving ( Show)
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Assign _n _e )  =
    (sem_Expr_Assign _n (sem_Expr _e ) )
sem_Expr (Call _n _params )  =
    (sem_Expr_Call (sem_Identifier _n ) (sem_ExprList _params ) )
sem_Expr (Integer _v )  =
    (sem_Expr_Integer _v )
sem_Expr (LFalse )  =
    (sem_Expr_LFalse )
sem_Expr (LTrue )  =
    (sem_Expr_LTrue )
sem_Expr (Var _n )  =
    (sem_Expr_Var _n )
-- semantic domain
type T_Expr  = ( Expr )
data Inh_Expr  = Inh_Expr {}
data Syn_Expr  = Syn_Expr {self_Syn_Expr :: Expr }
wrap_Expr :: T_Expr  ->
             Inh_Expr  ->
             Syn_Expr 
wrap_Expr sem (Inh_Expr )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Expr _lhsOself ))
sem_Expr_Assign :: Variable ->
                   T_Expr  ->
                   T_Expr 
sem_Expr_Assign n_ e_  =
    (let _lhsOself :: Expr 
         _eIself :: Expr 
         _self =
             Assign n_ _eIself
         _lhsOself =
             _self
         ( _eIself) =
             e_ 
     in  ( _lhsOself))
sem_Expr_Call :: T_Identifier  ->
                 T_ExprList  ->
                 T_Expr 
sem_Expr_Call n_ params_  =
    (let _lhsOself :: Expr 
         _nIself :: Identifier 
         _paramsIself :: ExprList 
         _self =
             Call _nIself _paramsIself
         _lhsOself =
             _self
         ( _nIself) =
             n_ 
         ( _paramsIself) =
             params_ 
     in  ( _lhsOself))
sem_Expr_Integer :: Int ->
                    T_Expr 
sem_Expr_Integer v_  =
    (let _lhsOself :: Expr 
         _self =
             Integer v_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_LFalse :: T_Expr 
sem_Expr_LFalse  =
    (let _lhsOself :: Expr 
         _self =
             LFalse
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_LTrue :: T_Expr 
sem_Expr_LTrue  =
    (let _lhsOself :: Expr 
         _self =
             LTrue
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expr_Var :: Variable ->
                T_Expr 
sem_Expr_Var n_  =
    (let _lhsOself :: Expr 
         _self =
             Var n_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ExprList ----------------------------------------------------
type ExprList  = [Expr ]
-- cata
sem_ExprList :: ExprList  ->
                T_ExprList 
sem_ExprList list  =
    (Prelude.foldr sem_ExprList_Cons sem_ExprList_Nil (Prelude.map sem_Expr list) )
-- semantic domain
type T_ExprList  = ( ExprList )
data Inh_ExprList  = Inh_ExprList {}
data Syn_ExprList  = Syn_ExprList {self_Syn_ExprList :: ExprList }
wrap_ExprList :: T_ExprList  ->
                 Inh_ExprList  ->
                 Syn_ExprList 
wrap_ExprList sem (Inh_ExprList )  =
    (let ( _lhsOself) = sem 
     in  (Syn_ExprList _lhsOself ))
sem_ExprList_Cons :: T_Expr  ->
                     T_ExprList  ->
                     T_ExprList 
sem_ExprList_Cons hd_ tl_  =
    (let _lhsOself :: ExprList 
         _hdIself :: Expr 
         _tlIself :: ExprList 
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_ExprList_Nil :: T_ExprList 
sem_ExprList_Nil  =
    (let _lhsOself :: ExprList 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Identifier --------------------------------------------------
data Identifier  = Function (Function) 
                 | Variable (Variable) 
                 deriving ( Eq,Ord,Show)
-- cata
sem_Identifier :: Identifier  ->
                  T_Identifier 
sem_Identifier (Function _n )  =
    (sem_Identifier_Function _n )
sem_Identifier (Variable _n )  =
    (sem_Identifier_Variable _n )
-- semantic domain
type T_Identifier  = ( Identifier )
data Inh_Identifier  = Inh_Identifier {}
data Syn_Identifier  = Syn_Identifier {self_Syn_Identifier :: Identifier }
wrap_Identifier :: T_Identifier  ->
                   Inh_Identifier  ->
                   Syn_Identifier 
wrap_Identifier sem (Inh_Identifier )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Identifier _lhsOself ))
sem_Identifier_Function :: Function ->
                           T_Identifier 
sem_Identifier_Function n_  =
    (let _lhsOself :: Identifier 
         _self =
             Function n_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Identifier_Variable :: Variable ->
                           T_Identifier 
sem_Identifier_Variable n_  =
    (let _lhsOself :: Identifier 
         _self =
             Variable n_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Program -----------------------------------------------------
data Program  = Program (Stmt ) 
-- cata
sem_Program :: Program  ->
               T_Program 
sem_Program (Program _s )  =
    (sem_Program_Program (sem_Stmt _s ) )
-- semantic domain
type T_Program  = ( Program )
data Inh_Program  = Inh_Program {}
data Syn_Program  = Syn_Program {self_Syn_Program :: Program }
wrap_Program :: T_Program  ->
                Inh_Program  ->
                Syn_Program 
wrap_Program sem (Inh_Program )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Program _lhsOself ))
sem_Program_Program :: T_Stmt  ->
                       T_Program 
sem_Program_Program s_  =
    (let _lhsOself :: Program 
         _sOlabels :: Label
         _sIblocks :: (IntMap (Block Stmt))
         _sIdeclarations :: (Map String Stmt)
         _sIfinal :: (Maybe [Label])
         _sIflow :: Flow
         _sIinit :: (Maybe Label)
         _sIlabel :: Label
         _sIlabels :: Label
         _sIself :: Stmt 
         _self =
             Program _sIself
         _lhsOself =
             _self
         _sOlabels =
             ({-# LINE 16 "src/MF/Languages/Closure/AG/Flow.ag" #-}
              error "missing rule: Program.Program.s.labels"
              {-# LINE 360 "src/MF/Languages/Closure/AG.hs" #-}
              )
         ( _sIblocks,_sIdeclarations,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sIself) =
             s_ _sOlabels 
     in  ( _lhsOself))
-- Stmt --------------------------------------------------------
data Stmt  = Declaration (Function) (([Variable])) (Stmt ) 
           | Expr (Expr ) 
           | If (Expr ) (Stmt ) (Stmt ) 
           | Return (Expr ) 
           | Sequence (Stmt ) (Stmt ) 
           | Skip 
           | While (Expr ) (Stmt ) 
           deriving ( Show)
-- cata
sem_Stmt :: Stmt  ->
            T_Stmt 
sem_Stmt (Declaration _n _params _s )  =
    (sem_Stmt_Declaration _n _params (sem_Stmt _s ) )
sem_Stmt (Expr _e )  =
    (sem_Stmt_Expr (sem_Expr _e ) )
sem_Stmt (If _e _l _r )  =
    (sem_Stmt_If (sem_Expr _e ) (sem_Stmt _l ) (sem_Stmt _r ) )
sem_Stmt (Return _e )  =
    (sem_Stmt_Return (sem_Expr _e ) )
sem_Stmt (Sequence _l _r )  =
    (sem_Stmt_Sequence (sem_Stmt _l ) (sem_Stmt _r ) )
sem_Stmt (Skip )  =
    (sem_Stmt_Skip )
sem_Stmt (While _e _s )  =
    (sem_Stmt_While (sem_Expr _e ) (sem_Stmt _s ) )
-- semantic domain
type T_Stmt  = Label ->
               ( (IntMap (Block Stmt)),(Map String Stmt),(Maybe [Label]),Flow,(Maybe Label),Label,Label,Stmt )
data Inh_Stmt  = Inh_Stmt {labels_Inh_Stmt :: Label}
data Syn_Stmt  = Syn_Stmt {blocks_Syn_Stmt :: (IntMap (Block Stmt)),declarations_Syn_Stmt :: (Map String Stmt),final_Syn_Stmt :: (Maybe [Label]),flow_Syn_Stmt :: Flow,init_Syn_Stmt :: (Maybe Label),label_Syn_Stmt :: Label,labels_Syn_Stmt :: Label,self_Syn_Stmt :: Stmt }
wrap_Stmt :: T_Stmt  ->
             Inh_Stmt  ->
             Syn_Stmt 
wrap_Stmt sem (Inh_Stmt _lhsIlabels )  =
    (let ( _lhsOblocks,_lhsOdeclarations,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOself) = sem _lhsIlabels 
     in  (Syn_Stmt _lhsOblocks _lhsOdeclarations _lhsOfinal _lhsOflow _lhsOinit _lhsOlabel _lhsOlabels _lhsOself ))
sem_Stmt_Declaration :: Function ->
                        ([Variable]) ->
                        T_Stmt  ->
                        T_Stmt 
sem_Stmt_Declaration n_ params_ s_  =
    (\ _lhsIlabels ->
         (let _lhsOblocks :: (IntMap (Block Stmt))
              _lhsOdeclarations :: (Map String Stmt)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              __tup1 :: ((Label,Label))
              _sOlabels :: Label
              _label :: Label
              _lhsOflow :: Flow
              _lhsOself :: Stmt 
              _lhsOlabels :: Label
              _sIblocks :: (IntMap (Block Stmt))
              _sIdeclarations :: (Map String Stmt)
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIself :: Stmt 
              _lhsOblocks =
                  ({-# LINE 101 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 430 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 101 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal Skip
                   {-# LINE 435 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 88 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _declarations_augmented_syn [_declarations_augmented_f1]
                   {-# LINE 440 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _declarations_augmented_f1 =
                  ({-# LINE 88 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   M.union $ M.singleton n_ _self
                   {-# LINE 445 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 25 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _label
                   {-# LINE 450 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 40 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 455 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 58 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 460 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              __tup1 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_sOlabels,_) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 467 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 472 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 101 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _sIblocks
                   {-# LINE 477 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _declarations_augmented_syn =
                  ({-# LINE 88 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _sIdeclarations
                   {-# LINE 482 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 67 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _sIflow
                   {-# LINE 487 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _self =
                  Declaration n_ params_ _sIself
              _lhsOself =
                  _self
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 496 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              ( _sIblocks,_sIdeclarations,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sIself) =
                  s_ _sOlabels 
          in  ( _lhsOblocks,_lhsOdeclarations,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOself)))
sem_Stmt_Expr :: T_Expr  ->
                 T_Stmt 
sem_Stmt_Expr e_  =
    (\ _lhsIlabels ->
         (let _lhsOblocks :: (IntMap (Block Stmt))
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              __tup2 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOdeclarations :: (Map String Stmt)
              _lhsOflow :: Flow
              _lhsOself :: Stmt 
              _eIself :: Expr 
              _lhsOblocks =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 519 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 524 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 25 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _label
                   {-# LINE 529 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 36 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 534 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 52 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 539 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              __tup2 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lhsOlabels,_) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 546 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 551 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 556 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 83 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 561 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 67 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   []
                   {-# LINE 566 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _self =
                  Expr _eIself
              _lhsOself =
                  _self
              ( _eIself) =
                  e_ 
          in  ( _lhsOblocks,_lhsOdeclarations,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOself)))
sem_Stmt_If :: T_Expr  ->
               T_Stmt  ->
               T_Stmt  ->
               T_Stmt 
sem_Stmt_If e_ l_ r_  =
    (\ _lhsIlabels ->
         (let _lhsOblocks :: (IntMap (Block Stmt))
              _lhsOflow :: Flow
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              __tup3 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOdeclarations :: (Map String Stmt)
              _lhsOself :: Stmt 
              _lhsOlabels :: Label
              _rOlabels :: Label
              _eIself :: Expr 
              _lIblocks :: (IntMap (Block Stmt))
              _lIdeclarations :: (Map String Stmt)
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIself :: Stmt 
              _rIblocks :: (IntMap (Block Stmt))
              _rIdeclarations :: (Map String Stmt)
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIself :: Stmt 
              _lhsOblocks =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 613 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 618 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 73 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 623 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 73 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   (++) $ [(_label, fromJust _lIinit), (_label, fromJust _rIinit)]
                   {-# LINE 628 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 25 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _label
                   {-# LINE 633 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 36 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 638 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 56 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   pure (++) <*> _lIfinal <*> _rIfinal
                   {-# LINE 643 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              __tup3 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lOlabels,_) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 650 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 655 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 660 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 83 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 665 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 73 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 670 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _self =
                  If _eIself _lIself _rIself
              _lhsOself =
                  _self
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 679 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 16 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 684 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              ( _eIself) =
                  e_ 
              ( _lIblocks,_lIdeclarations,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIself) =
                  l_ _lOlabels 
              ( _rIblocks,_rIdeclarations,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIself) =
                  r_ _rOlabels 
          in  ( _lhsOblocks,_lhsOdeclarations,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOself)))
sem_Stmt_Return :: T_Expr  ->
                   T_Stmt 
sem_Stmt_Return e_  =
    (\ _lhsIlabels ->
         (let _lhsOblocks :: (IntMap (Block Stmt))
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              __tup4 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOdeclarations :: (Map String Stmt)
              _lhsOflow :: Flow
              _lhsOself :: Stmt 
              _eIself :: Expr 
              _lhsOblocks =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 711 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 716 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 25 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _label
                   {-# LINE 721 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 36 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 726 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 52 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 731 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              __tup4 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lhsOlabels,_) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 738 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 743 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 748 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 83 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 753 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 67 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   []
                   {-# LINE 758 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _self =
                  Return _eIself
              _lhsOself =
                  _self
              ( _eIself) =
                  e_ 
          in  ( _lhsOblocks,_lhsOdeclarations,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOself)))
sem_Stmt_Sequence :: T_Stmt  ->
                     T_Stmt  ->
                     T_Stmt 
sem_Stmt_Sequence l_ r_  =
    (\ _lhsIlabels ->
         (let _lhsOflow :: Flow
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOblocks :: (IntMap (Block Stmt))
              _lhsOdeclarations :: (Map String Stmt)
              _lhsOself :: Stmt 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lOlabels :: Label
              _rOlabels :: Label
              _lIblocks :: (IntMap (Block Stmt))
              _lIdeclarations :: (Map String Stmt)
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lIself :: Stmt 
              _rIblocks :: (IntMap (Block Stmt))
              _rIdeclarations :: (Map String Stmt)
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rIself :: Stmt 
              _lhsOflow =
                  ({-# LINE 71 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 801 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 71 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   (++) $ if isNothing _rIinit || isNothing _lIfinal then [] else [(l, fromJust _rIinit) | l <- fromJust _lIfinal]
                   {-# LINE 806 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 38 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lIinit <|> _rIinit
                   {-# LINE 811 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 54 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _rIfinal <|> _lIfinal
                   {-# LINE 816 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 95 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 821 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 83 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 826 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 71 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 831 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _self =
                  Sequence _lIself _rIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _rIlabel
                   {-# LINE 840 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 845 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lOlabels =
                  ({-# LINE 16 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 850 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 16 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 855 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              ( _lIblocks,_lIdeclarations,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lIself) =
                  l_ _lOlabels 
              ( _rIblocks,_rIdeclarations,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rIself) =
                  r_ _rOlabels 
          in  ( _lhsOblocks,_lhsOdeclarations,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOself)))
sem_Stmt_Skip :: T_Stmt 
sem_Stmt_Skip  =
    (\ _lhsIlabels ->
         (let _lhsOblocks :: (IntMap (Block Stmt))
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              __tup5 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOdeclarations :: (Map String Stmt)
              _lhsOflow :: Flow
              _lhsOself :: Stmt 
              _lhsOblocks =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 878 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 883 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 25 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _label
                   {-# LINE 888 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 36 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 893 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 52 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 898 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              __tup5 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lhsOlabels,_) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 905 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 910 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 915 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 83 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 920 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 67 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   []
                   {-# LINE 925 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _self =
                  Skip
              _lhsOself =
                  _self
          in  ( _lhsOblocks,_lhsOdeclarations,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOself)))
sem_Stmt_While :: T_Expr  ->
                  T_Stmt  ->
                  T_Stmt 
sem_Stmt_While e_ s_  =
    (\ _lhsIlabels ->
         (let _lhsOblocks :: (IntMap (Block Stmt))
              _lhsOflow :: Flow
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              __tup6 :: ((Label,Label))
              _sOlabels :: Label
              _label :: Label
              _lhsOdeclarations :: (Map String Stmt)
              _lhsOself :: Stmt 
              _lhsOlabels :: Label
              _eIself :: Expr 
              _sIblocks :: (IntMap (Block Stmt))
              _sIdeclarations :: (Map String Stmt)
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sIself :: Stmt 
              _lhsOblocks =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 960 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 965 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 75 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 970 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 75 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   (++) $ [(_label, fromJust _sIinit)] ++ [(l', _label) | l' <- fromJust _sIfinal]
                   {-# LINE 975 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 25 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _label
                   {-# LINE 980 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 36 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 985 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 52 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 990 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              __tup6 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_sOlabels,_) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 997 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 24 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 1002 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 99 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _sIblocks
                   {-# LINE 1007 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 83 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _sIdeclarations
                   {-# LINE 1012 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 75 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _sIflow
                   {-# LINE 1017 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              _self =
                  While _eIself _sIself
              _lhsOself =
                  _self
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/Closure/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 1026 "src/MF/Languages/Closure/AG.hs" #-}
                   )
              ( _eIself) =
                  e_ 
              ( _sIblocks,_sIdeclarations,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sIself) =
                  s_ _sOlabels 
          in  ( _lhsOblocks,_lhsOdeclarations,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOself)))