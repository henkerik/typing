

-- UUAGC 0.9.36 (src/MF/Languages/PHP/AG.ag)
module MF.Languages.PHP.AG where

{-# LINE 2 "src/MF/Languages/PHP/AG/Base.ag" #-}

import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative ((<*>)), (<$>), pure)
import Prelude              hiding (sequence)
import MF.Languages.PHP.Types
{-# LINE 14 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Languages/PHP/AG/Flow.ag" #-}

import qualified MF.Flowable as F
import MF.Flowable hiding (Return)
import Data.IntMap as IM
import Data.Map as M
import Data.Maybe
import Control.Applicative
{-# LINE 24 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Languages/PHP/AG/PP.ag" #-}

import CCO.Printing as P hiding (render, join) 
{-# LINE 29 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Languages/PHP/AG/Simplify.ag" #-}

import Data.IntMap as IM
import CCO.Component

{-# LINE 36 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}

import Data.Graph.Inductive hiding (graphviz', Node, empty, nodes)
import Data.Graph.Inductive.Tree
{-# LINE 42 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Languages/PHP/AG/Typing.ag" #-}

import MF.Languages.PHP.Types

import MF.Core as C
import Data.IntMap as IM
import Data.Set as S
import Data.Map as M
import Data.Maybe
import qualified Debug.Trace as T
import Data.List as L
{-# LINE 55 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 2 "src/MF/Languages/PHP/AG/Checking.ag" #-}

import CCO.Component
import MF.Utils
{-# LINE 61 "src/MF/Languages/PHP/AG.hs" #-}
{-# LINE 112 "src/MF/Languages/PHP/AG/Base.ag" #-}


sequence :: [Node] -> Node
sequence []     = Skip
sequence [x]    = x
sequence xs     = foldr Sequence (last xs) (L.init xs)


buildDocument before open stmts = Document before open (sequence stmts)


instance Tree OptionalString where
    fromTree = undefined
    toTree = parseTree [ app "Some" (Some <$> arg), app "None" (pure None)]


instance Tree Node where
    fromTree = undefined    
    toTree = parseTree [ -- Document
                         app "Document"      (buildDocument <$> arg <*> arg <*> arg <*> arg <*> arg)
                         
                         --  Open
                       , app "ShortOpenTag"  (pure ShortOpenTag)
                       , app "FullOpenTag"   (pure FullOpenTag)
                       , app "ASPOpenTag"    (pure ASPOpenTag)
                       
                         -- Close
                       , app "CloseTag"      (pure CloseTag)
                       , app "ASPCloseTag"   (pure ASPCloseTag)
                       
                         -- InlineHTML
                       , app "Literal"       (Literal <$> arg)
                       
                         -- Statement
                       , app "Expr"          (Expr <$> arg)
                       , app "While"         (While <$> arg <*> arg)
                       , app "Block"         (sequence <$> arg)
                       , app "If"            (If <$> arg <*> arg <*> arg <*> arg)
                       , app "Return"        (Return <$> arg)
                       
                         -- ElseIfStatement
                       , app "ElseIf"        (ElseIf <$> arg <*> arg)
    
                         -- Expression
                       , app "Assign"        (Assign <$> arg <*> arg)
                       , app "LNumber"       (id <$> arg)
                       , app "True"          (pure LTrue)
                       , app "False"         (pure LFalse)
                       
                       , app "Plus"          (Plus <$> arg <*> arg)
                       , app "Min"           (Min <$> arg <*> arg)
                       , app "Mul"           (Mul <$> arg <*> arg)
                       , app "Mod"           (Mod <$> arg <*> arg)
                       
                       , app "IsEqual"       (IsEqual <$> arg <*> arg)
                       , app "Or"            (Or <$> arg <*> arg)
                       , app "FunctionCall"  (buildFunctionCall <$> arg <*> arg)
                       
                         -- FunctionName 
                       , app "FunctionName"  (FunctionName <$> arg)
                       
                       
                         -- Strings
                       , app "ConstantEncapsedString" (id <$> arg)
                       , app "DoubleQuoted"  (id <$> arg)
                       , app "DQContent"     (DQContent <$> arg)
                       
                       
                         -- ReferenceVariable
                       , app "ArrayAccess"   (ArrayAccess <$> arg <*> arg)
                       
                         -- CompoundVariable
                       , app "Variable"      (Variable <$> arg)
                       
                         -- This is the ? in the grammar
                       , app "Some"          (id <$> arg)
                       , app "None"          (pure Skip)
                       
                         -- SimpleVariableName
                       , app "Simple"        (Simple <$> arg)
                       
                       -- LNumber
                       , app "Deci"          ((\value -> Deci $ read value) <$> arg)
                       
                       -- FunctionDecl
                       , app "FunctionDecl"  (buildFunctionDecl <$> arg <*> arg <*> arg)

                       -- Params 
                       , app "Param"         (Param <$> arg)
                       ]

buildFunctionDecl name params stmt = FunctionDecl name params (sequence stmt)
buildFunctionCall name params      = FunctionCall name params -- (sequence params)

{-# LINE 157 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 114 "src/MF/Languages/PHP/AG/Flow.ag" #-}

lookupDeclaration name declarations = case M.lookup name declarations of 
                                          Nothing   -> error $ "Calling an undefined function: " ++ name
                                          Just info -> info
{-# LINE 164 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 124 "src/MF/Languages/PHP/AG/Flow.ag" #-}

data Declaration = Declaration { functionName :: String, ln :: Label, lx :: Label }
{-# LINE 169 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 202 "src/MF/Languages/PHP/AG/Flow.ag" #-}


nextUnique :: Int -> (Int, Int)
nextUnique u = (u+1, u)

-- 'Execute' the AG, TODO: move to AG.ag
execute p = wrap_Node (sem_Node p) inh
    where
        inh = Inh_Node { labels_Inh_Node = 0 }   

-- Make Node an instance of Flowable, in this way it can be consumed by a monotome framework
instance Flowable Node where
    init     = fromJust . init_Syn_Node . execute
    final    = fromJust . final_Syn_Node . execute
    flow     = flow_Syn_Node . execute
    nodes    = nodes_Syn_Node . execute
    blocks   = IM.map (\block f -> f block) . blocks_Syn_Node . execute  

{-# LINE 190 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 96 "src/MF/Languages/PHP/AG/PP.ag" #-}


render :: Doc -> String
render = render_ 1000 

instance Printable Node where
    pp = pp_Syn_Node . execute

{-# LINE 201 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 96 "src/MF/Languages/PHP/AG/Simplify.ag" #-}

        

buildVariable label = Variable $ Simple $ "#" ++ show label 

-- | Generic function to outline a node based on a mapping
-- outline :: (Label -> Node -> Node) -> Node -> IntMap Node -> Node
exstract build update node mapping = sequence $ buildSequence ++ [update node mapping]
    where
        buildSequence = IM.elems . IM.mapWithKey build $ mapping
        
-- | Outlines function calls, every function call is moved in front of the expression and replaced by a variable. The function calls itself are placed just before the 
-- | expression
exstractFunctions = exstract build update
    where
        build label (FunctionCall (FunctionName name) params) = SimplifiedFunctionCall name params $ Just (buildVariable label)
        update = const
        
            
-- | Simplifies a function call        
exstractParameters node mapping = exstract build update node filtered
    where
        build label expr = Expr $ Assign (buildVariable label) expr
        filtered = IM.filter (not . isVariable) mapping 
        
        isVariable :: Node -> Bool
        isVariable (Variable _) = True
        isVariable n            = False
        
        update (SimplifiedFunctionCall name params result) foo = SimplifiedFunctionCall name (IM.elems . IM.mapWithKey buildParam $ mapping) result
        
        buildParam label expr | isVariable expr = Param expr
                              | otherwise       = Param $ buildVariable label
        
        

simplify node = let a = simplified_Syn_Node . execute $ node
                    b = removed_Syn_Node . execute $ a
                    c = fixPoint (exstractFunctions_Syn_Node . execute) $ b
                    d = exstractParameters_Syn_Node . execute $ c
                    
                in d -- . (removed_Syn_Node . execute) -- . (fixPoint (exstractFunctions_Syn_Node . execute)) . (simplified_Syn_Node . execute)


simplifier :: Component Node Node
simplifier = component $ return . simplify


{-# LINE 252 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 83 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}



visualize p = trace ("nodes: " ++ show nodeList ++ ", edges: " ++ show edgeList ++ ", p: " ++ show p) $ graphviz' (mkGraph nodeList edgeList :: Gr String ())
    where
        nodeList = nodeList_Syn_Node . execute $ p
        edgeList = edgeList_Syn_Node . execute $ p
        

        
graphviz' g = let n = labNodes g
                  e = labEdges g
                  
                  ns = concatMap sn n
                  es = concatMap se e
                  
                  sn (l, a)     = show l ++ " [label=\"" ++ a ++ " (" ++ show l ++ ") \"];"
                  se (l, l', a) = show l ++ " -> " ++ show l' ++ ";"
                  
              in "digraph AST {" ++ ns ++ es ++ "}"
  
        
{-# LINE 277 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 17 "src/MF/Languages/PHP/AG/Typing.ag" #-}


data SimplifiedName = Name String
                    | Nested SimplifiedName
                    
levels (Name _) = 0
levels (Nested n) = levels n + 1

name (Name n) = n
name (Nested n) = name n

{-# LINE 291 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 98 "src/MF/Languages/PHP/AG/Typing.ag" #-}


solve :: Node -> ValueMap (Stack :-> Mapping)
solve = C.solve (lift transfer) (M.singleton [] M.empty) M.empty Forward
    where        
        transfer :: Block Node -> Mapping -> Mapping
        transfer (Normal Skip)          = id
        transfer (Normal (Expect _ _))  = id
        transfer (Normal (Expr e))      = updateMapping e
        transfer (Normal (If c _ _ _ )) = updateMapping c
        transfer (Normal (While c _))   = updateMapping c
        transfer (Normal (Return e))    = updateMapping $ Return e
        
        transfer (Call lc lr (SimplifiedFunctionCall name params result)) = M.mapKeys identifierToParameter . M.filterWithKey (flip $ const isParameter)
            where
                isParameter (Identifier x) = x `elem` names
                isParameter _              = False
                identifierToParameter (Identifier x) = Parameter $ fromJust (elemIndex x names)
                names = L.map (toName . removeParam) params                                             
        
        transfer (Entry  (FunctionDecl name params _)) = M.mapKeys parameterToIdentifier 
            where
                parameterToIdentifier (Parameter p) = Identifier $ (toName . removeParam) (params !! p)
        
        transfer (Exit   (FunctionDecl name params _)) = M.filterWithKey (flip $ const isReturnValue)
            where
                isReturnValue ReturnValue = True
                isReturnValue _           = False
        
        transfer (F.Return lc lr (SimplifiedFunctionCall name params result)) = maybe id (\node -> M.mapKeys (returnValueToIdentifier node)) result 
            where
                returnValueToIdentifier node ReturnValue = Identifier $ toName node
        

        updateMapping :: Node -> Mapping -> Mapping
        updateMapping node mapping = mapping_Syn_Node . execute $ node
            where
                execute node = wrap_Node (sem_Node node) (Inh_Node { labels_Inh_Node = 0, mapping_Inh_Node = mapping, simplifiedName_Inh_Node = []})
                
        removeParam :: Node -> Node   
        removeParam (Param expr) = expr 

        toName :: Node -> String
        toName (Variable (Simple name)) = name
{-# LINE 338 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 11 "src/MF/Languages/PHP/AG/Checking.ag" #-}
 
tyNum = S.fromList [TyInt, TyFloat] 
{-# LINE 343 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 61 "src/MF/Languages/PHP/AG/Checking.ag" #-}
            


data Warning = Coercion Node Node TypeSet TypeSet
             | UndefinedVariable Node Node TypeSet
             | UnequalType Node Node Node TypeSet TypeSet
             deriving (Eq, Ord, Show)
             


violatedConstraints :: Set Constraint -> Set Constraint -> Set Constraint
violatedConstraints constraints expected = trace (show expected) $ S.filter isViolations . (fixPoint resolve) $ expected 
    where
        isViolations (l :==: expectedType) | not (resolvedType <: expectedType) = True
                                           | resolvedType == S.empty            = True
                                           | otherwise                          = False
                                                                                where
                                                                                    resolvedType = resolveType constraints l
                                         
        isViolations (l1 :<=: l2)          | resolveType constraints l1 <: resolveType constraints l2 = False
                                           | otherwise                                                = True
        
        
toWarning :: Node -> IntMap Node -> Set Constraint -> Constraint -> Warning
toWarning stmt nodes constraints (l1 :<=: l2)                                    = trace "@@@ Unqualtype" $ UnequalType stmt (lookupNode l1 nodes) (lookupNode l2 nodes) (resolveType constraints l1) (resolveType constraints l2)
toWarning stmt nodes constraints (l :==: expectedType) | resolvedType == S.empty = trace "@@@ Undefined"  $ UndefinedVariable stmt (lookupNode l nodes) expectedType
                                                       | otherwise               = trace "@@@ Coercion"   $ Coercion stmt (lookupNode l nodes) expectedType resolvedType
                                                                                 where
                                                                                     resolvedType = resolveType constraints l

lookupNode l nodes = case IM.lookup l nodes of 
                         Just t  -> t
                         Nothing -> error $ "Failed to lookup node: " ++ show l

-------------------------------------------------------------------------------
-- Checking
-------------------------------------------------------------------------------
     
checker :: Component Node [Warning]
checker = component $ return . check
             
check :: Node -> [Warning]
check p = trace ("mappings: " ++ show mappings) $ concat . L.map S.toList . IM.elems $ mergeWith findWarnings nodes' mappings
    where
        solve = MF.Languages.PHP.AG.solve
        -- ValueMap (Stack :-> Identifier :-> TypeSet) -> ValueMap (Identifier :-> TypeSet)
        
        -- mappings = IM.map (\value -> fromJust $ M.lookup [] value) $ MF.Languages.PHP.AG.solve p
        mappings = IM.map (M.fold join M.empty) (solve p)
        nodes'   = IM.map (\continuation -> toNode $ continuation id) (blocks p)
        
        -- blocks  = blocks_Syn_Node . execute $ p
        --nodes = IM.map toNode blocks

        findWarnings node mapping = trace ("finding warings for " ++ show node ++ " with " ++ show mapping) $ warnings_Syn_Node . execute $ node
            where
                execute node = wrap_Node (sem_Node node) (Inh_Node { labels_Inh_Node = 0, mapping_Inh_Node = mapping, simplifiedName_Inh_Node = []}) 
                -- {
                -- , d_Inh_Node = 0 })
                

-------------------------------------------------------------------------------
-- Reporting
-------------------------------------------------------------------------------

reporter :: Component [Warning] Doc
reporter = component $ return . report     
             

report :: [Warning] -> Doc
report [] = text "OK" 
report xs = text (show $ length xs) >|< text " warning(s) were found:" >-< above (L.map displayWarning xs)

displayWarning (Coercion stmt node expected resolved) = 
    text "Coercion: " >-< 
        indent 4 (text "Expected: " >|< text (show expected)) >-< 
        indent 4 (text "Found: " >|< text (show resolved)) >-<
    text "In the expression: " >-< 
        indent 4 (pp node) >-<
    text "In the statement: " >-<
        indent 4 (pp stmt)
        
displayWarning (UndefinedVariable stmt node expected) = 
    text "UndefinedVariable: " >-< 
        indent 4 (text "Expected: " >|< text (show expected)) >-<
    text "In the expression: " >-< 
        indent 4 (pp node) >-<
    text "In the statement: " >-<
        indent 4 (pp stmt)

displayWarning (UnequalType stmt left right resolvedLeft resolvedRight) = 
    text "UnequalType: " >-<
        indent 4 (text "Found: " >|< text (show resolvedLeft) >|< text " for " >|< pp left) >-<
        indent 4 (text "Found: " >|< text (show resolvedRight) >|< text " for " >|< pp right) >-<
    text "In the statement: " >-<
        indent 4 (pp stmt)
        
{-# LINE 443 "src/MF/Languages/PHP/AG.hs" #-}

{-# LINE 14 "src/MF/Languages/PHP/AG/Debugging.ag" #-}


buildExpect params = Expect expr (read $ "fromList" ++ value)
    where
        (Param expr)                     = params !! 0
        (Param (DQContent (Some value))) = params !! 1
        

annotator :: Component Node Node
annotator = component $ return . annotate

annotate = annotated_Syn_Node . execute

{-# LINE 459 "src/MF/Languages/PHP/AG.hs" #-}
-- Node --------------------------------------------------------
data Node  = ASPCloseTag 
           | ASPOpenTag 
           | ArrayAccess (Node ) (Node ) 
           | Assign (Node ) (Node ) 
           | Block (Node ) 
           | CloseTag 
           | ConstantEncapsedString (Node ) 
           | DQContent (OptionalString ) 
           | Deci (Integer) 
           | Document (([Node])) (Node ) (Node ) (Node ) (([Node])) 
           | DoubleQuoted (Node ) 
           | ElseIf (Node ) (Node ) 
           | Expect (Node ) (TypeSet) 
           | Expr (Node ) 
           | FullOpenTag 
           | FunctionCall (Node ) (ParamList ) 
           | FunctionDecl (String) (ParamList ) (Node ) 
           | FunctionName (String) 
           | If (Node ) (Node ) (([Node])) (Node ) 
           | IsEqual (Node ) (Node ) 
           | LFalse 
           | LTrue 
           | Literal (String) 
           | Min (Node ) (Node ) 
           | Mod (Node ) (Node ) 
           | Mul (Node ) (Node ) 
           | Or (Node ) (Node ) 
           | Param (Node ) 
           | Plus (Node ) (Node ) 
           | Return (Node ) 
           | Sequence (Node ) (Node ) 
           | ShortOpenTag 
           | Simple (String) 
           | SimplifiedFunctionCall (String) (ParamList ) ((Maybe Node)) 
           | Skip 
           | String (String) 
           | Variable (Node ) 
           | While (Node ) (Node ) 
           deriving ( Eq,Ord,Show)
-- cata
sem_Node :: Node  ->
            T_Node 
sem_Node (ASPCloseTag )  =
    (sem_Node_ASPCloseTag )
sem_Node (ASPOpenTag )  =
    (sem_Node_ASPOpenTag )
sem_Node (ArrayAccess _rv _index )  =
    (sem_Node_ArrayAccess (sem_Node _rv ) (sem_Node _index ) )
sem_Node (Assign _rv _e )  =
    (sem_Node_Assign (sem_Node _rv ) (sem_Node _e ) )
sem_Node (Block _s )  =
    (sem_Node_Block (sem_Node _s ) )
sem_Node (CloseTag )  =
    (sem_Node_CloseTag )
sem_Node (ConstantEncapsedString _n )  =
    (sem_Node_ConstantEncapsedString (sem_Node _n ) )
sem_Node (DQContent _value )  =
    (sem_Node_DQContent (sem_OptionalString _value ) )
sem_Node (Deci _value )  =
    (sem_Node_Deci _value )
sem_Node (Document _before _open _stmt _close _after )  =
    (sem_Node_Document _before (sem_Node _open ) (sem_Node _stmt ) (sem_Node _close ) _after )
sem_Node (DoubleQuoted _n )  =
    (sem_Node_DoubleQuoted (sem_Node _n ) )
sem_Node (ElseIf _e _s )  =
    (sem_Node_ElseIf (sem_Node _e ) (sem_Node _s ) )
sem_Node (Expect _expr _ty )  =
    (sem_Node_Expect (sem_Node _expr ) _ty )
sem_Node (Expr _e )  =
    (sem_Node_Expr (sem_Node _e ) )
sem_Node (FullOpenTag )  =
    (sem_Node_FullOpenTag )
sem_Node (FunctionCall _name _params )  =
    (sem_Node_FunctionCall (sem_Node _name ) (sem_ParamList _params ) )
sem_Node (FunctionDecl _name _params _stmt )  =
    (sem_Node_FunctionDecl _name (sem_ParamList _params ) (sem_Node _stmt ) )
sem_Node (FunctionName _value )  =
    (sem_Node_FunctionName _value )
sem_Node (If _c _l _elseIfs _r )  =
    (sem_Node_If (sem_Node _c ) (sem_Node _l ) _elseIfs (sem_Node _r ) )
sem_Node (IsEqual _l _r )  =
    (sem_Node_IsEqual (sem_Node _l ) (sem_Node _r ) )
sem_Node (LFalse )  =
    (sem_Node_LFalse )
sem_Node (LTrue )  =
    (sem_Node_LTrue )
sem_Node (Literal _value )  =
    (sem_Node_Literal _value )
sem_Node (Min _l _r )  =
    (sem_Node_Min (sem_Node _l ) (sem_Node _r ) )
sem_Node (Mod _l _r )  =
    (sem_Node_Mod (sem_Node _l ) (sem_Node _r ) )
sem_Node (Mul _l _r )  =
    (sem_Node_Mul (sem_Node _l ) (sem_Node _r ) )
sem_Node (Or _l _r )  =
    (sem_Node_Or (sem_Node _l ) (sem_Node _r ) )
sem_Node (Param _e )  =
    (sem_Node_Param (sem_Node _e ) )
sem_Node (Plus _l _r )  =
    (sem_Node_Plus (sem_Node _l ) (sem_Node _r ) )
sem_Node (Return _e )  =
    (sem_Node_Return (sem_Node _e ) )
sem_Node (Sequence _f _s )  =
    (sem_Node_Sequence (sem_Node _f ) (sem_Node _s ) )
sem_Node (ShortOpenTag )  =
    (sem_Node_ShortOpenTag )
sem_Node (Simple _value )  =
    (sem_Node_Simple _value )
sem_Node (SimplifiedFunctionCall _name _params _result )  =
    (sem_Node_SimplifiedFunctionCall _name (sem_ParamList _params ) _result )
sem_Node (Skip )  =
    (sem_Node_Skip )
sem_Node (String _value )  =
    (sem_Node_String _value )
sem_Node (Variable _n )  =
    (sem_Node_Variable (sem_Node _n ) )
sem_Node (While _c _s )  =
    (sem_Node_While (sem_Node _c ) (sem_Node _s ) )
-- semantic domain
type T_Node  = Declaration ->
               (Map String Declaration) ->
               Label ->
               Mapping ->
               ([SimplifiedName -> SimplifiedName]) ->
               ( Node ,(IntMap (Block Node)),(IntMap Node),(Set Constraint),Node ,(Map String Declaration),([UEdge]),(Set Constraint),Node ,Node ,(Maybe [Label]),Flow,(Maybe Label),Label,Label,Mapping,([LNode String]),(IntMap Node),(IntMap Node),Doc,Node ,Node ,Node ,SimplifiedName,(Set Warning))
data Inh_Node  = Inh_Node {declaration_Inh_Node :: Declaration,declarations'_Inh_Node :: (Map String Declaration),labels_Inh_Node :: Label,mapping_Inh_Node :: Mapping,simplifiedName_Inh_Node :: ([SimplifiedName -> SimplifiedName])}
data Syn_Node  = Syn_Node {annotated_Syn_Node :: Node ,blocks_Syn_Node :: (IntMap (Block Node)),callMapping_Syn_Node :: (IntMap Node),constraints_Syn_Node :: (Set Constraint),copy_Syn_Node :: Node ,declarations_Syn_Node :: (Map String Declaration),edgeList_Syn_Node :: ([UEdge]),expected_Syn_Node :: (Set Constraint),exstractFunctions_Syn_Node :: Node ,exstractParameters_Syn_Node :: Node ,final_Syn_Node :: (Maybe [Label]),flow_Syn_Node :: Flow,init_Syn_Node :: (Maybe Label),label_Syn_Node :: Label,labels_Syn_Node :: Label,mapping_Syn_Node :: Mapping,nodeList_Syn_Node :: ([LNode String]),nodes_Syn_Node :: (IntMap Node),paramMapping_Syn_Node :: (IntMap Node),pp_Syn_Node :: Doc,removed_Syn_Node :: Node ,self_Syn_Node :: Node ,simplified_Syn_Node :: Node ,simplifiedName'_Syn_Node :: SimplifiedName,warnings_Syn_Node :: (Set Warning)}
wrap_Node :: T_Node  ->
             Inh_Node  ->
             Syn_Node 
wrap_Node sem (Inh_Node _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIsimplifiedName )  =
    (let ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings) = sem _lhsIdeclaration _lhsIdeclarations' _lhsIlabels _lhsImapping _lhsIsimplifiedName 
     in  (Syn_Node _lhsOannotated _lhsOblocks _lhsOcallMapping _lhsOconstraints _lhsOcopy _lhsOdeclarations _lhsOedgeList _lhsOexpected _lhsOexstractFunctions _lhsOexstractParameters _lhsOfinal _lhsOflow _lhsOinit _lhsOlabel _lhsOlabels _lhsOmapping _lhsOnodeList _lhsOnodes _lhsOparamMapping _lhsOpp _lhsOremoved _lhsOself _lhsOsimplified _lhsOsimplifiedName' _lhsOwarnings ))
sem_Node_ASPCloseTag :: T_Node 
sem_Node_ASPCloseTag  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 629 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 634 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 639 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 644 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 649 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 654 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 659 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 664 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 669 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 674 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   P.empty
                   {-# LINE 679 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 684 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   ASPCloseTag
                   {-# LINE 689 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   ASPCloseTag
                   {-# LINE 694 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ASPCloseTag
                   {-# LINE 699 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ASPCloseTag
                   {-# LINE 704 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ASPCloseTag
                   {-# LINE 709 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  ASPCloseTag
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ASPCloseTag
                   {-# LINE 716 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 721 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 726 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 731 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 736 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 741 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 748 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.ASPCloseTag.lhs.final"
                   {-# LINE 753 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.ASPCloseTag.lhs.init"
                   {-# LINE 758 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.ASPCloseTag.lhs.label"
                   {-# LINE 763 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 768 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 773 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.ASPCloseTag.lhs.simplifiedName'"
                   {-# LINE 778 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_ASPOpenTag :: T_Node 
sem_Node_ASPOpenTag  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 816 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 821 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 826 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 831 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 836 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 841 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 846 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 851 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 856 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 861 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   P.empty
                   {-# LINE 866 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 871 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   ASPOpenTag
                   {-# LINE 876 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   ASPOpenTag
                   {-# LINE 881 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ASPOpenTag
                   {-# LINE 886 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ASPOpenTag
                   {-# LINE 891 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ASPOpenTag
                   {-# LINE 896 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  ASPOpenTag
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ASPOpenTag
                   {-# LINE 903 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 908 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 913 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 918 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 923 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 928 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 935 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.ASPOpenTag.lhs.final"
                   {-# LINE 940 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.ASPOpenTag.lhs.init"
                   {-# LINE 945 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.ASPOpenTag.lhs.label"
                   {-# LINE 950 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 955 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 960 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.ASPOpenTag.lhs.simplifiedName'"
                   {-# LINE 965 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_ArrayAccess :: T_Node  ->
                        T_Node  ->
                        T_Node 
sem_Node_ArrayAccess rv_ index_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              _lhsOsimplifiedName' :: SimplifiedName
              _rvOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              __tup1 :: ((Label,Label))
              _rvOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _rvOdeclaration :: Declaration
              _rvOdeclarations' :: (Map String Declaration)
              _rvOmapping :: Mapping
              _indexOdeclaration :: Declaration
              _indexOdeclarations' :: (Map String Declaration)
              _indexOlabels :: Label
              _indexOmapping :: Mapping
              _indexOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _rvIannotated :: Node 
              _rvIblocks :: (IntMap (Block Node))
              _rvIcallMapping :: (IntMap Node)
              _rvIconstraints :: (Set Constraint)
              _rvIcopy :: Node 
              _rvIdeclarations :: (Map String Declaration)
              _rvIedgeList :: ([UEdge])
              _rvIexpected :: (Set Constraint)
              _rvIexstractFunctions :: Node 
              _rvIexstractParameters :: Node 
              _rvIfinal :: (Maybe [Label])
              _rvIflow :: Flow
              _rvIinit :: (Maybe Label)
              _rvIlabel :: Label
              _rvIlabels :: Label
              _rvImapping :: Mapping
              _rvInodeList :: ([LNode String])
              _rvInodes :: (IntMap Node)
              _rvIparamMapping :: (IntMap Node)
              _rvIpp :: Doc
              _rvIremoved :: Node 
              _rvIself :: Node 
              _rvIsimplified :: Node 
              _rvIsimplifiedName' :: SimplifiedName
              _rvIwarnings :: (Set Warning)
              _indexIannotated :: Node 
              _indexIblocks :: (IntMap (Block Node))
              _indexIcallMapping :: (IntMap Node)
              _indexIconstraints :: (Set Constraint)
              _indexIcopy :: Node 
              _indexIdeclarations :: (Map String Declaration)
              _indexIedgeList :: ([UEdge])
              _indexIexpected :: (Set Constraint)
              _indexIexstractFunctions :: Node 
              _indexIexstractParameters :: Node 
              _indexIfinal :: (Maybe [Label])
              _indexIflow :: Flow
              _indexIinit :: (Maybe Label)
              _indexIlabel :: Label
              _indexIlabels :: Label
              _indexImapping :: Mapping
              _indexInodeList :: ([LNode String])
              _indexInodes :: (IntMap Node)
              _indexIparamMapping :: (IntMap Node)
              _indexIpp :: Doc
              _indexIremoved :: Node 
              _indexIself :: Node 
              _indexIsimplified :: Node 
              _indexIsimplifiedName' :: SimplifiedName
              _indexIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 1067 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _rvIlabel)
                   {-# LINE 1072 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 1077 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1082 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1087 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 1092 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _rvIpp >|< text "[" >|< _indexIpp >|< text "]"
                   {-# LINE 1097 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 37 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rvIsimplifiedName'
                   {-# LINE 1102 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 38 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   Nested : _lhsIsimplifiedName
                   {-# LINE 1107 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup1 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_rvOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 1114 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup1
                   {-# LINE 1119 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _indexIblocks
                   {-# LINE 1124 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _indexIcallMapping
                   {-# LINE 1129 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rvIconstraints `S.union` _indexIconstraints
                   {-# LINE 1134 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _indexIdeclarations
                   {-# LINE 1139 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _rvIedgeList ++ _indexIedgeList
                   {-# LINE 1144 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _rvIexpected `S.union` _indexIexpected
                   {-# LINE 1149 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvIflow ++ _indexIflow
                   {-# LINE 1154 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _rvInodeList ++ _indexInodeList
                   {-# LINE 1159 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _indexInodes
                   {-# LINE 1164 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _indexIparamMapping
                   {-# LINE 1169 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _indexIwarnings
                   {-# LINE 1174 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   ArrayAccess _rvIannotated _indexIannotated
                   {-# LINE 1179 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   ArrayAccess _rvIcopy _indexIcopy
                   {-# LINE 1184 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIexstractFunctions _indexIexstractFunctions
                   {-# LINE 1189 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIexstractParameters _indexIexstractParameters
                   {-# LINE 1194 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIremoved _indexIremoved
                   {-# LINE 1199 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  ArrayAccess _rvIself _indexIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ArrayAccess _rvIsimplified _indexIsimplified
                   {-# LINE 1206 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 1211 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 1216 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 1221 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 1226 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1231 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1238 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _indexIfinal
                   {-# LINE 1243 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _indexIinit
                   {-# LINE 1248 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _indexIlabels
                   {-# LINE 1253 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _indexImapping
                   {-# LINE 1258 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1263 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1268 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1273 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _indexOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1278 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _indexOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1283 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _indexOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1288 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _indexOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1293 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _indexOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1298 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _rvIannotated,_rvIblocks,_rvIcallMapping,_rvIconstraints,_rvIcopy,_rvIdeclarations,_rvIedgeList,_rvIexpected,_rvIexstractFunctions,_rvIexstractParameters,_rvIfinal,_rvIflow,_rvIinit,_rvIlabel,_rvIlabels,_rvImapping,_rvInodeList,_rvInodes,_rvIparamMapping,_rvIpp,_rvIremoved,_rvIself,_rvIsimplified,_rvIsimplifiedName',_rvIwarnings) =
                  rv_ _rvOdeclaration _rvOdeclarations' _rvOlabels _rvOmapping _rvOsimplifiedName 
              ( _indexIannotated,_indexIblocks,_indexIcallMapping,_indexIconstraints,_indexIcopy,_indexIdeclarations,_indexIedgeList,_indexIexpected,_indexIexstractFunctions,_indexIexstractParameters,_indexIfinal,_indexIflow,_indexIinit,_indexIlabel,_indexIlabels,_indexImapping,_indexInodeList,_indexInodes,_indexIparamMapping,_indexIpp,_indexIremoved,_indexIself,_indexIsimplified,_indexIsimplifiedName',_indexIwarnings) =
                  index_ _indexOdeclaration _indexOdeclarations' _indexOlabels _indexOmapping _indexOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Assign :: T_Node  ->
                   T_Node  ->
                   T_Node 
sem_Node_Assign rv_ e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              _lhsOnodeList :: ([LNode String])
              _lhsOedgeList :: ([UEdge])
              _lhsOconstraints :: (Set Constraint)
              _lhsOmapping :: Mapping
              __tup2 :: ((Label,Label))
              _rvOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOsimplifiedName' :: SimplifiedName
              _rvOdeclaration :: Declaration
              _rvOdeclarations' :: (Map String Declaration)
              _rvOmapping :: Mapping
              _rvOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOlabels :: Label
              _eOmapping :: Mapping
              _eOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _rvIannotated :: Node 
              _rvIblocks :: (IntMap (Block Node))
              _rvIcallMapping :: (IntMap Node)
              _rvIconstraints :: (Set Constraint)
              _rvIcopy :: Node 
              _rvIdeclarations :: (Map String Declaration)
              _rvIedgeList :: ([UEdge])
              _rvIexpected :: (Set Constraint)
              _rvIexstractFunctions :: Node 
              _rvIexstractParameters :: Node 
              _rvIfinal :: (Maybe [Label])
              _rvIflow :: Flow
              _rvIinit :: (Maybe Label)
              _rvIlabel :: Label
              _rvIlabels :: Label
              _rvImapping :: Mapping
              _rvInodeList :: ([LNode String])
              _rvInodes :: (IntMap Node)
              _rvIparamMapping :: (IntMap Node)
              _rvIpp :: Doc
              _rvIremoved :: Node 
              _rvIself :: Node 
              _rvIsimplified :: Node 
              _rvIsimplifiedName' :: SimplifiedName
              _rvIwarnings :: (Set Warning)
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIcopy :: Node 
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIsimplifiedName' :: SimplifiedName
              _eIwarnings :: (Set Warning)
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 1404 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 1409 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 1414 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 1419 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _rvIpp >|< text " = " >|< _eIpp
                   {-# LINE 1424 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   [(_label, "="), (_rvIlabel, render _rvIpp)] ++ _eInodeList
                   {-# LINE 1429 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   [(_label, _rvIlabel, ()), (_label, _eIlabel, ())] ++ _eIedgeList
                   {-# LINE 1434 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 56 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 1439 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.fromList [(_rvIlabel :<=: _eIlabel), (_label :<=: _rvIlabel)] `S.union` _eIconstraints
                   {-# LINE 1444 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 90 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   updateMapping (Identifier $ name _rvIsimplifiedName') _eIlabel (levels _rvIsimplifiedName') _constraints _eImapping
                   {-# LINE 1449 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 20 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 1454 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup2 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_rvOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1461 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup2
                   {-# LINE 1466 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvIblocks `IM.union` _eIblocks
                   {-# LINE 1471 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _rvIcallMapping `IM.union` _eIcallMapping
                   {-# LINE 1476 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvIdeclarations `M.union` _eIdeclarations
                   {-# LINE 1481 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 1486 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvIflow ++ _eIflow
                   {-# LINE 1491 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvInodes `IM.union` _eInodes
                   {-# LINE 1496 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _rvIparamMapping `IM.union` _eIparamMapping
                   {-# LINE 1501 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _rvIwarnings `S.union` _eIwarnings
                   {-# LINE 1506 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Assign _rvIannotated _eIannotated
                   {-# LINE 1511 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Assign _rvIcopy _eIcopy
                   {-# LINE 1516 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Assign _rvIexstractFunctions _eIexstractFunctions
                   {-# LINE 1521 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Assign _rvIexstractParameters _eIexstractParameters
                   {-# LINE 1526 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Assign _rvIremoved _eIremoved
                   {-# LINE 1531 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Assign _rvIself _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Assign _rvIsimplified _eIsimplified
                   {-# LINE 1538 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 1543 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 1548 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 1553 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 1558 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1563 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1570 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 1575 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 1580 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 1585 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eIsimplifiedName'
                   {-# LINE 1590 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rvOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1595 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rvOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1600 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rvOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1605 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rvOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1610 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1615 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1620 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rvIlabels
                   {-# LINE 1625 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rvImapping
                   {-# LINE 1630 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1635 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _rvIannotated,_rvIblocks,_rvIcallMapping,_rvIconstraints,_rvIcopy,_rvIdeclarations,_rvIedgeList,_rvIexpected,_rvIexstractFunctions,_rvIexstractParameters,_rvIfinal,_rvIflow,_rvIinit,_rvIlabel,_rvIlabels,_rvImapping,_rvInodeList,_rvInodes,_rvIparamMapping,_rvIpp,_rvIremoved,_rvIself,_rvIsimplified,_rvIsimplifiedName',_rvIwarnings) =
                  rv_ _rvOdeclaration _rvOdeclarations' _rvOlabels _rvOmapping _rvOsimplifiedName 
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIcopy,_eIdeclarations,_eIedgeList,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIremoved,_eIself,_eIsimplified,_eIsimplifiedName',_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Block :: T_Node  ->
                  T_Node 
sem_Node_Block s_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _sIannotated :: Node 
              _sIblocks :: (IntMap (Block Node))
              _sIcallMapping :: (IntMap Node)
              _sIconstraints :: (Set Constraint)
              _sIcopy :: Node 
              _sIdeclarations :: (Map String Declaration)
              _sIedgeList :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIexstractFunctions :: Node 
              _sIexstractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIsimplifiedName' :: SimplifiedName
              _sIwarnings :: (Set Warning)
              _lhsOinit =
                  ({-# LINE 62 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIinit
                   {-# LINE 1708 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIfinal
                   {-# LINE 1713 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIblocks
                   {-# LINE 1718 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _sIcallMapping
                   {-# LINE 1723 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _sIconstraints
                   {-# LINE 1728 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIdeclarations
                   {-# LINE 1733 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _sIedgeList
                   {-# LINE 1738 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _sIexpected
                   {-# LINE 1743 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIflow
                   {-# LINE 1748 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _sInodeList
                   {-# LINE 1753 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sInodes
                   {-# LINE 1758 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _sIparamMapping
                   {-# LINE 1763 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _sIpp
                   {-# LINE 1768 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _sIwarnings
                   {-# LINE 1773 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Block _sIannotated
                   {-# LINE 1778 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Block _sIcopy
                   {-# LINE 1783 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Block _sIexstractFunctions
                   {-# LINE 1788 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Block _sIexstractParameters
                   {-# LINE 1793 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Block _sIremoved
                   {-# LINE 1798 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Block _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Block _sIsimplified
                   {-# LINE 1805 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 1810 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 1815 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 1820 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 1825 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 1830 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 1837 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 1842 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 1847 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 1852 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _sIsimplifiedName'
                   {-# LINE 1857 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 1862 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 1867 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 1872 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 1877 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 1882 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIcopy,_sIdeclarations,_sIedgeList,_sIexpected,_sIexstractFunctions,_sIexstractParameters,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sImapping,_sInodeList,_sInodes,_sIparamMapping,_sIpp,_sIremoved,_sIself,_sIsimplified,_sIsimplifiedName',_sIwarnings) =
                  s_ _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_CloseTag :: T_Node 
sem_Node_CloseTag  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 1922 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 1927 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 1932 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 1937 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 1942 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 1947 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 1952 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 1957 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 1962 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 1967 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   P.empty
                   {-# LINE 1972 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 1977 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   CloseTag
                   {-# LINE 1982 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   CloseTag
                   {-# LINE 1987 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1992 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 1997 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2002 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  CloseTag
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   CloseTag
                   {-# LINE 2009 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 2014 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 2019 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 2024 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 2029 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2034 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2041 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.CloseTag.lhs.final"
                   {-# LINE 2046 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.CloseTag.lhs.init"
                   {-# LINE 2051 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.CloseTag.lhs.label"
                   {-# LINE 2056 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 2061 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2066 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.CloseTag.lhs.simplifiedName'"
                   {-# LINE 2071 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_ConstantEncapsedString :: T_Node  ->
                                   T_Node 
sem_Node_ConstantEncapsedString n_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _nOdeclaration :: Declaration
              _nOdeclarations' :: (Map String Declaration)
              _nOlabels :: Label
              _nOmapping :: Mapping
              _nOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _nIannotated :: Node 
              _nIblocks :: (IntMap (Block Node))
              _nIcallMapping :: (IntMap Node)
              _nIconstraints :: (Set Constraint)
              _nIcopy :: Node 
              _nIdeclarations :: (Map String Declaration)
              _nIedgeList :: ([UEdge])
              _nIexpected :: (Set Constraint)
              _nIexstractFunctions :: Node 
              _nIexstractParameters :: Node 
              _nIfinal :: (Maybe [Label])
              _nIflow :: Flow
              _nIinit :: (Maybe Label)
              _nIlabel :: Label
              _nIlabels :: Label
              _nImapping :: Mapping
              _nInodeList :: ([LNode String])
              _nInodes :: (IntMap Node)
              _nIparamMapping :: (IntMap Node)
              _nIpp :: Doc
              _nIremoved :: Node 
              _nIself :: Node 
              _nIsimplified :: Node 
              _nIsimplifiedName' :: SimplifiedName
              _nIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 2140 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 2145 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 2150 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 2155 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _nIedgeList
                   {-# LINE 2160 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 2165 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 2170 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _nInodeList
                   {-# LINE 2175 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 2180 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 2185 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _nIpp
                   {-# LINE 2190 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 2195 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   ConstantEncapsedString _nIannotated
                   {-# LINE 2200 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   ConstantEncapsedString _nIcopy
                   {-# LINE 2205 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIexstractFunctions
                   {-# LINE 2210 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIexstractParameters
                   {-# LINE 2215 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIremoved
                   {-# LINE 2220 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  ConstantEncapsedString _nIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ConstantEncapsedString _nIsimplified
                   {-# LINE 2227 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 2232 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 2237 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 2242 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 2247 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2252 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2259 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIfinal
                   {-# LINE 2264 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIinit
                   {-# LINE 2269 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIlabel
                   {-# LINE 2274 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 2279 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 2284 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nIsimplifiedName'
                   {-# LINE 2289 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 2294 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 2299 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 2304 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2309 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 2314 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _nIannotated,_nIblocks,_nIcallMapping,_nIconstraints,_nIcopy,_nIdeclarations,_nIedgeList,_nIexpected,_nIexstractFunctions,_nIexstractParameters,_nIfinal,_nIflow,_nIinit,_nIlabel,_nIlabels,_nImapping,_nInodeList,_nInodes,_nIparamMapping,_nIpp,_nIremoved,_nIself,_nIsimplified,_nIsimplifiedName',_nIwarnings) =
                  n_ _nOdeclaration _nOdeclarations' _nOlabels _nOmapping _nOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_DQContent :: T_OptionalString  ->
                      T_Node 
sem_Node_DQContent value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOsimplified :: Node 
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _valueIannotated :: OptionalString 
              _valueIcopy :: OptionalString 
              _valueIexstractFunctions :: OptionalString 
              _valueIexstractParameters :: OptionalString 
              _valueIparamMapping :: (IntMap Node)
              _valueIremoved :: OptionalString 
              _valueIself :: OptionalString 
              _valueIsimplified :: OptionalString 
              _valueIvalue :: String
              _lhsOsimplified =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   String _valueIvalue
                   {-# LINE 2364 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2369 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2374 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 2379 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2384 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 2389 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2394 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2399 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 2404 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2409 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _valueIparamMapping
                   {-# LINE 2414 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   P.empty
                   {-# LINE 2419 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2424 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   DQContent _valueIannotated
                   {-# LINE 2429 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   DQContent _valueIcopy
                   {-# LINE 2434 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIexstractFunctions
                   {-# LINE 2439 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIexstractParameters
                   {-# LINE 2444 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIremoved
                   {-# LINE 2449 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  DQContent _valueIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   DQContent _valueIsimplified
                   {-# LINE 2456 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 2461 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 2466 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 2471 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 2476 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2481 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.DQContent.lhs.final"
                   {-# LINE 2488 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.DQContent.lhs.init"
                   {-# LINE 2493 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.DQContent.lhs.label"
                   {-# LINE 2498 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 2503 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2508 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.DQContent.lhs.simplifiedName'"
                   {-# LINE 2513 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _valueIannotated,_valueIcopy,_valueIexstractFunctions,_valueIexstractParameters,_valueIparamMapping,_valueIremoved,_valueIself,_valueIsimplified,_valueIvalue) =
                  value_ 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Deci :: Integer ->
                 T_Node 
sem_Node_Deci value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              _lhsOconstraints :: (Set Constraint)
              _lhsOexpected :: (Set Constraint)
              __tup3 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOnodeList =
                  ({-# LINE 65 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 2556 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 65 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, show value_)]
                   {-# LINE 2561 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 2566 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 2571 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 2576 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 2581 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 62 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text $ show value_
                   {-# LINE 2586 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 62 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 2591 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 33 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 2596 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup3 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lhsOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 2603 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup3
                   {-# LINE 2608 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2613 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2618 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 2623 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 2628 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 2633 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 65 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 2638 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 2643 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 2648 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 2653 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Deci value_
                   {-# LINE 2658 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Deci value_
                   {-# LINE 2663 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2668 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2673 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2678 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Deci value_
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Deci value_
                   {-# LINE 2685 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 2690 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 2695 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 2700 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 2705 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 2710 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 2717 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.Deci.lhs.final"
                   {-# LINE 2722 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.Deci.lhs.init"
                   {-# LINE 2727 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 2732 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.Deci.lhs.simplifiedName'"
                   {-# LINE 2737 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Document :: ([Node]) ->
                     T_Node  ->
                     T_Node  ->
                     T_Node  ->
                     ([Node]) ->
                     T_Node 
sem_Node_Document before_ open_ stmt_ close_ after_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _stmtOdeclarations' :: (Map String Declaration)
              _lhsOpp :: Doc
              _stmtOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _openOdeclaration :: Declaration
              _openOdeclarations' :: (Map String Declaration)
              _openOlabels :: Label
              _openOmapping :: Mapping
              _openOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _stmtOdeclaration :: Declaration
              _stmtOlabels :: Label
              _stmtOmapping :: Mapping
              _closeOdeclaration :: Declaration
              _closeOdeclarations' :: (Map String Declaration)
              _closeOlabels :: Label
              _closeOmapping :: Mapping
              _closeOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _openIannotated :: Node 
              _openIblocks :: (IntMap (Block Node))
              _openIcallMapping :: (IntMap Node)
              _openIconstraints :: (Set Constraint)
              _openIcopy :: Node 
              _openIdeclarations :: (Map String Declaration)
              _openIedgeList :: ([UEdge])
              _openIexpected :: (Set Constraint)
              _openIexstractFunctions :: Node 
              _openIexstractParameters :: Node 
              _openIfinal :: (Maybe [Label])
              _openIflow :: Flow
              _openIinit :: (Maybe Label)
              _openIlabel :: Label
              _openIlabels :: Label
              _openImapping :: Mapping
              _openInodeList :: ([LNode String])
              _openInodes :: (IntMap Node)
              _openIparamMapping :: (IntMap Node)
              _openIpp :: Doc
              _openIremoved :: Node 
              _openIself :: Node 
              _openIsimplified :: Node 
              _openIsimplifiedName' :: SimplifiedName
              _openIwarnings :: (Set Warning)
              _stmtIannotated :: Node 
              _stmtIblocks :: (IntMap (Block Node))
              _stmtIcallMapping :: (IntMap Node)
              _stmtIconstraints :: (Set Constraint)
              _stmtIcopy :: Node 
              _stmtIdeclarations :: (Map String Declaration)
              _stmtIedgeList :: ([UEdge])
              _stmtIexpected :: (Set Constraint)
              _stmtIexstractFunctions :: Node 
              _stmtIexstractParameters :: Node 
              _stmtIfinal :: (Maybe [Label])
              _stmtIflow :: Flow
              _stmtIinit :: (Maybe Label)
              _stmtIlabel :: Label
              _stmtIlabels :: Label
              _stmtImapping :: Mapping
              _stmtInodeList :: ([LNode String])
              _stmtInodes :: (IntMap Node)
              _stmtIparamMapping :: (IntMap Node)
              _stmtIpp :: Doc
              _stmtIremoved :: Node 
              _stmtIself :: Node 
              _stmtIsimplified :: Node 
              _stmtIsimplifiedName' :: SimplifiedName
              _stmtIwarnings :: (Set Warning)
              _closeIannotated :: Node 
              _closeIblocks :: (IntMap (Block Node))
              _closeIcallMapping :: (IntMap Node)
              _closeIconstraints :: (Set Constraint)
              _closeIcopy :: Node 
              _closeIdeclarations :: (Map String Declaration)
              _closeIedgeList :: ([UEdge])
              _closeIexpected :: (Set Constraint)
              _closeIexstractFunctions :: Node 
              _closeIexstractParameters :: Node 
              _closeIfinal :: (Maybe [Label])
              _closeIflow :: Flow
              _closeIinit :: (Maybe Label)
              _closeIlabel :: Label
              _closeIlabels :: Label
              _closeImapping :: Mapping
              _closeInodeList :: ([LNode String])
              _closeInodes :: (IntMap Node)
              _closeIparamMapping :: (IntMap Node)
              _closeIpp :: Doc
              _closeIremoved :: Node 
              _closeIself :: Node 
              _closeIsimplified :: Node 
              _closeIsimplifiedName' :: SimplifiedName
              _closeIwarnings :: (Set Warning)
              _lhsOinit =
                  ({-# LINE 54 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _stmtIinit
                   {-# LINE 2870 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 76 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _stmtIfinal
                   {-# LINE 2875 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 136 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _stmtIdeclarations
                   {-# LINE 2880 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 2885 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 25 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "<?" >-< _stmtIpp >-< text "?>"
                   {-# LINE 2890 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 35 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   []
                   {-# LINE 2895 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _openIblocks `IM.union` _stmtIblocks `IM.union` _closeIblocks
                   {-# LINE 2900 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _openIcallMapping `IM.union` _stmtIcallMapping `IM.union` _closeIcallMapping
                   {-# LINE 2905 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _openIconstraints `S.union` _stmtIconstraints `S.union` _closeIconstraints
                   {-# LINE 2910 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _openIdeclarations `M.union` _stmtIdeclarations `M.union` _closeIdeclarations
                   {-# LINE 2915 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _openIedgeList ++ _stmtIedgeList ++ _closeIedgeList
                   {-# LINE 2920 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _openIexpected `S.union` _stmtIexpected `S.union` _closeIexpected
                   {-# LINE 2925 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _openIflow ++ _stmtIflow ++ _closeIflow
                   {-# LINE 2930 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _openInodeList ++ _stmtInodeList ++ _closeInodeList
                   {-# LINE 2935 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _openInodes `IM.union` _stmtInodes `IM.union` _closeInodes
                   {-# LINE 2940 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _openIparamMapping `IM.union` _stmtIparamMapping `IM.union` _closeIparamMapping
                   {-# LINE 2945 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _openIwarnings `S.union` _stmtIwarnings `S.union` _closeIwarnings
                   {-# LINE 2950 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Document before_ _openIannotated _stmtIannotated _closeIannotated after_
                   {-# LINE 2955 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Document before_ _openIcopy _stmtIcopy _closeIcopy after_
                   {-# LINE 2960 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Document before_ _openIexstractFunctions _stmtIexstractFunctions _closeIexstractFunctions after_
                   {-# LINE 2965 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Document before_ _openIexstractParameters _stmtIexstractParameters _closeIexstractParameters after_
                   {-# LINE 2970 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Document before_ _openIremoved _stmtIremoved _closeIremoved after_
                   {-# LINE 2975 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Document before_ _openIself _stmtIself _closeIself after_
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Document before_ _openIsimplified _stmtIsimplified _closeIsimplified after_
                   {-# LINE 2982 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 2987 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 2992 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 2997 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 3002 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3007 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3014 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _closeIlabel
                   {-# LINE 3019 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _closeIlabels
                   {-# LINE 3024 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _closeImapping
                   {-# LINE 3029 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _closeIsimplifiedName'
                   {-# LINE 3034 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _openOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3039 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _openOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3044 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _openOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 3049 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _openOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3054 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _openOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3059 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3064 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _openIlabels
                   {-# LINE 3069 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _openImapping
                   {-# LINE 3074 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _closeOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3079 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _closeOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3084 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _closeOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 3089 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _closeOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 3094 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _closeOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3099 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _openIannotated,_openIblocks,_openIcallMapping,_openIconstraints,_openIcopy,_openIdeclarations,_openIedgeList,_openIexpected,_openIexstractFunctions,_openIexstractParameters,_openIfinal,_openIflow,_openIinit,_openIlabel,_openIlabels,_openImapping,_openInodeList,_openInodes,_openIparamMapping,_openIpp,_openIremoved,_openIself,_openIsimplified,_openIsimplifiedName',_openIwarnings) =
                  open_ _openOdeclaration _openOdeclarations' _openOlabels _openOmapping _openOsimplifiedName 
              ( _stmtIannotated,_stmtIblocks,_stmtIcallMapping,_stmtIconstraints,_stmtIcopy,_stmtIdeclarations,_stmtIedgeList,_stmtIexpected,_stmtIexstractFunctions,_stmtIexstractParameters,_stmtIfinal,_stmtIflow,_stmtIinit,_stmtIlabel,_stmtIlabels,_stmtImapping,_stmtInodeList,_stmtInodes,_stmtIparamMapping,_stmtIpp,_stmtIremoved,_stmtIself,_stmtIsimplified,_stmtIsimplifiedName',_stmtIwarnings) =
                  stmt_ _stmtOdeclaration _stmtOdeclarations' _stmtOlabels _stmtOmapping _stmtOsimplifiedName 
              ( _closeIannotated,_closeIblocks,_closeIcallMapping,_closeIconstraints,_closeIcopy,_closeIdeclarations,_closeIedgeList,_closeIexpected,_closeIexstractFunctions,_closeIexstractParameters,_closeIfinal,_closeIflow,_closeIinit,_closeIlabel,_closeIlabels,_closeImapping,_closeInodeList,_closeInodes,_closeIparamMapping,_closeIpp,_closeIremoved,_closeIself,_closeIsimplified,_closeIsimplifiedName',_closeIwarnings) =
                  close_ _closeOdeclaration _closeOdeclarations' _closeOlabels _closeOmapping _closeOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_DoubleQuoted :: T_Node  ->
                         T_Node 
sem_Node_DoubleQuoted n_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _nOdeclaration :: Declaration
              _nOdeclarations' :: (Map String Declaration)
              _nOlabels :: Label
              _nOmapping :: Mapping
              _nOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _nIannotated :: Node 
              _nIblocks :: (IntMap (Block Node))
              _nIcallMapping :: (IntMap Node)
              _nIconstraints :: (Set Constraint)
              _nIcopy :: Node 
              _nIdeclarations :: (Map String Declaration)
              _nIedgeList :: ([UEdge])
              _nIexpected :: (Set Constraint)
              _nIexstractFunctions :: Node 
              _nIexstractParameters :: Node 
              _nIfinal :: (Maybe [Label])
              _nIflow :: Flow
              _nIinit :: (Maybe Label)
              _nIlabel :: Label
              _nIlabels :: Label
              _nImapping :: Mapping
              _nInodeList :: ([LNode String])
              _nInodes :: (IntMap Node)
              _nIparamMapping :: (IntMap Node)
              _nIpp :: Doc
              _nIremoved :: Node 
              _nIself :: Node 
              _nIsimplified :: Node 
              _nIsimplifiedName' :: SimplifiedName
              _nIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 3174 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 3179 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 3184 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 3189 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _nIedgeList
                   {-# LINE 3194 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 3199 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 3204 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _nInodeList
                   {-# LINE 3209 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 3214 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 3219 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _nIpp
                   {-# LINE 3224 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 3229 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   DoubleQuoted _nIannotated
                   {-# LINE 3234 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   DoubleQuoted _nIcopy
                   {-# LINE 3239 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   DoubleQuoted _nIexstractFunctions
                   {-# LINE 3244 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   DoubleQuoted _nIexstractParameters
                   {-# LINE 3249 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   DoubleQuoted _nIremoved
                   {-# LINE 3254 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  DoubleQuoted _nIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   DoubleQuoted _nIsimplified
                   {-# LINE 3261 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 3266 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 3271 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 3276 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 3281 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3286 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3293 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIfinal
                   {-# LINE 3298 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIinit
                   {-# LINE 3303 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIlabel
                   {-# LINE 3308 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 3313 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 3318 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nIsimplifiedName'
                   {-# LINE 3323 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3328 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3333 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 3338 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3343 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3348 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _nIannotated,_nIblocks,_nIcallMapping,_nIconstraints,_nIcopy,_nIdeclarations,_nIedgeList,_nIexpected,_nIexstractFunctions,_nIexstractParameters,_nIfinal,_nIflow,_nIinit,_nIlabel,_nIlabels,_nImapping,_nInodeList,_nInodes,_nIparamMapping,_nIpp,_nIremoved,_nIself,_nIsimplified,_nIsimplifiedName',_nIwarnings) =
                  n_ _nOdeclaration _nOdeclarations' _nOlabels _nOmapping _nOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_ElseIf :: T_Node  ->
                   T_Node  ->
                   T_Node 
sem_Node_ElseIf e_ s_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOlabels :: Label
              _eOmapping :: Mapping
              _eOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIcopy :: Node 
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIsimplifiedName' :: SimplifiedName
              _eIwarnings :: (Set Warning)
              _sIannotated :: Node 
              _sIblocks :: (IntMap (Block Node))
              _sIcallMapping :: (IntMap Node)
              _sIconstraints :: (Set Constraint)
              _sIcopy :: Node 
              _sIdeclarations :: (Map String Declaration)
              _sIedgeList :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIexstractFunctions :: Node 
              _sIexstractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIsimplifiedName' :: SimplifiedName
              _sIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIblocks `IM.union` _sIblocks
                   {-# LINE 3450 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 3455 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eIconstraints `S.union` _sIconstraints
                   {-# LINE 3460 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIdeclarations `M.union` _sIdeclarations
                   {-# LINE 3465 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _eIedgeList ++ _sIedgeList
                   {-# LINE 3470 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _eIexpected `S.union` _sIexpected
                   {-# LINE 3475 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIflow ++ _sIflow
                   {-# LINE 3480 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _eInodeList ++ _sInodeList
                   {-# LINE 3485 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eInodes `IM.union` _sInodes
                   {-# LINE 3490 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 3495 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _eIpp >|< _sIpp
                   {-# LINE 3500 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _eIwarnings `S.union` _sIwarnings
                   {-# LINE 3505 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   ElseIf _eIannotated _sIannotated
                   {-# LINE 3510 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   ElseIf _eIcopy _sIcopy
                   {-# LINE 3515 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIexstractFunctions _sIexstractFunctions
                   {-# LINE 3520 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIexstractParameters _sIexstractParameters
                   {-# LINE 3525 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIremoved _sIremoved
                   {-# LINE 3530 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  ElseIf _eIself _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ElseIf _eIsimplified _sIsimplified
                   {-# LINE 3537 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 3542 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 3547 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 3552 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 3557 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3562 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3569 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIfinal
                   {-# LINE 3574 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIinit
                   {-# LINE 3579 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIlabel
                   {-# LINE 3584 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 3589 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 3594 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _sIsimplifiedName'
                   {-# LINE 3599 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3604 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3609 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 3614 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3619 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3624 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3629 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3634 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 3639 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 3644 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3649 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIcopy,_eIdeclarations,_eIedgeList,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIremoved,_eIself,_eIsimplified,_eIsimplifiedName',_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName 
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIcopy,_sIdeclarations,_sIedgeList,_sIexpected,_sIexstractFunctions,_sIexstractParameters,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sImapping,_sInodeList,_sInodes,_sIparamMapping,_sIpp,_sIremoved,_sIself,_sIsimplified,_sIsimplifiedName',_sIwarnings) =
                  s_ _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Expect :: T_Node  ->
                   TypeSet ->
                   T_Node 
sem_Node_Expect expr_ ty_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              _lhsOnodeList :: ([LNode String])
              _lhsOwarnings :: (Set Warning)
              __tup4 :: ((Label,Label))
              _exprOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _exprOdeclaration :: Declaration
              _exprOdeclarations' :: (Map String Declaration)
              _exprOmapping :: Mapping
              _exprOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _exprIannotated :: Node 
              _exprIblocks :: (IntMap (Block Node))
              _exprIcallMapping :: (IntMap Node)
              _exprIconstraints :: (Set Constraint)
              _exprIcopy :: Node 
              _exprIdeclarations :: (Map String Declaration)
              _exprIedgeList :: ([UEdge])
              _exprIexpected :: (Set Constraint)
              _exprIexstractFunctions :: Node 
              _exprIexstractParameters :: Node 
              _exprIfinal :: (Maybe [Label])
              _exprIflow :: Flow
              _exprIinit :: (Maybe Label)
              _exprIlabel :: Label
              _exprIlabels :: Label
              _exprImapping :: Mapping
              _exprInodeList :: ([LNode String])
              _exprInodes :: (IntMap Node)
              _exprIparamMapping :: (IntMap Node)
              _exprIpp :: Doc
              _exprIremoved :: Node 
              _exprIself :: Node 
              _exprIsimplified :: Node 
              _exprIsimplifiedName' :: SimplifiedName
              _exprIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 3725 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 3730 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 3735 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 3740 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 3745 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 3750 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 80 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 3755 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 3760 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 94 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "## Expect: " >|< _exprIpp >|< text " == " >|< text (show ty_)
                   {-# LINE 3765 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 38 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   [(_label, "expect: " ++ render _exprIpp ++ " == " ++ show ty_)]
                   {-# LINE 3770 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 78 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :<=: _exprIlabel) `S.union` _exprIconstraints
                   {-# LINE 3775 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 35 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.singleton (_exprIlabel :==: ty_) `S.union` _exprIexpected
                   {-# LINE 3780 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 47 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _exprInodes
                   {-# LINE 3785 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _copy _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 3790 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup4 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_exprOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 3797 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup4
                   {-# LINE 3802 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _exprIblocks
                   {-# LINE 3807 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exprIcallMapping
                   {-# LINE 3812 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 3817 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _exprIdeclarations
                   {-# LINE 3822 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _exprIedgeList
                   {-# LINE 3827 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 3832 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _exprIflow
                   {-# LINE 3837 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 3842 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exprIparamMapping
                   {-# LINE 3847 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Expect _exprIannotated ty_
                   {-# LINE 3852 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Expect _exprIcopy ty_
                   {-# LINE 3857 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Expect _exprIexstractFunctions ty_
                   {-# LINE 3862 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Expect _exprIexstractParameters ty_
                   {-# LINE 3867 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Expect _exprIremoved ty_
                   {-# LINE 3872 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Expect _exprIself ty_
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Expect _exprIsimplified ty_
                   {-# LINE 3879 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 3884 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 3889 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 3894 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 3899 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 3904 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 3911 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _exprIlabels
                   {-# LINE 3916 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _exprImapping
                   {-# LINE 3921 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _exprIsimplifiedName'
                   {-# LINE 3926 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exprOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 3931 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exprOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 3936 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exprOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 3941 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exprOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 3946 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _exprIannotated,_exprIblocks,_exprIcallMapping,_exprIconstraints,_exprIcopy,_exprIdeclarations,_exprIedgeList,_exprIexpected,_exprIexstractFunctions,_exprIexstractParameters,_exprIfinal,_exprIflow,_exprIinit,_exprIlabel,_exprIlabels,_exprImapping,_exprInodeList,_exprInodes,_exprIparamMapping,_exprIpp,_exprIremoved,_exprIself,_exprIsimplified,_exprIsimplifiedName',_exprIwarnings) =
                  expr_ _exprOdeclaration _exprOdeclarations' _exprOlabels _exprOmapping _exprOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Expr :: T_Node  ->
                 T_Node 
sem_Node_Expr e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOedgeList :: ([UEdge])
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOremoved :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOwarnings :: (Set Warning)
              __tup5 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIcopy :: Node 
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIsimplifiedName' :: SimplifiedName
              _eIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 4019 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 4024 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 35 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 4029 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 35 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 4034 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 4039 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "expr")]
                   {-# LINE 4044 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4049 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4054 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4059 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 4064 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 80 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 4069 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 38 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   case _eIcopy of
                       (Assign rv (FunctionCall (FunctionName name) params)) -> SimplifiedFunctionCall name params $ Just rv
                       (FunctionCall (FunctionName name) params)             -> SimplifiedFunctionCall name params Nothing
                       copy                                                  -> Expr copy
                   {-# LINE 4077 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 64 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   exstractFunctions (Expr _eIexstractFunctions) _eIcallMapping
                   {-# LINE 4082 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 56 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 4087 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 20 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 4092 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 43 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 4097 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _copy _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 4102 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup5 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_eOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 4109 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup5
                   {-# LINE 4114 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 4119 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 4124 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 4129 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 4134 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 35 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _eIedgeList
                   {-# LINE 4139 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 4144 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 4149 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _eInodeList
                   {-# LINE 4154 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 4159 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 4164 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _eIpp
                   {-# LINE 4169 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Expr _eIannotated
                   {-# LINE 4174 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Expr _eIcopy
                   {-# LINE 4179 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Expr _eIexstractFunctions
                   {-# LINE 4184 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Expr _eIexstractParameters
                   {-# LINE 4189 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Expr _eIremoved
                   {-# LINE 4194 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Expr _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Expr _eIsimplified
                   {-# LINE 4201 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 4206 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 4211 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 4216 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4223 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 4228 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 4233 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eIsimplifiedName'
                   {-# LINE 4238 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4243 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4248 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4253 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4258 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIcopy,_eIdeclarations,_eIedgeList,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIremoved,_eIself,_eIsimplified,_eIsimplifiedName',_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_FullOpenTag :: T_Node 
sem_Node_FullOpenTag  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 4298 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 4303 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 4308 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 4313 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 4318 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 4323 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 4328 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 4333 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 4338 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 4343 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   P.empty
                   {-# LINE 4348 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 4353 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   FullOpenTag
                   {-# LINE 4358 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   FullOpenTag
                   {-# LINE 4363 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FullOpenTag
                   {-# LINE 4368 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FullOpenTag
                   {-# LINE 4373 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FullOpenTag
                   {-# LINE 4378 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  FullOpenTag
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FullOpenTag
                   {-# LINE 4385 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 4390 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 4395 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 4400 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 4405 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 4410 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4417 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.FullOpenTag.lhs.final"
                   {-# LINE 4422 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.FullOpenTag.lhs.init"
                   {-# LINE 4427 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.FullOpenTag.lhs.label"
                   {-# LINE 4432 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 4437 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4442 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.FullOpenTag.lhs.simplifiedName'"
                   {-# LINE 4447 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_FunctionCall :: T_Node  ->
                         T_ParamList  ->
                         T_Node 
sem_Node_FunctionCall name_ params_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              _lhsOcallMapping :: (IntMap Node)
              _lhsOexstractFunctions :: Node 
              _lhsOannotated :: Node 
              __tup6 :: ((Label,Label))
              _nameOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOcopy :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _nameOdeclaration :: Declaration
              _nameOdeclarations' :: (Map String Declaration)
              _nameOmapping :: Mapping
              _nameOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _paramsOlabels :: Label
              _nameIannotated :: Node 
              _nameIblocks :: (IntMap (Block Node))
              _nameIcallMapping :: (IntMap Node)
              _nameIconstraints :: (Set Constraint)
              _nameIcopy :: Node 
              _nameIdeclarations :: (Map String Declaration)
              _nameIedgeList :: ([UEdge])
              _nameIexpected :: (Set Constraint)
              _nameIexstractFunctions :: Node 
              _nameIexstractParameters :: Node 
              _nameIfinal :: (Maybe [Label])
              _nameIflow :: Flow
              _nameIinit :: (Maybe Label)
              _nameIlabel :: Label
              _nameIlabels :: Label
              _nameImapping :: Mapping
              _nameInodeList :: ([LNode String])
              _nameInodes :: (IntMap Node)
              _nameIparamMapping :: (IntMap Node)
              _nameIpp :: Doc
              _nameIremoved :: Node 
              _nameIself :: Node 
              _nameIsimplified :: Node 
              _nameIsimplifiedName' :: SimplifiedName
              _nameIwarnings :: (Set Warning)
              _paramsIannotated :: ParamList 
              _paramsIcallMapping :: (IntMap Node)
              _paramsIcopy :: ParamList 
              _paramsIexstractFunctions :: ParamList 
              _paramsIexstractParameters :: ParamList 
              _paramsIlabel :: Label
              _paramsIlabels :: Label
              _paramsIparamMapping :: (IntMap Node)
              _paramsIpp :: Doc
              _paramsIremoved :: ParamList 
              _paramsIself :: ParamList 
              _paramsIsimplified :: ParamList 
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4532 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4537 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 4542 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 4547 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _nameIpp >|< text "()"
                   {-# LINE 4552 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 55 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _copy
                   {-# LINE 4557 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   buildVariable _label
                   {-# LINE 4562 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 10 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   case _nameIcopy of
                       (FunctionName "check")   -> buildExpect _paramsIcopy
                       otherwise                -> _copy
                   {-# LINE 4569 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup6 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_nameOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 4576 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup6
                   {-# LINE 4581 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nameIblocks
                   {-# LINE 4586 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nameIconstraints
                   {-# LINE 4591 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nameIdeclarations
                   {-# LINE 4596 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _nameIedgeList
                   {-# LINE 4601 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _nameIexpected
                   {-# LINE 4606 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nameIflow
                   {-# LINE 4611 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _nameInodeList
                   {-# LINE 4616 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nameInodes
                   {-# LINE 4621 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _nameIparamMapping `IM.union` _paramsIparamMapping
                   {-# LINE 4626 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _nameIwarnings
                   {-# LINE 4631 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   FunctionCall _nameIannotated _paramsIannotated
                   {-# LINE 4636 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   FunctionCall _nameIcopy _paramsIcopy
                   {-# LINE 4641 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIexstractFunctions _paramsIexstractFunctions
                   {-# LINE 4646 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIexstractParameters _paramsIexstractParameters
                   {-# LINE 4651 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIremoved _paramsIremoved
                   {-# LINE 4656 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionCall _nameIself _paramsIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionCall _nameIsimplified _paramsIsimplified
                   {-# LINE 4663 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 4668 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 4673 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 4678 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 4685 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nameIfinal
                   {-# LINE 4690 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nameIinit
                   {-# LINE 4695 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 4700 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nameImapping
                   {-# LINE 4705 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nameIsimplifiedName'
                   {-# LINE 4710 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nameOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 4715 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nameOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 4720 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nameOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 4725 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nameOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 4730 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _paramsOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nameIlabels
                   {-# LINE 4735 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _nameIannotated,_nameIblocks,_nameIcallMapping,_nameIconstraints,_nameIcopy,_nameIdeclarations,_nameIedgeList,_nameIexpected,_nameIexstractFunctions,_nameIexstractParameters,_nameIfinal,_nameIflow,_nameIinit,_nameIlabel,_nameIlabels,_nameImapping,_nameInodeList,_nameInodes,_nameIparamMapping,_nameIpp,_nameIremoved,_nameIself,_nameIsimplified,_nameIsimplifiedName',_nameIwarnings) =
                  name_ _nameOdeclaration _nameOdeclarations' _nameOlabels _nameOmapping _nameOsimplifiedName 
              ( _paramsIannotated,_paramsIcallMapping,_paramsIcopy,_paramsIexstractFunctions,_paramsIexstractParameters,_paramsIlabel,_paramsIlabels,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOlabels 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_FunctionDecl :: String ->
                         T_ParamList  ->
                         T_Node  ->
                         T_Node 
sem_Node_FunctionDecl name_ params_ stmt_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _stmtOdeclaration :: Declaration
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              __tup7 :: ((Label,Label,Label))
              _paramsOlabels :: Label
              _ln :: Label
              _lx :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOexpected :: (Set Constraint)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _stmtOdeclarations' :: (Map String Declaration)
              _stmtOlabels :: Label
              _stmtOmapping :: Mapping
              _stmtOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _paramsIannotated :: ParamList 
              _paramsIcallMapping :: (IntMap Node)
              _paramsIcopy :: ParamList 
              _paramsIexstractFunctions :: ParamList 
              _paramsIexstractParameters :: ParamList 
              _paramsIlabel :: Label
              _paramsIlabels :: Label
              _paramsIparamMapping :: (IntMap Node)
              _paramsIpp :: Doc
              _paramsIremoved :: ParamList 
              _paramsIself :: ParamList 
              _paramsIsimplified :: ParamList 
              _stmtIannotated :: Node 
              _stmtIblocks :: (IntMap (Block Node))
              _stmtIcallMapping :: (IntMap Node)
              _stmtIconstraints :: (Set Constraint)
              _stmtIcopy :: Node 
              _stmtIdeclarations :: (Map String Declaration)
              _stmtIedgeList :: ([UEdge])
              _stmtIexpected :: (Set Constraint)
              _stmtIexstractFunctions :: Node 
              _stmtIexstractParameters :: Node 
              _stmtIfinal :: (Maybe [Label])
              _stmtIflow :: Flow
              _stmtIinit :: (Maybe Label)
              _stmtIlabel :: Label
              _stmtIlabels :: Label
              _stmtImapping :: Mapping
              _stmtInodeList :: ([LNode String])
              _stmtInodes :: (IntMap Node)
              _stmtIparamMapping :: (IntMap Node)
              _stmtIpp :: Doc
              _stmtIremoved :: Node 
              _stmtIself :: Node 
              _stmtIsimplified :: Node 
              _stmtIsimplifiedName' :: SimplifiedName
              _stmtIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 158 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 4826 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 158 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_ln, Entry _self)
                                  ,(_lx, Exit _self)]
                   {-# LINE 4832 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 140 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _declarations_augmented_syn [_declarations_augmented_f1]
                   {-# LINE 4837 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _declarations_augmented_f1 =
                  ({-# LINE 140 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.union $ M.singleton name_ _declaration
                   {-# LINE 4842 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 4847 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_ln, _stmtIlabel, ())]
                   {-# LINE 4852 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 109 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4857 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 109 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   (++) $ [(_ln, fromJust _stmtIinit)]
                   {-# LINE 4862 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 18 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 4867 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 18 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_ln, "function " ++ name_)]
                   {-# LINE 4872 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 35 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 4877 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 35 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 4882 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 64 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 4887 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Nothing
                   {-# LINE 4892 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _declaration =
                  ({-# LINE 138 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Declaration name_ _ln _lx
                   {-# LINE 4897 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOdeclaration =
                  ({-# LINE 139 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _declaration
                   {-# LINE 4902 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 4907 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "function " >|< text name_ >|< text "() {" >-< indent 4 _stmtIpp >-< text "}"
                   {-# LINE 4912 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _ln
                   {-# LINE 4917 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 56 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 4922 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup7 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, ln) -> case nextUnique __cont of { (__cont, lx) -> (__cont, ln,lx)}}}
              (_paramsOlabels,_,_) =
                  ({-# LINE 33 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 4929 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_ln,_) =
                  ({-# LINE 33 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 4934 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_,_lx) =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup7
                   {-# LINE 4939 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 158 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _stmtIblocks
                   {-# LINE 4944 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping `IM.union` _stmtIcallMapping
                   {-# LINE 4949 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _stmtIconstraints
                   {-# LINE 4954 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _declarations_augmented_syn =
                  ({-# LINE 140 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _stmtIdeclarations
                   {-# LINE 4959 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _stmtIedgeList
                   {-# LINE 4964 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _stmtIexpected
                   {-# LINE 4969 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 109 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _stmtIflow
                   {-# LINE 4974 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 18 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _stmtInodeList
                   {-# LINE 4979 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 35 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _stmtInodes
                   {-# LINE 4984 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping `IM.union` _stmtIparamMapping
                   {-# LINE 4989 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   FunctionDecl name_ _paramsIannotated _stmtIannotated
                   {-# LINE 4994 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   FunctionDecl name_ _paramsIcopy _stmtIcopy
                   {-# LINE 4999 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIexstractFunctions _stmtIexstractFunctions
                   {-# LINE 5004 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIexstractParameters _stmtIexstractParameters
                   {-# LINE 5009 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIremoved _stmtIremoved
                   {-# LINE 5014 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionDecl name_ _paramsIself _stmtIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionDecl name_ _paramsIsimplified _stmtIsimplified
                   {-# LINE 5021 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 5026 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 5031 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 5036 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 5041 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5046 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5053 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5058 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _stmtIlabels
                   {-# LINE 5063 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _stmtImapping
                   {-# LINE 5068 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _stmtIsimplifiedName'
                   {-# LINE 5073 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5078 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 5083 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5088 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _stmtOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5093 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _paramsIannotated,_paramsIcallMapping,_paramsIcopy,_paramsIexstractFunctions,_paramsIexstractParameters,_paramsIlabel,_paramsIlabels,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOlabels 
              ( _stmtIannotated,_stmtIblocks,_stmtIcallMapping,_stmtIconstraints,_stmtIcopy,_stmtIdeclarations,_stmtIedgeList,_stmtIexpected,_stmtIexstractFunctions,_stmtIexstractParameters,_stmtIfinal,_stmtIflow,_stmtIinit,_stmtIlabel,_stmtIlabels,_stmtImapping,_stmtInodeList,_stmtInodes,_stmtIparamMapping,_stmtIpp,_stmtIremoved,_stmtIself,_stmtIsimplified,_stmtIsimplifiedName',_stmtIwarnings) =
                  stmt_ _stmtOdeclaration _stmtOdeclarations' _stmtOlabels _stmtOmapping _stmtOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_FunctionName :: String ->
                         T_Node 
sem_Node_FunctionName value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOpp :: Doc
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 5136 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 83 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text value_
                   {-# LINE 5141 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5146 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5151 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 5156 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 5161 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 5166 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5171 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 5176 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 5181 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 5186 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 5191 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 5196 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   FunctionName value_
                   {-# LINE 5201 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   FunctionName value_
                   {-# LINE 5206 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5211 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5216 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5221 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  FunctionName value_
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   FunctionName value_
                   {-# LINE 5228 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 5233 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 5238 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 5243 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 5248 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5253 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5260 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.FunctionName.lhs.final"
                   {-# LINE 5265 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.FunctionName.lhs.init"
                   {-# LINE 5270 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.FunctionName.lhs.label"
                   {-# LINE 5275 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 5280 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5285 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.FunctionName.lhs.simplifiedName'"
                   {-# LINE 5290 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_If :: T_Node  ->
               T_Node  ->
               ([Node]) ->
               T_Node  ->
               T_Node 
sem_Node_If c_ l_ elseIfs_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              _lhsOexpected :: (Set Constraint)
              _lhsOwarnings :: (Set Warning)
              __tup8 :: ((Label,Label))
              _cOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _cOdeclaration :: Declaration
              _cOdeclarations' :: (Map String Declaration)
              _cOmapping :: Mapping
              _cOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOlabels :: Label
              _lOmapping :: Mapping
              _lOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _cIannotated :: Node 
              _cIblocks :: (IntMap (Block Node))
              _cIcallMapping :: (IntMap Node)
              _cIconstraints :: (Set Constraint)
              _cIcopy :: Node 
              _cIdeclarations :: (Map String Declaration)
              _cIedgeList :: ([UEdge])
              _cIexpected :: (Set Constraint)
              _cIexstractFunctions :: Node 
              _cIexstractParameters :: Node 
              _cIfinal :: (Maybe [Label])
              _cIflow :: Flow
              _cIinit :: (Maybe Label)
              _cIlabel :: Label
              _cIlabels :: Label
              _cImapping :: Mapping
              _cInodeList :: ([LNode String])
              _cInodes :: (IntMap Node)
              _cIparamMapping :: (IntMap Node)
              _cIpp :: Doc
              _cIremoved :: Node 
              _cIself :: Node 
              _cIsimplified :: Node 
              _cIsimplifiedName' :: SimplifiedName
              _cIwarnings :: (Set Warning)
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIcopy :: Node 
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIsimplifiedName' :: SimplifiedName
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIcopy :: Node 
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIsimplifiedName' :: SimplifiedName
              _rIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 5424 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 5429 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 45 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 5434 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 45 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 5439 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 103 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5444 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 103 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, fromJust _lIinit), (_label, fromJust _rIinit)]
                   {-# LINE 5449 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 44 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5454 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 44 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "if")]
                   {-# LINE 5459 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5464 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5469 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5474 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 5479 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 84 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   pure (++) <*> _lIfinal <*> _rIfinal
                   {-# LINE 5484 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 5489 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 35 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "if (" >|< _cIpp >|< text ") {" >-< indent 4 _lIpp >-< text "} else {" >-< indent 4 _rIpp >-< text "}"
                   {-# LINE 5494 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 5499 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 5504 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 23 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 5509 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 45 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 5514 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _copy _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 5519 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup8 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_cOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 5526 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup8
                   {-# LINE 5531 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _lIblocks `IM.union` _rIblocks
                   {-# LINE 5536 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 5541 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 5546 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 5551 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 45 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _cIedgeList ++ _lIedgeList ++ _rIedgeList
                   {-# LINE 5556 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 103 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _cIflow ++ _lIflow ++ _rIflow
                   {-# LINE 5561 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 44 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _cInodeList ++ _lInodeList ++ _rInodeList
                   {-# LINE 5566 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 5571 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _cIparamMapping `IM.union` _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 5576 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   If _cIannotated _lIannotated elseIfs_ _rIannotated
                   {-# LINE 5581 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   If _cIcopy _lIcopy elseIfs_ _rIcopy
                   {-# LINE 5586 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   If _cIexstractFunctions _lIexstractFunctions elseIfs_ _rIexstractFunctions
                   {-# LINE 5591 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   If _cIexstractParameters _lIexstractParameters elseIfs_ _rIexstractParameters
                   {-# LINE 5596 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   If _cIremoved _lIremoved elseIfs_ _rIremoved
                   {-# LINE 5601 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  If _cIself _lIself elseIfs_ _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   If _cIsimplified _lIsimplified elseIfs_ _rIsimplified
                   {-# LINE 5608 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 5613 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 5618 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 5623 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 5628 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 5633 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 5640 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 5645 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 5650 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rIsimplifiedName'
                   {-# LINE 5655 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5660 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5665 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 5670 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5675 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5680 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5685 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 5690 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 5695 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5700 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 5705 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 5710 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 5715 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 5720 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 5725 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _cIannotated,_cIblocks,_cIcallMapping,_cIconstraints,_cIcopy,_cIdeclarations,_cIedgeList,_cIexpected,_cIexstractFunctions,_cIexstractParameters,_cIfinal,_cIflow,_cIinit,_cIlabel,_cIlabels,_cImapping,_cInodeList,_cInodes,_cIparamMapping,_cIpp,_cIremoved,_cIself,_cIsimplified,_cIsimplifiedName',_cIwarnings) =
                  c_ _cOdeclaration _cOdeclarations' _cOlabels _cOmapping _cOsimplifiedName 
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIcopy,_lIdeclarations,_lIedgeList,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIremoved,_lIself,_lIsimplified,_lIsimplifiedName',_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIcopy,_rIdeclarations,_rIedgeList,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIremoved,_rIself,_rIsimplified,_rIsimplifiedName',_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_IsEqual :: T_Node  ->
                    T_Node  ->
                    T_Node 
sem_Node_IsEqual l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup9 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIcopy :: Node 
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIsimplifiedName' :: SimplifiedName
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIcopy :: Node 
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIsimplifiedName' :: SimplifiedName
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 68 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 5833 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 68 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 5838 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 69 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 5843 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 69 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 5848 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 5853 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.union $ S.singleton (_lIlabel :<=: _rIlabel)
                   {-# LINE 5858 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 79 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 5863 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 79 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "==")]
                   {-# LINE 5868 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 5873 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 5878 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 5883 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 77 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _lIpp >|< text " == " >|< _rIpp
                   {-# LINE 5888 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup9 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 5895 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup9
                   {-# LINE 5900 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 5905 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 5910 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 68 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 5915 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 5920 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 69 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 5925 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 5930 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 5935 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 79 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 5940 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 5945 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 5950 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 5955 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 5960 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   IsEqual _lIannotated _rIannotated
                   {-# LINE 5965 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IsEqual _lIcopy _rIcopy
                   {-# LINE 5970 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 5975 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIexstractParameters _rIexstractParameters
                   {-# LINE 5980 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIremoved _rIremoved
                   {-# LINE 5985 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  IsEqual _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IsEqual _lIsimplified _rIsimplified
                   {-# LINE 5992 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 5997 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 6002 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 6007 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 6012 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6017 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6024 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIfinal
                   {-# LINE 6029 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIinit
                   {-# LINE 6034 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 6039 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 6044 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rIsimplifiedName'
                   {-# LINE 6049 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6054 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6059 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6064 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6069 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 6074 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 6079 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 6084 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 6089 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 6094 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIcopy,_lIdeclarations,_lIedgeList,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIremoved,_lIself,_lIsimplified,_lIsimplifiedName',_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIcopy,_rIdeclarations,_rIedgeList,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIremoved,_rIself,_rIsimplified,_rIsimplifiedName',_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_LFalse :: T_Node 
sem_Node_LFalse  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              _lhsOconstraints :: (Set Constraint)
              __tup10 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOnodeList =
                  ({-# LINE 62 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6138 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 62 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, render _pp    )]
                   {-# LINE 6143 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6148 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6153 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6158 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 6163 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "false"
                   {-# LINE 6168 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 64 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6173 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup10 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lhsOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 6180 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup10
                   {-# LINE 6185 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6190 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6195 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 6200 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 6205 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6210 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6215 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 62 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 6220 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6225 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6230 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6235 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   LFalse
                   {-# LINE 6240 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   LFalse
                   {-# LINE 6245 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6250 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6255 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6260 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  LFalse
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   LFalse
                   {-# LINE 6267 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 6272 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 6277 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 6282 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 6287 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6292 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6299 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.LFalse.lhs.final"
                   {-# LINE 6304 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.LFalse.lhs.init"
                   {-# LINE 6309 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6314 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.LFalse.lhs.simplifiedName'"
                   {-# LINE 6319 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_LTrue :: T_Node 
sem_Node_LTrue  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              _lhsOconstraints :: (Set Constraint)
              __tup11 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOnodeList =
                  ({-# LINE 59 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6359 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 59 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, render _pp    )]
                   {-# LINE 6364 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6369 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6374 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6379 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 6384 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 64 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "true"
                   {-# LINE 6389 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 64 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 6394 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup11 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lhsOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 6401 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup11
                   {-# LINE 6406 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6411 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6416 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 6421 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 6426 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6431 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6436 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 59 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 6441 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6446 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6451 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6456 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   LTrue
                   {-# LINE 6461 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   LTrue
                   {-# LINE 6466 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6471 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6476 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6481 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  LTrue
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   LTrue
                   {-# LINE 6488 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 6493 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 6498 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 6503 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 6508 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6513 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6520 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.LTrue.lhs.final"
                   {-# LINE 6525 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.LTrue.lhs.init"
                   {-# LINE 6530 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6535 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.LTrue.lhs.simplifiedName'"
                   {-# LINE 6540 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Literal :: String ->
                    T_Node 
sem_Node_Literal value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6579 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6584 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 6589 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 6594 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 6599 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6604 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 6609 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 6614 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 6619 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 6624 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   P.empty
                   {-# LINE 6629 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 6634 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Literal value_
                   {-# LINE 6639 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Literal value_
                   {-# LINE 6644 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 6649 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 6654 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 6659 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Literal value_
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Literal value_
                   {-# LINE 6666 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 6671 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 6676 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 6681 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 6686 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 6691 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 6698 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.Literal.lhs.final"
                   {-# LINE 6703 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.Literal.lhs.init"
                   {-# LINE 6708 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.Literal.lhs.label"
                   {-# LINE 6713 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 6718 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 6723 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.Literal.lhs.simplifiedName'"
                   {-# LINE 6728 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Min :: T_Node  ->
                T_Node  ->
                T_Node 
sem_Node_Min l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup12 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIcopy :: Node 
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIsimplifiedName' :: SimplifiedName
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIcopy :: Node 
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIsimplifiedName' :: SimplifiedName
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 6830 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 6835 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 6840 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 6845 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 73 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 6850 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 73 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "-")]
                   {-# LINE 6855 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 6860 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 6865 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 6870 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 70 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _lIpp >|< text " - " >|< _rIpp
                   {-# LINE 6875 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup12 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 6882 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup12
                   {-# LINE 6887 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 6892 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 6897 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 6902 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 6907 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 6912 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 6917 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 6922 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 73 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 6927 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 6932 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 6937 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 6942 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 6947 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Min _lIannotated _rIannotated
                   {-# LINE 6952 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Min _lIcopy _rIcopy
                   {-# LINE 6957 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Min _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 6962 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Min _lIexstractParameters _rIexstractParameters
                   {-# LINE 6967 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Min _lIremoved _rIremoved
                   {-# LINE 6972 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Min _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Min _lIsimplified _rIsimplified
                   {-# LINE 6979 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 6984 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 6989 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 6994 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 6999 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7004 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7011 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIfinal
                   {-# LINE 7016 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIinit
                   {-# LINE 7021 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 7026 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 7031 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rIsimplifiedName'
                   {-# LINE 7036 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7041 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7046 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7051 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7056 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7061 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7066 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 7071 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 7076 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7081 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIcopy,_lIdeclarations,_lIedgeList,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIremoved,_lIself,_lIsimplified,_lIsimplifiedName',_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIcopy,_rIdeclarations,_rIedgeList,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIremoved,_rIself,_rIsimplified,_rIsimplifiedName',_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Mod :: T_Node  ->
                T_Node  ->
                T_Node 
sem_Node_Mod l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup13 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIcopy :: Node 
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIsimplifiedName' :: SimplifiedName
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIcopy :: Node 
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIsimplifiedName' :: SimplifiedName
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 7187 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7192 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 25 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7197 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 25 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyInt), (_rIlabel :==: S.singleton TyInt)]
                   {-# LINE 7202 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 77 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7207 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 77 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "%")]
                   {-# LINE 7212 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7217 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7222 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7227 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 74 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _lIpp >|< text " % " >|< _rIpp
                   {-# LINE 7232 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup13 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 7239 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup13
                   {-# LINE 7244 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 7249 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 7254 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 7259 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 7264 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 7269 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 25 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 7274 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 7279 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 77 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 7284 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 7289 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 7294 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 7299 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 7304 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Mod _lIannotated _rIannotated
                   {-# LINE 7309 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Mod _lIcopy _rIcopy
                   {-# LINE 7314 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Mod _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 7319 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Mod _lIexstractParameters _rIexstractParameters
                   {-# LINE 7324 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Mod _lIremoved _rIremoved
                   {-# LINE 7329 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Mod _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Mod _lIsimplified _rIsimplified
                   {-# LINE 7336 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 7341 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 7346 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 7351 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 7356 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7361 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7368 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIfinal
                   {-# LINE 7373 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIinit
                   {-# LINE 7378 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 7383 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 7388 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rIsimplifiedName'
                   {-# LINE 7393 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7398 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7403 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7408 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7413 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7418 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7423 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 7428 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 7433 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7438 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIcopy,_lIdeclarations,_lIedgeList,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIremoved,_lIself,_lIsimplified,_lIsimplifiedName',_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIcopy,_rIdeclarations,_rIedgeList,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIremoved,_rIself,_rIsimplified,_rIsimplifiedName',_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Mul :: T_Node  ->
                T_Node  ->
                T_Node 
sem_Node_Mul l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup14 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIcopy :: Node 
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIsimplifiedName' :: SimplifiedName
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIcopy :: Node 
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIsimplifiedName' :: SimplifiedName
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 7544 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 7549 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7554 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 7559 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 75 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 7564 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 75 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "*")]
                   {-# LINE 7569 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7574 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7579 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7584 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _lIpp >|< text " * " >|< _rIpp
                   {-# LINE 7589 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup14 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 7596 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup14
                   {-# LINE 7601 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 7606 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 7611 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 7616 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 7621 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 7626 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 7631 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 7636 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 75 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 7641 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 7646 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 7651 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 7656 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 7661 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Mul _lIannotated _rIannotated
                   {-# LINE 7666 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Mul _lIcopy _rIcopy
                   {-# LINE 7671 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Mul _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 7676 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Mul _lIexstractParameters _rIexstractParameters
                   {-# LINE 7681 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Mul _lIremoved _rIremoved
                   {-# LINE 7686 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Mul _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Mul _lIsimplified _rIsimplified
                   {-# LINE 7693 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 7698 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 7703 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 7708 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 7713 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 7718 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 7725 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIfinal
                   {-# LINE 7730 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIinit
                   {-# LINE 7735 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 7740 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 7745 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rIsimplifiedName'
                   {-# LINE 7750 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7755 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7760 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 7765 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7770 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 7775 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 7780 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 7785 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 7790 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 7795 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIcopy,_lIdeclarations,_lIedgeList,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIremoved,_lIself,_lIsimplified,_lIsimplifiedName',_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIcopy,_rIdeclarations,_rIedgeList,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIremoved,_rIself,_rIsimplified,_rIsimplifiedName',_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Or :: T_Node  ->
               T_Node  ->
               T_Node 
sem_Node_Or l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOexpected :: (Set Constraint)
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup15 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIcopy :: Node 
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIsimplifiedName' :: SimplifiedName
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIcopy :: Node 
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIsimplifiedName' :: SimplifiedName
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 68 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 7901 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 68 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyBool)
                   {-# LINE 7906 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 7911 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: S.singleton TyBool), (_rIlabel :==: S.singleton TyBool)]
                   {-# LINE 7916 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 7921 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 7926 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 7931 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 79 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _lIpp >|< text " || " >|< _rIpp
                   {-# LINE 7936 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup15 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 7943 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup15
                   {-# LINE 7948 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 7953 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 7958 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 68 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 7963 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 7968 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 7973 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 7978 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 7983 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 7988 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 7993 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 7998 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 8003 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8008 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Or _lIannotated _rIannotated
                   {-# LINE 8013 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Or _lIcopy _rIcopy
                   {-# LINE 8018 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Or _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 8023 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Or _lIexstractParameters _rIexstractParameters
                   {-# LINE 8028 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Or _lIremoved _rIremoved
                   {-# LINE 8033 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Or _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Or _lIsimplified _rIsimplified
                   {-# LINE 8040 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 8045 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 8050 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 8055 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 8060 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8065 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8072 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIfinal
                   {-# LINE 8077 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIinit
                   {-# LINE 8082 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8087 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8092 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rIsimplifiedName'
                   {-# LINE 8097 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8102 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8107 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8112 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8117 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8122 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8127 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8132 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 8137 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8142 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIcopy,_lIdeclarations,_lIedgeList,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIremoved,_lIself,_lIsimplified,_lIsimplifiedName',_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIcopy,_rIdeclarations,_rIedgeList,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIremoved,_rIself,_rIsimplified,_rIsimplifiedName',_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Param :: T_Node  ->
                  T_Node 
sem_Node_Param e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              _lhsOparamMapping :: (IntMap Node)
              __tup16 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIcopy :: Node 
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIsimplifiedName' :: SimplifiedName
              _eIwarnings :: (Set Warning)
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8217 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8222 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8227 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 8232 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 90 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _eIpp
                   {-# LINE 8237 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.singleton _label _eIcopy
                   {-# LINE 8242 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup16 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_eOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 8249 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup16
                   {-# LINE 8254 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 8259 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 8264 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 8269 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 8274 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _eIedgeList
                   {-# LINE 8279 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 8284 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 8289 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _eInodeList
                   {-# LINE 8294 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eInodes
                   {-# LINE 8299 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _eIwarnings
                   {-# LINE 8304 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Param _eIannotated
                   {-# LINE 8309 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Param _eIcopy
                   {-# LINE 8314 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Param _eIexstractFunctions
                   {-# LINE 8319 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Param _eIexstractParameters
                   {-# LINE 8324 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Param _eIremoved
                   {-# LINE 8329 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Param _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Param _eIsimplified
                   {-# LINE 8336 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 8341 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 8346 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 8351 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 8356 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8361 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8368 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIfinal
                   {-# LINE 8373 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIinit
                   {-# LINE 8378 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 8383 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eImapping
                   {-# LINE 8388 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eIsimplifiedName'
                   {-# LINE 8393 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8398 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8403 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8408 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8413 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIcopy,_eIdeclarations,_eIedgeList,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIremoved,_eIself,_eIsimplified,_eIsimplifiedName',_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Plus :: T_Node  ->
                 T_Node  ->
                 T_Node 
sem_Node_Plus l_ r_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              __tup17 :: ((Label,Label))
              _lOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lOdeclaration :: Declaration
              _lOdeclarations' :: (Map String Declaration)
              _lOmapping :: Mapping
              _lOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _rOdeclaration :: Declaration
              _rOdeclarations' :: (Map String Declaration)
              _rOlabels :: Label
              _rOmapping :: Mapping
              _rOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _lIannotated :: Node 
              _lIblocks :: (IntMap (Block Node))
              _lIcallMapping :: (IntMap Node)
              _lIconstraints :: (Set Constraint)
              _lIcopy :: Node 
              _lIdeclarations :: (Map String Declaration)
              _lIedgeList :: ([UEdge])
              _lIexpected :: (Set Constraint)
              _lIexstractFunctions :: Node 
              _lIexstractParameters :: Node 
              _lIfinal :: (Maybe [Label])
              _lIflow :: Flow
              _lIinit :: (Maybe Label)
              _lIlabel :: Label
              _lIlabels :: Label
              _lImapping :: Mapping
              _lInodeList :: ([LNode String])
              _lInodes :: (IntMap Node)
              _lIparamMapping :: (IntMap Node)
              _lIpp :: Doc
              _lIremoved :: Node 
              _lIself :: Node 
              _lIsimplified :: Node 
              _lIsimplifiedName' :: SimplifiedName
              _lIwarnings :: (Set Warning)
              _rIannotated :: Node 
              _rIblocks :: (IntMap (Block Node))
              _rIcallMapping :: (IntMap Node)
              _rIconstraints :: (Set Constraint)
              _rIcopy :: Node 
              _rIdeclarations :: (Map String Declaration)
              _rIedgeList :: ([UEdge])
              _rIexpected :: (Set Constraint)
              _rIexstractFunctions :: Node 
              _rIexstractParameters :: Node 
              _rIfinal :: (Maybe [Label])
              _rIflow :: Flow
              _rIinit :: (Maybe Label)
              _rIlabel :: Label
              _rIlabels :: Label
              _rImapping :: Mapping
              _rInodeList :: ([LNode String])
              _rInodes :: (IntMap Node)
              _rIparamMapping :: (IntMap Node)
              _rIpp :: Doc
              _rIremoved :: Node 
              _rIself :: Node 
              _rIsimplified :: Node 
              _rIsimplifiedName' :: SimplifiedName
              _rIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 8517 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :==: S.singleton TyInt)
                   {-# LINE 8522 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 69 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8527 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 69 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, _lIlabel, ()), (_label, _rIlabel, ())]
                   {-# LINE 8532 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   foldr ($) _expected_augmented_syn [_expected_augmented_f1]
                   {-# LINE 8537 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_f1 =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.union $ S.fromList [(_lIlabel :==: tyNum), (_rIlabel :==: tyNum)]
                   {-# LINE 8542 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 71 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8547 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 71 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "+")]
                   {-# LINE 8552 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8557 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8562 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8567 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 68 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _lIpp >|< text " + " >|< _rIpp
                   {-# LINE 8572 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup17 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 8579 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup17
                   {-# LINE 8584 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIblocks `IM.union` _rIblocks
                   {-# LINE 8589 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIcallMapping `IM.union` _rIcallMapping
                   {-# LINE 8594 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lIconstraints `S.union` _rIconstraints
                   {-# LINE 8599 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIdeclarations `M.union` _rIdeclarations
                   {-# LINE 8604 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 69 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lIedgeList ++ _rIedgeList
                   {-# LINE 8609 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected_augmented_syn =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIexpected `S.union` _rIexpected
                   {-# LINE 8614 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIflow ++ _rIflow
                   {-# LINE 8619 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 71 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lInodeList ++ _rInodeList
                   {-# LINE 8624 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lInodes `IM.union` _rInodes
                   {-# LINE 8629 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _lIparamMapping `IM.union` _rIparamMapping
                   {-# LINE 8634 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 8639 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _lIwarnings `S.union` _rIwarnings
                   {-# LINE 8644 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Plus _lIannotated _rIannotated
                   {-# LINE 8649 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Plus _lIcopy _rIcopy
                   {-# LINE 8654 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Plus _lIexstractFunctions _rIexstractFunctions
                   {-# LINE 8659 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Plus _lIexstractParameters _rIexstractParameters
                   {-# LINE 8664 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Plus _lIremoved _rIremoved
                   {-# LINE 8669 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Plus _lIself _rIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Plus _lIsimplified _rIsimplified
                   {-# LINE 8676 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 8681 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 8686 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 8691 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 8696 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 8701 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 8708 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIfinal
                   {-# LINE 8713 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIinit
                   {-# LINE 8718 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _rIlabels
                   {-# LINE 8723 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rImapping
                   {-# LINE 8728 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _rIsimplifiedName'
                   {-# LINE 8733 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8738 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8743 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 8748 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8753 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 8758 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 8763 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lIlabels
                   {-# LINE 8768 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lImapping
                   {-# LINE 8773 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _rOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 8778 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _lIannotated,_lIblocks,_lIcallMapping,_lIconstraints,_lIcopy,_lIdeclarations,_lIedgeList,_lIexpected,_lIexstractFunctions,_lIexstractParameters,_lIfinal,_lIflow,_lIinit,_lIlabel,_lIlabels,_lImapping,_lInodeList,_lInodes,_lIparamMapping,_lIpp,_lIremoved,_lIself,_lIsimplified,_lIsimplifiedName',_lIwarnings) =
                  l_ _lOdeclaration _lOdeclarations' _lOlabels _lOmapping _lOsimplifiedName 
              ( _rIannotated,_rIblocks,_rIcallMapping,_rIconstraints,_rIcopy,_rIdeclarations,_rIedgeList,_rIexpected,_rIexstractFunctions,_rIexstractParameters,_rIfinal,_rIflow,_rIinit,_rIlabel,_rIlabels,_rImapping,_rInodeList,_rInodes,_rIparamMapping,_rIpp,_rIremoved,_rIself,_rIsimplified,_rIsimplifiedName',_rIwarnings) =
                  r_ _rOdeclaration _rOdeclarations' _rOlabels _rOmapping _rOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Return :: T_Node  ->
                   T_Node 
sem_Node_Return e_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              _lhsOexstractFunctions :: Node 
              _lhsOmapping :: Mapping
              _lhsOwarnings :: (Set Warning)
              __tup18 :: ((Label,Label))
              _eOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOexpected :: (Set Constraint)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOsimplifiedName' :: SimplifiedName
              _eOdeclaration :: Declaration
              _eOdeclarations' :: (Map String Declaration)
              _eOmapping :: Mapping
              _eOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _eIannotated :: Node 
              _eIblocks :: (IntMap (Block Node))
              _eIcallMapping :: (IntMap Node)
              _eIconstraints :: (Set Constraint)
              _eIcopy :: Node 
              _eIdeclarations :: (Map String Declaration)
              _eIedgeList :: ([UEdge])
              _eIexpected :: (Set Constraint)
              _eIexstractFunctions :: Node 
              _eIexstractParameters :: Node 
              _eIfinal :: (Maybe [Label])
              _eIflow :: Flow
              _eIinit :: (Maybe Label)
              _eIlabel :: Label
              _eIlabels :: Label
              _eImapping :: Mapping
              _eInodeList :: ([LNode String])
              _eInodes :: (IntMap Node)
              _eIparamMapping :: (IntMap Node)
              _eIpp :: Doc
              _eIremoved :: Node 
              _eIself :: Node 
              _eIsimplified :: Node 
              _eIsimplifiedName' :: SimplifiedName
              _eIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 8853 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 8858 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 53 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 8863 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 53 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, _eIlabel, ())]
                   {-# LINE 8868 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 107 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 8873 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 107 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, lx _lhsIdeclaration)]
                   {-# LINE 8878 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 8883 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "return")]
                   {-# LINE 8888 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 8893 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 8898 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 8903 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 8908 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 80 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 8913 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 8918 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 39 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "return " >|< _eIpp
                   {-# LINE 8923 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 66 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   exstractFunctions (Return _eIexstractFunctions) _eIcallMapping
                   {-# LINE 8928 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 56 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eIconstraints
                   {-# LINE 8933 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 92 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   updateMapping ReturnValue                             _eIlabel 0                            _constraints _eImapping
                   {-# LINE 8938 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 20 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _eIexpected
                   {-# LINE 8943 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 43 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _eInodes
                   {-# LINE 8948 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _copy _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 8953 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup18 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_eOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 8960 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup18
                   {-# LINE 8965 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIblocks
                   {-# LINE 8970 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _eIcallMapping
                   {-# LINE 8975 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 8980 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIdeclarations
                   {-# LINE 8985 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 53 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _eIedgeList
                   {-# LINE 8990 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 8995 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 107 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIflow
                   {-# LINE 9000 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _eInodeList
                   {-# LINE 9005 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 9010 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _eIparamMapping
                   {-# LINE 9015 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Return _eIannotated
                   {-# LINE 9020 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Return _eIcopy
                   {-# LINE 9025 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Return _eIexstractFunctions
                   {-# LINE 9030 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Return _eIexstractParameters
                   {-# LINE 9035 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Return _eIremoved
                   {-# LINE 9040 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Return _eIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Return _eIsimplified
                   {-# LINE 9047 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 9052 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 9057 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 9062 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9067 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9074 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _eIlabels
                   {-# LINE 9079 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _eIsimplifiedName'
                   {-# LINE 9084 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9089 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9094 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9099 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _eOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9104 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _eIannotated,_eIblocks,_eIcallMapping,_eIconstraints,_eIcopy,_eIdeclarations,_eIedgeList,_eIexpected,_eIexstractFunctions,_eIexstractParameters,_eIfinal,_eIflow,_eIinit,_eIlabel,_eIlabels,_eImapping,_eInodeList,_eInodes,_eIparamMapping,_eIpp,_eIremoved,_eIself,_eIsimplified,_eIsimplifiedName',_eIwarnings) =
                  e_ _eOdeclaration _eOdeclarations' _eOlabels _eOmapping _eOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Sequence :: T_Node  ->
                     T_Node  ->
                     T_Node 
sem_Node_Sequence f_ s_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              __tup19 :: ((Label,Label))
              _fOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOexpected :: (Set Constraint)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _fOdeclaration :: Declaration
              _fOdeclarations' :: (Map String Declaration)
              _fOmapping :: Mapping
              _fOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _fIannotated :: Node 
              _fIblocks :: (IntMap (Block Node))
              _fIcallMapping :: (IntMap Node)
              _fIconstraints :: (Set Constraint)
              _fIcopy :: Node 
              _fIdeclarations :: (Map String Declaration)
              _fIedgeList :: ([UEdge])
              _fIexpected :: (Set Constraint)
              _fIexstractFunctions :: Node 
              _fIexstractParameters :: Node 
              _fIfinal :: (Maybe [Label])
              _fIflow :: Flow
              _fIinit :: (Maybe Label)
              _fIlabel :: Label
              _fIlabels :: Label
              _fImapping :: Mapping
              _fInodeList :: ([LNode String])
              _fInodes :: (IntMap Node)
              _fIparamMapping :: (IntMap Node)
              _fIpp :: Doc
              _fIremoved :: Node 
              _fIself :: Node 
              _fIsimplified :: Node 
              _fIsimplifiedName' :: SimplifiedName
              _fIwarnings :: (Set Warning)
              _sIannotated :: Node 
              _sIblocks :: (IntMap (Block Node))
              _sIcallMapping :: (IntMap Node)
              _sIconstraints :: (Set Constraint)
              _sIcopy :: Node 
              _sIdeclarations :: (Map String Declaration)
              _sIedgeList :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIexstractFunctions :: Node 
              _sIexstractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIsimplifiedName' :: SimplifiedName
              _sIwarnings :: (Set Warning)
              _lhsOedgeList =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 9208 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, _fIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 9213 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 101 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 9218 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 101 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   (++) $ if isNothing _sIinit || isNothing _fIfinal then [] else [(l, fromJust _sIinit) | l <- fromJust _fIfinal]
                   {-# LINE 9223 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 26 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 9228 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 26 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, ";")]
                   {-# LINE 9233 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9238 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9243 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9248 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _fIinit <|> _sIinit
                   {-# LINE 9253 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 82 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIfinal <|> _fIfinal
                   {-# LINE 9258 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 9263 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _fIpp >|< text ";" >-< _sIpp
                   {-# LINE 9268 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup19 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_fOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 9275 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup19
                   {-# LINE 9280 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _fIblocks `IM.union` _sIblocks
                   {-# LINE 9285 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _fIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 9290 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _fIconstraints `S.union` _sIconstraints
                   {-# LINE 9295 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _fIdeclarations `M.union` _sIdeclarations
                   {-# LINE 9300 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 27 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _fIedgeList ++ _sIedgeList
                   {-# LINE 9305 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _fIexpected `S.union` _sIexpected
                   {-# LINE 9310 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 101 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _fIflow ++ _sIflow
                   {-# LINE 9315 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 26 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _fInodeList ++ _sInodeList
                   {-# LINE 9320 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _fInodes `IM.union` _sInodes
                   {-# LINE 9325 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _fIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 9330 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _fIwarnings `S.union` _sIwarnings
                   {-# LINE 9335 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Sequence _fIannotated _sIannotated
                   {-# LINE 9340 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Sequence _fIcopy _sIcopy
                   {-# LINE 9345 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Sequence _fIexstractFunctions _sIexstractFunctions
                   {-# LINE 9350 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Sequence _fIexstractParameters _sIexstractParameters
                   {-# LINE 9355 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Sequence _fIremoved _sIremoved
                   {-# LINE 9360 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Sequence _fIself _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Sequence _fIsimplified _sIsimplified
                   {-# LINE 9367 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 9372 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 9377 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 9382 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 9387 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9392 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9399 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 9404 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 9409 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _sIsimplifiedName'
                   {-# LINE 9414 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _fOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9419 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _fOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9424 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _fOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9429 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _fOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9434 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 9439 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 9444 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _fIlabels
                   {-# LINE 9449 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _fImapping
                   {-# LINE 9454 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 9459 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _fIannotated,_fIblocks,_fIcallMapping,_fIconstraints,_fIcopy,_fIdeclarations,_fIedgeList,_fIexpected,_fIexstractFunctions,_fIexstractParameters,_fIfinal,_fIflow,_fIinit,_fIlabel,_fIlabels,_fImapping,_fInodeList,_fInodes,_fIparamMapping,_fIpp,_fIremoved,_fIself,_fIsimplified,_fIsimplifiedName',_fIwarnings) =
                  f_ _fOdeclaration _fOdeclarations' _fOlabels _fOmapping _fOsimplifiedName 
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIcopy,_sIdeclarations,_sIedgeList,_sIexpected,_sIexstractFunctions,_sIexstractParameters,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sImapping,_sInodeList,_sInodes,_sIparamMapping,_sIpp,_sIremoved,_sIself,_sIsimplified,_sIsimplifiedName',_sIwarnings) =
                  s_ _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_ShortOpenTag :: T_Node 
sem_Node_ShortOpenTag  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 9501 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 9506 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 9511 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 9516 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 9521 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 9526 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 9531 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 9536 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 24 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 9541 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 9546 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   P.empty
                   {-# LINE 9551 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 9556 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   ShortOpenTag
                   {-# LINE 9561 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   ShortOpenTag
                   {-# LINE 9566 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ShortOpenTag
                   {-# LINE 9571 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ShortOpenTag
                   {-# LINE 9576 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ShortOpenTag
                   {-# LINE 9581 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  ShortOpenTag
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   ShortOpenTag
                   {-# LINE 9588 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 9593 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 9598 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 9603 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 9608 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9613 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9620 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.ShortOpenTag.lhs.final"
                   {-# LINE 9625 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.ShortOpenTag.lhs.init"
                   {-# LINE 9630 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.ShortOpenTag.lhs.label"
                   {-# LINE 9635 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 9640 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9645 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.ShortOpenTag.lhs.simplifiedName'"
                   {-# LINE 9650 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Simple :: String ->
                   T_Node 
sem_Node_Simple value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOconstraints :: (Set Constraint)
              __tup20 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOmapping :: Mapping
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9691 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9696 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 9701 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 9706 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text value_
                   {-# LINE 9711 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 42 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _simplifiedName
                   {-# LINE 9716 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _simplifiedName =
                  ({-# LINE 43 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   foldr ($) (Name value_) _lhsIsimplifiedName
                   {-# LINE 9721 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 74 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   case M.lookup (Identifier value_) _lhsImapping of
                       Just c  -> S.singleton (_label :==: fromArrayRepeatedly (levels _simplifiedName    ) c)
                       Nothing -> S.singleton (_label :==: S.empty)
                   {-# LINE 9728 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup20 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lhsOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 9735 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup20
                   {-# LINE 9740 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 9745 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 9750 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 9755 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 9760 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 9765 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 9770 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 9775 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 9780 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 9785 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 9790 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Simple value_
                   {-# LINE 9795 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Simple value_
                   {-# LINE 9800 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 9805 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 9810 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 9815 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Simple value_
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Simple value_
                   {-# LINE 9822 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 9827 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 9832 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 9837 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 9842 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 9847 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 9854 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.Simple.lhs.final"
                   {-# LINE 9859 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.Simple.lhs.init"
                   {-# LINE 9864 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 9869 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_SimplifiedFunctionCall :: String ->
                                   T_ParamList  ->
                                   (Maybe Node) ->
                                   T_Node 
sem_Node_SimplifiedFunctionCall name_ params_ result_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              __tup21 :: ((Label,Label,Label,Label,Label))
              _paramsOlabels :: Label
              _la :: Label
              _lb :: Label
              _lc :: Label
              _lr :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOpp :: Doc
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _paramsIannotated :: ParamList 
              _paramsIcallMapping :: (IntMap Node)
              _paramsIcopy :: ParamList 
              _paramsIexstractFunctions :: ParamList 
              _paramsIexstractParameters :: ParamList 
              _paramsIlabel :: Label
              _paramsIlabels :: Label
              _paramsIparamMapping :: (IntMap Node)
              _paramsIpp :: Doc
              _paramsIremoved :: ParamList 
              _paramsIself :: ParamList 
              _paramsIsimplified :: ParamList 
              _lhsOblocks =
                  ({-# LINE 153 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 9928 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 153 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.fromList [(_lb, Normal Skip)
                                  ,(_lc, Call _lc _lr _self)
                                  ,(_lr, F.Return _lc _lr _self)
                                  ,(_la, Normal Skip)]
                   {-# LINE 9936 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 111 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 9941 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 111 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   let (Declaration name ln lx) = lookupDeclaration name_ _lhsIdeclarations'
                   in (++) [(_lb, _la), (_lb, _lc), (_lr, _la), (_lc, ln), (lx, _lr)]
                   {-# LINE 9947 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 23 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 9952 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 23 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_lc, name_ ++ "() [lb: " ++ show _lb ++ ", lc: " ++ show _lc ++ ", lr: " ++ show _lr ++ ", la: " ++ show _la ++ "]")]
                   {-# LINE 9957 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 42 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 9962 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 42 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 9967 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 56 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just _lb
                   {-# LINE 9972 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 78 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just [_la]
                   {-# LINE 9977 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 41 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   case result_ of
                      Just v  -> pp v >|< text " := " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                      Nothing -> text ":: " >|< text name_ >|< text "(" >|< _paramsIpp >|< text ")"
                   {-# LINE 9984 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 70 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   exstractFunctions (SimplifiedFunctionCall name_ _paramsIexstractFunctions result_) _paramsIcallMapping
                   {-# LINE 9989 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 92 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   exstractParameters _copy _paramsIparamMapping
                   {-# LINE 9994 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _label =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _lc
                   {-# LINE 9999 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup21 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, la) -> case nextUnique __cont of { (__cont, lb) -> case nextUnique __cont of { (__cont, lc) -> case nextUnique __cont of { (__cont, lr) -> (__cont, la,lb,lc,lr)}}}}}
              (_paramsOlabels,_,_,_,_) =
                  ({-# LINE 41 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 10006 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_la,_,_,_) =
                  ({-# LINE 41 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 10011 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_,_lb,_,_) =
                  ({-# LINE 38 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 10016 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_,_,_lc,_) =
                  ({-# LINE 39 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 10021 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_,_,_,_lr) =
                  ({-# LINE 40 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup21
                   {-# LINE 10026 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 153 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10031 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _paramsIcallMapping
                   {-# LINE 10036 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 10041 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 10046 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 10051 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10056 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 111 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 10061 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 23 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 10066 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 42 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10071 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _paramsIparamMapping
                   {-# LINE 10076 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 11 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 10081 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10086 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIannotated result_
                   {-# LINE 10091 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIcopy result_
                   {-# LINE 10096 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIexstractFunctions result_
                   {-# LINE 10101 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIexstractParameters result_
                   {-# LINE 10106 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIremoved result_
                   {-# LINE 10111 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  SimplifiedFunctionCall name_ _paramsIself result_
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   SimplifiedFunctionCall name_ _paramsIsimplified result_
                   {-# LINE 10118 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 10123 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 10128 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10133 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10140 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10145 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _paramsIlabels
                   {-# LINE 10150 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10155 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.SimplifiedFunctionCall.lhs.simplifiedName'"
                   {-# LINE 10160 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _paramsIannotated,_paramsIcallMapping,_paramsIcopy,_paramsIexstractFunctions,_paramsIexstractParameters,_paramsIlabel,_paramsIlabels,_paramsIparamMapping,_paramsIpp,_paramsIremoved,_paramsIself,_paramsIsimplified) =
                  params_ _paramsOlabels 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Skip :: T_Node 
sem_Node_Skip  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              __tup22 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOblocks =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 10202 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 10207 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 41 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10212 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 41 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "[skip]")]
                   {-# LINE 10217 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10222 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10227 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10232 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 10237 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 80 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 10242 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 10247 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   P.empty
                   {-# LINE 10252 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup22 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lhsOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 10259 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup22
                   {-# LINE 10264 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10269 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 10274 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 10279 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 10284 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 10289 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10294 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 10299 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 41 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 10304 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10309 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 10314 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10319 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Skip
                   {-# LINE 10324 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Skip
                   {-# LINE 10329 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 10334 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 10339 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 10344 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Skip
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Skip
                   {-# LINE 10351 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 10356 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 10361 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 10366 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 10371 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10376 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10383 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10388 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.Skip.lhs.simplifiedName'"
                   {-# LINE 10393 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_String :: String ->
                   T_Node 
sem_Node_String value_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              __tup23 :: ((Label,Label))
              _lhsOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10434 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10439 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10444 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 10449 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 56 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "\"" >|< text value_ >|< text "\""
                   {-# LINE 10454 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup23 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_lhsOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 10461 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup23
                   {-# LINE 10466 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10471 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 10476 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.empty
                   {-# LINE 10481 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   M.empty
                   {-# LINE 10486 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 10491 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10496 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 10501 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 12 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   []
                   {-# LINE 10506 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.empty
                   {-# LINE 10511 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 10516 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.empty
                   {-# LINE 10521 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   String value_
                   {-# LINE 10526 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   String value_
                   {-# LINE 10531 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 10536 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 10541 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 10546 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  String value_
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   String value_
                   {-# LINE 10553 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 10558 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 10563 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 10568 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 10573 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10578 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10585 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.String.lhs.final"
                   {-# LINE 10590 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: Node.String.lhs.init"
                   {-# LINE 10595 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10600 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: Node.String.lhs.simplifiedName'"
                   {-# LINE 10605 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_Variable :: T_Node  ->
                     T_Node 
sem_Node_Variable n_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOconstraints :: (Set Constraint)
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOpp :: Doc
              _lhsOsimplifiedName' :: SimplifiedName
              __tup24 :: ((Label,Label))
              _nOlabels :: Label
              _label :: Label
              _lhsOblocks :: (IntMap (Block Node))
              _lhsOcallMapping :: (IntMap Node)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOedgeList :: ([UEdge])
              _lhsOexpected :: (Set Constraint)
              _lhsOflow :: Flow
              _lhsOparamMapping :: (IntMap Node)
              _lhsOwarnings :: (Set Warning)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractFunctions :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOfinal :: (Maybe [Label])
              _lhsOinit :: (Maybe Label)
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _nOdeclaration :: Declaration
              _nOdeclarations' :: (Map String Declaration)
              _nOmapping :: Mapping
              _nOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _nIannotated :: Node 
              _nIblocks :: (IntMap (Block Node))
              _nIcallMapping :: (IntMap Node)
              _nIconstraints :: (Set Constraint)
              _nIcopy :: Node 
              _nIdeclarations :: (Map String Declaration)
              _nIedgeList :: ([UEdge])
              _nIexpected :: (Set Constraint)
              _nIexstractFunctions :: Node 
              _nIexstractParameters :: Node 
              _nIfinal :: (Maybe [Label])
              _nIflow :: Flow
              _nIinit :: (Maybe Label)
              _nIlabel :: Label
              _nIlabels :: Label
              _nImapping :: Mapping
              _nInodeList :: ([LNode String])
              _nInodes :: (IntMap Node)
              _nIparamMapping :: (IntMap Node)
              _nIpp :: Doc
              _nIremoved :: Node 
              _nIself :: Node 
              _nIsimplified :: Node 
              _nIsimplifiedName' :: SimplifiedName
              _nIwarnings :: (Set Warning)
              _lhsOconstraints =
                  ({-# LINE 70 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   foldr ($) _constraints_augmented_syn [_constraints_augmented_f1]
                   {-# LINE 10676 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_f1 =
                  ({-# LINE 70 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   S.union $ S.singleton (_label :<=: _nIlabel)
                   {-# LINE 10681 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 56 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 10686 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 56 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, render _pp    )]
                   {-# LINE 10691 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 10696 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 10701 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 10706 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 10711 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 48 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "$" >|< _nIpp
                   {-# LINE 10716 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 40 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nIsimplifiedName'
                   {-# LINE 10721 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup24 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_nOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 10728 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup24
                   {-# LINE 10733 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOblocks =
                  ({-# LINE 147 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIblocks
                   {-# LINE 10738 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _nIcallMapping
                   {-# LINE 10743 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints_augmented_syn =
                  ({-# LINE 70 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nIconstraints
                   {-# LINE 10748 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIdeclarations
                   {-# LINE 10753 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _nIedgeList
                   {-# LINE 10758 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _nIexpected
                   {-# LINE 10763 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 97 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIflow
                   {-# LINE 10768 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 56 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _nInodeList
                   {-# LINE 10773 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nInodes
                   {-# LINE 10778 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _nIparamMapping
                   {-# LINE 10783 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _nIwarnings
                   {-# LINE 10788 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   Variable _nIannotated
                   {-# LINE 10793 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Variable _nIcopy
                   {-# LINE 10798 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Variable _nIexstractFunctions
                   {-# LINE 10803 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Variable _nIexstractParameters
                   {-# LINE 10808 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Variable _nIremoved
                   {-# LINE 10813 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  Variable _nIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   Variable _nIsimplified
                   {-# LINE 10820 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 10825 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 10830 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 10835 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 10840 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 10845 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 10852 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 72 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIfinal
                   {-# LINE 10857 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 50 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIinit
                   {-# LINE 10862 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nIlabels
                   {-# LINE 10867 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _nImapping
                   {-# LINE 10872 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 10877 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 10882 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 10887 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 10892 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _nIannotated,_nIblocks,_nIcallMapping,_nIconstraints,_nIcopy,_nIdeclarations,_nIedgeList,_nIexpected,_nIexstractFunctions,_nIexstractParameters,_nIfinal,_nIflow,_nIinit,_nIlabel,_nIlabels,_nImapping,_nInodeList,_nInodes,_nIparamMapping,_nIpp,_nIremoved,_nIself,_nIsimplified,_nIsimplifiedName',_nIwarnings) =
                  n_ _nOdeclaration _nOdeclarations' _nOlabels _nOmapping _nOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
sem_Node_While :: T_Node  ->
                  T_Node  ->
                  T_Node 
sem_Node_While c_ s_  =
    (\ _lhsIdeclaration
       _lhsIdeclarations'
       _lhsIlabels
       _lhsImapping
       _lhsIsimplifiedName ->
         (let _lhsOblocks :: (IntMap (Block Node))
              _lhsOedgeList :: ([UEdge])
              _lhsOflow :: Flow
              _lhsOnodeList :: ([LNode String])
              _lhsOnodes :: (IntMap Node)
              _lhsOlabel :: Label
              _lhsOinit :: (Maybe Label)
              _lhsOfinal :: (Maybe [Label])
              _lhsOpp :: Doc
              _lhsOexstractFunctions :: Node 
              _lhsOexpected :: (Set Constraint)
              _lhsOwarnings :: (Set Warning)
              __tup25 :: ((Label,Label))
              _cOlabels :: Label
              _label :: Label
              _lhsOcallMapping :: (IntMap Node)
              _lhsOconstraints :: (Set Constraint)
              _lhsOdeclarations :: (Map String Declaration)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: Node 
              _lhsOcopy :: Node 
              _lhsOexstractParameters :: Node 
              _lhsOremoved :: Node 
              _lhsOself :: Node 
              _lhsOsimplified :: Node 
              _lhsOlabels :: Label
              _lhsOmapping :: Mapping
              _lhsOsimplifiedName' :: SimplifiedName
              _cOdeclaration :: Declaration
              _cOdeclarations' :: (Map String Declaration)
              _cOmapping :: Mapping
              _cOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _sOdeclaration :: Declaration
              _sOdeclarations' :: (Map String Declaration)
              _sOlabels :: Label
              _sOmapping :: Mapping
              _sOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _cIannotated :: Node 
              _cIblocks :: (IntMap (Block Node))
              _cIcallMapping :: (IntMap Node)
              _cIconstraints :: (Set Constraint)
              _cIcopy :: Node 
              _cIdeclarations :: (Map String Declaration)
              _cIedgeList :: ([UEdge])
              _cIexpected :: (Set Constraint)
              _cIexstractFunctions :: Node 
              _cIexstractParameters :: Node 
              _cIfinal :: (Maybe [Label])
              _cIflow :: Flow
              _cIinit :: (Maybe Label)
              _cIlabel :: Label
              _cIlabels :: Label
              _cImapping :: Mapping
              _cInodeList :: ([LNode String])
              _cInodes :: (IntMap Node)
              _cIparamMapping :: (IntMap Node)
              _cIpp :: Doc
              _cIremoved :: Node 
              _cIself :: Node 
              _cIsimplified :: Node 
              _cIsimplifiedName' :: SimplifiedName
              _cIwarnings :: (Set Warning)
              _sIannotated :: Node 
              _sIblocks :: (IntMap (Block Node))
              _sIcallMapping :: (IntMap Node)
              _sIconstraints :: (Set Constraint)
              _sIcopy :: Node 
              _sIdeclarations :: (Map String Declaration)
              _sIedgeList :: ([UEdge])
              _sIexpected :: (Set Constraint)
              _sIexstractFunctions :: Node 
              _sIexstractParameters :: Node 
              _sIfinal :: (Maybe [Label])
              _sIflow :: Flow
              _sIinit :: (Maybe Label)
              _sIlabel :: Label
              _sIlabels :: Label
              _sImapping :: Mapping
              _sInodeList :: ([LNode String])
              _sInodes :: (IntMap Node)
              _sIparamMapping :: (IntMap Node)
              _sIpp :: Doc
              _sIremoved :: Node 
              _sIself :: Node 
              _sIsimplified :: Node 
              _sIsimplifiedName' :: SimplifiedName
              _sIwarnings :: (Set Warning)
              _lhsOblocks =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _blocks_augmented_syn [_blocks_augmented_f1]
                   {-# LINE 10996 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_f1 =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label $ Normal _self
                   {-# LINE 11001 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOedgeList =
                  ({-# LINE 49 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _edgeList_augmented_syn [_edgeList_augmented_f1]
                   {-# LINE 11006 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_f1 =
                  ({-# LINE 49 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, _cIlabel, ()), (_label, _sIlabel, ())]
                   {-# LINE 11011 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 105 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 11016 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 105 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   (++) $ [(_label, fromJust _sIinit)] ++ [(l', _label) | l' <- fromJust _sIfinal]
                   {-# LINE 11021 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodeList =
                  ({-# LINE 48 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   foldr ($) _nodeList_augmented_syn [_nodeList_augmented_f1]
                   {-# LINE 11026 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_f1 =
                  ({-# LINE 48 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   (++) [(_label, "while")]
                   {-# LINE 11031 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOnodes =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   foldr ($) _nodes_augmented_syn [_nodes_augmented_f1]
                   {-# LINE 11036 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_f1 =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   IM.union $ IM.singleton _label _self
                   {-# LINE 11041 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 29 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _label
                   {-# LINE 11046 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just _label
                   {-# LINE 11051 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 80 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   Just [_label]
                   {-# LINE 11056 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _pp
                   {-# LINE 11061 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _pp =
                  ({-# LINE 37 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   text "while (" >|< _cIpp >|< text ") {" >-< indent 4 _sIpp >-< text "}"
                   {-# LINE 11066 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 68 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   exstractFunctions (While _cIexstractFunctions _sIexstractFunctions) _cIcallMapping
                   {-# LINE 11071 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _constraints =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _cIconstraints
                   {-# LINE 11076 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _expected =
                  ({-# LINE 22 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.fromList [(_cIlabel :==: S.singleton TyBool)] `S.union` _cIexpected
                   {-# LINE 11081 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexpected =
                  ({-# LINE 23 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _expected
                   {-# LINE 11086 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes =
                  ({-# LINE 45 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   _cInodes
                   {-# LINE 11091 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOwarnings =
                  ({-# LINE 58 "src/MF/Languages/PHP/AG/Checking.ag" #-}
                   S.map (toWarning _copy _nodes     _constraints    ) (violatedConstraints  _constraints     _expected    )
                   {-# LINE 11096 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              __tup25 =
                  case _lhsIlabels of { __cont | __cont `seq` True -> case nextUnique __cont of { (__cont, label) -> (__cont, label)}}
              (_cOlabels,_) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 11103 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              (_,_label) =
                  ({-# LINE 28 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   __tup25
                   {-# LINE 11108 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _blocks_augmented_syn =
                  ({-# LINE 151 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _cIblocks `IM.union` _sIblocks
                   {-# LINE 11113 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _cIcallMapping `IM.union` _sIcallMapping
                   {-# LINE 11118 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOconstraints =
                  ({-# LINE 52 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _constraints
                   {-# LINE 11123 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOdeclarations =
                  ({-# LINE 129 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _cIdeclarations `M.union` _sIdeclarations
                   {-# LINE 11128 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _edgeList_augmented_syn =
                  ({-# LINE 49 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _cIedgeList ++ _sIedgeList
                   {-# LINE 11133 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 105 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _cIflow ++ _sIflow
                   {-# LINE 11138 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodeList_augmented_syn =
                  ({-# LINE 48 "src/MF/Languages/PHP/AG/Visualizer.ag" #-}
                   _cInodeList ++ _sInodeList
                   {-# LINE 11143 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _nodes_augmented_syn =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _nodes
                   {-# LINE 11148 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _cIparamMapping `IM.union` _sIparamMapping
                   {-# LINE 11153 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   While _cIannotated _sIannotated
                   {-# LINE 11158 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   While _cIcopy _sIcopy
                   {-# LINE 11163 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   While _cIexstractFunctions _sIexstractFunctions
                   {-# LINE 11168 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   While _cIexstractParameters _sIexstractParameters
                   {-# LINE 11173 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   While _cIremoved _sIremoved
                   {-# LINE 11178 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  While _cIself _sIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   While _cIsimplified _sIsimplified
                   {-# LINE 11185 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 11190 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 11195 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 11200 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11205 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11212 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _sIlabels
                   {-# LINE 11217 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOmapping =
                  ({-# LINE 85 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _sImapping
                   {-# LINE 11222 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOsimplifiedName' =
                  ({-# LINE 31 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _sIsimplifiedName'
                   {-# LINE 11227 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _cOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 11232 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _cOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 11237 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _cOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsImapping
                   {-# LINE 11242 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _cOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 11247 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclaration
                   {-# LINE 11252 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIdeclarations'
                   {-# LINE 11257 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _cIlabels
                   {-# LINE 11262 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _cImapping
                   {-# LINE 11267 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _sOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   _lhsIsimplifiedName
                   {-# LINE 11272 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _cIannotated,_cIblocks,_cIcallMapping,_cIconstraints,_cIcopy,_cIdeclarations,_cIedgeList,_cIexpected,_cIexstractFunctions,_cIexstractParameters,_cIfinal,_cIflow,_cIinit,_cIlabel,_cIlabels,_cImapping,_cInodeList,_cInodes,_cIparamMapping,_cIpp,_cIremoved,_cIself,_cIsimplified,_cIsimplifiedName',_cIwarnings) =
                  c_ _cOdeclaration _cOdeclarations' _cOlabels _cOmapping _cOsimplifiedName 
              ( _sIannotated,_sIblocks,_sIcallMapping,_sIconstraints,_sIcopy,_sIdeclarations,_sIedgeList,_sIexpected,_sIexstractFunctions,_sIexstractParameters,_sIfinal,_sIflow,_sIinit,_sIlabel,_sIlabels,_sImapping,_sInodeList,_sInodes,_sIparamMapping,_sIpp,_sIremoved,_sIself,_sIsimplified,_sIsimplifiedName',_sIwarnings) =
                  s_ _sOdeclaration _sOdeclarations' _sOlabels _sOmapping _sOsimplifiedName 
          in  ( _lhsOannotated,_lhsOblocks,_lhsOcallMapping,_lhsOconstraints,_lhsOcopy,_lhsOdeclarations,_lhsOedgeList,_lhsOexpected,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOlabel,_lhsOlabels,_lhsOmapping,_lhsOnodeList,_lhsOnodes,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOsimplifiedName',_lhsOwarnings)))
-- OptionalString ----------------------------------------------
data OptionalString  = None 
                     | Some (String) 
                     deriving ( Eq,Ord,Show)
-- cata
sem_OptionalString :: OptionalString  ->
                      T_OptionalString 
sem_OptionalString (None )  =
    (sem_OptionalString_None )
sem_OptionalString (Some _value )  =
    (sem_OptionalString_Some _value )
-- semantic domain
type T_OptionalString  = ( OptionalString ,OptionalString ,OptionalString ,OptionalString ,(IntMap Node),OptionalString ,OptionalString ,OptionalString ,String)
data Inh_OptionalString  = Inh_OptionalString {}
data Syn_OptionalString  = Syn_OptionalString {annotated_Syn_OptionalString :: OptionalString ,copy_Syn_OptionalString :: OptionalString ,exstractFunctions_Syn_OptionalString :: OptionalString ,exstractParameters_Syn_OptionalString :: OptionalString ,paramMapping_Syn_OptionalString :: (IntMap Node),removed_Syn_OptionalString :: OptionalString ,self_Syn_OptionalString :: OptionalString ,simplified_Syn_OptionalString :: OptionalString ,value_Syn_OptionalString :: String}
wrap_OptionalString :: T_OptionalString  ->
                       Inh_OptionalString  ->
                       Syn_OptionalString 
wrap_OptionalString sem (Inh_OptionalString )  =
    (let ( _lhsOannotated,_lhsOcopy,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOparamMapping,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOvalue) = sem 
     in  (Syn_OptionalString _lhsOannotated _lhsOcopy _lhsOexstractFunctions _lhsOexstractParameters _lhsOparamMapping _lhsOremoved _lhsOself _lhsOsimplified _lhsOvalue ))
sem_OptionalString_None :: T_OptionalString 
sem_OptionalString_None  =
    (let _lhsOvalue :: String
         _lhsOparamMapping :: (IntMap Node)
         _lhsOannotated :: OptionalString 
         _lhsOcopy :: OptionalString 
         _lhsOexstractFunctions :: OptionalString 
         _lhsOexstractParameters :: OptionalString 
         _lhsOremoved :: OptionalString 
         _lhsOself :: OptionalString 
         _lhsOsimplified :: OptionalString 
         _lhsOvalue =
             ({-# LINE 27 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              ""
              {-# LINE 11314 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 11319 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
              None
              {-# LINE 11324 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _copy =
             ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
              None
              {-# LINE 11329 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _exstractFunctions =
             ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 11334 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _exstractParameters =
             ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 11339 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 11344 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _self =
             None
         _simplified =
             ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              None
              {-# LINE 11351 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
              _annotated
              {-# LINE 11356 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
              _copy
              {-# LINE 11361 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOexstractFunctions =
             ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              _exstractFunctions
              {-# LINE 11366 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOexstractParameters =
             ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              _exstractParameters
              {-# LINE 11371 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 11376 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 11383 "src/MF/Languages/PHP/AG.hs" #-}
              )
     in  ( _lhsOannotated,_lhsOcopy,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOparamMapping,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOvalue))
sem_OptionalString_Some :: String ->
                           T_OptionalString 
sem_OptionalString_Some value_  =
    (let _lhsOvalue :: String
         _lhsOparamMapping :: (IntMap Node)
         _lhsOannotated :: OptionalString 
         _lhsOcopy :: OptionalString 
         _lhsOexstractFunctions :: OptionalString 
         _lhsOexstractParameters :: OptionalString 
         _lhsOremoved :: OptionalString 
         _lhsOself :: OptionalString 
         _lhsOsimplified :: OptionalString 
         _lhsOvalue =
             ({-# LINE 25 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              value_
              {-# LINE 11401 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOparamMapping =
             ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              IM.empty
              {-# LINE 11406 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _annotated =
             ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
              Some value_
              {-# LINE 11411 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _copy =
             ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
              Some value_
              {-# LINE 11416 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _exstractFunctions =
             ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 11421 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _exstractParameters =
             ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 11426 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _removed =
             ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 11431 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _self =
             Some value_
         _simplified =
             ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              Some value_
              {-# LINE 11438 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOannotated =
             ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
              _annotated
              {-# LINE 11443 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
              _copy
              {-# LINE 11448 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOexstractFunctions =
             ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              _exstractFunctions
              {-# LINE 11453 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOexstractParameters =
             ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              _exstractParameters
              {-# LINE 11458 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOremoved =
             ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              _removed
              {-# LINE 11463 "src/MF/Languages/PHP/AG.hs" #-}
              )
         _lhsOself =
             _self
         _lhsOsimplified =
             ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
              _simplified
              {-# LINE 11470 "src/MF/Languages/PHP/AG.hs" #-}
              )
     in  ( _lhsOannotated,_lhsOcopy,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOparamMapping,_lhsOremoved,_lhsOself,_lhsOsimplified,_lhsOvalue))
-- ParamList ---------------------------------------------------
type ParamList  = [Node ]
-- cata
sem_ParamList :: ParamList  ->
                 T_ParamList 
sem_ParamList list  =
    (Prelude.foldr sem_ParamList_Cons sem_ParamList_Nil (Prelude.map sem_Node list) )
-- semantic domain
type T_ParamList  = Label ->
                    ( ParamList ,(IntMap Node),ParamList ,ParamList ,ParamList ,Label,Label,(IntMap Node),Doc,ParamList ,ParamList ,ParamList )
data Inh_ParamList  = Inh_ParamList {labels_Inh_ParamList :: Label}
data Syn_ParamList  = Syn_ParamList {annotated_Syn_ParamList :: ParamList ,callMapping_Syn_ParamList :: (IntMap Node),copy_Syn_ParamList :: ParamList ,exstractFunctions_Syn_ParamList :: ParamList ,exstractParameters_Syn_ParamList :: ParamList ,label_Syn_ParamList :: Label,labels_Syn_ParamList :: Label,paramMapping_Syn_ParamList :: (IntMap Node),pp_Syn_ParamList :: Doc,removed_Syn_ParamList :: ParamList ,self_Syn_ParamList :: ParamList ,simplified_Syn_ParamList :: ParamList }
wrap_ParamList :: T_ParamList  ->
                  Inh_ParamList  ->
                  Syn_ParamList 
wrap_ParamList sem (Inh_ParamList _lhsIlabels )  =
    (let ( _lhsOannotated,_lhsOcallMapping,_lhsOcopy,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOlabel,_lhsOlabels,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified) = sem _lhsIlabels 
     in  (Syn_ParamList _lhsOannotated _lhsOcallMapping _lhsOcopy _lhsOexstractFunctions _lhsOexstractParameters _lhsOlabel _lhsOlabels _lhsOparamMapping _lhsOpp _lhsOremoved _lhsOself _lhsOsimplified ))
sem_ParamList_Cons :: T_Node  ->
                      T_ParamList  ->
                      T_ParamList 
sem_ParamList_Cons hd_ tl_  =
    (\ _lhsIlabels ->
         (let _lhsOpp :: Doc
              _lhsOcallMapping :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: ParamList 
              _lhsOcopy :: ParamList 
              _lhsOexstractFunctions :: ParamList 
              _lhsOexstractParameters :: ParamList 
              _lhsOremoved :: ParamList 
              _lhsOself :: ParamList 
              _lhsOsimplified :: ParamList 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _hdOdeclaration :: Declaration
              _hdOdeclarations' :: (Map String Declaration)
              _hdOlabels :: Label
              _hdOmapping :: Mapping
              _hdOsimplifiedName :: ([SimplifiedName -> SimplifiedName])
              _tlOlabels :: Label
              _hdIannotated :: Node 
              _hdIblocks :: (IntMap (Block Node))
              _hdIcallMapping :: (IntMap Node)
              _hdIconstraints :: (Set Constraint)
              _hdIcopy :: Node 
              _hdIdeclarations :: (Map String Declaration)
              _hdIedgeList :: ([UEdge])
              _hdIexpected :: (Set Constraint)
              _hdIexstractFunctions :: Node 
              _hdIexstractParameters :: Node 
              _hdIfinal :: (Maybe [Label])
              _hdIflow :: Flow
              _hdIinit :: (Maybe Label)
              _hdIlabel :: Label
              _hdIlabels :: Label
              _hdImapping :: Mapping
              _hdInodeList :: ([LNode String])
              _hdInodes :: (IntMap Node)
              _hdIparamMapping :: (IntMap Node)
              _hdIpp :: Doc
              _hdIremoved :: Node 
              _hdIself :: Node 
              _hdIsimplified :: Node 
              _hdIsimplifiedName' :: SimplifiedName
              _hdIwarnings :: (Set Warning)
              _tlIannotated :: ParamList 
              _tlIcallMapping :: (IntMap Node)
              _tlIcopy :: ParamList 
              _tlIexstractFunctions :: ParamList 
              _tlIexstractParameters :: ParamList 
              _tlIlabel :: Label
              _tlIlabels :: Label
              _tlIparamMapping :: (IntMap Node)
              _tlIpp :: Doc
              _tlIremoved :: ParamList 
              _tlIself :: ParamList 
              _tlIsimplified :: ParamList 
              _lhsOpp =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   _hdIpp >|< text "," >|< _tlIpp
                   {-# LINE 11554 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _hdIcallMapping `IM.union` _tlIcallMapping
                   {-# LINE 11559 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _hdIparamMapping `IM.union` _tlIparamMapping
                   {-# LINE 11564 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   (:) _hdIannotated _tlIannotated
                   {-# LINE 11569 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   (:) _hdIcopy _tlIcopy
                   {-# LINE 11574 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   (:) _hdIexstractFunctions _tlIexstractFunctions
                   {-# LINE 11579 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   (:) _hdIexstractParameters _tlIexstractParameters
                   {-# LINE 11584 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   (:) _hdIremoved _tlIremoved
                   {-# LINE 11589 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   (:) _hdIsimplified _tlIsimplified
                   {-# LINE 11596 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 11601 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 11606 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 11611 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 11616 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11621 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11628 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _tlIlabel
                   {-# LINE 11633 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _tlIlabels
                   {-# LINE 11638 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _hdOdeclaration =
                  ({-# LINE 132 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: ParamList.Cons.hd.declaration"
                   {-# LINE 11643 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _hdOdeclarations' =
                  ({-# LINE 130 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: ParamList.Cons.hd.declarations'"
                   {-# LINE 11648 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _hdOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 11653 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _hdOmapping =
                  ({-# LINE 86 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: ParamList.Cons.hd.mapping"
                   {-# LINE 11658 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _hdOsimplifiedName =
                  ({-# LINE 30 "src/MF/Languages/PHP/AG/Typing.ag" #-}
                   error "missing rule: ParamList.Cons.hd.simplifiedName"
                   {-# LINE 11663 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _tlOlabels =
                  ({-# LINE 16 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _hdIlabels
                   {-# LINE 11668 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              ( _hdIannotated,_hdIblocks,_hdIcallMapping,_hdIconstraints,_hdIcopy,_hdIdeclarations,_hdIedgeList,_hdIexpected,_hdIexstractFunctions,_hdIexstractParameters,_hdIfinal,_hdIflow,_hdIinit,_hdIlabel,_hdIlabels,_hdImapping,_hdInodeList,_hdInodes,_hdIparamMapping,_hdIpp,_hdIremoved,_hdIself,_hdIsimplified,_hdIsimplifiedName',_hdIwarnings) =
                  hd_ _hdOdeclaration _hdOdeclarations' _hdOlabels _hdOmapping _hdOsimplifiedName 
              ( _tlIannotated,_tlIcallMapping,_tlIcopy,_tlIexstractFunctions,_tlIexstractParameters,_tlIlabel,_tlIlabels,_tlIparamMapping,_tlIpp,_tlIremoved,_tlIself,_tlIsimplified) =
                  tl_ _tlOlabels 
          in  ( _lhsOannotated,_lhsOcallMapping,_lhsOcopy,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOlabel,_lhsOlabels,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified)))
sem_ParamList_Nil :: T_ParamList 
sem_ParamList_Nil  =
    (\ _lhsIlabels ->
         (let _lhsOpp :: Doc
              _lhsOcallMapping :: (IntMap Node)
              _lhsOparamMapping :: (IntMap Node)
              _lhsOannotated :: ParamList 
              _lhsOcopy :: ParamList 
              _lhsOexstractFunctions :: ParamList 
              _lhsOexstractParameters :: ParamList 
              _lhsOremoved :: ParamList 
              _lhsOself :: ParamList 
              _lhsOsimplified :: ParamList 
              _lhsOlabel :: Label
              _lhsOlabels :: Label
              _lhsOpp =
                  ({-# LINE 18 "src/MF/Languages/PHP/AG/PP.ag" #-}
                   P.empty
                   {-# LINE 11693 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcallMapping =
                  ({-# LINE 51 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11698 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOparamMapping =
                  ({-# LINE 81 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   IM.empty
                   {-# LINE 11703 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _annotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   []
                   {-# LINE 11708 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _copy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   []
                   {-# LINE 11713 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 11718 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _exstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 11723 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _removed =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 11728 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _self =
                  []
              _simplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   []
                   {-# LINE 11735 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOannotated =
                  ({-# LINE 6 "src/MF/Languages/PHP/AG/Debugging.ag" #-}
                   _annotated
                   {-# LINE 11740 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 167 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _copy
                   {-# LINE 11745 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractFunctions =
                  ({-# LINE 60 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractFunctions
                   {-# LINE 11750 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOexstractParameters =
                  ({-# LINE 88 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _exstractParameters
                   {-# LINE 11755 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOremoved =
                  ({-# LINE 34 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _removed
                   {-# LINE 11760 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOself =
                  _self
              _lhsOsimplified =
                  ({-# LINE 13 "src/MF/Languages/PHP/AG/Simplify.ag" #-}
                   _simplified
                   {-# LINE 11767 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 19 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   error "missing rule: ParamList.Nil.lhs.label"
                   {-# LINE 11772 "src/MF/Languages/PHP/AG.hs" #-}
                   )
              _lhsOlabels =
                  ({-# LINE 17 "src/MF/Languages/PHP/AG/Flow.ag" #-}
                   _lhsIlabels
                   {-# LINE 11777 "src/MF/Languages/PHP/AG.hs" #-}
                   )
          in  ( _lhsOannotated,_lhsOcallMapping,_lhsOcopy,_lhsOexstractFunctions,_lhsOexstractParameters,_lhsOlabel,_lhsOlabels,_lhsOparamMapping,_lhsOpp,_lhsOremoved,_lhsOself,_lhsOsimplified)))