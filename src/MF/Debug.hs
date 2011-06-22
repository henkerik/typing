module MF.Debug where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import qualified Data.IntMap as M

import MF.Flowable
import MF.Languages.PHP.Parser
import MF.Languages.PHP.Flow

--visualize :: Flowable f => String
visualize f = graphviz' graph 
    where
        graph :: Gr () ()
        graph = mkGraph nodes edges
        nodes = map (\l -> (l, ())) . labels $ f
        edges = labUEdges . flow $ f
        
--labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(i,j) -> (i,j,()))

test = let ast = parse "$a = 1;"
           graph = visualize ast
       in graph