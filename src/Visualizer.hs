module Main where

import CCO.Component             (Component, component, printer, ioWrap)
import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import Control.Arrow             (Arrow (arr), (>>>))
import Control.Monad             
import MF.Languages.PHP.AG       (Node, visualize, simplifier, annotator)

reader :: Component ATerm Node
reader = component toTree

visualizer :: Component Node String
visualizer = component $ return . visualize

main = ioWrap (parser >>> reader >>> annotator >>> simplifier >>> visualizer)
