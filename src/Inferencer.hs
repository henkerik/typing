module Main where

import CCO.Component             (Component, component, printer, ioWrap)
import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import CCO.Printing              (render_, Doc)
import Control.Arrow             (Arrow (arr), (>>>))
import Control.Monad             
import MF.Languages.PHP.AG       (Node, solve, simplifier, checker, reporter, annotator)

reader :: Component ATerm Node
reader = component toTree

render :: Component Doc String
render = component $ return . render_ 80

main = ioWrap (parser >>> reader >>> annotator >>> simplifier >>> checker >>> reporter >>> render)