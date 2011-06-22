module Main where

import CCO.Component             (Component, component, printer, ioWrap)
import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import Control.Arrow             (Arrow (arr), (>>>))
import Control.Monad             
import MF.Languages.PHP.AG       (Node, simplifier)

reader :: Component ATerm Node
reader = component toTree


printer' :: Component Node String
printer' = component $ return . show

main = ioWrap (parser >>> reader >>> simplifier >>> printer)