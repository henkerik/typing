module Main where

import CCO.Component             (Component, component, printer, ioWrap)
import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import Control.Arrow             (Arrow (arr), (>>>))
import Control.Monad             
import MF.Languages.PHP.AG       (Node)

reader :: Component ATerm Node
reader = component toTree

debugger :: Component Node String
debugger = component $ \doc -> return $ show doc

main = ioWrap (parser >>> reader >>> printer)