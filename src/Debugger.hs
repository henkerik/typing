module Main where

import CCO.Component             (Component, component, printer, ioWrap)
import CCO.Tree                  (ATerm, Tree (toTree, fromTree), parser)
import Control.Arrow             (Arrow (arr), (>>>))
import Control.Monad             
import MF.Languages.PHP.AG       (Node, simplifier, annotator)
import MF.Flowable
import Prelude                   hiding (init)

reader :: Component ATerm Node
reader = component toTree

debugger :: Component Node String
debugger = component $ \doc -> return $ "Init: " ++ (show . init $ doc) ++ ", Final: " ++ (show . final $ doc) ++ ", Flow: " ++ (show . flow $ doc)

main = ioWrap (parser >>> reader >>> annotator >>> simplifier >>> debugger)