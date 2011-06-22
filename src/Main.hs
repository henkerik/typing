module Main where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import MF.Flowable
import MF.Languages.While.AST
--import MF.Languages.While.Analyses.LifeVariableAnalysis as LVA

-- import MF.Languages.While.Analyses.AE as AE
import MF.Languages.While.Analyses.LV as LV

--import MF.Languages.While.Flow

{--
myProgram1 = Program blocks flow 1 (IS.singleton 7)
    where
        blocks = IM.fromList [(1, Assign "x" (Const 2))
                             ,(2, Assign "y" (Const 4))
                             ,(3, Assign "x" (Const 1))
                             ,(4, Expr   (Plus (Var "y") (Var "x")))
                             ,(5, Assign "z" (Var "y"))
                             ,(6, Assign "z" (Plus (Var "y") (Var "y")))
                             ,(7, Assign "x" (Var "z"))]

        flow = IM.fromList [(1, IS.singleton 2)
                            ,(2, IS.singleton 3)
                            ,(3, IS.singleton 4)
                            ,(4, IS.fromList [5,6])
                            ,(5, IS.singleton 7)
                            ,(6, IS.singleton 7)]
                            
myProgram2 = Program blocks flow 1 (IS.singleton 6)
    where
        blocks = IM.fromList [(1, Assign "x" (Plus (Var "a") (Var "b")))
                             ,(2, Assign "y" (Times (Var "a") (Var "b")))
                             ,(3, Expr (Greater (Var "y") (Plus (Var "a") (Var "b"))))
                             ,(4, Assign "a" (Plus (Var "a") (Const 1)))
                             ,(5, Assign "x" (Plus (Var "a") (Var "b")))]

        flow = IM.fromList [(1, IS.singleton 2)
                            ,(2, IS.singleton 3)
                            ,(3, IS.singleton 4)
                            ,(4, IS.singleton 5)
                            ,(5, IS.singleton 3)]     
                            
--}
                            
ast1 = L undefined (Sequence 
        (L 1 (Assign "x" (Plus (Var "a") (Var "b"))))
        (L undefined (Sequence 
            (L 2 (Assign "y" (Times (Var "a") (Var "b"))))
            (L 3 (While 
                (Greater (Var "y") (Plus (Var "a") (Var "b"))) 
                (L undefined (Sequence 
                    (L 4 (Assign "a" (Plus (Var "a") (Const 1))))
                    (L 5 (Assign "x" (Plus (Var "a") (Var "b"))))
                ))
            ))
        ))
      )

                       
                             
--main = print $ LVA.solve ast1
main = print $ LV.solve ast1