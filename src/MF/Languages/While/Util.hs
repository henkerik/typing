module MF.Languages.While.Util where

import MF.Languages.While.AST
import qualified Data.Set as S

fv :: Expression -> S.Set Identifier
fv (Plus l r) = fv l `S.union` fv r
fv (Times l r) = fv l `S.union` fv r
fv (Greater l r) = fv l `S.union` fv r
fv (Var name) = S.singleton name
fv (Const _)  = S.empty

