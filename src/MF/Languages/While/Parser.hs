module MF.Languages.While.Parser where

import MF.Languages.While.AST
import Text.ParserCombinators.UU hiding (Const)
import Text.ParserCombinators.UU.Utils

pProgram = Program <$> pStmt

pStmt = Sequence <$> pStmt' <*> pStmt
    <|> pStmt'

pStmt' = Assign <$> pLower <* pSymbol "=" *> pExpr <* pSymbol ";"
     <|> If     <$> pSymbol "if" *> pExpr <* pSymbol "then" *> pStmt <* pSymbol "else" *> pStmt <* pSymbol "end"

pExpr = foldr pChainl pFactor [anyOp [Plus]] 

pFactor = Const <$> pInteger
      <|> Var   <$> pLower
      <|> pPacked (pSymbol "(") pExpr (pSymbol ")")