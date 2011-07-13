module Program where
    
type Variable = String
type Function = String

data Identifier = Variable Variable
                | Function Function
    
data Stmt = Expr Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Sequence Stmt Stmt
          | Skip

data Expr = Assign Variable Expr
          | Call Identifier [Expr]
          | Closure [Variable] Stmt
          | Var Variable
          | Const Con
          
data Con = Bool Bool
         | Int Int
          
