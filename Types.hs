module Types where


--Define a recursive data type for expression which represents the only valid
--strings that an Expr can take

data Expr =   Num Double	| Var Char 		| Sub Expr Expr
       		| Div Expr Expr | Mul Expr Expr | Add Expr Expr   deriving (Show, Eq)
