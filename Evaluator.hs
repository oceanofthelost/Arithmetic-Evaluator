module Evaluator where

import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)
import           Parser
import           Types

evaluateExpression :: String -> [(Char, Double)] -> Double
evaluateExpression s m = evaluate (M.fromList m) (fromMaybe ( error "Parser error in the expression") (stringToExpr s) )

--takes an expression and a Map of variables and the variables value
--and then evaluate the expression.
evaluate :: M.Map Char Double -> Expr -> Double
evaluate m expression =
	case expression of
		(Num d)						->	d
		(Var x)						->	fromMaybe (error ("Variable lookup failed") ) (M.lookup x m)
		(Add exprLeft exprRight)	-> 	(evaluate m exprLeft) + (evaluate m exprRight)
		(Sub exprLeft exprRight)	-> 	(evaluate m exprLeft) - (evaluate m exprRight)
		(Mul exprLeft exprRight)	-> 	(evaluate m exprLeft) * (evaluate m exprRight)
		(Div exprLeft exprRight)	-> 	(evaluate m exprLeft) / (evaluate m exprRight)
