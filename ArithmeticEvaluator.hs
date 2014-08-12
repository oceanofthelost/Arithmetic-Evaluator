module ArithmeticEvaluator where

import 			 Evaluator
--I am using http://hackage.haskell.org/package/ParserFunction-0.0.8/docs/src/Text-ParserCombinators-Parsec-ParserFunction.html
-- and http://hackage.haskell.org/package/ParserFunction-0.0.6/docs/src/Text-ParserCombinators-Parsec-ParserFunction.html
--as the basis for my expression evaluator.

--flow of data
--Input: String
--Output: Double
--Flow from Input to Output
--evaluateExpression -> stringToExpr -> makeExpression -> Parsers -> evaluate

--TODO:
--write functions:
	--evaluateExpression
	--stringToExpr
	--evaluate

--rewright monad interface as an applicitive interface
--I can use http://stackoverflow.com/questions/15123552/translate-from-monad-to-applicative
--which describes converting do notaton betwen the monadic laws and how to then convert the
--monad form to a applicative form by substitution


--Takes an Expr, parses it and then evaluates the expression
main :: String -> [(Char, Double)] -> Double
main s m = evaluateExpression s m