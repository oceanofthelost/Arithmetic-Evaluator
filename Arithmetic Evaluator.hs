import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Data.Map as M


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


--Define a recursive data type for expression which represents the only valid
--strings that an Expr can take
data Expr = 	  Num Double	| Var Char 	| Sub Expr Expr
          		| Div Expr Expr 	| Mul Expr Expr 	| Add Expr Expr 
          		| Exp Expr deriving (Show, Eq)

--Takes an Expr, parses it and then evaluates the expression
evaluateExpression :: String -> [(Char, Double)] -> Double
evaluateExpression = undefined

stringToExpr :: String -> Maybe Expr
stringToExpr = undefined

--will start to parse a Expr. 
buildExpr :: Parser Expr
buildExpr = buildExpressionParser expressionTable factor

--We create a table of operators and there associativity
expressionTable :: [[Operator Char st Expr]]
expressionTable =  [
 	[operationInfix"*" Mul AssocLeft, operationInfix "/" Div AssocLeft],
	[operationInfix "+" Add AssocLeft, operationInfix "-" Sub AssocLeft]
             	]
    where
        operationInfix s function assoc = Infix  (do{ string s; return function}) assoc


  
 --parse a expression. If not valid we then parse a number
factor :: Parser Expr
factor = do 
 	char '(' 
 	x <- buildExpr
 	char ')' 
	return x
     	<|> variables
  

--parse a variable. If not variable we then parse a number. 
--variables can only be size of 1
variables :: Parser Expr
variables = do
	x <- letter 
	return (Var x)
	<|> number

--Had lots of problems getting my number parser working. So I used the one 
--on http://hackage.haskell.org/package/ParserFunction-0.0.8/docs/src/Text-ParserCombinators-Parsec-ParserFunction.html 
--to implement the number parser.
number :: Parser Expr
number = do
    br  <- many digit
    let d :: Double
        d = fromInteger (foldl ((. ch2num) . (+) . (*10)) 0 br)
    option (Num (d)) ((do
        char '.'
        ar <- many1 digit
        return $ (Num (d + foldr (fd) 0 ar)) ))
        where
            fd a b = (fromInteger (ch2num a) + b) / 10
            fe = toInteger . fromEnum
            ch2num = (subtract $ fe '0') . fe

--takes an expression and a Map of variables and the variables value 
--and then evaluate the expression.
evaluate :: M.Map String Double -> Expr -> Double
evaluate = undefined