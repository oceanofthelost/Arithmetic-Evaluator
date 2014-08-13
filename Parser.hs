module Parser where

import           Text.ParserCombinators.Parsec hiding ((<|>),many)
import           Text.ParserCombinators.Parsec.Expr
import 			 Control.Applicative 
import           Types

--will start to parse a Expr.
buildExpr :: Parser Expr
buildExpr = buildExpressionParser expressionTable factor
    <?> "expression"


--We create a table of operators and there associativity
expressionTable :: [[Operator Char st Expr]]
expressionTable =
	[
 		[operationInfix "*" Mul AssocLeft, operationInfix "/" Div AssocLeft],
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
    <|> number
    <|> variables
    <?> "simple expression"


--parse a variable. If not variable we then parse a number.
--variables can only be size of 1
variables :: Parser Expr
variables = do
    ds <- letter
    return (Var ds)
    <?> "variable"

--Had lots of problems getting my number parser working. So I used the one
--on http://hackage.haskell.org/package/ParserFunction-0.0.8/docs/src/Text-ParserCombinators-Parsec-ParserFunction.html
--to implement the number parser.
number :: Parser Expr
number = (Num . read) <$> (many1 digit)
--number = many1 digit >>= \ ds -> return (Num (read ds)) <?> "number"
{-
number = do
    ds <- many1 digit
    return (Num (read ds))
    <?> "number"
-}



--convert a string to an expression using maybe
stringToExpr :: String -> Maybe Expr
stringToExpr x = either (const Nothing) (Just) (parse buildExpr "" ( "(" ++ (filter (/= ' ')  x) ++ ")" ))