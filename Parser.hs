module Parser where


import           Text.ParserCombinators.Parsec hiding ((<|>),many)
import           Text.ParserCombinators.Parsec.Expr
import           Control.Applicative
import           Types

--will start to parse a Expr.
buildExpr :: Parser Expr
buildExpr = buildExpressionParser expressionTable factor <?> "expression"


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


--variable parser 
variables :: Parser Expr
variables = Var <$> letter <?> "variable"


--parser for numbers
number :: Parser Expr
number = (Num . read) <$> (many1 digit) <?> "number"

--convert a string to an expression using maybe
stringToExpr :: String -> Maybe Expr
stringToExpr x = either (const Nothing) (Just) (parse buildExpr "" ( "(" ++ (filter (/= ' ')  x) ++ ")" ))
