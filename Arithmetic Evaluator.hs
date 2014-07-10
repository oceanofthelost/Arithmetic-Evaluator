import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

makeExpression:: Parser Integer
makeExpression = buildExpressionParser table factor <?> "expression"

table :: [[ Operator Char st Integer ]]
table = [
      		[ operationInfix "*" (*) AssocLeft, operationInfix "/" div AssocLeft ],
      		[ operationInfix "+" (+) AssocLeft, operationInfix "-" (-) AssocLeft ]
      	]
    	where
		operationInfix s f assoc = Infix (do { string s ; return f }) assoc
  

factor = do 
 	char '(' 
 	x <- makeExpression
 	char ')' 
	return x
     <|> number
  
number :: Parser Integer
number = do { ds <- many1 digit; return (read ds) } <?> "number"
