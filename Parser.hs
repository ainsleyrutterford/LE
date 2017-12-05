import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec () String

{-
Here is a grammar we might want to parse:

<expr> ::= <term> ("+" <term>)*
<term> ::= ('0' | '1' | ... | '9')+

-}

data Expr = ExprTerm Term [Term] deriving Show
data Term = Term Int deriving Show

expr :: Parser Expr
expr = ExprTerm <$> term <*> many (string "+" *> term)

term :: Parser Term
term = Term <$> decimal

{- We test this by executing the following in ghci:

< parseMaybe expr "1+2+3"

-}

