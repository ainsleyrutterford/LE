module While where

import Yoda
import Data.List
import Prelude hiding (Num)
import qualified Prelude (Num)

type Num = Integer
type Var = String
type Z = Integer
type T = Bool
type State = Var -> Z

data Aexp = N Num
          | V Var
          | Mult Aexp Aexp
          | Add Aexp Aexp
          | Sub Aexp Aexp
          deriving (Show, Eq, Read)

data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Eq Aexp Aexp
          | Le Aexp Aexp
          deriving (Show, Eq, Read)

data Stm = Ass Var Aexp
         | Skip
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         deriving (Show, Eq, Read)

-- The while language is a parser of |Stm| terms. We allow multiple
-- semi-colons at the top level.
while :: Parser Stm
while = stms

{-
-- The code below implements left-associative operations in decreasing
-- precedence. It is superceded by the `precedence` function below.


aexp, aexp', aexp'', aexp''' :: Parser Aexp
aexp    = chainl aexp'   ((:*:) <$ tok "*")
aexp'   = chainl aexp''  ((:+:) <$ tok "+")
aexp''  = chainl aexp''' ((:-:) <$ tok "-")
aexp''' = Num <$> num
     <|> Var <$> var
     <|> tok "(" *> aexp <* tok ")"
-}

precedence :: [Parser (a -> a -> a)] -> Parser a -> Parser a
precedence ops arg = foldl' build arg ops
  where build term ops = chainl term ops

aexp = precedence [Mult <$ tok "*"
                  ,Add <$ tok "+" <|> Sub <$ tok "-" ]
     ( N <$> num
   <|> V <$> var
   <|> tok "(" *> aexp <* tok ")")

bexp :: Parser Bexp
bexp = precedence [And <$ tok "&"]
      ( TRUE <$ tok "true"
    <|> FALSE <$ tok "false"
    <|> Eq <$> aexp <* tok "=" <*> aexp
    <|> Le <$> aexp <* tok "<=" <*> aexp
    <|> Neg <$ tok "!" <*> bexp
    <|> tok "(" *> bexp <* tok ")")

stms :: Parser Stm
stms = chainl stm (Comp <$ tok ";")

stm = Ass <$> var <* tok ":=" <*> aexp
  <|> Skip <$  tok "skip"
  <|> If <$ tok "if" <*> bexp <* tok "then" <*> stm <* tok "else" <*> stm
  <|> While <$ tok "while" <*> bexp <* tok "do" <*> stm
  <|> tok "(" *> stms <* tok ")"

chainl p op = p >>= rest where
  rest x = do f <- op
              y <- p
              rest (f x y)
       <|> return x

num :: Parser Num
num = read <$> some (oneOf ['0' .. '9']) <* whitespace

var :: Parser String
var = some (oneOf ['a' .. 'z']) <* whitespace

whitespace :: Parser ()
whitespace = () <$ many (oneOf " \t\n\r")

tok :: String -> Parser String
tok t = string t <* whitespace

fromJust :: Maybe a -> a
fromJust Nothing = error "fromJust Nothing"
fromJust (Just x) = x

-- Note that there is potential ambiguity in the following example:
example = "if true then x := 5 else z := 2 + 1 ; u := 2 * 3 + 4"

-- the assignment |u := ...| is a statment after the conditional.

-- If multiple statements are desired within the statements of the if
-- clause, then explicit parentheses are required:
example2 = "if true then ( x := 5 ; z:=3 ) else (z := 2 + 1 ; v := 3 + 4) ; u := 2"
