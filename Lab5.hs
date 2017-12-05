import Yoda
import Lab1

-- 1.1 Basic Parser Functions

-- 1 (a).
-- parse (string "hello") "hello world"
-- returns [(" world","hello")]

-- (b).
-- parse (many (char 'a')) "aab"
-- returns [("b","aa"), ("ab","a"), ("aab","")]

-- (c).
-- parse (some (char 'a')) "aab"
-- returns [("b","aa"), ("ab","a")]

-- (d).
-- parse (oneOf "ab") "ac"
-- returns [("c",'a')]
-- parse (oneOf "ab") "bc"
-- returns [("c",'b')]

-- (e).
-- parse (noneOf "ab") "c"
-- returns [("",'c')]
-- parse (noneOf "ab") "a"
-- returns []

-- 2.
whitespace' :: Parser ()
whitespace' = many (oneOf " \t") *> pure ()

-- 3.
tok :: String -> Parser String
tok s = string s <* whitespace

-- 4.
number :: Parser Int
number = (fmap read (some (oneOf ['0' .. '9']))) <* whitespace
-- This is the same as:
-- number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace

-- 1.2 Robot Parser

-- 1.
parseRobot :: Parser Robot
parseRobot = Forward <$ tok "forward" <*> number <*> parseRobot
         <|> Right' <$ tok "right" <*> parseRobot
         <|> Left' <$ tok "left" <*> parseRobot
         <|> Stop <$ tok "stop"

-- 2. Whitespace function that deals with commas, semicolons and new lines.
whitespace :: Parser ()
whitespace = many (oneOf ",;\r\n\t ") *> pure ()

-- 1.3 Bad Parser

-- 1. This parser won't work as it contains left recursion. The parser can
-- repeatedly call itself.

-- 2. Grammar in BNF form:
-- <DataB> ::= <DataB> <number> | "a" <number> | Empty
-- <number> ::= ('0' | ... | '9')+

-- Modified grammar:
-- <DataB> ::= "a" <number> <DataB'> | <DataB'>
-- <DataB'> ::= <number> <DataB'> | Empty
-- <number> ::= ('0' | ... | '9')+

data DataB = A Int DataB' | B DataB'

data DataB' = B' Int DataB' | D
parseBad :: Parser DataB
parseBad = A <$ tok "a" <*> number <*> parseBad'
       <|> B <$> parseBad'

parseBad' :: Parser DataB'
parseBad' = B' <$> number <*> parseBad'
        <|> D <$ tok ""
