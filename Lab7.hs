import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative
import Data.Monoid

-- 1 Parsing

type Parser = Parsec () String

turn :: Parser String
turn = some (oneOf "0123456789.") <* space

data Chess = Turn Move Move Chess
           | EndGame
  deriving Show

chess :: Parser Chess
chess = Turn <$ turn <*> move <* space <*> move <* space <*> chess
    <|> EndGame <$ string ""

data Move = Move MoveP Quant
          | Cstl Bool
          | End Winner
  deriving Show

move :: Parser Move
move = try (Move <$> movep <*> quant)
   <|> try (Cstl <$> bool)
   <|> End <$> winner

bool :: Parser Bool
bool = True <$ string "0-0-0"
   <|> False <$ string "0-0"

data Quant = Prom Piece Quant
           | Chck Quant
           | Null
  deriving Show

quant :: Parser Quant
quant = Prom <$> (King <$ string "K"
              <|> Queen <$ string "Q"
              <|> Rook <$ string "R"
              <|> Knight <$ string "N"
              <|> Bishop <$ string "B") <*> quant
    <|> Chck <$ (string "+" <|> string "#") <*> quant
    <|> Null <$ string ""

data MoveP = Alg Piece Cell
           | Smh Cell Cell
           | AlgDis Piece Cell Cell
           | Tke Piece Cell
   deriving Show

movep :: Parser MoveP
movep = try (Smh <$> cell <*> cell)
    <|> try (Tke <$> piece <* string "x" <*> cell)
    <|> try (AlgDis <$> piece <*> cell <*> cell)
    <|> try (Alg <$> piece <*> cell)

data Winner = White
            | Black
            | Draw
            | AO
  deriving Show

winner :: Parser Winner
winner = try (White <$ string "1-0")
     <|> try (Black <$ string "0-1")
     <|> try (Draw <$ string "1/2-1/2")
     <|> AO <$ string ""

data Cell = Cell Char Int
  deriving Show

cell :: Parser Cell
cell = Cell <$> lowerChar <*> number

number :: Parser Int
number = (some (oneOf ['0' .. '9']) >>= return . read)

data Piece = King
           | Queen
           | Rook
           | Knight
           | Bishop
           | Pawn
  deriving (Show, Eq)

piece :: Parser Piece
piece = King <$ string "K"
    <|> Queen <$ string "Q"
    <|> Rook <$ string "R"
    <|> Knight <$ string "N"
    <|> Bishop <$ string "B"
    <|> Pawn <$ string ""
