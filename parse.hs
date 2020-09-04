module AST where
import Data.Char

import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec hiding (spaces)

{-
data Punto = Punto(Int, Int)
data Linea = Linea(Punto, Punto)
data Color = Rojo | Azul | Amarillo | Negro | Blanco
data Radio = Float

data Grafico = Poligono [Linea]
            | Circulo Punto Radio
-}


--Parser que matchea uno de los sig chars
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

spaces :: Parser ()
spaces = skipMany space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val

var :: Parser String
var = do
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
  where
    firstChar = satisfy (\a -> a == '_')
    nonFirstChar = satisfy (\a -> isLetter a || isDigit a || a == '_')


data Parentheses = Parentheses Integer
                   deriving (Eq,Show)

parens :: Parser Parentheses
parens = do
    void $ char '('
    e <- many1 digit
    void $ char ')'
    return (Parentheses (read e))

---------------------
data Forma = Texto String
           | Cuadrado Integer
           | Rectangulo Integer Integer
 deriving(Show, Eq)

parseTexto :: Parser Forma
parseTexto = do
                char '"'
                x <- many (noneOf("\""))
                char '"'
                return $ Texto x

parseCuadrado :: Parser Forma
parseCuadrado = do
				n <- many1 digit
				return (read n)

{-
parseRectangulo :: Parser Forma
parseRectangulo = do 
                    x <- digit
                    spaces
                    y <- digit
                    return (Rectangulo . read x y)

-}