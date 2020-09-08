module Main where

import Data.Char
import System.Environment
import Control.Monad

import Text.Parsec hiding (spaces)
import Text.Parsec.String

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces) --"hiding (spaces)" es porque hice mi propia funcion spaces

data Color = Rojo | Azul | Amarillo | Negro | Blanco deriving (Show)
data Punto = Punto Integer Integer deriving (Eq, Show)

data Forma = Texto String               --Texto ("texto", Punto)
           | Linea [Punto]              --Linea([Punto, Punto])
           | Cuadrado Integer           --Cuadrado (Lado, Punto)
           | Rectangulo Integer Integer --Rectangulo (Lado, Lado, Punto)
           | Circulo Integer Integer    --Circulo (Radio, Punto)
           | Poligono [Punto]           --Poligono ([Punto, Punto, Punto])
 deriving(Show, Eq)

{-
ToDo:
Reemplazar Integer por Double
Elipse: recibe un punto en el espacio, y dos doubles
Agregar colores: rojo azul verde amarillo cian magenta negro blanco
-}

main :: IO ()
main = do
           (expr:_) <- getArgs
           putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input =
    case parse parseExpr "" input of 
        Right val  -> "Found value: " ++ show val 
        Left err   -> case parse parsePunto "" input of
                        Right val  -> "Found value: " ++ show val
                        Left err   -> "Fail on: " ++ show err
           
    

parseExpr :: Parser Forma
parseExpr =  parseTexto
         <|> parseLinea
         <|> parseCuadrado
         <|> parseRectangulo

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

spaces :: Parser ()
spaces = void $ many $ oneOf(" \n\t")

parsePunto :: Parser Punto
parsePunto = do
                string "Punto"
                char '('
                e0 <- many1 digit
                char ','
                e1 <- many1 digit
                char ')'
                return $ (Punto (read e0) (read e1))


--El parse punto no devuelve un Punto, devuelve un either
parseLinea :: Parser Forma
parseLinea = do
                string "Linea"
                char '('
                p1 <- parsePunto
                char ','
                p2 <- parsePunto
                char ')'
                return $ (Linea [p1, p2])

parseTexto :: Parser Forma
parseTexto = do
                string "Texto"
                char '"'
                x <- many (noneOf("\""))
                char '"'
                return $ Texto x

parseCuadrado :: Parser Forma
parseCuadrado = do
                    string "Cuadrado" --idenficador para declarar un cuadrado
                    char '('
                    x <- many1 digit
                    char ')'
                    return $ (Cuadrado (read x))


parseRectangulo :: Parser Forma
parseRectangulo = do
                    string "Rectangulo"
                    char '('
                    e0 <- many1 digit
                    char ','
                    e1 <- many1 digit
                    char ')'
                    return $ (Rectangulo (read e0) (read e1))                  

{-

Uso sin compilar:

*Main> regularParse parseTexto "\"Hola mundo\""
*Main> regularParse parsePunto "1 2"
*Main> regularParse parseLinea  "2 1 2 3"

Uso compilado: 

1) Compilar:
$ ghc -o main parsev2.hs

2) Ejecutar:

$ ./main "Texto\"Hola Mundo\""
Found value: Texto "Hola Mundo"

$ ./main "Rectangulo(2,2)"
Found value: Rectangulo 2 2

$ ./main "Cuadrado(2)"
Found value: Cuadrado 2

$ ./main "Punto(2,2)"
Found value: Punto 2 2

$ ./main "Linea(Punto(2,2),Punto(3,3))"
Found value: Linea [Punto 2 2,Punto 3 3]

-}
