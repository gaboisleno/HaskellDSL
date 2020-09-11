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


split :: String -> [String]
split [] = [""]
split (c:cs) | c == ';' = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs


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
                lexeme $ string "Punto"
                lexeme $ char '('
                e0 <- many1 digit
                lexeme $ char ','
                e1 <- many1 digit
                lexeme $ char ')'
                return $ (Punto (read e0) (read e1))


--El parse punto no devuelve un Punto, devuelve un either
parseLinea :: Parser Forma
parseLinea = do
                lexeme $ string "Linea"
                lexeme $ char '('
                p1 <- parsePunto
                lexeme $ char ','
                p2 <- parsePunto
                lexeme $ char ')'
                return $ (Linea [p1, p2])

parseTexto :: Parser Forma
parseTexto = do
                lexeme $ string "Texto"
                lexeme $ char '"'
                x <- many (noneOf("\""))
                lexeme $ char '"'
                return $ Texto x

parseCuadrado :: Parser Forma
parseCuadrado = do
                    lexeme $ string "Cuadrado" --idenficador para declarar un cuadrado
                    lexeme $ char '('
                    x <- many1 digit
                    lexeme $ char ')'
                    return $ (Cuadrado (read x))


parseRectangulo :: Parser Forma
parseRectangulo = do
                    lexeme $ string "Rectangulo"
                    lexeme $ char '('
                    e0 <- many1 digit
                    lexeme $ char ','
                    e1 <- many1 digit
                    lexeme $ char ')'
                    return $ (Rectangulo (read e0) (read e1))                  


lexeme :: Parser a -> Parser a
lexeme p = do
            spaces
            x <- p 
            spaces
            return x

{-Eval-}