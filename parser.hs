module Main where

import Data.Char
import System.Environment
import Control.Monad

import Text.Printf --Remove this when it is finished

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces) --"hiding (spaces)" es porque hice mi propia funcion spaces

import Text.LaTeX
import Text.LaTeX.Packages.TikZ.Simple

{-AST-}

data Color = Rojo | Azul | Amarillo | Negro | Blanco deriving (Show)
data Punto = Punto Integer Integer deriving (Eq, Show)

data Forma = Texto String               --Texto ("texto", Punto)
           | Linea [Punto]              --Linea([Punto, Punto])
           | Cuadrado Integer           --Cuadrado (Lado, Punto)
           | Rectangulo Integer Integer --Rectangulo (Lado, Lado, Punto)
           | Circulo Integer Integer    --Circulo (Radio, Punto)
           | Poligono [Punto]           --Poligono ([Punto, Punto, Punto])
 deriving(Show, Eq)

{-Main-}

main :: IO ()
main = do
           (expr:_) <- getArgs
           putStrLn (show (commands expr))
           --putStrLn (commands expr)

{-Parser-}

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ';' = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

process :: [String] -> [Forma]
process [] = []
process [x] = [readExpr x]
process (x:xs) = [readExpr x] ++ process xs

commands :: String -> [Forma]
commands x = process (split x)

readExpr :: String -> Forma
readExpr input = case parse (spaces >> parseExpr) "" input of 
        Left err   ->  Cuadrado 2
        Right val  ->  val
        --FIX THIS !!!!!
        --La funcion debe devolver un tipo Forma, pero si falla devuelve tipo ParserError. No se como hacerlo.

parseExpr :: Parser Forma
parseExpr = parseTexto
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
                    lexeme $ string "Cuadrado"
                    lexeme $ char '('
                    x <- many1 digit
                    lexeme $ char ')'
                    return $ (Cuadrado (read x))


parseRectangulo :: Parser Forma
parseRectangulo = do
                    spaces >> string "Rectangulo"
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

--Procesa una Forma y la transforma en Figure
formToFigure :: Forma -> String
formToFigure (Cuadrado x) = printf "Rectangle (0,0) %d %d" x x
formToFigure (Rectangulo x y) = printf "Rectangle (0,0) %d %d" x y
-- ...
-- ...
-- ...

--Procesa un array de Forma y lo transforma en un array de Figure
convertForms :: [Forma] -> [String]
convertForms [] = []
convertForms [x] = [formToFigure x] 
convertForms (x:xs) = [formToFigure x] ++ convertForms xs
