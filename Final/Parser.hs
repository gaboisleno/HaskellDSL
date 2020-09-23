module Parser where

import Data.Char
import System.Environment
import Control.Monad

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Text.Parsec.Number(floating)

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces, try)

import AST

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ';' = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs


process :: [String] -> [Forma]
process [] = []
process (x:xs) = case readExpr x of
                Left err -> fail ("Error: " ++ show err) putStrLn
                Right forma -> [forma] ++ process xs


commands :: String -> [Forma]
commands x = process (filter (not . null ) (split x))


readExpr :: String -> Either ParseError Forma
readExpr input = parse (spaces >> parseExpr) "" input

parseExpr :: Parser Forma
parseExpr =  parseTexto
         <|> parseLinea
         <|> parseCuadrado
         <|> parseCirculo
         <|> parseRectangulo
         <|> parsePoligono
         <|> parseElipse

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

spaces :: Parser ()
spaces = void $ many $ oneOf(" \n\t")

parsePunto :: Parser Punto
parsePunto = do
                lexeme $  string "Punto"
                lexeme $  char '('
                e0     <- floating
                lexeme $  char ','
                e1     <- floating
                lexeme $  char ')'
                return $  (Punto e0 e1)

parseLinea :: Parser Forma
parseLinea = do
                lexeme $  string "Linea"
                lexeme $  char '['
                p      <- ( `sepBy` char ',' ) parsePunto
                lexeme $  char ']'
                return $  (Linea p)

parseTexto :: Parser Forma
parseTexto = do
                lexeme $  string "Texto"
                lexeme $  char '('
                p      <- parsePunto
                lexeme $  char ','
                lexeme $  char '"'
                e0     <- many (noneOf("\""))
                lexeme $  char '"'
                return $  (Texto p e0)

parseCuadrado :: Parser Forma
parseCuadrado = do
                    lexeme $ try (string "Cuadrado")
                    lexeme $  char '('
                    p      <- parsePunto
                    lexeme $ char ','
                    e0      <- floating
                    lexeme $  char ')'
                    return $  (Cuadrado p e0)


parseRectangulo :: Parser Forma
parseRectangulo = do
                    lexeme $  string "Rectangulo"
                    lexeme $  char '('
                    p      <- parsePunto
                    lexeme $  char ','
                    e0     <- floating
                    lexeme $  char ','
                    e1     <- floating
                    lexeme $  char ')'
                    return $  (Rectangulo p e0 e1)

parsePoligono :: Parser Forma
parsePoligono = do
                    lexeme $  string "Poligono"
                    lexeme $  char '['
                    p      <- ( `sepBy` char ',' ) parsePunto
                    lexeme $  char ']'
                    return $  (Poligono p)

parseCirculo :: Parser Forma
parseCirculo = do
                    lexeme $  try (string "Circulo")
                    lexeme $  char '('
                    p      <- parsePunto
                    lexeme $  char ','
                    e0      <- floating
                    lexeme $  char ')'
                    return $  (Circulo p e0)

parseElipse :: Parser Forma
parseElipse = do
                lexeme $ try (string "Elipse")
                lexeme $ char '('
                p      <- parsePunto
                lexeme $ char ','
                e0     <- floating
                lexeme $ char ','
                e1     <- floating
                lexeme $ char ')'
                return $ (Elipse p e0 e1)

lexeme :: Parser a -> Parser a
lexeme p = do
            spaces
            x <- p
            spaces
            return x
