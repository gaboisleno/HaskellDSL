module Parser where

import Data.Char
import System.Environment
import Control.Monad

import Text.Parsec hiding (spaces)
import Text.Parsec.String

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces) --"hiding (spaces)" es porque hice mi propia funcion spaces

import AST

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
