module AST where

import Data.Char
import System.Environment
import Control.Monad

import Text.Parsec hiding (spaces)
import Text.Parsec.String

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces)

--hiding spaces porque yo hago mi propia funcion spaces

data Punto = Punto Integer Integer deriving (Show)

data Linea = Linea Punto Punto deriving (Show)

data Forma = Texto String
           | Cuadrado Integer
           | Rectangulo Integer Integer
 deriving(Show, Eq)


regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

spaces :: Parser ()
spaces = void $ many $ oneOf(" \n\t")


parsePunto :: Parser Punto
parsePunto = do
				spaces
				e0 <- many1 digit
				spaces
				e1 <- many1 digit
				return $ (Punto (read e0) (read e1))


--El parse punto no devuelve un Punto, devuelve un either
parseLinea :: Parser Linea
parseLinea = do
			p1 <- parsePunto
			p2 <- parsePunto
			return $ (Linea (p1) (p2))

parseTexto :: Parser Forma
parseTexto = do
                char '"'
                x <- many (noneOf("\""))
                char '"'
                return $ Texto x

parseCuadrado :: Parser Forma
parseCuadrado = do
				x <- many1 digit
				return $ (Cuadrado (read x))


parseRectangulo :: Parser Forma
parseRectangulo = do
					spaces
					e0 <- many1 digit
					spaces
					e1 <- many1 digit
					return $ (Rectangulo (read e0) (read e1))
                   

{-
*AST> regularParse parseTexto "\"Hola mundo\""
*AST> regularParse parsePunto "1 2"
*AST> regularParse parseLinea  "2 1 2 3"
-}
