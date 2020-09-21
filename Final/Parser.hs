module Parser where

import Data.Char
import System.Environment
import Control.Monad

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Text.Parsec.Number(floating)

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces) --"hiding (spaces)" es porque hice mi propia funcion spaces

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
commands x = process (split x)

readExpr :: String -> Either ParseError Forma
readExpr input = parse (spaces >> parseExpr) "" input

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
                e0 <- floating
                lexeme $ char ','
                e1 <- floating
                lexeme $ char ')'
                return $ (Punto e0 e1)

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
                    x <- floating
                    lexeme $ char ')'
                    return $ (Cuadrado x)


parseRectangulo :: Parser Forma
parseRectangulo = do
                    spaces >> string "Rectangulo"
                    lexeme $ char '('
                    e0 <- floating
                    lexeme $ char ','
                    e1 <- floating
                    lexeme $ char ')'
                    return $ (Rectangulo e0 e1)


lexeme :: Parser a -> Parser a
lexeme p = do
            spaces
            x <- p
            spaces
            return x
