{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import qualified Data.Text as T

import System.Environment
import System.Process
import Control.Monad

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Text.ParserCombinators.Parsec hiding (spaces) --"hiding (spaces)" es porque hice mi propia funcion spaces

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Text.Parsec.Number(floating)

import GHC.Float

import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.TikZ.Simple

{-AST-}

data Color = Rojo | Azul | Amarillo | Negro | Blanco deriving (Show)
data Punto = Punto Float Float deriving (Eq, Show)

data Forma = Texto Punto [Char]           --Texto ("texto", Punto)
           | Linea [Punto]                --Linea([Punto, Punto])
           | Cuadrado Punto Float         --Cuadrado (Lado, Punto)
           | Rectangulo Punto Float Float --Rectangulo (Lado, Lado, Punto)
           | Circulo Punto Float          --Circulo (Radio, Punto)
           | Poligono [Punto]             --Poligono ([Punto, Punto, Punto])
 deriving(Show, Eq)

{-Main-}

main :: IO ()
main =
    do
        (file:_) <- getArgs
        code <- readFile file
        execLaTeXT (tikzsimple (convertForms(commands code))) >>= renderFile (file++".tex")
        callCommand ("pdflatex "++file++".tex")
        putStrLn "Done."

{-Parser-}

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
                lexeme $  char '('
                p      <- many1 parsePunto
                return $  (Linea p)

parseTexto :: Parser Forma
parseTexto = do
                lexeme $  string "Texto"
                p      <- parsePunto
                lexeme $  char '"'
                x      <- many (noneOf("\""))
                lexeme $  char '"'
                return $  (Texto p x)

parseCuadrado :: Parser Forma
parseCuadrado = do
                    lexeme $ string "Cuadrado"
                    p      <- parsePunto
                    lexeme $  char '('
                    x      <- floating
                    lexeme $  char ')'
                    return $  (Cuadrado p x)


parseRectangulo :: Parser Forma
parseRectangulo = do
                    lexeme $  string "Rectangulo"
                    p      <- parsePunto
                    lexeme $  char '('
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
                    lexeme $  string "Circulo"
                    lexeme $  char '('
                    p      <- parsePunto
                    lexeme $  char ','
                    r      <- floating
                    lexeme $  char ')'
                    return $  (Circulo p r)


lexeme :: Parser a -> Parser a
lexeme p = do
            spaces
            x <- p
            spaces
            return x

{-Eval-}

punto2Point :: Punto -> Point
punto2Point (Punto a b) = ((float2Double a), (float2Double b))

--Procesa una Forma y la transforma en Figure
formToFigure :: Forma -> Figure
formToFigure (Cuadrado p x)     = Rectangle (punto2Point p) (float2Double x) (float2Double x)
formToFigure (Texto p x)        = Text (punto2Point p) (TeXRaw(T.pack x))
formToFigure (Rectangulo p x y) = Rectangle (punto2Point p) (float2Double x) (float2Double y)
formToFigure (Poligono a)       = Polygon (map (punto2Point) a)
formToFigure (Circulo p r)      = Circle (punto2Point p) (float2Double r)

--Procesa un array de Forma y lo transforma en un array de Figure
convertForms :: [Forma] -> [Figure]
convertForms [] = []
convertForms [x] = [formToFigure x]
convertForms (x:xs) = [formToFigure x] ++ convertForms xs

tikzsimple :: [Figure] -> LaTeXT IO ()
tikzsimple x = thePreamble >> document (theBody x)

thePreamble :: LaTeXT IO ()
thePreamble = do
  documentclass [] article
  usepackage [] tikz

theBody :: [Figure] -> LaTeXT IO ()
theBody x =  do 
    mapM_ (center . tikzpicture . figuretikz) [final(x)] --Lista de las figuras a dibujar

final :: [Figure] -> Figure
final a = Figures a
