{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import qualified Data.Text as T

import System.Environment
import System.Process
import Control.Monad

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Text.ParserCombinators.Parsec hiding (spaces, try) --"hiding (spaces)" es porque hice mi propia funcion spaces

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
           | Elipse Punto Float Float     --Elipse
           | Grafico_Linea [Float] String
           | Grafico_Torta [Float]
           | Grafico_Barras [Float]
 deriving(Show, Eq)


{-Main-}

main :: IO ()
main =
    do
        (file:_) <- getArgs
        code <- readFile file
        execLaTeXT (tikzsimple (convertForms(commands code))) >>= renderFile (file++".tex")
        callCommand ("pdflatex "++file++".tex")
        callCommand ("rm "++file++".aux")
        callCommand ("rm "++file++".log")
        callCommand ("rm "++file++".tex")
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
         <|> parseElipse
         <|> parseGraficoLinea

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

spaces :: Parser ()
spaces = void $ many $ oneOf(" \n\t")

parsePunto :: Parser Punto
parsePunto = do
                lexeme $  try (string "Punto")
                lexeme $  char '('
                e0     <- floating
                lexeme $  char ','
                e1     <- floating
                lexeme $  char ')'
                return $  (Punto e0 e1)

parseLinea :: Parser Forma
parseLinea = do
                lexeme $  try (string "Linea")
                lexeme $  char '['
                p      <- ( `sepBy` char ',' ) parsePunto
                lexeme $  char ']'
                return $  (Linea p)

parseTexto :: Parser Forma
parseTexto = do
                lexeme $  try (string "Texto")
                lexeme $  char '('
                p      <- parsePunto
                lexeme $  char ','
                lexeme $  char '"'
                e0     <- many (noneOf("\""))
                lexeme $  char '"'
                return $  (Texto p e0)

parseCuadrado :: Parser Forma
parseCuadrado = do
                    lexeme $  try (string "Cuadrado")
                    lexeme $  char '('
                    p      <- parsePunto
                    lexeme $  char ','
                    e0     <- floating
                    lexeme $  char ')'
                    return $  (Cuadrado p e0)


parseRectangulo :: Parser Forma
parseRectangulo = do
                    lexeme $  try (string "Rectangulo")
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
                    lexeme $  try (string "Poligono")
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
                lexeme $  try (string "Elipse")
                lexeme $  char '('
                p      <- parsePunto
                lexeme $  char ','
                e0     <- floating
                lexeme $  char ','
                e1     <- floating
                lexeme $  char ')'
                return $  (Elipse p e0 e1)

parseGraficoLinea :: Parser Forma
parseGraficoLinea = do 
                        lexeme $  try (string "Grafico_Linea")
                        lexeme $  char '('
                        lexeme $  char '['
                        e0     <- ( `sepBy` char ',') (spaces >>  floating)
                        lexeme $  char ']'
                        lexeme $  char ','
                        lexeme $  char '"'
                        e1     <- many (noneOf("\""))
                        lexeme $  char '"'  
                        lexeme $  char ')'
                        return $  (Grafico_Linea e0 e1)

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
formToFigure (Texto p s)        = Text (punto2Point p) (TeXRaw(T.pack s))
formToFigure (Rectangulo p x y) = Rectangle (punto2Point p) (float2Double x) (float2Double y)
formToFigure (Poligono a)       = Polygon (map (punto2Point) a)
formToFigure (Circulo p x)      = Circle (punto2Point p) (float2Double x)
formToFigure (Linea a)          = Line (map (punto2Point) a)
formToFigure (Elipse p x y)     = Ellipse (punto2Point p) (float2Double x) (float2Double y)
formToFigure (Grafico_Linea a s)  = final (graficoLinea2Figure a s)

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

{-Grafico de lineas-}

graficoLinea2Figure :: [Float] -> String -> [Figure]
graficoLinea2Figure a s = 
    ( graficoLineaBase ++ [ Line(floats2Puntos (reverseList a)) ]) ++ [Text (10,5) (TeXRaw(T.pack s))]

graficoLineaBase :: [Figure]
graficoLineaBase =  [ Line[(0,0), (10,0)], Line[(0,0), (0,10)] ] ++ (generateRecodsY 10) ++ (generateRecodsX 10) 


generateRecodsY :: Integer -> [Figure]
generateRecodsY n
    | (n == 0) = []
    | (n > 0) = [ Line [(0, (fromIntegral n)), (0.3, (fromIntegral n))] ] ++ generateRecodsY (n-1)

generateRecodsX :: Integer -> [Figure]
generateRecodsX n
    | (n == 0) = []
    | (n > 0) = [ Line [((fromIntegral n), 0), ((fromIntegral n), 0.3)] ] ++ generateRecodsX (n-1)
    
floats2Puntos :: [Float] -> [Point]
floats2Puntos [] = []
floats2Puntos (x:xs) =  [ punto2Point (Punto (fromIntegral(length xs)) x) ] ++ floats2Puntos xs 

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

