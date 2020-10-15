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

commands :: String -> [Archivo]
commands x = process (filter (not . null ) (split x))

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ';' = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs


process :: [String] -> [Archivo]
process [] = []
process (x:xs) = case readExpr x of
                Left err -> fail ("Error: " ++ show err) putStrLn
                Right archivo -> [archivo] ++ process xs


readExpr :: String -> Either ParseError Archivo
readExpr input = parse (spaces >> parseExpr) "" input

parseExpr :: Parser Archivo
parseExpr =  parseArchivo    
         
parseArchivo :: Parser Archivo 
parseArchivo = do
                spaces
                e0     <- many (noneOf(" ="))
                lexeme $  char '='
                --f      <- ( `sepBy` string "++" ) parseFigura
                f      <- many1 parseFigura
                return $  (Archivo e0 f)
         
parseFigura :: Parser Forma
parseFigura = parseTexto
           <|> parseLinea
           <|> parseCuadrado
           <|> parseCirculo
           <|> parseRectangulo
           <|> parsePoligono
           <|> parseElipse
           <|> parseGraficoTorta
           <|> parseLoop

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

spaces :: Parser ()
spaces = void $ many $ oneOf(" \n\t")

lexeme :: Parser a -> Parser a
lexeme p = do
            spaces
            x <- p
            spaces
            return x

parsePunto :: Parser Punto --(x y)
parsePunto = do
                lexeme $ char '('
                spaces
                e0     <- floating
                spaces
                e1     <- floating
                spaces
                lexeme $ char ')'
                return $  (Punto e0 e1)

parseLinea :: Parser Forma --Linea (x y) (x y) ... n
parseLinea = do
                lexeme $  try (string "Linea")
                spaces
                p      <- many1 (try  parsePunto)
                c      <- try parseColor <|> defaultColor
                return $  (Linea p c)

parseTexto :: Parser Forma --Texto (x y) "Hola"
parseTexto = do
                lexeme $  try (string "Texto")
                spaces
                p      <- parsePunto
                lexeme $  char '"'
                e0     <- many (noneOf("\""))
                lexeme $  char '"'
                c      <- try parseColor <|> defaultColor
                return $  (Texto p e0 c)

parseCuadrado :: Parser Forma --Cuadrado (x y) j
parseCuadrado = do
                    lexeme $ try (string "Cuadrado")
                    p      <- parsePunto
                    spaces
                    e0     <- floating
                    spaces
                    c      <- try parseColor <|> defaultColor
                    return $  (Cuadrado p e0 c)

parseRectangulo :: Parser Forma --Rectangulo (x y) j k
parseRectangulo = do
                    lexeme $  try (string "Rectangulo")
                    spaces
                    p      <- parsePunto
                    spaces
                    e0     <- floating
                    spaces
                    e1     <- floating
                    c      <- try parseColor <|> defaultColor
                    return $  (Rectangulo p e0 e1 c)

parsePoligono :: Parser Forma --Poligono (x y) (x y) ... n
parsePoligono = do
                    lexeme $  try (string "Poligono")
                    spaces
                    p      <- many1 (try parsePunto)
                    c      <- try parseColor <|> defaultColor
                    return $  (Poligono p c)

parseCirculo :: Parser Forma --Circulo (x y) j
parseCirculo = do
                    lexeme $  try (string "Circulo")
                    p      <- parsePunto
                    spaces
                    e0     <- floating
                    spaces
                    c      <- try parseColor <|> defaultColor
                    return $  (Circulo p e0 c)

parseElipse :: Parser Forma --Elipse (x y) j k
parseElipse = do
                lexeme $ try (string "Elipse")
                spaces
                p      <- parsePunto
                spaces
                e0     <- floating
                spaces
                e1     <- floating
                c      <- try parseColor <|> defaultColor
                return $ (Elipse p e0 e1 c)


{--              Grafico de Torta                 --}
parseDato :: Parser Dato
parseDato = do
               lexeme $  char '"'
               e1     <- many (noneOf("\""))
               lexeme $  char '"'
               spaces
               e0     <- floating
               return $  (Dato e0 e1)

parseGraficoTorta :: Parser Forma --GraficoTorta "Algo" x "Otro" y ... n
parseGraficoTorta = do
                        lexeme $  try (string "GraficoTorta")
                        spaces
                        p      <- many1 (try parseDato)
                        c      <- try parseColor <|> defaultColor
                        return $  (GraficoTorta p c)


parseLoop :: Parser Forma
parseLoop = do
            lexeme $ try (string "Repetir")
            cond <- parseCond
            spaces
            stmt <- many1 (parseFigura)
            lexeme $ string "Fin"
            return $ (Repetidor cond stmt)

parseCond :: Parser Cond
parseCond = do
            e0     <- many digit
            lexeme $ char ':'
            e1     <- many digit
            lexeme $ char ':'
            e2     <- many digit
            return $ (Cond (read e0) (read e1) (read e2))
{-------------------------------------------------}


{--                   Color                     --}
{--  Etiqueta "-c" para agregar color a figura  --}
defaultColor :: Parser Pintura
defaultColor = return Negro

parseColor :: Parser Pintura
parseColor = do
                lexeme $ try (string "-c")
                spaces
                c      <- parsePintura
                return $ (c)

parsePintura :: Parser Pintura
parsePintura = do 
                 lexeme $ char '"' 
                 e0     <- many (noneOf "\"")
                 lexeme $ char '"'
                 return $  ( foo e0 )

foo :: String -> Pintura
foo x 
    | x == "Rojo"     = Rojo
    | x == "Amarillo" = Amarillo
    | x == "Azul"     = Azul
    | x == "Cian"     = Cian
    | x == "Magenta"  = Fucsia
    | x == "Verde"    = Verde
    | x == "Blanco"   = Blanco
    | otherwise       = Negro
{-------------------------------------------------}
