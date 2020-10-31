module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Token hiding (lexeme) --Utilizamos nuestra propia funcion lexeme
import Text.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token
import AST

--Funcion lexeme modificada
lexeme :: Parser a -> Parser a
lexeme p = do
            spaces
            x <- p
            spaces
            return x

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","if","then", ":=",
                                                     "else", "skip", "repeat", "until", "end",
                                                     "Linea", "Texto","Cuadrado", "Rectangulo",
                                                     "Poligono", "Circulo", "Elipse", "newFile",
                                                     "endFile", "GraficoTorta", ".c"]
                                  })

-----------------------------------
--- Parser de archivos
-----------------------------------
{--
commands :: String -> [Archivo]
commands x = process (filter (not . null ) (split x))

split :: String -> [String]   
split [] = [""]
split (c:cs) | c == '@' = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs


process :: [String] -> [Archivo]
process [] = []
process (x:xs) = case readExpr x of
                Left err -> fail ("Error: " ++ show err) putStrLn
                Right archivo -> [archivo] ++ process xs

readExpr :: String -> Either ParseError Archivo
readExpr input = parse (spaces >> parseArchivo) "" input


--}
-----------------------------------
--- Parser de expressiones enteras
-----------------------------------

floatExp :: Parser FloatExp
floatExp = buildExpressionParser aOperators aTerm

aOperators = [ [Prefix (reservedOp lis "-" >> return (UMinus))          ]
             , [Infix  (reservedOp lis "*" >> return (Times )) AssocLeft,
                Infix  (reservedOp lis "/" >> return (Div   )) AssocLeft]
             , [Infix  (reservedOp lis "+" >> return (Plus  )) AssocLeft,
                Infix  (reservedOp lis "-" >> return (Minus )) AssocLeft]
              ]

-- reservedOp lis "?" >> return (Tern ))

aTerm =  parens lis floatExp
     <|> liftM Var (identifier lis)
     <|> liftM Const (float lis)


-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser BoolExp
boolexp = buildExpressionParser bOperators bTerm

bOperators = [ [Prefix (reservedOp lis "~" >> return (Not))          ]
             , [Infix  (reservedOp lis "&" >> return (And)) AssocLeft,
                Infix  (reservedOp lis "|"  >> return (Or )) AssocLeft]
             ]

bTerm =  parens lis boolexp
     <|> (reserved lis "true"  >> return (BTrue ))
     <|> (reserved lis "false" >> return (BFalse))
     <|> rExpression

rExpression =
  do a1 <- floatExp
     op <- relation
     a2 <- floatExp
     return (op a1 a2)

relation =   (reservedOp lis ">"  >> return Gt )
         <|> (reservedOp lis "<"  >> return Lt )
         <|> (reservedOp lis "="  >> return Eq )
         <|> (reservedOp lis "<>" >> return NEq)


-----------------------------------
--- Parser de formas
-----------------------------------

parseForma :: Parser Forma
parseForma = parseLinea
          <|> parseTexto
          <|> parseCuadrado
          <|> parseRectangulo
          <|> parsePoligono
          <|> parseCirculo
          <|> parseElipse
          <|> parseGraficoTorta

parsePunto :: Parser Punto
parsePunto = do
                lexeme $ char '('
                spaces
                e0     <- floatExp
                spaces
                e1     <- floatExp
                spaces
                lexeme $ char ')'
                return $  (Punto e0 e1)


parseLinea :: Parser Forma
parseLinea = do
                reserved lis "Linea"
                spaces
                p      <- many1 (try  parsePunto)
                c      <- try parseColor <|> defaultColor
                return $  (Linea p c)

parseTexto :: Parser Forma
parseTexto = do
                reserved lis "Texto"
                spaces
                p      <- parsePunto
                lexeme $  char '"'
                e0     <- many (noneOf("\""))
                lexeme $  char '"'
                c      <- try parseColor <|> defaultColor
                return $  (Texto p e0 c)

parseCuadrado :: Parser Forma
parseCuadrado = do
                    reserved lis "Cuadrado"
                    p      <- parsePunto
                    spaces
                    e0     <- floatExp
                    spaces
                    c      <- try parseColor <|> defaultColor
                    return $  (Cuadrado p e0 c)

parseRectangulo :: Parser Forma
parseRectangulo = do
                    reserved lis "Rectangulo"
                    spaces
                    p      <- parsePunto
                    spaces
                    e0     <- floatExp
                    spaces
                    e1     <- floatExp
                    c      <- try parseColor <|> defaultColor
                    return $  (Rectangulo p e0 e1 c)

parsePoligono :: Parser Forma
parsePoligono = do
                    reserved lis "Poligono"
                    spaces
                    p      <- many1 (try parsePunto)
                    c      <- try parseColor <|> defaultColor
                    return $  (Poligono p c)

parseCirculo :: Parser Forma
parseCirculo = do
                    reserved lis "Circulo"
                    spaces
                    p      <- parsePunto
                    spaces
                    e0     <- floatExp
                    spaces
                    c      <- try parseColor <|> defaultColor
                    spaces
                    return $  (Circulo p e0 c)

parseElipse :: Parser Forma
parseElipse = do
                reserved lis "Elipse"
                spaces
                p      <- parsePunto
                spaces
                e0     <- floatExp
                spaces
                e1     <- floatExp
                c      <- try parseColor <|> defaultColor
                return $ (Elipse p e0 e1 c)

parseDato :: Parser Dato
parseDato = do
               lexeme $  char '"'
               e1     <- many (noneOf("\""))
               lexeme $  char '"'
               spaces
               e0     <- floatExp
               return $  (Dato e0 e1)

parseGraficoTorta :: Parser Forma
parseGraficoTorta = do
                        reserved lis "GraficoTorta"
                        spaces
                        p      <- many1 (try parseDato)
                        c      <- try parseColor <|> defaultColor
                        spaces
                        return $  (GraficoTorta p c)


-----------------------------------
--- Parser de comandos
-----------------------------------

parseArchivos ::  Parser [Archivo] 
parseArchivos = do
                 a <- many1 (try parseArchivo)
                 return $ a

parseArchivo ::  Parser Archivo 
parseArchivo = do
                spaces
                e0     <- many (noneOf(" ="))
                lexeme $  char '='
                reserved lis "newFile"
                f      <- comm
                reserved lis "endFile"
                return $  (Archivo e0 f)

comm :: Parser Comm
comm = parens lis comm
     <|> sequenceOfComm

listToSeq [] = Skip
listToSeq [x] = x
listToSeq (x:xs) = Seq x (listToSeq xs)

sequenceOfComm :: Parser Comm
sequenceOfComm =
  do list <- (sepBy1 comm' (semi lis))
     return $ (listToSeq list)

comm' :: Parser Comm
comm' =    ifComm
       <|> repeatComm
       <|> skipComm
       <|> assignComm
       <|> drawComm


ifComm :: Parser Comm
ifComm =
  do reserved lis "if"
     cond  <- boolexp
     reserved lis "then"
     stmt1 <- comm
     reserved lis "else"
     stmt2 <- comm
     reserved lis "end"
     return $ Cond cond stmt1 stmt2

skipComm :: Parser Comm
skipComm = reserved lis "skip" >> return Skip

assignComm :: Parser Comm
assignComm = do var <- identifier lis
                reservedOp lis ":="
                expr <- floatExp
                return $ Let var expr

drawComm :: Parser Comm
drawComm = do
           form <- parseForma
           return $ Draw form

repeatComm :: Parser Comm
repeatComm =
  do reserved lis "repeat"
     stmt1 <- comm
     reserved lis "until"
     cond <- boolexp
     reserved lis "end"
     return $ Repeat cond stmt1

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError [Archivo]
parseComm = parse (totParser parseArchivos)


------------------------------------
-- Función de parseo de color
------------------------------------

--Etiqueta ".c" para agregar color a figura
parseColor :: Parser Pintura
parseColor = do
                reserved lis ".c"
                spaces
                c      <- parsePintura
                return $ (c)

defaultColor :: Parser Pintura
defaultColor = return Negro

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
