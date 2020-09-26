module Eval where

import qualified Data.Text as T
import GHC.Float(float2Double)              --Importar libreria con funcion float2Double

import Text.LaTeX                           --Importar libreria HaTex
import Text.LaTeX.Base.Syntax               --Importar libreria con funcion TeXRaw
import Text.LaTeX.Packages.TikZ.Simple      --Importar libreria HaTex TikZ

import AST

tikzsimple :: [Figure] -> LaTeXT IO ()
tikzsimple listaFigure = thePreamble >> document (theBody(listaFigure))

thePreamble :: LaTeXT IO ()
thePreamble = do
  documentclass [] article
  usepackage [] tikz

theBody :: [Figure] -> LaTeXT IO ()
theBody listaFigure = mapM_ (center . tikzpicture . figuretikz) [figuresToFigure(listaFigure)]

figuresToFigure :: [Figure] -> Figure
figuresToFigure a = Figures a

formToFigure :: Forma -> Figure
formToFigure (Cuadrado p x)       = Rectangle (punto2Point p) (float2Double x) (float2Double x)
formToFigure (Texto p s)          = Text (punto2Point p) (TeXRaw(T.pack s))
formToFigure (Rectangulo p x y)   = Rectangle (punto2Point p) (float2Double x) (float2Double y)
formToFigure (Poligono a)         = Polygon (map (punto2Point) a)
formToFigure (Circulo p x)        = Circle (punto2Point p) (float2Double x)
formToFigure (Linea a)            = Line (map (punto2Point) a)
formToFigure (Elipse p x y)       = Ellipse (punto2Point p) (float2Double x) (float2Double y)
formToFigure (GraficoTorta d)     = figuresToFigure(generarGraficoTorta d)
formToFigure (GraficoLinea a s)   = figuresToFigure(graficoLinea2Figure a s)
formToFigure (Pintado c f)        = Colored (BasicColor ( pintura2Color c ) ) $ (formToFigure f)

convertForms :: [Forma] -> [Figure]
convertForms [] = []
convertForms (x:xs) = [formToFigure x] ++ convertForms xs

punto2Point :: Punto -> Point
punto2Point (Punto a b) = ((float2Double a), (float2Double b))

pintura2Color :: Pintura -> Color
pintura2Color c 
            | c == Rojo     = Red
            | c == Amarillo = Yellow
            | c == Azul     = Blue
            | c == Verde    = Green
            | c == Cian     = Cyan
            | c == Fucsia   = Magenta
            | c == Blanco   = White
            | otherwise     = Black

{----------Funciones para grafico de torta----------}
--Obtengo el Float de un Dato
getFloatFromData :: Dato -> Float
getFloatFromData (Dato a b) = a

--Obtengo el String de un Dato
getStringFromData :: Dato -> [Char]
getStringFromData (Dato a b) = b

porcentajeToPuntoLinea :: Float -> Point
porcentajeToPuntoLinea p = (float2Double (cos ( (p*360/100) * 2 * pi/360) * 4), float2Double (sin ( (p*360/100) * 2 * pi/360) * 4))

porcentajeToPuntoTexto :: Float -> Point
porcentajeToPuntoTexto p = (float2Double (cos ( (p*360/100) * 2 * pi/360) * 2.5), float2Double (sin ( (p*360/100) * 2 * pi/360) * 2.5))

--Ejecuto funcion recursiva para generar las lineas del grafico de torta con los datos proporcionados y concateno el circulo base
generarGraficoTorta :: [Dato] -> [Figure]
generarGraficoTorta d = generarLineasGraficoTorta d 100 ++ [ LineWidth (Pt 2) $ Circle (0,0) 4, CircleFilled (0,0) 0.05, LineWidth (Pt 3) $ Line [(0,0), (4,0)] ]

--Funcion recursiva para para generar las lineas del grafico de torta junto con el texto de cada Dato
generarLineasGraficoTorta :: [Dato] -> Float -> [Figure]
generarLineasGraficoTorta [] _ = []
generarLineasGraficoTorta (x:xs) porcentaje = [datoToLineaGraficoTorta x porcentaje] ++ generarLineasGraficoTorta xs (porcentaje - getFloatFromData(x))

--Convierto un Dato en una linea y un texto ubicados en la posicion correcta en relacion al porcentaje restante del circulo
datoToLineaGraficoTorta :: Dato -> Float -> Figure
datoToLineaGraficoTorta d porcentaje = if porcentaje == 100
                                        then Figures [LineWidth (Pt 3) $ Line [(0,0), porcentajeToPuntoLinea(getFloatFromData(d))],  Text (porcentajeToPuntoTexto(getFloatFromData(d)/2)) (TeXRaw(T.pack (getStringFromData(d))))]
                                        else Figures [LineWidth (Pt 3) $ Line [(0,0), porcentajeToPuntoLinea( 100-porcentaje+getFloatFromData(d) )],  Text (porcentajeToPuntoTexto(100-porcentaje+getFloatFromData(d)/2)) (TeXRaw(T.pack (getStringFromData(d))))]


{----------Funciones para grafico de linea----------}

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
