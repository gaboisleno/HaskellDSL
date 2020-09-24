module Eval where

import qualified Data.Text as T

import GHC.Float(float2Double)              --Importar libreria con funcion float2Double

import Text.LaTeX                           --Importar libreria HaTex
import Text.LaTeX.Base.Syntax               --Importar libreria con funcion TeXRaw
import Text.LaTeX.Packages.TikZ.Simple      --Importar libreria HaTex TikZ

import AST

generateTex :: [Forma] -> IO ()
generateTex listaForma = execLaTeXT (tikzsimple(convertForms(listaForma))) >>= renderFile "dibujo.tex"

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
formToFigure (GraficoTorta d)     = figuresToFigure(generarGraficoTorta d 100)

convertForms :: [Forma] -> [Figure]
convertForms [] = []
convertForms [x] = [formToFigure x]
convertForms (x:xs) = [formToFigure x] ++ convertForms xs

punto2Point :: Punto -> Point
punto2Point (Punto a b) = ((float2Double a), (float2Double b))

--Funciones para grafico de torta
getFloatFromData :: Dato -> Float
getFloatFromData (Dato a b) = a

getStringFromData :: Dato -> [Char]
getStringFromData (Dato a b) = b

generarGraficoTorta :: [Dato] -> Float -> [Figure]
generarGraficoTorta [] _ = []
generarGraficoTorta [x] porcentaje = [datoToFigure x porcentaje]
generarGraficoTorta (x:xs) porcentaje = [datoToFigure x porcentaje] ++ generarGraficoTorta xs (porcentaje - getFloatFromData(x))

datoToFigure :: Dato -> Float -> Figure
datoToFigure d porcentaje = if porcentaje == 100
                            then LineWidth (Pt 3) $ Line [(0,0), porcentajeToPuntoLinea(getFloatFromData(d))]
                            else LineWidth (Pt 3) $ Line [(0,0), porcentajeToPuntoLinea( 100-porcentaje+getFloatFromData(d) )]

porcentajeToPuntoLinea :: Float -> Point
porcentajeToPuntoLinea p = (float2Double (cos ( (p*360/100) * 2 * pi/360) * 4), float2Double (sin ( (p*360/100) * 2 * pi/360) * 4))

porcentajeToPuntoTexto :: Float -> Point
porcentajeToPuntoTexto p = (float2Double (cos ( (p*360/100) * 2 * pi/360) * 2.5), float2Double (sin ( (p*360/100) * 2 * pi/360) * 2.5))
