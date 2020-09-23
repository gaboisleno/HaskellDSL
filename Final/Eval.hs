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
theBody listaFigure = mapM_ (center . tikzpicture . figuretikz) [ figuraFinal( listaFigure )]

figuraFinal :: [Figure] -> Figure
figuraFinal a = Figures a

formToFigure :: Forma -> Figure
formToFigure (Cuadrado p x)      = Rectangle (punto2Point p) (float2Double x) (float2Double x)
formToFigure (Texto p s)          = Text (punto2Point p) (TeXRaw(T.pack s))
formToFigure (Rectangulo p x y) = Rectangle (punto2Point p) (float2Double x) (float2Double y)
formToFigure (Poligono a)         = Polygon (map (punto2Point) a)
formToFigure (Circulo p x)       = Circle (punto2Point p) (float2Double x)
formToFigure (Linea a)            = Line (map (punto2Point) a)
--formToFigure (Elipse p x y)     = Elipse (punto2Point p) (float2Double x) (float2Double y)

convertForms :: [Forma] -> [Figure]
convertForms [] = []
convertForms [x] = [formToFigure x]
convertForms (x:xs) = [formToFigure x] ++ convertForms xs

punto2Point :: Punto -> Point
punto2Point (Punto a b) = ((float2Double a), (float2Double b))
