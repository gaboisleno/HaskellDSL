module Eval where

import Text.LaTeX
import Text.LaTeX.Packages.TikZ.Simple
import AST

generateTex :: [Forma] -> IO ()
generateTex listaFigure = execLaTeXT (tikzsimple(convertForms(listaFigure))) >>= renderFile "dibujo.tex"

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

getX :: Punto -> Double
getX (Punto x y) = x

getY :: Punto -> Double
getY (Punto x y) = y

formToFigure :: Forma -> Figure
formToFigure (Cuadrado x) =  Rectangle (0,0) x x
formToFigure (Rectangulo p x y) =  Rectangle (getX(p),getY(p)) x y
formToFigure (Circulo x y ) =  Circle (3,3) x

convertForms :: [Forma] -> [Figure]
convertForms [] = []
convertForms [x] = [formToFigure x]
convertForms (x:xs) = [formToFigure x] ++ convertForms xs
