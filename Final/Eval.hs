module Eval where

import GHC.Float
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

formToFigure :: Forma -> Figure
formToFigure (Cuadrado x) = Rectangle (0,0) (float2Double x) (float2Double x)
formToFigure (Rectangulo x y) = Rectangle (0,0) (float2Double x) (float2Double y)
--formToFigure (Circulo x) = Circle (0,0) (float2Double x)

convertForms :: [Forma] -> [Figure]
convertForms [] = []
convertForms [x] = [formToFigure x]
convertForms (x:xs) = [formToFigure x] ++ convertForms xs
