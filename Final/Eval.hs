module Eval where

import Text.LaTeX
import Text.LaTeX.Packages.TikZ.Simple
import AST

main :: IO ()
main = execLaTeXT tikzsimple >>= renderFile "dibujo.tex"

tikzsimple :: LaTeXT IO ()
tikzsimple = thePreamble >> document theBody

thePreamble :: LaTeXT IO ()
thePreamble = do
  documentclass [] article
  usepackage [] tikz

theBody :: LaTeXT IO ()
theBody = mapM_ (center . tikzpicture . figuretikz) [ figuraFinal( convertForms[{-CodigoParseado-}] )]

figuraFinal :: [Figure] -> Figure
figuraFinal a = Figures a

formToFigure :: Forma -> Figure
formToFigure (Cuadrado x) =  Rectangle (0,0) x x
formToFigure (Rectangulo x y) =  Rectangle (3,3) x y
formToFigure (Circulo x) =  Circle (3,3) x

convertForms :: [Forma] -> [Figure]
convertForms [] = []
convertForms [x] = [formToFigure x]
convertForms (x:xs) = [formToFigure x] ++ convertForms xs
