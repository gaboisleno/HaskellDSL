module Main where

import System.Environment (getArgs)
import System.Process

import Text.LaTeX                           --Importar libreria HaTex

import Parser(commands)

import Eval

generarPDF a = 
            do
                execLaTeXT (tikzsimple (archivoToFigures(a))) >>= renderFile (getNombreArchivo(a)++".tex")
                callCommand ("pdflatex "++getNombreArchivo(a)++".tex")
                callCommand ("rm "++getNombreArchivo(a)++".aux")
                callCommand ("rm "++getNombreArchivo(a)++".log")
                callCommand ("rm "++getNombreArchivo(a)++".tex")

main :: IO()
main =
    do
        (file:_) <- getArgs
        code <- readFile file
        mapM generarPDF (commands code)
        callCommand ("rm *.o *.hi")
        putStrLn "Done."
        
