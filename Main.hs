module Main where

import System.Environment (getArgs)
import System.Process

import Text.LaTeX                           --Importar libreria HaTex

import Parser(commands)

import Eval

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
