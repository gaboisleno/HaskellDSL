module Main where

import System.Environment (getArgs)
import System.Process

import Text.LaTeX                           --Importar libreria HaTex

import Parser (parseComm)

import Eval
---------------------------------------------------------

{--
-- Codigo para probar parser
main :: IO ()
main = do arg:_ <- getArgs
          run arg

run :: [Char] -> IO ()
run ifile = 
    do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
      Right (x:xs)    -> print (eval x)
    putStrLn "Done."
--}

generarPDF a = 
            do
                execLaTeXT (tikzsimple (convertirFormas(eval(a)))) >>= renderFile (getNombreArchivo(a)++".tex")
                callCommand ("pdflatex "++getNombreArchivo(a)++".tex")
                callCommand ("rm "++getNombreArchivo(a)++".aux")
                callCommand ("rm "++getNombreArchivo(a)++".log")
                callCommand ("rm "++getNombreArchivo(a)++".tex") 

main :: IO ()
main =
    do
        (file:_) <- getArgs
        code <- readFile file
        case parseComm file code of
               Left error -> print error
               Right t    -> mapM_ generarPDF (t)
        callCommand ("rm *.o *.hi")  
        putStrLn "Done."