module Main where

import System.Environment (getArgs)
import System.Process

import Text.LaTeX                           --Importar libreria HaTex

import Parser (parseComm)

import Eval
---------------------------------------------------------

{--
main :: IO ()
main = do arg:_ <- getArgs
          run arg

run :: [Char] -> IO ()
run ifile = 
    do
    s <- readFile ifile
    case parseComm ifile s of
      Left error -> print error
      Right t    -> print (eval t)
    putStrLn "Done."
--}

main :: IO ()
main =
    do
        (file:_) <- getArgs
        code <- readFile file
        case parseComm file code of
               Left error -> print error
               Right t    -> execLaTeXT (tikzsimple (convertirFormas(eval(t)))) >>= renderFile (file++".tex")       
        callCommand ("pdflatex "++file++".tex")
        callCommand ("rm "++file++".aux")
        callCommand ("rm "++file++".log")
        callCommand ("rm "++file++".tex")
        callCommand ("rm *.o *.hi")
        putStrLn "Done."
     