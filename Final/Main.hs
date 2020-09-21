module Main where

import System.Environment (getArgs)
import System.Process

import Parser(commands)

import Eval

main :: IO ()
main = do
           (expr:_) <- getArgs
           run expr
           --putStrLn (commands expr)

run :: String -> IO()
run codigo =
    do
    a <- readFile codigo
    generateTex (commands a)
    callCommand "latex -output-format=pdf dibujo.tex"
    putStrLn "Fin"
