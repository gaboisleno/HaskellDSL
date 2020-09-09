{- Estos son los unicos imports que estan en el ejemplo de clase

module Main where

import System.Environment (getArgs)
import Parser (parseComm)

import Eval
-}

module Main where

import System.Environment (getArgs)
import Text.Parsec (parse)

import Parser

main :: IO ()
main = do
           (expr:_) <- getArgs
           putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input =
    case parse parseExpr "" input of
        Right val  -> "Found value: " ++ show val
        Left err   -> case parse parsePunto "" input of
                        Right val  -> "Found value: " ++ show val
                        Left err   -> "Fail on: " ++ show err
