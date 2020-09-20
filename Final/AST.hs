module AST where

data Color = Rojo | Azul | Amarillo | Negro | Blanco deriving (Show)

data Punto = Punto Float Float deriving (Eq, Show)

data Forma = Texto String               --Texto ("texto", Punto)
           | Linea [Punto]              --Linea([Punto, Punto])
           | Cuadrado Float           --Cuadrado (Lado, Punto)
           | Rectangulo Float Float --Rectangulo (Lado, Lado, Punto)
           | Circulo Float     --Circulo (Radio, Punto)
           | Poligono [Punto]           --Poligono ([Punto, Punto, Punto])
 deriving(Show, Eq)
