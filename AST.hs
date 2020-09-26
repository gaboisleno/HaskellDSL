module AST where

data Color = Rojo | Azul | Amarillo | Negro | Blanco deriving (Show)

data Punto = Punto Float Float deriving (Eq, Show)

data Dato = Dato Float [Char] deriving (Eq, Show)

data Forma = Texto Punto [Char]           --Texto ("texto", Punto)
           | Linea [Punto]                --Linea([Punto, Punto])
           | Cuadrado Punto Float         --Cuadrado (Lado, Punto)
           | Rectangulo Punto Float Float --Rectangulo (Lado, Lado, Punto)
           | Circulo Punto Float          --Circulo (Radio, Punto)
           | Poligono [Punto]             --Poligono ([Punto, Punto, Punto])
           | Elipse Punto Float Float     --Elipse
           | GraficoTorta [Dato]
           | GraficoLinea [Float] String
 deriving(Show, Eq)
