module AST where

data Color = Rojo | Azul | Amarillo | Negro | Blanco deriving (Show)

data Punto = Punto Integer Integer deriving (Eq, Show)

data Forma = Texto String               --Texto ("texto", Punto)
           | Linea [Punto]              --Linea([Punto, Punto])
           | Cuadrado Double           --Cuadrado (Lado, Punto)
           | Rectangulo Integer Integer --Rectangulo (Lado, Lado, Punto)
           | Circulo Integer Integer    --Circulo (Radio, Punto)
           | Poligono [Punto]           --Poligono ([Punto, Punto, Punto])
 deriving(Show, Eq)
