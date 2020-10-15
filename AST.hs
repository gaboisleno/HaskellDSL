module AST where

data Archivo = Archivo [Char] [Forma] deriving (Eq, Show)

data Pintura = Rojo | Azul | Amarillo | Verde | Cian | Fucsia | Negro | Blanco deriving (Eq, Show)

data Punto = Punto Float Float deriving (Eq, Show)

data Dato = Dato Float [Char] deriving (Eq, Show)

data Cond = Cond Integer Integer Integer deriving (Eq, Show) 

data Forma = Texto Punto [Char] Pintura           
           | Linea [Punto] Pintura                
           | Cuadrado Punto Float Pintura         
           | Rectangulo Punto Float Float Pintura 
           | Circulo Punto Float Pintura          
           | Poligono [Punto] Pintura             
           | Elipse Punto Float Float Pintura     
           | GraficoTorta [Dato] Pintura
           | Repetidor Cond [Forma]
 deriving(Show, Eq)

 


