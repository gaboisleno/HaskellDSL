module AST where

type Variable = String

data FloatVar = Const Float
              | Var Variable
 deriving(Show, Eq)

data Archivo = Archivo [Char] [Comando] deriving (Eq, Show)

data Pintura = Rojo | Azul | Amarillo | Verde | Cian | Fucsia | Negro | Blanco deriving (Eq, Show)

data Punto = Punto FloatVar FloatVar deriving (Eq, Show)

data Dato = Dato FloatVar [Char] deriving (Eq, Show)

data Cond = Cond Integer Integer Integer deriving (Eq, Show) 

data Comando = Texto Punto [Char] Pintura           
             | Linea [Punto] Pintura                
             | Cuadrado Punto FloatVar Pintura         
             | Rectangulo Punto FloatVar FloatVar Pintura 
             | Circulo Punto FloatVar Pintura          
             | Poligono [Punto] Pintura             
             | Elipse Punto FloatVar FloatVar Pintura     
             | GraficoTorta [Dato] Pintura
             | Repetidor Cond [Comando]
             | Let Variable FloatVar
           
 deriving(Show, Eq)


