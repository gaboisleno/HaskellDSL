module AST where

type Variable = String

-- Expresiones Aritmeticas
data FloatExp = Const Double
              | Var Variable
              | UMinus FloatExp
              | Plus FloatExp FloatExp
              | Minus FloatExp FloatExp
              | Times FloatExp FloatExp
              | Div FloatExp FloatExp
 deriving (Eq, Show)

data Archivo = Archivo [Char] [Comando] deriving (Eq, Show)

data Pintura = Rojo | Azul | Amarillo | Verde | Cian | Fucsia | Negro | Blanco deriving (Eq, Show)

data Punto = Punto FloatExp FloatExp deriving (Eq, Show)

data Dato = Dato FloatExp [Char] deriving (Eq, Show)

data Cond = Cond Integer Integer Integer deriving (Eq, Show) 

data Comando = Texto Punto [Char] Pintura           
             | Linea [Punto] Pintura                
             | Cuadrado Punto FloatExp Pintura         
             | Rectangulo Punto FloatExp FloatExp Pintura 
             | Circulo Punto FloatExp Pintura          
             | Poligono [Punto] Pintura             
             | Elipse Punto FloatExp FloatExp Pintura     
             | GraficoTorta [Dato] Pintura
             | Repetidor Cond [Comando]
             | Let Variable FloatExp
           
 deriving(Show, Eq)


