module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones Aritmeticas
data FloatExp = Const Double
              | Var Variable
              | UMinus FloatExp
              | Plus FloatExp FloatExp
              | Minus FloatExp FloatExp
              | Times FloatExp FloatExp
              | Div FloatExp FloatExp
              | Tern BoolExp FloatExp FloatExp
 deriving (Eq, Show)

-- Expresiones Booleanas
data BoolExp = BTrue
               | BFalse
               | Eq FloatExp FloatExp
               | Lt FloatExp FloatExp
               | Gt FloatExp FloatExp
               | And BoolExp BoolExp
               | Or BoolExp BoolExp
               | Not BoolExp
 deriving (Eq, Show)

-- Comandos (sentencias)
data Comm = Skip
            | Let Variable FloatExp
            | Draw Forma
            | Seq Comm Comm
            | Cond BoolExp Comm Comm
            | Repeat BoolExp Comm
 deriving (Eq, Show)

data Archivo = Archivo [Char] Comm deriving (Eq, Show)

data Pintura = Rojo | Azul | Amarillo | Verde | Cian | Fucsia | Negro | Blanco deriving (Eq, Show)

data Punto = Punto FloatExp FloatExp deriving (Eq, Show)

data Dato = Dato FloatExp [Char] deriving (Eq, Show)

data Forma = Texto Punto [Char] Pintura           
           | Linea [Punto] Pintura                
           | Cuadrado Punto FloatExp Pintura         
           | Rectangulo Punto FloatExp FloatExp Pintura 
           | Circulo Punto FloatExp Pintura          
           | Poligono [Punto] Pintura             
           | Elipse Punto FloatExp FloatExp Pintura     
           | GraficoTorta [Dato] Dato Pintura 
 deriving(Show, Eq)