module Eval where

import qualified Data.Text as T
import GHC.Float(float2Double)              --Importar libreria con funcion float2Double

import Text.LaTeX                           --Importar libreria HaTex
import Text.LaTeX.Base.Syntax               --Importar libreria con funcion TeXRaw
import Text.LaTeX.Packages.TikZ.Simple      --Importar libreria HaTex TikZ

import AST

-- Estados
type State = [(Variable,Comm)]

-- Evalua un programa en el estado nulo. Devuelvo un estado con los comandos.
-- Si es una variable entonces su nombre y un comando Let
-- Si es una forma entonces "Figura" y un comando Draw (Obtengo una constante de todas las expresiones que no lo sean)
eval :: Archivo -> State
eval (Archivo nombre comm) = evalComm comm initState

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Double
lookfor var ( (x, (Let y (Const z))) :xs) = if   var == x 
                                            then z
                                            else lookfor var xs
lookfor var ( (x, y) :xs) = lookfor var xs

-- Cambia el valor de una variable en un estado
updateVariable :: Variable -> Double -> State -> State
updateVariable var valor [] = [(var, (Let var (Const valor)) )]
updateVariable var valor ((x,y):xs) = if   var == x 
                                      then (var, (Let var (Const valor))):xs 
                                      else (x,y): updateVariable var valor xs

-- Agrega forma a un estado
updateForma :: Forma -> State -> State
updateForma form estado = estado ++ [("Figura", (Draw form) )]

listarPuntos :: [Punto] -> State -> [Punto]
listarPuntos [(Punto x y)] estado = [Punto (Const (evalFloatExp x estado)) (Const (evalFloatExp y estado))]
listarPuntos ((Punto x y):xs) estado = [Punto (Const (evalFloatExp x estado)) (Const (evalFloatExp y estado))] ++ listarPuntos xs estado

listarDatos :: [Dato] -> State -> [Dato]
listarDatos [(Dato x y)] estado = [Dato (Const (evalFloatExp x estado)) y]
listarDatos ((Dato x y):xs) estado = [Dato (Const (evalFloatExp x estado)) y] ++ listarDatos xs estado

-- Evalua un comando en un estado dado
evalComm :: Comm -> State -> State
evalComm Skip e = e
evalComm (Let var x) estado = let valor = evalFloatExp x estado 
                                in updateVariable var valor estado
evalComm (Draw form) estado = updateForma (evalForma form estado) estado                           
evalComm (Seq com1 com2) estado = let estado' = evalComm com1 estado
                                  in evalComm com2 estado'
evalComm (Cond expBool com1 com2) estado = if (evalBoolExp expBool estado)
                                                 then evalComm com1 estado
                                                 else evalComm com2 estado
evalComm (Repeat expBool comm) estado = evalComm (Seq comm (Cond expBool Skip (Repeat expBool comm))) estado

-- Evalua una forma. Transformo expresiones de una forma en constantes para luego convertirlas en Figure
evalForma :: Forma -> State -> Forma
evalForma (Linea a c) estado                    = Linea (listarPuntos a estado) c
evalForma (Texto (Punto x y) a c) estado        = Texto (Punto (Const (evalFloatExp x estado)) (Const (evalFloatExp y estado))) a c
evalForma (Cuadrado (Punto x y) a c) estado     = Cuadrado (Punto (Const (evalFloatExp x estado)) (Const (evalFloatExp y estado))) (Const (evalFloatExp a estado)) c
evalForma (Rectangulo (Punto x y) a b c) estado = Rectangulo (Punto (Const (evalFloatExp x estado)) (Const (evalFloatExp y estado))) (Const (evalFloatExp a estado)) (Const (evalFloatExp b estado)) c
evalForma (Poligono a c) estado                 = Poligono (listarPuntos a estado) c
evalForma (Circulo (Punto x y) a c) estado      = Circulo (Punto (Const (evalFloatExp x estado)) (Const (evalFloatExp y estado))) (Const (evalFloatExp a estado)) c
evalForma (Elipse (Punto x y) a b c) estado     = Elipse (Punto (Const (evalFloatExp x estado)) (Const (evalFloatExp y estado))) (Const (evalFloatExp a estado)) (Const (evalFloatExp b estado)) c
evalForma (GraficoTorta a o c) estado           = GraficoTorta (listarDatos a estado) o c

-- Evalua una expresion entera
evalFloatExp :: FloatExp -> State -> Double
evalFloatExp (Const valor) estado = valor
evalFloatExp (Var variable) estado = lookfor variable estado
evalFloatExp (UMinus expInt) estado = let valor = evalFloatExp expInt estado
                                    in -valor
evalFloatExp (Plus exp1 exp2) estado = let valor1 = evalFloatExp exp1 estado
                                           valor2 = evalFloatExp exp2 estado
                                           in valor1 + valor2

evalFloatExp (Minus exp1 exp2) estado = let valor1 = evalFloatExp exp1 estado
                                            valor2 = evalFloatExp exp2 estado
                                            in valor1 - valor2

evalFloatExp (Times exp1 exp2) estado = let valor1 = evalFloatExp exp1 estado
                                            valor2 = evalFloatExp exp2 estado
                                            in valor1 * valor2   

evalFloatExp (Div exp1 exp2) estado = let valor1 = evalFloatExp exp1 estado
                                          valor2 = evalFloatExp exp2 estado
                                          in (/) valor1 valor2                                                                                                                                                                     

-- Evalua una expresion entera
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue estado = True
evalBoolExp BFalse estado = False
evalBoolExp (Eq exp1 exp2) estado = let valor1 = evalFloatExp exp1 estado
                                        valor2 = evalFloatExp exp2 estado
                                        in valor1 == valor2

evalBoolExp (Lt exp1 exp2) estado = let valor1 = evalFloatExp exp1 estado
                                        valor2 = evalFloatExp exp2 estado
                                        in valor1 < valor2

evalBoolExp (Gt exp1 exp2) estado = let valor1 = evalFloatExp exp1 estado
                                        valor2 = evalFloatExp exp2 estado
                                        in valor1 > valor2

evalBoolExp (And exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                         valor2 = evalBoolExp exp2 estado
                                         in valor1 && valor2

evalBoolExp (Or exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                        valor2 = evalBoolExp exp2 estado
                                        in valor1 || valor2
evalBoolExp (Not exp1) estado = not (evalBoolExp exp1 estado)



--Funciones para construir documento con dibujo

--Obtener el nombre del archivo para asignar a PDF
getNombreArchivo :: Archivo -> [Char]
getNombreArchivo (Archivo nombre comm) = nombre

{-- Funciones para obtener documento PDF --}
tikzsimple :: [Figure] -> LaTeXT IO ()
tikzsimple listaFigure = thePreamble >> document (theBody(listaFigure))

thePreamble :: LaTeXT IO ()
thePreamble = do
  documentclass [] article
  usepackage [] tikz

--A todo el listado de Figure lo transformo en un solo Figure antes de enviarlo
theBody :: [Figure] -> LaTeXT IO ()
theBody listaFigure = mapM_ (center . tikzpicture . figuretikz) [figuresToFigure(listaFigure)]
{------------------------------------------}

figuresToFigure :: [Figure] -> Figure
figuresToFigure a = Figures a

--Transformo el Punto del AST al Point de HaTeX
puntoToPoint :: Punto -> Point
puntoToPoint (Punto (Const a) (Const b)) = (a, b)

pinturaToColor :: Pintura -> Color
pinturaToColor c 
             | c == Rojo     = Red
             | c == Amarillo = Yellow
             | c == Azul     = Blue
             | c == Verde    = Green
             | c == Cian     = Cyan
             | c == Fucsia   = Magenta
             | c == Blanco   = White
             | otherwise     = Black

--Transformo la Forma en un Figure de HaTeX
formaToFigure :: Forma -> Figure
formaToFigure (Texto p s c)                         = Colored (BasicColor (pinturaToColor c) ) $ Text (puntoToPoint p) (TeXRaw(T.pack s))
formaToFigure (Linea a c)                           = Colored (BasicColor (pinturaToColor c) ) $ Line (map (puntoToPoint) a)
formaToFigure (Cuadrado p (Const x) c)              = Colored (BasicColor (pinturaToColor c) ) $ Rectangle (puntoToPoint p) x x
formaToFigure (Rectangulo p (Const x) (Const y) c)  = Colored (BasicColor (pinturaToColor c) ) $ Rectangle (puntoToPoint p) x y
formaToFigure (Poligono a c)                        = Colored (BasicColor (pinturaToColor c) ) $ Polygon (map (puntoToPoint) a)
formaToFigure (Circulo p (Const x) c)               = Colored (BasicColor (pinturaToColor c) ) $ Circle (puntoToPoint p) x
formaToFigure (Elipse p (Const x) (Const y) c)      = Colored (BasicColor (pinturaToColor c) ) $ Ellipse (puntoToPoint p) x y
formaToFigure (GraficoTorta d o c)                  = Colored (BasicColor (pinturaToColor c) ) $ figuresToFigure(generarGraficoTorta d o)

convertirFormas :: State -> [Figure]
convertirFormas [] = []
convertirFormas ((var, Let v e):xs) = [] ++ convertirFormas xs
convertirFormas ((var, Draw form):xs) = [formaToFigure form] ++ convertirFormas xs


{--                   Funciones para grafico de torta                   --}
--Obtengo el Double de un Dato
getDoubleFromData :: Dato -> Double
getDoubleFromData (Dato (Const a) b) = a

--Obtengo el String de un Dato para imprimir en grafico de torta
getStringFromData :: Dato -> [Char]
getStringFromData (Dato a b) = b

--Transformo un porcentaje en un punto para dibujar una linea basandome que la primer linea del circulo esta en la posicion: (0.0 0.0) (0.0 4.0)
porcentajeToPuntoLinea :: Double -> Point
porcentajeToPuntoLinea p =( (cos ( (p*360/100) * 2 * pi/360) * 4),  (sin ( (p*360/100) * 2 * pi/360) * 4)  )

--Transformo un porcentaje en un punto para dibujar una texto basandome que la primer linea del circulo esta en la posicion: (0.0 0.0) (0.0 4.0)
porcentajeToPuntoTexto :: Double -> Point
porcentajeToPuntoTexto p =( (cos ( (p*360/100) * 2 * pi/360) * 2.5), (sin ( (p*360/100) * 2 * pi/360) * 2.5)  )

--Ejecuto funcion recursiva para generar las lineas del grafico de torta con los datos proporcionados y concateno el circulo base
generarGraficoTorta :: [Dato] -> Dato -> [Figure]
generarGraficoTorta d (Dato (Const x) y) = if x == 0.0 
                                        then generarLineasGraficoTorta d 100.0 ++ [ LineWidth (Pt 2) $ Circle (0,0) 4, CircleFilled (0,0) 0.05, LineWidth (Pt 3) $ Line [(0,0), (4,0)] ]
                                        else  if obtenerPorcentaje d >= 100.0
                                                then generarLineasGraficoTorta d 100 ++ [ LineWidth (Pt 2) $ Circle (0,0) 4, CircleFilled (0,0) 0.05, LineWidth (Pt 3) $ Line [(0,0), (4,0)] ]
                                                else generarLineasGraficoTorta d 100 ++ [ LineWidth (Pt 2) $ Circle (0,0) 4, CircleFilled (0,0) 0.05, LineWidth (Pt 3) $ Line [(0,0), (4,0)] ] ++ [Text (porcentajeToPuntoTexto( 100-(( 100-(obtenerPorcentaje d))/2))) (TeXRaw(T.pack y))]

--Funcion recursiva para para generar las lineas del grafico de torta junto con el texto de cada Dato
generarLineasGraficoTorta :: [Dato] -> Double -> [Figure]
generarLineasGraficoTorta [] _ = []
generarLineasGraficoTorta (x:xs) porcentaje = [datoToLineaGraficoTorta x porcentaje] ++ generarLineasGraficoTorta xs (porcentaje - getDoubleFromData(x))

--Obtengo porcentaje total de un listado de Dato
obtenerPorcentaje :: [Dato] -> Double
obtenerPorcentaje [] = 0.0
obtenerPorcentaje ((Dato (Const x) y):xs) = x + obtenerPorcentaje(xs)

--Convierto un Dato en una linea y un texto ubicados en la posicion correcta en relacion al porcentaje restante del circulo
datoToLineaGraficoTorta :: Dato -> Double -> Figure
datoToLineaGraficoTorta d porcentaje = if porcentaje == 100.0
                                        then Figures [LineWidth (Pt 3) $ Line [(0,0), porcentajeToPuntoLinea(getDoubleFromData(d))],  Text (porcentajeToPuntoTexto(getDoubleFromData(d)/2)) (TeXRaw(T.pack (getStringFromData(d))))]
                                        else Figures [LineWidth (Pt 3) $ Line [(0,0), porcentajeToPuntoLinea( 100-porcentaje+getDoubleFromData(d) )],  Text (porcentajeToPuntoTexto(100-porcentaje+getDoubleFromData(d)/2)) (TeXRaw(T.pack (getStringFromData(d))))]

{------------------------------------------------------------------------}