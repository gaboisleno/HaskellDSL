#DSL Programacion Profesional

###Uso sin compilar:

```sh
*Main> regularParse parseTexto "\"Hola mundo\""
*Main> regularParse parsePunto "1 2"
*Main> regularParse parseLinea  "2 1 2 3"
```
###Uso compilado: 

1) Compilar:	

```sh
$ ghc -o main parsev2.hs
```

2) Ejecutar:

```sh
$ ./main "Texto\"Hola Mundo\""
Found value: Texto "Hola Mundo"

$ ./main "Rectangulo(2,2)"
Found value: Rectangulo 2 2

$ ./main "Cuadrado(2)"
Found value: Cuadrado 2

$ ./main "Punto(2,2)"
Found value: Punto 2 2

$ ./main "Linea(Punto(2,2),Punto(3,3))"
Found value: Linea [Punto 2 2,Punto 3 3]
```

###Docs + Instalation
| Package | Source |
| ------- | ------ |
| Cabal | https://www.haskell.org/cabal/ |
| Haskell | https://www.haskell.org |
| Hatex | https://hackage.haskell.org/package/HaTeX |
| Latex | https://www.latex-project.org |
| Parsec | https://hackage.haskell.org/package/parsec |
| Tikz | https://www.overleaf.com/learn/latex/TikZ_package |
| 
