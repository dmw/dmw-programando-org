<!-- -*- coding: utf-8; -*- -->




Desafío Octubre - Noviembre 2012: Fortran IV
===

Este es un analizador estatico de **Fortran IV** que puede generar
el Arbol Sintáctico Abstracto del código fuente de programas **Fortran IV**
utilizando *GraphViz*. Esta escrito en Haskell y usa una biblioteca
de *Parser Combinators* llamada **Parsec**. El parser es enteramente
basado en combinadores.

[Desafío Octubre - Noviembre 2012: Fortran IV](http://www.programando.org/blog/2012/10/desafio-octubre-codigo-spaghetti/)

El parser esta en el archivo:
[FortranParser.hs](https://github.com/dmw/dmw-programando-org/blob/master/fortran4/FortranParser.hs)

El procesador del AST y Grafo esta en el archivo:
[FortranAST.hs](https://github.com/dmw/dmw-programando-org/blob/master/fortran4/FortranAST.hs)

El programa principal que procesa la linea de comandos esta en el archivo:
[Main.hs](https://github.com/dmw/dmw-programando-org/blob/master/fortran4/Main.hs)



¿Como compilar?
---

Se necesita GHC, y se debe ejecutar el siguiente comando:


```shell

./build.sh


```


Para ejecutarlo, se correr el comando **fortran4** una vez que está
compilado con el archivo **Fortran IV** de entrada que se quiera dibujar.


```shell

./fortran4 --proc=x11 --select=example1.f90

```


Para obtener ayuda del comando se debe ejectura con el flag **--help**

```shell

07:43 [dmw@scada:2 fortran4]$ ./fortran4 --help
fortran4
  -a FILE_A  --input_a=FILE_A  input file A
  -b FILE_B  --input_b=FILE_B  input file B
  -x PROC_G  --proc=PROC_G     processing type file
  -s PROC_S  --select=PROC_S   processing selection file
  -v         --verbose         verbose output
  -V         --version         displays program version
  -h         --help            displays this message

```


Resultados
---


![Resultado Primer Ejemplo](https://github.com/dmw/dmw-programando-org/raw/master/fortran4/example1.f90.png)

Que se encuentra en el archivo:

[example1.f90](https://github.com/dmw/dmw-programando-org/raw/master/fortran4/example1.f90)


![Resultado Segundo Ejemplo](https://github.com/dmw/dmw-programando-org/raw/master/fortran4/example2.f90.png)

Que se encuentra en el archivo:

[example2.f90](https://github.com/dmw/dmw-programando-org/raw/master/fortran4/example2.f90)

