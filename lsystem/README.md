<!-- -*- coding: utf-8; -*- -->




Desafío Julio - Agosto 2012: L-Systems
===

Este es un programa que dibuja L-Systems basado en un lenguaje de dominio
específico. Esta planteado como desafio en el blog www.programando.org en el
siguiente post:

[Desafío Julio - Agosto 2012: L-Systems](http://www.programando.org/blog/2012/07/desafio-julio-agosto-2012-l-systems/)

El código fuente Haskell del compilador de L-Systems esta en el archivo
[lsys.hs](https://github.com/dmw/dmw-programando-org/blob/master/lsystem/lsys.hs)




¿Como compilar?
---

Se necesita GHC, y se debe ejecutar el siguiente comando:


```shell

./build.sh


```


Para ejecutarlo, se correr el comando **lsys.bin** una vez que está
compilado con el archivo LSYS de entrada que se quiera dibujar.


```shell

./lsys.bin input-4.lsys

```

El ejemplo anterior dibujara el L-System en pantalla y entregará
el archivo **input-4.lsys.svg**.




Resultados
---


![Resultado Primer Problema](https://github.com/dmw/dmw-programando-org/raw/master/lsystem/input-1.lsys.png)

Que se encuentra en el archivo:

[input-1.lsys](https://github.com/dmw/dmw-programando-org/raw/master/lsystem/input-1.lsys)

El segundo problema esta en el archivo:

[input-2.lsys](https://github.com/dmw/dmw-programando-org/raw/master/lsystem/input-2.lsys)

Y el resultado esta aca:

![Resultado Segundo Problema](https://github.com/dmw/dmw-programando-org/raw/master/lsystem/input-2.lsys.png)

El tercer problema esta en el archivo:

[input-3.lsys](https://github.com/dmw/dmw-programando-org/raw/master/lsystem/input-3.lsys)

Y el resultado esta aca:

![Resultado Tercer Problema](https://github.com/dmw/dmw-programando-org/raw/master/lsystem/input-3.lsys.png)

Ademas se agregó el archivo
[input-4.lsys](https://github.com/dmw/dmw-programando-org/raw/master/lsystem/input-4.lsys) que
contiene un bien conocido L-System y acá esta el resultado:

![Resultado Ejemplo 4](https://github.com/dmw/dmw-programando-org/raw/master/lsystem/input-4.lsys.png)


