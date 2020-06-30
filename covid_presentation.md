---
title: Aplicaci�n y aproximaciones al modelamiento SIR para el SARS-CoV-2.
author: Mora, S. & Hernandez, A. & Camargo, A.
date: 2020-05-26
output: revealjs::revealjs_presentation
---
<style>
.small-code pre code {
  font-size: 1em;
}
</style>

Introducci�n
========================================================




En diciembre de 2019 en la ciudad de Wuhan (China) se registraron varios casos de personas enfermas con una neumon�a viral, que posteriormente ser�a atribuida a un nuevo coronavirus, el COVID-19 o SARS-CoV-2. Para enero 30 del 2020 la OMS (organizaci�n mundial de la salud) lo declar� un problema de salud p�blica internacional. En los siguientes meses el virus se propag� r�pidamente por el continente asi�tico, luego por el continente europeo y finalmente alrededor del mundo. Para el 11 de marzo del 2020, la OMS reconoci� al SARS-CoV-2 como una pandemia global. 


Objetivo General
========================================================

*Crear un modelo que permita entender la propagaci�n del SARS-CoV-2, prediciendo la curva de infectados y su duraci�n a lo largo del tiempo en Colombia.*

Objetivos Especificos
========================================================

1. �C�ando ser� el pico de la pandemia?
2. �C�antas personas estar�n infectadas en el pico de la pandemia?
3. �C�antos casos severos se tendr�n a tener en el pa�s?
4. �C�antos casos necesitar�n cuidados intensivos?
5. �C�antos fallecidos habr�n?
6. �C�ando finalizar� la pandemia?

M�s objetivos
========================================================

* Generar reportes que describan la situaci�n de cada pa�s.
* Crear gr�ficos con las predicciones de las curvas de infectados. 
* Construir una simulaci�n que compare movimiento libre vs restringido.
* Construir una simulaci�n que compare la distancia social.
* Construir un mapa para visualizar la cantidad de incdencias por pa�s.


Supuestos
========================================================
Iniciando, es necesario indicar los supuestos que requiere la implementaci�n de este modelo como de la mayor�a, de los modelos epidemiol�gicos. 

* No existe inmunidad prev�a a la epidemia.
* Todos los individuos son similares en cuanto susceptibilidad y infectividad. Donde se pueden mezclar de forma homogenea.
* Un individuo que se recupera, desarrolla inmunidad. (*SIR*: Susceptible - Infecado - Recuperado).
* No hay cambios de comportamiento durante el curso de la epidemia.


Discuci�n de Supuestos
========================================================

* No hay inmunidad (natural o vacuna). Verdadero para los corona pero no para la influencia estacional. S� la mitad de la comunidad es inmune, entonces, el tama�o final de la epidemia cambiar�a dram�ticamente. 

* Una Comunidad homogenea. No verdadero para los corona o ninguna enfermedad. Determinar que heterogeneidad es importante, depende de la aplicaci�n (Existen muchas investigaciones en esta direcci�n). Algunos efectos son que entre 10% al 20% menos se infectan.

* No hay cambios durante la epidemia. Previniendo medidas durante el brote podr�an tener una gran diferencia en su resultado.



Modelado Suceptible - Infeccioso - Recuperado
========================================================
Modelo matem�tico _Susceptible_ - _Infeccioso_ - _Recuperado_ (SIR) (Kermack y McKendrick 1927). 

* $Susceptible$: Individuo que a�n no a tenido la enfermedad, pero no es inmune a ella. Y por tanto, puede volverse infeccioso al tener contacto con un infectado.

* $Infeccioso$: Individuo que actualmente esta comprometida por la enfermedad y capas de transmitirla a otros.

* $Recuperado$: individuo infeccioso que ya no se ve afectado por la enfermedad y que ya no puede transmitirla. No puede producirse una reinfecci�n, es decir, los individuos recuperados son inmunes a la enfermedad una vez la tuvieron.

Momento cero
========================================================
Suponemos que al momento cero:

* Todos los individuos son suceptibles excepto por $m$ individuos, que son infecciosos en el momento cero. 

* Una vez infectado, se recupera o muere. 

* Matem�ticamente por $S(t)$ $I(t)$ $R(t)$ denota el n�mero de susceptible, infeccioso y recuperado en la poblaci�n en el tiempo $t$. 

* Para todos los tiempos $S(t) + I(t) + R(t) = N$. 

En otras palabras, la poblaci�n es cerrada y no var�a en el tiempo.


Sistema de Ecuaciones Diferenciales Ordinarias
========================================================

De manera: $S \rightarrow I$ y $I \rightarrow R$. 


$$
\frac{\partial S(t)}{\partial t} = -\beta \cdot S(t) \cdot I(t) \\
$$

$$
\frac{\partial I(t)}{\partial t} = -\beta \cdot S(t) \cdot I(t) - \gamma \cdot I(t)\\
$$

$$
\frac{\partial R(t)}{\partial t} = - \gamma \cdot I(t)\\
$$



Sistema de Ecuaciones Diferenciales Ordinarias
========================================================
De manera $S \rightarrow I$

$\beta \cdot S(t) \cdot I(t)$

* Cada individuo tiene contactos a una velocidad $\alpha$ para reunirse con otra persona especifica.

* Una porci�n de $p$ contactos resulta en una infecci�n. 

* Entonces, $\beta = \alpha \cdot p$ es la tasa a la cual existen contactos infecciosos.



Sistema de Ecuaciones Diferenciales Ordinarias
========================================================

Esto denota el movimiento de los individuos por cada categor�a. 

De manera $S \rightarrow I$

$\beta \cdot S(t) \cdot I(t)$


$$
\sum_{j = 1}^{I(t)} \cdot \beta \cdot S(t) = \beta \cdot I(t) \cdot S(t)\\
$$

Nota: Este es un termino no lineal que consta de $I(t)$ y $S(t)$.


Sistema de Ecuaciones Diferenciales Ordinarias
========================================================

Esto denota el movimiento de los individuos por cada categor�a. 

De manera $I \rightarrow R$

$\gamma \cdot I(t)$. 

* Nuevo individuo infeccioso $I \rightarrow R$ su transici�n sucede a una velocidad constante $\gamma$. 

Es decir, que podemos interpretar que entre m�s peque�o sea $\gamma$ m�s individuos ser�n infectados y en consecuencia, ellos m�s podr�n transmitir la enfermedad a otros.


Sistema de Ecuaciones Diferenciales Ordinarias (resumen)
========================================================

Podemos resumir este sistema de la siguiente manera:

$$
S \longrightarrow I \longrightarrow R\\
$$

_donde,_
$$
S \longrightarrow I := \beta \cdot S(t) \cdot I(t)\\
$$
$$
I \longrightarrow R := \gamma \cdot I(t)
$$


N�mero b�sico de Reproducci�n
========================================================

El $R_0$ es definido:

"El n�mero esperado de casos secundarios por caso primario en una poblaci�n completamente susceptible". 

---(Diekmann, Heesterbeek, and Britton 2013).

Se calcula como:

$$
R_0 = N \cdot \frac{\beta}{\gamma}\\
$$


Para el caso del SARS-CoV-2, entendemos que por lo antrior que si $R_0$ es el n�mero promedio de individuos infectados por otro individuo. S� el valor de $R_0$ es alto, la probabilidad de pandemia es mayor. 


Umbral inmune de individuos
========================================================

El n�mero de $R_0$ tambi�n es usado para estimar el umbral inmune de individuos en una poblaci�n o _herd immune threshold (HIT por sus siglas en ingl�s)_:

* S� el n�mero de individuos no inmune o susceptibles es igual a 1, indica que el estado esta equilibrado. 

* S� el n�mero de individuos infectados es contante. 

* S� suponemos que la porci�n de personas es $p$ podemos formular un estado de la siguiente manera:

$$
R_0(1-p) = 1 \rightarrow 1 -p = \frac{1}{R_0} \rightarrow p_c = 1 - \frac{1}{R_0}
$$



Umbral inmune de individuos
========================================================

Por lo tanto, $p_c$ es el _HIT_ para detener la propagaci�n de la enfermedad. 

$$
p_c = 1 - \frac{1}{R_0}
$$

Es decir, que podemos vacunar una proporci�n de $p_c$ para aumentar la inmunidad de la poblaci�n y sus individuos.



Implementaci�n en R
========================================================
Donde, se usar� para resolver las derivadas del sistema de ecuaciones diferenciales ordinarias por medio del paquete deSolve (Soetaert, Petzoldt, and Setzer 2010). 

La informaci�n de invdividuos infectados por poblaciones _(paises o regiones)_ es extraida del paquete coronavirus (Rami Krispin, 2020).


```r
library(deSolve)
library(coronavirus)
library(tidyverse)
library(lubridate)
```


Modelaiento SIR
========================================================
Antes de ajustar el modelo SIR. Es expresar las derivadas del sistema de ecuaciones diferenciales ordinarias _ODE_ como una funci�n en R con su tiempo $t$.


```r
SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
        dS <- -beta * I * S / N
        dI <- beta * I * S / N - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
        }
    )
}
```




Ajustando Modelamiento SIR
========================================================




















































```
processing file: covid_presentation.Rpres
-- Attaching packages ---------------------------------------------------------------------------------------- tidyverse 1.3.0 --
v ggplot2 3.2.1     v purrr   0.3.3
v tibble  2.1.3     v dplyr   0.8.4
v tidyr   1.0.0     v stringr 1.4.0
v readr   1.3.1     v forcats 0.4.0
-- Conflicts ------------------------------------------------------------------------------------------- tidyverse_conflicts() --
x readr::col_factor() masks scales::col_factor()
x purrr::discard()    masks scales::discard()
x dplyr::filter()     masks stats::filter()
x dplyr::lag()        masks stats::lag()

Attaching package: 'lubridate'

The following object is masked from 'package:base':

    date

Quitting from lines 277-279 (covid_presentation.Rpres) 
Error: `path` does not exist: 'covid_dashboard/www/API_SP.POP.TOTL_DS2_en_excel_v2_988396.xls'
Adem�s: Warning messages:
1: package 'deSolve' was built under R version 3.6.1 
2: package 'tidyverse' was built under R version 3.6.2 
3: package 'ggplot2' was built under R version 3.6.2 
4: package 'tidyr' was built under R version 3.6.2 
5: package 'purrr' was built under R version 3.6.2 
6: package 'dplyr' was built under R version 3.6.2 
Ejecuci�n interrumpida
```
