---
title: Aplicación y aproximaciones al modelamiento SIR para el SARS-CoV-2.
author: Mora, S. & Hernandez, A. & Camargo, A.
date: 2020-05-26
output: revealjs::revealjs_presentation
---
<style>
.small-code pre code {
  font-size: 1em;
}
</style>

Introducción
========================================================




En diciembre de 2019 en la ciudad de Wuhan (China) se registraron varios casos de personas enfermas con una neumonía viral, que posteriormente sería atribuida a un nuevo coronavirus, el COVID-19 o SARS-CoV-2. Para enero 30 del 2020 la OMS (organización mundial de la salud) lo declaró un problema de salud pública internacional. En los siguientes meses el virus se propagó rápidamente por el continente asiático, luego por el continente europeo y finalmente alrededor del mundo. Para el 11 de marzo del 2020, la OMS reconoció al SARS-CoV-2 como una pandemia global. 


Objetivo General
========================================================

*Crear un modelo que permita entender la propagación del SARS-CoV-2, prediciendo la curva de infectados y su duración a lo largo del tiempo en Colombia.*

Objetivos Especificos
========================================================

1. ¿Cúando será el pico de la pandemia?
2. ¿Cúantas personas estarán infectadas en el pico de la pandemia?
3. ¿Cúantos casos severos se tendrán a tener en el país?
4. ¿Cúantos casos necesitarán cuidados intensivos?
5. ¿Cúantos fallecidos habrán?
6. ¿Cúando finalizará la pandemia?

Más objetivos
========================================================

* Generar reportes que describan la situación de cada país.
* Crear gráficos con las predicciones de las curvas de infectados. 
* Construir una simulación que compare movimiento libre vs restringido.
* Construir una simulación que compare la distancia social.
* Construir un mapa para visualizar la cantidad de incdencias por país.


Supuestos
========================================================
Iniciando, es necesario indicar los supuestos que requiere la implementación de este modelo como de la mayoría, de los modelos epidemiológicos. 

* No existe inmunidad prevía a la epidemia.
* Todos los individuos son similares en cuanto susceptibilidad y infectividad. Donde se pueden mezclar de forma homogenea.
* Un individuo que se recupera, desarrolla inmunidad. (*SIR*: Susceptible - Infecado - Recuperado).
* No hay cambios de comportamiento durante el curso de la epidemia.


Discución de Supuestos
========================================================

* No hay inmunidad (natural o vacuna). Verdadero para los corona pero no para la influencia estacional. Sí la mitad de la comunidad es inmune, entonces, el tamaño final de la epidemia cambiaría dramáticamente. 

* Una Comunidad homogenea. No verdadero para los corona o ninguna enfermedad. Determinar que heterogeneidad es importante, depende de la aplicación (Existen muchas investigaciones en esta dirección). Algunos efectos son que entre 10% al 20% menos se infectan.

* No hay cambios durante la epidemia. Previniendo medidas durante el brote podrían tener una gran diferencia en su resultado.



Modelado Suceptible - Infeccioso - Recuperado
========================================================
Modelo matemático _Susceptible_ - _Infeccioso_ - _Recuperado_ (SIR) (Kermack y McKendrick 1927). 

* $Susceptible$: Individuo que aún no a tenido la enfermedad, pero no es inmune a ella. Y por tanto, puede volverse infeccioso al tener contacto con un infectado.

* $Infeccioso$: Individuo que actualmente esta comprometida por la enfermedad y capas de transmitirla a otros.

* $Recuperado$: individuo infeccioso que ya no se ve afectado por la enfermedad y que ya no puede transmitirla. No puede producirse una reinfección, es decir, los individuos recuperados son inmunes a la enfermedad una vez la tuvieron.

Momento cero
========================================================
Suponemos que al momento cero:

* Todos los individuos son suceptibles excepto por $m$ individuos, que son infecciosos en el momento cero. 

* Una vez infectado, se recupera o muere. 

* Matemáticamente por $S(t)$ $I(t)$ $R(t)$ denota el número de susceptible, infeccioso y recuperado en la población en el tiempo $t$. 

* Para todos los tiempos $S(t) + I(t) + R(t) = N$. 

En otras palabras, la población es cerrada y no varía en el tiempo.


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

* Una porción de $p$ contactos resulta en una infección. 

* Entonces, $\beta = \alpha \cdot p$ es la tasa a la cual existen contactos infecciosos.



Sistema de Ecuaciones Diferenciales Ordinarias
========================================================

Esto denota el movimiento de los individuos por cada categoría. 

De manera $S \rightarrow I$

$\beta \cdot S(t) \cdot I(t)$


$$
\sum_{j = 1}^{I(t)} \cdot \beta \cdot S(t) = \beta \cdot I(t) \cdot S(t)\\
$$

Nota: Este es un termino no lineal que consta de $I(t)$ y $S(t)$.


Sistema de Ecuaciones Diferenciales Ordinarias
========================================================

Esto denota el movimiento de los individuos por cada categoría. 

De manera $I \rightarrow R$

$\gamma \cdot I(t)$. 

* Nuevo individuo infeccioso $I \rightarrow R$ su transición sucede a una velocidad constante $\gamma$. 

Es decir, que podemos interpretar que entre más pequeño sea $\gamma$ más individuos serán infectados y en consecuencia, ellos más podrán transmitir la enfermedad a otros.


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


Número básico de Reproducción
========================================================

El $R_0$ es definido:

"El número esperado de casos secundarios por caso primario en una población completamente susceptible". 

---(Diekmann, Heesterbeek, and Britton 2013).

Se calcula como:

$$
R_0 = N \cdot \frac{\beta}{\gamma}\\
$$


Para el caso del SARS-CoV-2, entendemos que por lo antrior que si $R_0$ es el número promedio de individuos infectados por otro individuo. Sí el valor de $R_0$ es alto, la probabilidad de pandemia es mayor. 


Umbral inmune de individuos
========================================================

El número de $R_0$ también es usado para estimar el umbral inmune de individuos en una población o _herd immune threshold (HIT por sus siglas en inglés)_:

* Sí el número de individuos no inmune o susceptibles es igual a 1, indica que el estado esta equilibrado. 

* Sí el número de individuos infectados es contante. 

* Sí suponemos que la porción de personas es $p$ podemos formular un estado de la siguiente manera:

$$
R_0(1-p) = 1 \rightarrow 1 -p = \frac{1}{R_0} \rightarrow p_c = 1 - \frac{1}{R_0}
$$



Umbral inmune de individuos
========================================================

Por lo tanto, $p_c$ es el _HIT_ para detener la propagación de la enfermedad. 

$$
p_c = 1 - \frac{1}{R_0}
$$

Es decir, que podemos vacunar una proporción de $p_c$ para aumentar la inmunidad de la población y sus individuos.



Implementación en R
========================================================
Donde, se usará para resolver las derivadas del sistema de ecuaciones diferenciales ordinarias por medio del paquete deSolve (Soetaert, Petzoldt, and Setzer 2010). 

La información de invdividuos infectados por poblaciones _(paises o regiones)_ es extraida del paquete coronavirus (Rami Krispin, 2020).


```r
library(deSolve)
library(coronavirus)
library(tidyverse)
library(lubridate)
```


Modelaiento SIR
========================================================
Antes de ajustar el modelo SIR. Es expresar las derivadas del sistema de ecuaciones diferenciales ordinarias _ODE_ como una función en R con su tiempo $t$.


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
Además: Warning messages:
1: package 'deSolve' was built under R version 3.6.1 
2: package 'tidyverse' was built under R version 3.6.2 
3: package 'ggplot2' was built under R version 3.6.2 
4: package 'tidyr' was built under R version 3.6.2 
5: package 'purrr' was built under R version 3.6.2 
6: package 'dplyr' was built under R version 3.6.2 
Ejecución interrumpida
```
