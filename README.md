# Covid-19 Dashboard

Authors: 

* [_Sergio Mora Pardo_](https://www.linkedin.com/in/sergiomorapardo/)
, github: [sergiomora03.github.io](https://sergiomora03.github.io/)

* _Alex Hernandez Paez_, github: [alexher90](https://github.com/alexher90)

* _Alex Camargo Garcia_, github: [halepz](https://github.com/halepz)

### Introducción
En diciembre de 2019 en la ciudad de Wuhan (China) se registraron varios casos de personas enfermas con
una neumonía viral, que posteriormente sería atribuida a un nuevo coronavirus, el COVID-19 o SARS-CoV-2.
Para enero 30 del 2020 la OMS (organización mundial de la salud) lo declaró un problema de salud pública
internacional. En los siguientes meses el virus se propagó rápidamente por el continente asiático, luego por
el continente europeo y finalmente alrededor del mundo. Para el 11 de marzo del 2020, la OMS reconoció
al SARS-CoV-2 como una pandemia global. Actualmente, el entendimiento de la propagación del virus, el
estudio de posibles vacunas y la creación de estrategias locales, nacionales y globales para la reducción de la
tasa de propagación del virus son las principales preocupaciones de las diferentes naciones y profesionales del
mundo.


Dada la coyuntura por la que estamos pasando actualmente, decidimos buscar una forma en la que pudiéramos
aportar al entendimiento de la pandemia, enfocándonos principalmente en su propagación y la predicción de
la curva infectados. Le dimos este enfoque pues en la literatura ya existen varios modelos que nos permiten
predecir el comportamiento de un patógeno y su propagación. Por otro lado, contamos con grandes bases de
datos dónde se han estado alimentando la información de los contagiados, su lugar de origen y la fecha del
contagio. Así pues, teníamos toda la información a nuestro alcance para poder aportar al entendimiento del
problema más grande que afecta a todo el mundo en este momento y sentimos que era nuestra obligación
responder al llamado.

# Contenido

|Files|Description|
|-----|-----------|
|Script | [App.R](https://github.com/sergiomora03/covid_dashboard/blob/master/app.R)|
|Dashboard|[Shiny V1](https://sergiomora123.shinyapps.io/covid19_dashboard/)|
|Dashboard|[Shiny V2](https://sergiomora123.shinyapps.io/covid_19/)|

## Objetivo
Dada la información que estaba a nuestro alcance y la situación global por la que aún estamos, el objetivo de
este trabajo es:

Crear un modelo que permita entender la propagación del SARS-CoV-2, prediciendo la curva de infectados y
su duración a lo largo del tiempo en Colombia. 

## Alcance:
Con el fin de alcanzar este objetivo, debíamos respoder:

1. ¿Cúando será el pico de la pandemia?
2. ¿Cúantas personas estarán infectadas en el pico de la pandemia?
3. ¿Cúantos casos severos se tendrán en el país?
4. ¿Cúantos casos necesitarán cuidados intensivos?
5. ¿Cúantos fallecidos habrán?
6. ¿Cúando finalizará la pandemia?

## Anexos:
Adicionalmente, se pensó en generalizar el modelo para todos los países afectados y de los que tenemos
información con el objetivo de:

* Generar reportes que describan la situación de cada país.
* Crear gráficos con las predicciones de las curvas de infectados.
* Construir una simulación que compare movimiento libre vs restringido.
* Construir una simulación que compare la distancia social.
* Construir un mapa para visualizar la cantidad de incdencias por país.

