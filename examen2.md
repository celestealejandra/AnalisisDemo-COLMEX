---
title: "Formulario | Análisis Demográfico"
author: "Celeste A. Díaz"
date: "10/30/2024"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Presentación

Este es un formulario con todas las fórmulas revisadas en el semestre 2025-1 de la Maestría en Demografía en el Colegio de México.

```{r include=FALSE}
pkgs <- c("tidyverse", "janitor", "patchwork", "readr",
          "readxl", "ggplot2", "writexl")
#si ya están instalados no es necesario volver a instalar
#install.packages(pkgs)
library(tidyverse)
library(janitor)
library(patchwork)
library(readxl)
library(writexl)
library(readr)
library(ggplot2)

load("~/Desktop/maestría/trabajos finales /análisis demográfico/markdown_examen/input/tab_mort.RData")
base <- base0 %>% 
select(edo, year,age, sex, pop, defs)
rm(list = setdiff(ls(), "base"))

```

## 1. La Tabla de Vida

Para crear una tabla de vida necesitamos correr el siguiente código:

```{r echo=TRUE}
tab_mort_func <- function(base) {
  base %>% 
    group_by(year, edo, sex) %>% 
    mutate(mx = ifelse(pop>0, defs/pop, 1), 
           ax = ifelse(
             age == 0 , ifelse( #si  la edad 0 
               sex == "males", ifelse(
                 mx >= 0.107, 0.33, 0.045+2.684*mx), ifelse( #formulas de coale
                   mx >= 0.107, 0.35, 0.053+2.8*mx)), 
             ifelse(age == 109, #si la edad es 109
                    ifelse(mx == 0, 0.5,1/mx), 0.5)),# la condición, y luego todos los casos
           qx = ifelse(mx>1 , 1,mx/(1+(1-ax)*mx)), #si mx es mayor a 1, 1, sino, mx/...
           px = pmax(0, 1-qx), #establece los máximos y los mínimos
           lx = c(1, cumprod(px))[-length(px)],
           dx = lx*qx,
           Lx = lead(lx, default = 0)+ax*dx, #lead = lx+1 
           Tx = rev(cumsum(rev(Lx))),
           Ex = Tx/lx) %>% 
    ungroup() %>% 
    replace_na(list(Ex=0))}
```

En esta sección vamos a explicar cada línea de código comparando con las fórmulas según aparecen en Preston, S., Heuveline, P., & Guillot, M. (2001). *Measuring and Modeling Population Processes* (1st ed.). Blackwell Publishers.

#### 1.1 Dx y Nx: Personas y Fallecimientos

Para descargar la base que estamos utilizando, **haga click en el siguiente enlace**: <https://tinyurl.com/cdiazcdb>

```{r}
#veamos nuestra base primero
head(base)
```

Tenemos cinco columnas, "edo" para la entidad, "year" para el año", "sex" para el sexo, "pop" y "defs" para la población y las defunciones. Convertiremos pop a Nx y defs a Dx:

```{r}
#convertimos pop y defs 
base <- base %>% 
  rename(Dx = defs,
         Nx = pop)
```

#### 1.2 Calculamos Mx

Posteriormente vamos a calcular Mx utilizando esta fórmula del Preston:

Esta fórmula calcula nMx como el resultado de dividir el número de muertes de la Cohorte del grupo de las edades entre x y x+n, en este caso utilizamos Nx como la cohorte observada. El código en R es este:

```{r}
base <- base %>% 
  group_by(year, edo, sex) %>% 
  mutate(mx = ifelse(Nx>0, Dx/Nx, 1)) 
```

Limitamos Mx a ser una probabilidad entre 0 y 1 dado que el grupo de edad tiene más de una persona.

#### 1.3 Calculamos ax

Para calcular Ax tenemos que tener en cuenta varias condiciones. La primera condición es que en la edad 0 tiene un tratamiento especial de acuerdo con las fórumlas de Coale:


Entonces si 1m0 (la mortalidad entre la edad 0 y la edad 1) es superior a .107 en los hombres se reemplaza por .330 y en mujeres por.350. Si es menor, se reemplaza por la siguiente fórmula:

```{r}
 
base %>% 
  group_by(year, edo, sex) %>% 
  mutate(age == 0 , ifelse( #si  la edad 0
    sex == "males", ifelse(
      mx >= 0.107, 0.33, 0.045+2.684*mx), ifelse( #formulas de coale
        mx >= 0.107, 0.35, 0.053+2.8*mx))) %>% 
  filter(age == 0)
```

Ahora para el grupo de edad 109, en caso de que mx sea 0, se da que es 0.5, en caso de que no sea cero se calcula por medio de dividir 1 entre el número calculado para mx:

```{r}
base %>%
  group_by(year, edo, sex) %>%
  mutate(
    ax = ifelse(age == 109, ifelse(mx == 0, 0.5, 1 / mx), NA)
  ) %>%
  filter(age == 109) 


```

Para el resto de las edades, se pone 0.5 partiendo del supuesto de que todos los individuos que fallecieron aportaron la mitad del año en el que fallecieron. De tal manera que el código completo para calcular ax queda así:

```{r}
base <- base %>% 
  group_by(year, edo, sex) %>% 
  mutate(ax = ifelse(
  age == 0 , ifelse( #si  la edad 0 
    sex == "males", ifelse(
      mx >= 0.107, 0.33, 0.045+2.684*mx), ifelse( #formulas de coale
        mx >= 0.107, 0.35, 0.053+2.8*mx)), 
  ifelse(age == 109, #si la edad es 109
         ifelse(mx == 0, 0.5,1/mx), 0.5)))

head(base)
```

#### 1.4 Funciones de probabilidad: qx y px
En esta sección calcularemos qx y px donde qx corresponde a la probabilidad condicional de fallecer en una edad y donde px es el complemento indicando la probabilidad de sobrevivir a esa edad. Ambas son condicionales, dado que se llegó a la edad x con vida. 
Las formulas son las siguientes:

Y en código se calculan así: 
```{r}
base <- base %>% 
  group_by(year, edo, sex) %>% 
  mutate( qx = ifelse(mx>1 , 1,mx/(1+(1-ax)*mx)), #si mx es mayor a 1, 1, sino, mx/...
          px = pmax(0, 1-qx)) #establece los máximos y los mínimos

```

#### 1.5 Creación de Cohorte Ficticia
Primero se escoge un rádix para iniciar nuestra cohorte ficticia, puede ser cualquier número 1, 10, 100 y sus múltiplos. Posteriormente, se calculan lx+n por medio de esta fórmula: 

Se calcularán los fallecimientos de la cohorte ficticia por medio de la siguiente fórmula: 


Lx van a ser los años persona vividos por los integrantes de la cohorte ficticia: 

El bloque de código correspondiente a estos cálculos es el siguiente: 
```{r}
base <- base %>% 
  group_by(year, edo, sex) %>% 
  mutate(lx = c(1, cumprod(px))[-length(px)], #calcula el valor acumulado de px
         #pon un 1 al inicio de la secuencia donde el tamaño del vect es px
dx = lx*qx, 
Lx = lead(lx, default = 0)+ax*dx) 
#toma el valor de lx de la siguiente edad
#suma ax por dx
#años vividos por los fallecidos mas los fallecidos de la cohorte ficticia
```

#### 1.6 Calcular Tx y Ex
Tx se deriva de la suma reversa de las edades para calcular los años persona vividos en la edad x y la fórmula es esta: 

Ex es el cociente resultante de los años persona vividos en la edad x y los sobrevivientes a esa edad. 

El código de R para esta operación es el siguiente: 
```{r}

base <- base %>% 
  mutate(Tx = rev(cumsum(rev(Lx))),
         Ex = Tx/lx)
```

Terminamos la tabla de vida desagrupando y reemplazando las esperanzas de vida por cero en donde existan NA's. 

```{r}
base <- base %>% 
  ungroup() %>% 
  replace_na(list(Ex=0))
```

Si corremos la función creada tab_mort_func habremos construido una tabla de vida: 
```{r}
base <- base %>% 
  rename(defs = Dx, #regresamos los nombres a pop y def 
         pop = Nx)
base <- tab_mort_func(base = base)
```

## 2. Descomposición de Arriaga
Primero seleccionamos los años entre los cuales queremos ver el cambio en la mortalidad, para este ejemplo utilizamos 1950 y 2023, tomamos a las mujeres: 
```{r}

desc_lt <- full_join(base %>% 
  filter( sex == "females", 
          year == 1950,
          edo == "República Mexicana") %>% 
  select(age, lx, Lx, Tx), ########base1
base %>% 
  filter( sex == "females", 
          year == 2023,
          edo == "República Mexicana") %>% 
  select(age, lx, Lx, Tx),##############base2
by= "age") %>% ######fin del join
  select(age= age, lx1= lx.x, Lx1 = Lx.x, Tx1 = Tx.x, 
         lx2= lx.y, Lx2 = Lx.y, Tx2 = Tx.y)###selec names
### x es t1 (1950) y y es t2(2023) 
```

Vamos a construir el efecto directo, el efecto indirecto, su diferencia y el cambio porcentual. 
La fórmula para el efecto directo y el efecto indirecto fueron tomadas de Arriaga, E. E. (1984). Measuring and Explaining the Change in Life Expectancies. Demography, 21(1), 83–96.

```{r}
arriaga <- desc_lt %>% 
  mutate(DEx = lx1*((Lx2/lx2)-(Lx1/lx1))) %>%  #Efecto directo
  mutate(OEx = lead(Tx2, default = 0)*
               ((lx1/lx2)-lead(lx1, default = 0)/
               lead(lx2, default = 0)),
         delta_x= DEx + OEx, 
         perc = 100*abs(delta_x)/sum(abs(delta_x), na.rm = T))
rm(desc_lt)
```

Y ahora revisaremos el cambio en la esperanza de vida en T1 y T2: 
```{r}
arriaga %>% 
  summarise(ex1_ex2 = sum(delta_x, na.rm = T))
```

## 3.Tablas de decrimento multiple
Para poder generar una tabla para calcular la intensidad del fallecimiento por una causa i, necesitamos primero tener una tabla con las causas de muerte. La intensidad de las causas de muerte i se representa oir nMxi. Descarga la base de causas https://tinyurl.com/dfmort19

La formula basica es esta: 
**formula**

La probabilidad se calcula asi: 
**formula**

Utiliza lx por lo tanto: 
**formula**

Utilizamos tambien dos razones: de probabilidad y de conteo
**formula**

Y por lo tanto: 
**formula**


### 3.1 Computar la tabla de vida con todas las causas de mortalidad
Contruyamos primero nuestra tabla de mortalidad: 

```{r}
tmort19 <- base %>% 
  filter(edo == "República Mexicana", # para toda la rep
         year == 2019) %>%  #año 2019
  select(age, sex, qx, lx) %>% 
  mutate(id= paste(age, sex, sep="")) #generamos un id

```
Revisemos las causas 
```{r}
names(table(df_causes1$disease_group))
```

Ahora vamos a crear una base donde generaremos la proporcion de ndxi con ndx, tambien separaremos las causas en i y en -i: 

```{r}
df_causes2 <- df_causes1 %>% 
  mutate(sexo = case_when(sexo == 1 ~ "males",
                         sexo == 2 ~ "females"), #nombre a la cat de sexo
         dos_causas = ifelse(
           disease_group == "Infecciosas", "Infecciosas", #i y -i en la formula
           "Resto")) %>% 
  group_by(sexo, age, dos_causas) %>%  #agrupo
  summarise(defs = sum(defs), .groups = "drop") %>% #resumo la tabla 
  group_by(sexo, age) %>% 
  mutate( prop = defs/sum(defs)) %>% #creamosla variable de ndxi /ndx
  ungroup() %>% 
  mutate(id = paste(age, sex, sep= "")) %>% #id 
  na.omit()
```

Pegamos la proporcion de muerte por causa i y la pegamos a la tabla de vida. 

```{r}
tdcmult <- full_join(
  tmort19, #tabla de vida
  df_causes2 %>% 
    filter(dos_causas == "Infecciosas") %>% #causas de muerte
    select(id, prop), by = "id") %>% 
  select(-id) %>% 
  replace_na(list(prop = 0)) %>% 
  na.omit()
```
**OJO**: Hay que tener cuidado con los NA's en las edades extremas y en la proporcion. 


### 3.2 Cronstruccion de Probabilidad 

Ahora que tenemos computada la base procedemos a generar nqxi, el cual calculamos con la multiplicacion de qx y la proporcion de fallecimientos por la causa i: 

```{r}
tdcmult <- tdcmult %>% 
  mutate( qxi = qx*prop)
```

### 3.3 Calcular conteo de fallecimientos por causas para la Cohorte fictica 

Para cacular los fallecimientos en la cohorte ficticia multiplicamos la probabilidad de fallecimiento por la causa i por los sobrevivientes de la cohorte ficticia: 

```{r}

tdcmult <- tdcmult %>% 
  mutate( dxi = qxi*lx)

```

Ahora calculamos los sobrevivientes: 

```{r}
tdcmult <- tdcmult %>% 
  group_by(sex) %>% #agrupamos por sexo
  mutate( lxi = rev(cumsum(dxi))) %>% 
  ungroup()
#rev para ir de lx+n a lx
#cumsum para sumas acumuladas 
```
Y finalmente calculamos la proporcion de sobrevivientes a la edad x que falleceran de la causa i:

```{r}
tdcmult <- tdcmult %>% 
  group_by(sex) %>% 
  mutate( prop2 = (lxi/lx)*100) %>% 
  ungroup()
#Proporción de sobrevivientes a la edad x que eventualmente fallecen de la causa i
```








