---
title: "Código de resolución para la tarea examen de Análisis Demográfico"
author: "Celeste Díaz"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

En este pequeño sitio web se encuentra el código comentado para resolver la tarea examen de análisis demográfico presentado del 02 al 03 de octubre de 2024.

### Índice

1.  Evaluación de No Específicados
2.  Creación de Índice de Whipple
3.  Prorrateo de N.E
4.  Corrección de Preferencia Digital
5.  Elaboración de Pirámides Poblacionales
6.  Análisis de Líneas de Vida
7.  Tasa de Crecimiento Anual Media y Años persona vividos
8.  Estandarización de la Tasa de Mortalidad

### 0. Descarga de Archivos

Puede descargar los archivos que se utilizan en este ejercicio en esta liga:

<https://drive.google.com/drive/folders/1sMfL_yOOdZAzF9jRJgxRylCKL9WXWXav?usp=share_link>

Empezamos con las paqueterías:

```{r}

library(tidyverse)
library(ungroup)
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(janitor)
```

### 1. Evaluación de N.E

Abrimos nuestro archivo de la base de datos del Censo de 2020, ya viene en formato long y sin los espacios del archivo original.

```{r}

#Seleccionando las dos entidades: Yucatán y Aguascalientes
#Obteniendo los datos del Censo Nacional de Población y Vivienda 2020
##Datos

load("input/base_mx.Rdata") #cargamos la base en formato Rdata
head(base_mx) #revisamos la base con head 
```

Ahora, seleccionamos nuestras entidades. Para este ejercicio utilizaremos Aguascalientes y Yucatán. Filtremos nuestra base de una vez también.

```{r}
#Seleccionamos las entidades con las que vamos a trabajar 
##Filtrado 

base_mx <- base_mx %>%
  filter(edo %in% c("Yucatán", "Aguascalientes"))

```

Ahora vamos a seleccionar a nuestros No Especificados, pidiéndole que nos arroje las observaciones donde "Edad" es un valor NA.

```{r}
#Seleccionando los N.E
NE <- base_mx %>% 
  filter(is.na(age)==T) %>% 
  group_by(edo) %>% 
  mutate(id = paste(edo,sex, sep = "-")) 

```

Ahora vamos a armar nuestra base usando el total de la población para poder sacar la proporción y posteriormente el porcentaje.

```{r}

##Agregamos el total de la población
POB <- base_mx%>% 
  group_by(edo, sex) %>% 
  summarise(pob = sum(pob), .groups = "drop") %>% 
  mutate(id = paste(edo, sex, sep = "-"))
#Juntamos las bases 
NE <- left_join(NE, POB, by="id") %>% 
  select(edo = edo.x,
         sex = sex.x,
         NE = pob.x, 
         pob = pob.y)

```

Ahora sacamos la proporción y los porcentajes:

```{r}
##Porcentajes de NE
Tabla1 <- NE %>%  
  mutate(prop_ne = NE/pob, 
         pct_ne = prop_ne*100) %>% 
  rename("Estado"= edo, "%NE"=pct_ne)

rm(NE, POB)#pequeña limpieza antes de seguir 

Tabla1
```

Como se puede observar, la tendencia en hombres y mujeres es bastante similar, la variación que se da es en función al estado. Dado esto, también podemos agregar que es un porcentaje muy bajo y entre ellos la diferencia es también pequeña tomando en cuenta sus diferencias de tamaño.

### 2. Creación del Índice de Whipple

Vamos a realizar el índice de Whipple en función a la fórmula presentada a continuación:

![Esta fórmula se encuentra en la página 138 de "The Methods and Materials of Demography" de Swanson y Siegel. ](images/paste-0171B932.png)

Crearemos el denominador y el numerador de la fórmula seleccionando únicamente las edades correspondientes:

```{r}

whipp <- base_mx %>% 
  filter(is.na(age)==F) #Quitamos a los NE 

whip_den <- whipp %>%  
  filter(age %in% seq(30,60,10)) %>% #nuestro denominador de 30-60 
  group_by(edo,sex) %>%  
  summarise(den = sum(pob), .groups = "drop") %>% #sacamos la suma
  mutate(id = paste(edo, sex, sep = "-"))#hacemos un id para unir 

whip_num <- whipp %>%  
  filter(age %in% seq(23,62,1)) %>% #23-62 para el denominador
  group_by(edo,sex) %>% 
  summarise(num= sum(pob), .groups = "drop") %>% #construimos el numerador
  mutate(num = num/10) %>% #dividimos entre diez segun la fórmula
  mutate(id = paste(edo, sex, sep = "-")) #id para pegar 

```

Ahora haremos el merge para poder realizar el resto de la fórmula, esto lo guardaremos también en su tablita:

```{r}
whipp <- left_join(whip_den,whip_num, by="id") %>% #pegar :)
  select(edo = edo.x, 
         sex = sex.x, 
         den = den, 
         num = num)

Tabla2 <- whipp %>% #tabla final ! 
  mutate(ind_w = den/num) %>% #dividimos el den entre el num
  rename("Estado"= edo, "Índice de Whipple"= ind_w, "Sexo" =sex) %>% #nombres 
  select(c(-den, -num)) #quitamos las columnas del den y el num

#limpieza de nuevo!
rm(whipp, whip_den, whip_num)
Tabla2
```

Y sólo porque sí, agregamos un grafiquito:

```{r}
#un grafiquito de a gratis 
Tabla2 %>% 
  mutate(Sexo = if_else(Sexo == "females", "Mujeres", "Hombres")) %>% 
ggplot() +
  aes(x = Sexo, y = `Índice de Whipple`, fill = Sexo) +
  geom_col() +
  scale_fill_manual(values = c("Mujeres" = "red", 
                               "Hombres" = "green")) + # Colores personalizados
  theme_minimal() +
  facet_wrap(vars(Estado))

ggsave("output/plotex1.png", dpi = 3000)
```

Como se puede ver la preferencia digital es muy pequeña en ambos estados pero en ambos se registro que las mujeres tenían una mayor preferencia digital. Nuevamente, se trata de una preferencia tan baja que las diferencias entre hombres y mujeres también son muy sutiles.

### 3. Prorrateo de NE 

Ahora tomaremos los NE que encontramos anteriormente y vamos a distribuirlos entre las edades. Primero quitaremos los NE y luego se los pegaremos en función al peso que tiene cada grupo, de esta manera, será proporcional.

```{r}
 base_pror <- base_mx %>% 
    filter(is.na(age)== F) %>%  #quitamos los NE
    group_by(sex) %>%  
    mutate(prop = pob/sum(pob)) %>%  #la proporción de cada edad
    ungroup() %>% 
  mutate(id = paste(edo,sex, sep = "-"))
 base_pror
```

Ahora sí, vamos a pegarle los NE:

```{r}

 nas <- base_mx %>%  #base dos 
    filter(is.na(age)==T) %>% #tomamos los NE
    select(edo, sex, pob_na = pob) %>% 
   mutate(id = paste(edo,sex, sep = "-"))
 
base_pror <- left_join(base_pror, nas, by="id")#los pegamos

base_pror <- base_pror %>% 
  mutate(pob_fin = pob+(pob_na*prop)) %>% 
  select(edo = edo.x, 
         age = age, 
         sex = sex.x,
         pob = pob_fin) %>% 
  mutate(pob = round(pob, digits = 0))#redondeamos a numeros enteros

rm(nas)
base_pror
```

### 4. Corrección de preferencia digital

Para corregir la preferencia digital agruparemos por grupos quinquenales y luego desagruparemos.

Primero agruparemos por grupos quinquenales, le pediremos a r que nos genere los "bins" (casillas) y las etiquetas.

```{r}
bins <- seq(0, max(base_pror$age) + 5, by = 5) #los breaks

labels <- paste(bins[-length(bins)], #etiquetas
                bins[-1] - 1,
                sep = "-")

```

Después, creamos nuestros grupos quinquenales con group_by y summarise:

```{r}
base_pror <- base_pror %>% #agregamos grupo de edad a la base
  mutate(age_group = cut(age, breaks = bins, labels = labels, right = F))

base_5 <- base_pror %>% 
  group_by(edo,sex,age_group) %>% 
  summarise(pob = sum(pob), .groups = "drop")
base_5
```

Ahora que ya los agrupamos los vamos a desagrupar ₍ᐢ. ̫.ᐢ₎

```{r}
#desagrupar por grupos quinquenales 
base_5H <-base_5 %>% #separamos por sexo
  filter(sex=="males")
base_5M <-base_5 %>% 
  filter(sex=="females")
base_5HA <- base_5H %>% #separamos por estado
  filter(edo== "Aguascalientes")
base_5HY <- base_5H %>% 
  filter(edo== "Yucatán")
base_5MA <- base_5M %>% 
  filter(edo== "Aguascalientes")
base_5MY <- base_5M %>% 
  filter(edo== "Yucatán")
#ahora si el ungroup
base_1_HA <- pclm(x = bins[-length(bins)],
                    base_5HA$pob, nlast = 10)
base_1_HY <- pclm(x = bins[-length(bins)],
                  base_5HY$pob, nlast = 10)
base_1_MA <- pclm(x = bins[-length(bins)],
                  base_5MA$pob, nlast = 10)
base_1_MY <- pclm(x = bins[-length(bins)],
                  base_5MY$pob, nlast = 10)

```

Y juntamos todo en una sola base para que quede sintético:

```{r}
#esta es la base de aguas
base_a_1 <- rbind(enframe(base_1_HA$fitted,
                           name = 'age', value = 'pob') %>%
                     mutate(age = c(0:109), sex = 'males',
                            .before = pob),
                   enframe(base_1_MA$fitted,
                           name = 'age', value = 'pob') %>%
                     mutate(age = c(0:109), sex = 'females',
                            .before = pob)) %>%
  mutate(pob2 = ifelse(sex == 'males', -pob, pob))

#esta es la base de yuc
base_y_1 <- rbind(enframe(base_1_HY$fitted,
                          name = 'age', value = 'pob') %>%
                    mutate(age = c(0:109), sex = 'males',
                           .before = pob),
                  enframe(base_1_MY$fitted,
                          name = 'age', value = 'pob') %>%
                    mutate(age = c(0:109), sex = 'females',
                           .before = pob)) %>%
  mutate(pob2 = ifelse(sex == 'males', -pob, pob))

base_y_1
base_a_1
```

### 5. Elaboración de pirámides población

¡Lo más divertido ( ˶ˆᗜˆ˵ )!

Hagamos la pirámide de Aguascalientes:

```{r}

#pirámide ags

base_a_1 %>%
  ggplot() +
  geom_bar(aes(x=age, y=pob2/10000,fill=age), #cambiamos la densidad porque hay muy poquita gente en ags
           stat='identity', show.legend = F)+
  coord_flip()+
  geom_hline(yintercept=0)+
  scale_y_continuous(
    limits=c(-1.5,1.5), #los límites 
    breaks=seq(-1.5,1.5,0.25),
    labels=as.character(c(seq(1.5,0,-0.25),
                          seq(0.25,1.5,0.25))))+
  scale_x_continuous(limits=c(-1,110), breaks = seq(0,110,5),
                     labels = seq(0,110,5))+
  annotate(
    geom="text", x=95, y =-1, label= "Hombres",color="black",size=3
  )  +
  annotate(
    geom="text", x=95, y =1, label= "Mujeres",color="black",size=3
  )+
  theme_light()+ #gradiente de verde a azul
  scale_fill_gradient2(mid = "green", high = "blue")+
  labs(y="Poblaciones (10,000)", x="Edad (años)",
       title = "Pirámide Poblacional para Aguascalientes 2020", 
       caption = "Fuente: Censo Nacional de Población y Vivienda 2020")
ggsave("output/pirags2020.png", dpi = 3000)

```

Nótese que para ponerle título y la anotación de la fuente utilizamos title= y caption= en el comando "labs".

Ahora hagamos la de Yucatán:

```{r}

base_y_1 %>%
  ggplot() +
  geom_bar(aes(x=age, y=pob2/10000,fill=age),
           stat='identity', show.legend = F)+
  coord_flip()+
  geom_hline(yintercept=0)+
  scale_y_continuous(
    limits=c(-2.5,2.5), #cambiamos los limites
    breaks=seq(-2.5,2.5,0.25),
    labels=as.character(c(seq(2.5,0,-0.25),
                          seq(0.25,2.5,0.25))))+
  scale_x_continuous(limits=c(-1,110), breaks = seq(0,110,5),
                     labels = seq(0,110,5))+
  annotate(
    geom="text", x=95, y =-1, label= "Hombres",color="black",size=3
  )  +
  annotate(
    geom="text", x=95, y =1, label= "Mujeres",color="black",size=3
  )+
  theme_light()+  #los colores del atardecer yucateco (extraño mi casa)
  scale_fill_gradient2(mid = "red", high = "yellow")+
  labs(y="Poblaciones (10,000)", x="Edad (años)",
       title = "Pirámide Poblacional para Yucatán 2020", 
       caption = "Fuente: Censo Nacional de Población y Vivienda 2020")
ggsave("output/piryuc2020.png", dpi = 3000)

```

Hagamos una limpieza rapida:

```{r}
keep_objects <- c("Tabla1", "Tabla2")
rm(list = setdiff(ls(), keep_objects))
```

En este código tenía tanta basura en mi ambiente que era más fácil decirle que se quedara solo con las tablas que llevamos.

### 6. Análisis de Líneas de Vida

Para el siguiente ejercicio transformaremos esta imagen:

![](images/paste-03F37795.png)

En una matriz para luego poder sacar los bimestres persona vividos. Creamos los vectores de los bimestres y los vectores de las personas, luego iremos rellenando conforme a cada línea utilizando 0 (blanco) y 1 (línea).

```{r}
bim <- c(1,2,3,4,5,6) #mis bimestres
per <- c(seq(1,11,1)) #mis personas 

bim_per <- matrix(0, nrow = length(per), ncol = length(bim)) #esqueleto
bim_per


#llenemos 
bim_per[1, 1:4] <- 1 #primera persona
bim_per[2, 1:3] <- 1 #segunda persona
bim_per[3, 1:6] <- 1 #tercera persona
bim_per[4, 4:6] <- 1 #cuarta persona
bim_per[5, 3:6] <- 1 #quinta persona
bim_per[6, 1:6] <- 1 #sexta persona
bim_per[7, 1:6] <- 1 #septima persona
bim_per[8, 1:2] <- 1 #octava persona
bim_per[9, 1:5] <- 1 #novena persona
bim_per[10, 6] <- 1 #décima persona
bim_per[11, 4:6] <- 1 #undécima persona

bim_per #revisamos nuestra matriz
```

Convirtamos nuestra matriz en un data frame y creemos la variable 'apor' para hacer referencia a los bimestres que aportaron. Al final obtendremos los bimestres personas vividos:

```{r}
bim_per <- as.data.frame(cbind(per,bim_per)) #la convertimos a df

colnames(bim_per) <- c("Persona",paste("Bimestre", 1:6))
bim_per
#saquemos los bimestres persona vividos
bpv <-bim_per %>% 
  mutate(bim_apor = `Bimestre 1`+`Bimestre 2`+`Bimestre 3`
         +`Bimestre 4`+`Bimestre 5`+`Bimestre 6`) %>% #sumamos los unos 
  mutate(apor = bim_apor/6) %>% #dividimos entre el tiempo
  summarise(bpv = round(sum(apor), digits = 1))#sumamos y redondeamos
bpv
```

En total se vivieron 7.2 bimestres persona vividos en esa población.

\[Anotación! Se utilizó la siguiente metodología como viene descrita en Measuring and Modeling Population Processes de Preston en la página 6\]

![](images/paste-8D4D2560.png)

### 7. Tasa de Crecimiento Media Anual y Años Persona Vividos

Instrucción: Una población determinada cuenta con 150 mil efectivos el 1 de enero del año 2000; para el año 2010 la población llegó a 275 mil personas y para el 1 de enero del año 2015 pasó a 200 mil. ¿Cuáles fueron las tasas de crecimiento medias anuales en cada periodo? ¿Cuántos años-persona se vivieron en cada periodo?

Primero voy a declarar mi número de euler para sacar mi logaritmo natural con la función log. Luego crearé los elementos de mi fórmula.

![También tomada del Preston](images/paste-D80BCED0.png)

Empecemos con el periodo 2000-2010:

```{r}
#tasas de crecimiento anual medio 
#periodo 2000-2010
euler <- 2.718281828459 #numero de euler para el LN
T= 10 #años entre 2000 y 2010
"Nt2/Nt1" <-275000/150000 #pob2010/pob2000
r_barra0010 <- (log(`Nt2/Nt1`, base = euler)/T)
r_barra0010
```

Ahora hagamos la del periodo 2010-2015. Ojo, aquí utilicé 'abs' para obtener el valor absoluto, es decir, sin signo negativo:

```{r}
#periodo1015
T= 5
"Nt2/Nt1" <-200000/275000
r_barra1015 <- abs((log(`Nt2/Nt1`, base = euler)/T))

r_barra0010 <- round((r_barra0010), digits = 5)
r_barra1015 <- abs(round((r_barra1015), digits = 5))
r_barra1015
```

Ya que tengo mis dos tasas, las uniré en una sola tablita y las convertiré a porcentaje:

```{r}

"Tasa de Crecimiento Media Anual" <- c(r_barra0010, r_barra1015)
"Periodo" <- c("2000-10", "2010-15")
Tabla3 <- as.data.frame(cbind(Periodo, `Tasa de Crecimiento Media Anual`))

Tabla3 <- Tabla3 %>% 
  mutate(`Tasa de Crecimiento Media Anual`=
           as.numeric(`Tasa de Crecimiento Media Anual`)) %>% 
  janitor::adorn_pct_formatting(digits = 1)
Tabla3
```

Ahora calculamos los años persona vividos utilizando la siguiente fórmula:

![También del Preston](images/paste-43140985.png)

Creamos también los elementos de nuestras fórmulas y juntamos todo en una tablita:

```{r}
#años persona vividos

#periodo 2000-2010
euler <- 2.718281828459 #numero de euler para el LN
T= 10 #años entre 2000 y 2010
"Nt2-Nt1" <-275000-150000 
"Nt2/Nt1" <-275000/150000 #pob2010/pob2000
APV1 <-  (T*`Nt2-Nt1`)/(log(`Nt2/Nt1`, base = euler))

#periodo2010-2015
T= 5 #años entre 2000 y 2010
"Nt2-Nt1" <-200000-275000 
"Nt2/Nt1" <-200000/275000 #pob2015/2010
APV2 <-  (T*`Nt2-Nt1`)/(log(`Nt2/Nt1`, base = euler))
APV <- c(APV1, APV2)
Tabla4 <- as.data.frame(cbind(Periodo, APV))
Tabla4 <- Tabla4 %>% 
  mutate(APV = as.numeric(APV)) %>% 
  mutate(APV = round(APV, digits = 0))
Tabla4
```

Durante el primer periodo, se registraron 2,062,244 Años Persona Vividos con un crecimiento del 6%. En el segundo periodo, la cifra fue de 1,177,565 con una tasa de 6.4%.

### 8. Estandarización de la tasa de mortalidad

Limpiar es vital:

```{r}
#limpieza 
keep_objects <- c("Tabla1", "Tabla2", "Tabla3", "Tabla4", "bpv")
rm(list = setdiff(ls(), keep_objects))
```

Ahora abriremos las bases del WPP para escoger dos países de la WPP 2024 en un año determinado, llevar a cabo su estandarización e interpretar. Para este ejercicio seleccionamos Corea del Sur y Corea del Norte. El año fue 2011 y se seleccionó porque marca el inicio del gobierno de Kim Jong Un en Corea del Norte, un punto de incomodidad y roces para la península. Se tomará a la región "East Asia" o "Este de Asia" como marco de referencia para la estandarización partiendo del supuesto de que es un reflejo del comportamiento más o menos general de los grupos de edad en la región:

```{r}
#muertes
base0 <- read_xlsx("input/WPP2024_MORT_F01_1_DEATHS_SINGLE_AGE_BOTH_SEXES.xlsx", 
                   sheet = "Estimates") %>% 
  filter(is.na(`...1`) == F) %>% 
  row_to_names(row_number = 1) %>% 
  rename(region =`Region, subregion, country or area *`)

base0 <-base0 %>% 
  filter(Type %in% c("Subregion", "Country/Area")) %>% #qué tipos queremos
  filter(region %in% c( "Eastern Asia", #escogemos nuestras zonas 
                        "Dem. People's Republic of Korea",
                        "Republic of Korea")) %>% #quitamos las variables que no nos sirven
  select(-c("Index" , "Variant" ,"Notes", "Location code","ISO3 Alpha-code",
            "ISO2 Alpha-code","SDMX code**", "Parent code", "Type"))

#poblacion -----
base1 <- readxl::read_xlsx("input/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", 
                           sheet = "Estimates") %>% 
  filter(is.na(`...1`) == F) %>% 
  row_to_names(row_number = 1) %>% 
  rename(region = `Region, subregion, country or area *`)

base1 <-base1 %>% 
  filter(Type %in% c("Subregion", "Country/Area")) %>% #qué tipos queremos
  filter(region %in% c( "Eastern Asia", #escogemos nuestras zonas 
                        "Dem. People's Republic of Korea",
                        "Republic of Korea")) %>% #quitamos las variables que no nos sirven
  select(-c("Index" , "Variant" ,"Notes", "Location code","ISO3 Alpha-code",
            "ISO2 Alpha-code","SDMX code**", "Parent code", "Type"))


```

Le damos formato a las bases y las unimos:

```{r}
base0 <- base0 %>% 
  gather(key = "age", value = "deaths", -region, -Year)
base0 <- base0 %>%  
  rename(year = Year)

base1 <- base1 %>% 
  gather(key = "age", value = "population", -region, -Year)
base1 <- base1 %>%  
  rename(year = Year) 

#agregar id -----------------
base0  <- base0  %>% 
  mutate(id= paste(region, year, age, sep = "-"))
base1  <- base1  %>% 
  mutate(id= paste(region, year, age, sep = "-"))

#merge ---------------------
base_fin <- left_join(base1, base0, by= "id")
base_fin <- base_fin %>% 
  select(region = region.x, 
         year = year.x, 
         age = age.x, 
         Dx = deaths, 
         Nx = population) %>% 
  type.convert()

```

Toca crear los indicadores, empecemos con la TBM \[Tasa Bruta de Mortalidad\] sin estandarizar:

```{r}
tbm <- base_fin %>% 
  group_by(region, year) %>% 
  summarise(tbm = 1000*sum(Dx)/sum(Nx), .groups = "drop" ) %>% 
  filter(year == 2011)
tbm
```

Ahora sacamos nuestro referente del Este de Asia:

```{r}
tbmEA <- base_fin %>% 
  group_by(region, year) %>% 
  mutate(Mx = Dx/Nx, 
         Cx = Nx/sum(Nx)) %>% 
  filter(region == "Eastern Asia") %>% 
  mutate(id = paste(year, age, sep = "-")) %>% 
  ungroup()
```

Y unimos para estandarizar:

```{r}
tbm <- base_fin %>% 
  group_by(region, year) %>% 
  mutate(Mx = Dx/Nx, 
         Cx = Nx/sum(Nx)) %>% 
  mutate(id = paste(year, age, sep = "-")) %>% 
  ungroup()

tbm_full <- full_join(tbm, tbmEA, by = "id") %>% 
  select(region = region.x, 
         year = year.x, 
         age = age.x, 
         Dx = Dx.x, 
         Mx = Mx.x, 
         Cx = Cx.x, 
         CxS = Cx.y)

```

Extraigamos la no estandarizada:

```{r}
tbm1 <- base_fin %>% 
  group_by(region, year) %>% 
  summarise(tbm = 1000*sum(Dx)/sum(Nx), .groups = "drop" ) %>% 
  filter(year == 2011) %>% 
  mutate(tbm = round(tbm, digits = 2))
colnames(tbm1) <- c("Región", "Año", "TBM")  
tbm1
```

Extraemos la estandarizada:

```{r}
tbm2 <- tbm_full %>%  
  group_by(region, year) %>% 
  mutate(MxCxS = Mx*CxS) %>% 
  summarise(ASCDR = 1000*sum(MxCxS)) %>% #tasa de mortalidad 
  filter(year == 2011) %>% 
  mutate(ASCDR = round(ASCDR, digits = 2 ))
colnames(tbm2) <- c("Región", "Año", "ASCDR")
tbm2
```

Unamos ambas para comparar:

```{r}

Tabla5 <- full_join(tbm1, tbm2, by= "Región") %>% 
  select(
    Región = Región, 
    Año = Año.x, 
    TBM = TBM, 
    ASCDR = ASCDR)
Tabla5
```

Como podemos ver antes de estandarizar, la distancia entre las TBM de ambos países parecía relativamente pequeña. Aunque obvimente Corea del Norte más alejada de la TBM de la región la diferencia parecía en cierta medida pequeña. Después de estandarizar se exacerban las diferencias. ¿Porqué?

Ahora realizaremos una descomposición de kitagawa para entender de donde vienen estas diferencias:

```{r}
base_desc <- full_join(
  full_join(
    tbm %>% 
      filter(region == "Dem. People's Republic of Korea", 
             year == 2011) %>% 
      select( -c(Dx, Nx)),
    tbm %>% 
      filter(region == "Republic of Korea", 
             year == 2011) %>% 
      select( -c(Dx, Nx)), by= "age") %>% 
    mutate(dif_mort = Mx.x - Mx.y, # la diferencia en la mortalidad entre las dos regiones
           dif_age = Cx.x - Cx.y) %>%  # diferencia en su estructura por edades
    type.convert(),
  tbm %>% 
    filter(region %in% c("Dem. People's Republic of Korea", "Republic of Korea"),
           year == 2011) %>% 
    select(region, age, Mx, Cx) %>% 
    type.convert() %>% 
    group_by(age) %>% 
    summarise(
      CxM = mean(Cx), # media de estructura por edades
      MxM = mean(Mx), .groups = "drop"), by= "age") %>%  # media de muertes
  mutate(efect_age0 = dif_age * MxM, # efecto edad = diferencia edad * media de muertes
         efect_mort0 = dif_mort * CxM) # efecto muerte = diferencia en muertes * media de estructura por edades

descomp <- base_desc %>% 
  summarise(efect_age0 = sum(efect_age0), # sumamos los efectos
            efect_mort0 = sum(efect_mort0))

# Porcentaje del efecto de estructuras por edad----------
100 * abs(descomp$efect_age0) / (abs(descomp$efect_age0) + # abs del efecto de la edad
                                   abs(descomp$efect_mort0)) # abs del efecto edad + efecto muerte


Efecto_CX <- 100 * abs(descomp$efect_age0) / (abs(descomp$efect_age0) + # abs del efecto de la edad
                                                abs(descomp$efect_mort0)) # abs del efecto edad + efecto muerte
Efecto_CX <- as.data.frame(Efecto_CX)
Efecto_CX

```

Podemos decir que el efecto de las estructuras por edad es del 24% , lo que implica que sí hay una relevancia de las edades en cómo se inflan las tasas brutas. Las causas podrían ser varias pero sería interesante ver los efectos de la gobernanza, la transición a un nuevo jefe de estado y las garantías individuales.

Sólo queda limpiar y guardar todo en un excel para almacernar y compartir la información:

```{r}

#Ultima limpieza: 

keep_objects <- c("Tabla1", "Tabla2", "Tabla3", "Tabla4", "Tabla5", "bpv", "Efecto_CX")
rm(list = setdiff(ls(), keep_objects))

#Exportación ----------


examen <- list("%Porcentaje de NE" = Tabla1, 
     "Índices de Whipple" = Tabla2, 
     "Bimestres Persona Vividos" = bpv, 
     "Tasa de Crecimiento Media anual" = Tabla3, 
     "Años Persona vividos" = Tabla4, 
     "Estandarización" = Tabla5, 
     "Porcentaje del Efecto de CX" = Efecto_CX )
writexl::write_xlsx(examen, "output/examen.xlsx")



```
