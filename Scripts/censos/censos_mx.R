#INSTALACION DE PAQUETERIAS ---------
install.packages("readxl")
install.packages("readr")
install.packages("tidyverse")
install.packages("stringr")
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
remotes::install_github("timriffe/DemoTools")

library(DemoTools
library(readxl)
library(readr)
library(tidyverse)
library(stringr)
library(ungroup)

data <-  read_xlsx("input/censos2020.xlsx", 
                   skip=6, col_names = F) #skip es para saltarse filas


names(data) <- (c("id", "edo", "age", "both", "males", "females"))

#LIMPIAR BASE ----


base_mx <- data %>% 
  filter(is.na(edo)== FALSE) %>% #quitamos los renglones en blanco
  select(-c("id", "both")) %>%  #quitamos la variable id y la suma de ambos sexos
  na.omit() %>%
  filter(
    age != "Total", #quitamos los totales de edad 
    !grepl("De", age), #es para filtrar de acuerdo a contenido de las variables 
    !grepl("85 años", age)) %>%  #quitamos a los de 85años y más 
  mutate(age= str_extract(age,"\\d+")) %>% ## quitar "año" a la variable edad #extraer el dígito "\\d+"
  gather(key = sex, value= pob, -age,-edo) %>%  #gather es para colocar las variables para tener un base en formato long
  type_convert() %>%  #detecta y cambia el formato de los datos 
  mutate(pob2= ifelse(sex =="females", pob, - pob))
    #si son mujeres positivo si son hombres es negativo
view(base_mx)



save(base_mx, file = "input/base_mx.RData") #guardar como RData


#PLOTTEAR PIRAMIDE DE POBLACION --------
load("input/base_mx.RData")


base_mx %>% 
  filter(edo == "Total", is.na(age) == FALSE) %>%    #filtra total y  quitar nas de edad 
ggplot() + #hacer gráfico
geom_bar(aes(x= age, y= pob2/1000000, fill = age), #el aes llena la grafica
          stat = "identity",
         show.legend = F)+ #quitamos la leyenda
  coord_flip() +  #de horizontal a vertical 
  geom_hline(yintercept = 0) + #linea que divide h y m en el 0  #h horiz
  scale_y_continuous(   #escala continua quita negativos
    limits = c(-1.25, 1.25), #limites del eje y  
    breaks = seq(-1.25, 1.25, 0.25),#tamaño de espacios
    labels = as.character(c(seq(1.25, 0, -0.25), #pone las etiquetas  
                        seq(0.25, 1.25, 0.25) #es una secuencia de 0.25 a 1.25
                        ))) + #en incrementos de 0.25
  scale_x_continuous(
    limits = c(-1,101),  #limite de 01 años a 101 años
    breaks= seq(0, 100,5) #en incrementos de 5 años 
  )+
  annotate(
    geom= "text", x= 95, y= -1,label= "Hombres",
    color= "black") +
  annotate(
    geom= "text", x= 95, y= 1,label= "Mujeres",
    color= "black") +
  theme_minimal() +
  scale_fill_viridis_c(option = "A")+
  labs(title= "Pirámide de Población 2020" ,y = "Población 2020", x= "Edad", 
       caption= "Fuente: Censo Nacional de Población y Vivienda 2020. INEGI")
  
ggsave(filename = "output/piramide1.png", dpi = 3200)

base_mx %>% 
  filter(age == is.na(age)) %>% 
  count(age)


#Prorateo----
base_mx_pror <- left_join(   #base prograteada 
  base_mx %>% #base uno
    filter(edo == "Total") %>%  #quitamos el total en la columna de edo
    group_by(sex) %>%  # agrupamos por sexo
    mutate(prop = pob/sum(pob)) %>%  #sacmos el peso de cada grupo de edad 
    filter(is.na(age)== F) %>%  #quitamos a los no especificados
    ungroup(), #desagrupamos 
  base_mx %>%  #base dos 
    filter(edo == "Total", is.na(age)==T) %>% #filtramos para tener total del edo con no especificados 
    select(sex, pob_na = pob), by= "sex") %>%  #seleccionamos columnas y agrupamos por sexo
  #aqui sucede el merge
  mutate(pob_fin = pob + pob_na*prop) %>% #columna de poblacion final con los NE
  select(age, sex, pob = pob_fin) #seleccionamos columna de interés.



load("input/base_mx.RData")
##QUINQUENAL MAESTRO ----

bins <- seq(0, max(base_mx_pror$age) + 5, by = 5)

labels <- paste(bins[-length(bins)],
                bins[-1] - 1,
                sep = "-")

base_mx_pror <- base_mx_pror %>%
  mutate(age_group = cut(age, breaks = bins, labels = labels, right = F))

base_mx_5 <- base_mx_pror %>%
  group_by(sex, age_group) %>%
  summarise(pob = sum(pob, na.rm = T),
            .groups = "drop") %>%
  mutate(pob2 = ifelse(sex == "males", -pob, pob))

#GRAFICA Q MAESTRO

base_mx_5 %>%
  ggplot() +
  geom_bar(aes(x = age_group, y = pob2/1000000, fill = age_group),
           stat = "identity",
           show.legend = F
  ) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    limits = c(-6, 6),
    breaks = seq(-6, 6, 1),
    labels = as.character(c(
      seq(6, 0, -1),
      seq(1, 6, 1)
    ))
  ) +
  annotate(
    geom = "text", x = 20, y = -5, label = "Hombres",
    color = "black", size = 4
  ) +
  annotate(
    geom = "text", x = 20, y = 5, label = "Mujeres",
    color = "black", size = 4
  ) +
  theme_minimal() + #para cambiar el fondo
  labs(y = "Población (millones)", x = "Rango Edad (años)") +
  scale_fill_viridis_d(option = "C")

##DESAGRUPAR LOS GRUPOS QUINQUENALES----

base_mx_5H <- base_mx_5 %>%
  filter(sex == 'males')

base_mx_5M <- base_mx_5 %>%
  filter(sex == 'females')

base_mx_1_H <- pclm(x = bins[-length(bins)],
                    base_mx_5H$pob, nlast = 10)

base_mx_1_M <- pclm(x = bins[-length(bins)],
                    base_mx_5M$pob, nlast = 10)

base_mx_1 <- rbind(enframe(base_mx_1_H$fitted,
                           name = 'age', value = 'pob') %>%
                     mutate(age = c(0:109), sex = 'males',
                            .before = pob),
                   enframe(base_mx_1_M$fitted,
                           name = 'age', value = 'pob') %>%
                     mutate(age = c(0:109), sex = 'females',
                            .before = pob)
                   
) %>%
  mutate(pob2 = ifelse(sex == 'males', -pob, pob))


base_mx_1 %>%
  ggplot() +
  geom_bar(aes(x=age, y=pob2/1000000,fill=age),
           stat='identity', show.legend = F)+
  coord_flip()+
  geom_hline(yintercept=0)+
  scale_y_continuous(
    limits=c(-1.25,1.25),
    breaks=seq(-1.25,1.25,0.25),
    labels=as.character(c(seq(1.25,0,-0.25),
                          seq(0.25,1.25,0.25))))+
  scale_x_continuous(limits=c(-1,110), breaks = seq(0,110,5),
                     labels = seq(0,110,5))+
  annotate(
    geom="text", x=95, y =-1, label= "Hombres",color="black",size=3
  )  +
  annotate(
    geom="text", x=95, y =1, label= "Mujeres",color="black",size=3
  )+
  theme_light()+
  scale_fill_viridis_c(option="C")+
  labs(y="Poblaciones (millones)", x="Edad (años)")





