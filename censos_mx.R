#INSTALACION DE PAQUETERIAS ---------
install.packages("tidyverse")
install.packages("readxl")
install.packages("readr")
install.packages("haven")
install.packages("tidyverse")
install.packages("esquisse")
library(haven)
library(readxl)
library(readr)
library(tidyverse)
library(stringr)
library(esquisse)

data <- read_excel("input/censos2020.xlsx", skip = 6)


names(data) <- (c("id", "edo", "age", "both", "males", "females"))


#limpieza de base de datos ----
base_mx <- data %>% 
  filter(is.na(edo)== FALSE) %>% #quitamos los renglones en blanco
  select(-c("id", "both")) %>%  #quitamos la variable id y la suma de ambos sexos
  na.omit() %>%
  filter(
    age != "Total", #quitamos los totales de edad 
    !grepl("De", age), #es para filtrar de acuerdo a contenido de las variables 
    !grepl("85 años", age)) %>%  #quitamos a los de 85años y más 
  mutate(age= str_extract(age,"\\d+")) %>% ## quitar "año" a la variable edad
  gather(key = sex, value= pob, -age,-edo) %>%  #gather es para colocar las variables para tener un base en formato long
  type_convert() %>%  #detecta y cambia el formato de los datos 
  mutate(pob2= ifelse(sex =="females", pob, - pob))

save(base_mx, file= "input/base_mx.r")



#pirámide poblacional ----

base_mx %>%  #llamamos a la base
  filter(!edo == "Total" & !is.na(age)) %>%  #seleccion de casos
  ggplot()+ #llamamos a ggplot para graficar
  geom_bar(aes(x = age, y = pob2 / 1000000, fill = age),  #grafica de barras 
  stat = "identity", #tipo de stat
  show.legend = FALSE) + #no me muestres la leyenda
  coord_flip()+ #le damos vuelta para darle forma de pirámide
  geom_hline(yintercept = 0)+ #linea que va en medio 
  scale_y_continuous( #le damos formato a la escala x 
  limits = c(-1.25,1.25),      #de x a x
  breaks = seq(-1.25,1.25,0.25), #en cuantos breaks 
  labels= as.character( #etiquetas
    c(seq(1.25, 0, -0.25),  #seq del 1.25 al 0 en decr de 0.25
      seq(0.25, 1.25, 0.25))))+ #seq de 0.25 a 1.25 en inc de 0.25
  scale_x_continuous(
    limits = c(-1, 101), 
    breaks = seq(0,100,5),
    labels= seq(0,100,5))+
  annotate(geom= "text", x=95, y= -1, label = "Hombres", #ponemos su texto
           color= "black", size=3)+
  annotate(geom = "text", x=95, y= 1, label= "Mujeres", 
           color = "black", size= 3) +
  theme_light()+
  scale_fill_viridis_c(option = "C")+
  labs(y = "Población (millones)", #agregamos etiquetas 
       x= "Edad", 
       title= "Pirámide de Población 2020", 
       caption = "Censo Nacional de Población y Vivienda 2020")
  ggsave(filename = "output/piramide1.png", dpi= 3200)
  
  
  #prograteo de no especificados ------

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

#índice de whipple -----
left_join(
base_mx_pror %>% 
  filter(
    age %in% c(25:60), #filtrar del 50 al 60
    age %% 10 == 0|age%% 10 == 5) %>%   #quedate con los numeros que terminan en 0 o en 5
  group_by(sex) %>% #agrupar por sexo
  summarise(pob_num = sum(pob),
            .groups= "drop"), #denominador del índice
base_mx_pror %>% 
  filter(age %in% c(23:62)) %>% 
  group_by(sex) %>% 
  summarise(pob_den = sum(pob), 
            .groups = "drop"),by= "sex") %>% 
  mutate(W=5 * pob_num/pob_den)
  









  






