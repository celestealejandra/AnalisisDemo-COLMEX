#INSTALACION DE PAQUETERIAS ---------
library(readxl)
library(readr)
library(tidyverse)
library(stringr)

data <-  read_xlsx("input/censo2020.xlsx", 
                   skip=6, col_names = F) #skip es para saltarse filas

names(data) <- (c("id", "edo", "age", "both", "males", "females"))

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

view(base_mx)
 library(ggplot2)
install.packages("esquisse")
library(esquisse)