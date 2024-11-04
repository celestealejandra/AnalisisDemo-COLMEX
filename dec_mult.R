#Tablas de decrimento múltiple por causas 
library(tidyverse)
library(readr)
library(readxl)
library(patchwork)
library(janitor)

#abrir data frame 
source(file = "r/tab_mort_func.R")
load("input/base0.RData")
load("input/df_causes2019.RData")


 tab_mort <- tab_mort_func(base = base0)

tmort19 <- tab_mort %>% 
  filter(edo == "República Mexicana",
         year == 2019) %>% 
  select(age, sex, qx, lx) %>% 
  mutate(id= paste(age, sex, sep="")) #generamos un id

names(table(df_causes1$disease_group))


#creo una base de datos donde sólo esten las infecciosas 
df_causes2 <- df_causes1 %>% 
  mutate(sexo = case_when(sexo == 1 ~ "males",
                         sexo == 2 ~ "females"), #nombre a la cat de sexo
         dos_causas = ifelse(
           disease_group == "Infecciosas", "Infecciosas", #i y -i en la formula
           "Resto")) %>% 
  group_by(sexo, age, dos_causas) %>%  #agrupo
  summarise(defs = sum(defs), .groups = "drop") %>% #resumo la tabla 
  group_by(sexo, age) %>% 
  mutate( prop = defs/sum(defs)) %>%
  ungroup() %>% 
  mutate(id = paste(age, sexo, sep=""))#creamosla variable de ndxi /ndx

df_causes2 %>% 
  filter(dos_causas == "Infecciosas") %>% 
  select(id, prop)

tdcmult <- full_join(
  tmort19, #tabla de vida
  df_causes2 %>% 
    filter(dos_causas == "Infecciosas") %>% #causas de muerte
    select(id, prop), by = "id") %>% 
  select(-id) %>% 
  replace_na(list(prop = 0)) %>% 
  na.omit()



tdcmult <- tdcmult %>% 
  mutate( qxi = qx*prop)

tdcmult <- tdcmult %>% 
  mutate( dxi = qxi*lx)

tdcmult <- tdcmult %>% 
  group_by(sex) %>% #agrupamos por sexo
  mutate( lxi = rev(cumsum(dxi))) %>% 
  ungroup()
#rev para ir de lx+n a lx
#cumsum para sumas acumuladas 

tdcmult <- tdcmult %>% 
  group_by(sex) %>% 
  mutate( prop2 = (lxi/lx)*100) %>% 
  ungroup()
#Proporción de sobrevivientes a la edad x que eventualmente fallecen de la causa i
  
