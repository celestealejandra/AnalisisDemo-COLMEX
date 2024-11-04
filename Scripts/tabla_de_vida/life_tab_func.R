#Realizamos la función de cálculo de tabla de mortalidad
#en caso de no tenerlos instalados
#install.packages("(tidyverse")
#install.packages("readr")
#install.packages("readxl")
#install.packages("patchwork")
#install.packages("janitor")
library(tidyverse)
library(readr)
library(readxl)
library(patchwork)
library(janitor)


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

