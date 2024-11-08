#instalación de librerías 
library(tidyverse)
library(readr)
library(readxl)
install.packages("patchwork")
library(patchwork)
library(janitor)

#abrir archivos ---------
Nx <- readxl::read_xlsx("input/0_Pob_Mitad_1950_2070.xlsx")
Nx <- Nx %>% 
  clean_names() %>% 
  rename( year = ano, age = edad, sex = sexo, pop = poblacion, 
          edo = entidad) %>% 
  mutate(id = paste(year, edo, age, sex))

Dx <- readxl::read_xlsx("input/1_Defunciones_1950_2070.xlsx")
Dx <- Dx %>% 
  clean_names() %>% 
  rename( year = ano, age = edad, sex = sexo, defs = defunciones, 
          edo = entidad) %>% 
  mutate(id = paste(year, edo, age, sex))



base0 <- full_join(Nx, Dx, by= "id") %>% 
  select(edo = edo.x, 
         year = year.x, 
         sex= sex.x,
         cve_geo = cve_geo.x, 
         age = age.x, 
         pop = pop, 
         defs = defs) %>% 
  mutate( sex = ifelse(sex == "Hombres", "males", "females")) #mutate sexo
save(base0, file= "input/base0.RData")
tab_mort <- base0 %>% 
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
  replace_na(list(Ex=0))

tab_mort_mx <-tab_mort %>% 
  group_by(year, edo, cve_geo, sex) %>%  #seleccionamos
  mutate(cx = lx/sum(lx), #creamos cx para las fórmulas 
         cxqx = cx*qx) %>% 
  ungroup() %>% 
  group_by(year, edo, cve_geo, age) %>% 
  summarise(qx= sum(cxqx, na.rm = T)/sum(cx, na.rm = T), .groups = "drop") %>% #creamos qcxm y qcxh 
  replace_na(list(qx=1)) %>% 
  group_by(year, edo,cve_geo) %>% 
  mutate(px = pmax(0,1 -qx),
         lx= c(1, cumprod(px))[-length(px)],
         dx = lx*qx,
         Lx = (lx + lead(lx, default = 0)) / 2, 
         Tx = rev(cumsum(rev(Lx))), 
         Ex = Tx/lx) %>%
  ungroup() %>% 
  replace_na(list(Ex = 0)) %>% 
  mutate( sex = "both", .before = qx)

tab_mort_mx <-rbind((tab_mort %>% 
                       select(year, edo, cve_geo, age, sex,  qx, px, lx, dx, Lx, Tx, Ex)), 
                    tab_mort_b %>% 
                      select(year, edo, cve_geo, age, sex,  qx, px, lx, dx, Lx, Tx, Ex))

tab_mort_mx <-tab_mort_mx %>% 
  arrange(age, year)



##############}
load("input/base0.R")
