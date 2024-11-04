library(tidyverse)
library(readr)
library(readxl)
library(patchwork)
library(janitor)
library(viridisLite)

#abrir todo---------------------------------------
load("input/base0.RData")
source("r/tab_mort_func.R")
#hacemos nuestra tabla de vida--------------------
tab_mort <- tab_mort_func(base = base0)


###calculamos los cambios en la esperanza de vida en para la república mexicana
###y para los años 1950 y 2023


tab_mort %>% 
  filter(year %in% c(1950, 2023), 
         edo == "República Mexicana",
         age == 0, 
         sex == "females") %>% 
  select(year, Ex) %>% 
  mutate(dif_ex = Ex-lag(Ex)) ##lag es para restas o sumas en la misma col




#descomposición de arriaga----------------------

desc_lt <- full_join(tab_mort %>% 
  filter( sex == "females", 
          year == 1950,
          edo == "República Mexicana") %>% 
  select(age, lx, Lx, Tx), ########base1
tab_mort %>% 
  filter( sex == "females", 
          year == 2023,
          edo == "República Mexicana") %>% 
  select(age, lx, Lx, Tx),##############base2
by= "age") %>% ######fin del join
  select(age= age, lx1= lx.x, Lx1 = Lx.x, Tx1 = Tx.x, 
         lx2= lx.y, Lx2 = Lx.y, Tx2 = Tx.y)###selec names
### x es t1 y y es t2 

arriaga <- desc_lt %>% 
  mutate(DEx = lx1*((Lx2/lx2)-(Lx1/lx1))) %>%  #Efecto directo
  mutate(OEx = lead(Tx2, default = 0)*
               ((lx1/lx2)-lead(lx1, default = 0)/
               lead(lx2, default = 0)),
         delta_x= DEx + OEx, 
         perc = 100*abs(delta_x)/sum(abs(delta_x), na.rm = T))

rm(desc_lt)



arriaga %>% 
  summarise(ex1_ex2 = sum(delta_x, na.rm = T))

#gráfico ----------------------------------
arriaga %>% 
  na.omit() %>% 
  ggplot() +
  geom_bar(aes(x= age, y= delta_x, fill=age),
           stat= "identity", show.legend = F) +
  theme_light() +
  scale_fill_viridis_c(option = "B") +
  scale_y_continuous(n.breaks = 20) +
  scale_x_continuous(n.breaks = 10, limits = c(-1, 110))+
  labs(title = "Progresión de Delta x en las edades", 
       caption = "Fuente: Elaboración Propia con Información de CONAPO",
       y= "Delta X",
       x= "Edad")
  
ggsave("output/delta_x.png", dpi = 3000)

arriaga %>% 
  na.omit() %>% 
  ggplot() +
  geom_bar(aes(x= age, y= perc, fill=age),
           stat= "identity", show.legend = F) +
  theme_light() +
  scale_fill_viridis_c(option = "B") +
  scale_y_continuous(n.breaks = 20) +
  scale_x_continuous(n.breaks = 10, limits = c(-1, 110))+
  labs(title = "Porcentaje de cambio en la e0 en las edades", 
       caption = "Fuente: Elaboración Propia con Información de CONAPO",
       y= "Porcentaje",
       x= "Edad")

ggsave("output/porcentaje.png", dpi = 3000)



#hombres entre 2000 y 2010

desc_lt <- full_join(tab_mort %>% 
                       filter( sex == "males", 
                               year == 2000,
                               edo == "República Mexicana") %>% 
                       select(age, lx, Lx, Tx), ########base1
                     tab_mort %>% 
                       filter( sex == "males", 
                               year == 2010,
                               edo == "República Mexicana") %>% 
                       select(age, lx, Lx, Tx),##############base2
                     by= "age") %>% ######fin del join
  select(age= age, lx1= lx.x, Lx1 = Lx.x, Tx1 = Tx.x, 
         lx2= lx.y, Lx2 = Lx.y, Tx2 = Tx.y)###selec names
### x es t1 y y es t2 

arriaga <- desc_lt %>% 
  mutate(DEx = lx1*((Lx2/lx2)-(Lx1/lx1))) %>%  #Efecto directo
  mutate(OEx = lead(Tx2, default = 0)*
           ((lx1/lx2)-lead(lx1, default = 0)/
              lead(lx2, default = 0)),
         delta_x= DEx + OEx, 
         perc = 100*abs(delta_x)/sum(abs(delta_x), na.rm = T))

rm(desc_lt)



arriaga %>% 
  summarise(ex1_ex2 = sum(delta_x, na.rm = T))

#gráfico ----------------------------------
arriaga %>% 
  na.omit() %>% 
  ggplot() +
  geom_bar(aes(x= age, y= delta_x, fill=age),
           stat= "identity", show.legend = F) +
  theme_light() +
  scale_fill_viridis_c(option = "B") +
  scale_y_continuous(n.breaks = 20) +
  scale_x_continuous(n.breaks = 10, limits = c(-1, 110))+
  labs(title = "Progresión de Delta x en las edades, Hombres 2000-10", 
       caption = "Fuente: Elaboración Propia con Información de CONAPO",
       y= "Delta X",
       x= "Edad")

ggsave("output/delta_x_h0010.png", dpi = 3000)

arriaga %>% 
  na.omit() %>% 
  ggplot() +
  geom_bar(aes(x= age, y= perc, fill=age),
           stat= "identity", show.legend = F) +
  theme_light() +
  scale_fill_viridis_c(option = "B") +
  scale_y_continuous(n.breaks = 20) +
  scale_x_continuous(n.breaks = 10, limits = c(-1, 110))+
  labs(title = "Porcentaje de cambio en la e0 en las edades, Hombres 2000-2010", 
       caption = "Fuente: Elaboración Propia con Información de CONAPO",
       y= "Porcentaje",
       x= "Edad")

ggsave("output/porcentaje_h0010.png", dpi = 3000)



















