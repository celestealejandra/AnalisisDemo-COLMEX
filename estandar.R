#Instalación de Paquetes-----
library(tidyverse)
library(readxl)
library(janitor)

#Abrir Archivos-------


base0 <- read_xlsx("input/WPP2024_MORT_F01_1_DEATHS_SINGLE_AGE_BOTH_SEXES.xlsx", 
                      sheet= "Estimates") %>% 
  filter(is.na(`...1`) == F) %>% 
  row_to_names(row_number = 1) %>% 
  select(-c("Index" , "Variant" ,"Notes", "Location code","ISO3 Alpha-code",
            "ISO2 Alpha-code","SDMX code**","Type", "Parent code" ))
names(base0)
#filtrar las 5 regiones , poner las edades como renglones
#Hacer lo mismo con los otros archivos


base0_long <- base0 %>% #filtramos por region
filter(`Region, subregion, country or area *` %in% c("Asia", "Africa",
                                                  "Americas", "Europe",
                                                  "Oceania", "World")) 
base0_long <- base0_long %>% #trasponemos la edad
  gather(key = "Age", value = "Population", `0`:`100+`)
#gather(key = "Nueva columna", Valor = "Población" (esta es la suma del gather), 
#rango de columnas)
       
base1 <- readxl::read_xlsx("input/WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx", 
                          sheet = "Estimates") %>% 
  filter(is.na(`...1`) == F) %>% 
  row_to_names(row_number = 1) %>% 
  select(-c("Index" , "Variant" ,"Notes", "Location code","ISO3 Alpha-code",
            "ISO2 Alpha-code","SDMX code**","Type", "Parent code" ))


base1_long <- base1 %>% #filtramos por region
  filter(`Region, subregion, country or area *` %in% c("Asia", "Africa",
                                                       "Americas", "Europe",
                                                       "Oceania", "World")) 
base1_long <- base1_long %>% #trasponemos la edad
  gather(key = "Age", value = "Population", `15`:`49`)

base2 <- readxl::read_xlsx("input/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", 
                           sheet = "Estimates") %>% 
  filter(is.na(`...1`) == F) %>% 
  row_to_names(row_number = 1) %>% 
  select(-c("Index" , "Variant" ,"Notes", "Location code","ISO3 Alpha-code",
            "ISO2 Alpha-code","SDMX code**","Type", "Parent code" ))

base2_long <- base2 %>%  
  filter(`Region, subregion, country or area *` %in% c("Asia", "Africa",
                                                       "Americas", "Europe",
                                                       "Oceania", "World")) 
base2_long <- base2_long %>% #trasponemos la edad
  gather(key = "Age", value = "Population", `0`:`100+`)

#conjuntando en un archivo
library(writexl) #librería para escribir

WPP_2024 <- list("Mortalidad" = base0_long, 
                 "Fecundidad" = base1_long, 
                 "Poblacion" = base2_long)
writexl::write_xlsx(WPP_2024, path = "output/WPP_2024_long.xlsx")




