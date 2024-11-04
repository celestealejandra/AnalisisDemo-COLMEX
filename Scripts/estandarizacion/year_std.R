#Instalación de Paquetes-----
library(tidyverse)
library(readxl)
library(janitor)
#Abrir Archivos
#Mortalidad -----------

base0 <- read_xlsx("input/WPP2024_MORT_F01_1_DEATHS_SINGLE_AGE_BOTH_SEXES.xlsx", 
                   sheet = "Estimates") %>% 
  filter(is.na(`...1`) == F) %>% 
  row_to_names(row_number = 1) %>% 
  rename(region =`Region, subregion, country or area *`)

base0 <-base0 %>% 
  filter(Type %in% c("Subregion", "Country/Area")) %>% #qué tipos queremos
  filter(region %in% c( "Eastern Asia", #escogemos nuestras zonas 
                        "Dem. People's Republic of Korea",
                        "Republic of Korea", 
                        "China","Japan")) %>% #quitamos las variables que no nos sirven
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
                        "Republic of Korea", 
                        "China","Japan")) %>% #quitamos las variables que no nos sirven
  select(-c("Index" , "Variant" ,"Notes", "Location code","ISO3 Alpha-code",
            "ISO2 Alpha-code","SDMX code**", "Parent code", "Type"))

#convertimos a formato long --------
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


#construccion de indicadores ------
  base_full <-base_fin %>% 
  group_by(region, year) %>% 
  mutate(Mx = Dx/Nx, 
         Cx = Nx/sum(Nx)) %>% 
  filter(year %in% c(2023,2003)) %>% 
    mutate(id = paste(region,year,age, sep= "-"))
  
  
#creamos nuestro ponderador-------
  cx <- base_full %>% 
    group_by(age) %>% 
    summarise(Cx = mean(Cx))
  
  base_full <- full_join( base_full, cx, by= "age") %>% 
    select(region, year, Dx, Nx, Mx, Cx = Cx.x, 
           CxS = Cx.y)
  
#cuadros a comprarar-------
  tbm1 <- base_full %>% 
    group_by(region, year) %>% 
    summarise(tbm = 1000*sum(Dx)/sum(Nx), .groups = "drop" ) %>% 
    filter(year %in% c(2023,2003)) %>% 
    mutate(tbm = round(tbm, digits = 2))
  colnames(tbm1) <- c("Región", "Año", "TBM")  
  
  
  tbm2 <- base_full %>%  
    group_by(region, year) %>% 
    mutate(MxCxS = Mx*CxS) %>% 
    summarise(CDRYS = 1000*sum(MxCxS)) %>% #tasa de mortalidad 
    filter(year %in% c(2003,2023)) %>% 
    mutate(CDRYS = round(CDRYS, digits = 2 ))
  colnames(tbm2) <- c("Región", "Año", "ASCDR")
  
  
  
