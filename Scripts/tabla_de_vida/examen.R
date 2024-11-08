#### Tarea Exámen Análisis Demográfico
##Celeste Alejandra Díaz 
##fecha de creación: 07/11/2024
library(tidyverse)
library(readr)
library(readxl)
library(patchwork)
library(janitor)


#calculo de tabla de mortalidad-------
anos <- c(0,1,5,seq(10,100,10)) #mis años para la tabla
labels <- c(0,1,5,seq(10,100,10))

per <- rep(1,30) 

age <- c(7.1, 2.46, 3.9, 0.91, 9.62, 11.21,21.91,
         25.52, 27.59, 11.78, 23.56, 27.69, 27.92,
         21.76, 22.28, 17.93, 17.79,
         22.24, 27.44, 23.93, 40.46, 
         6 8.74, 77.78, 67.44, 36.99, 
         73.78, 33.19, 62.84, 53.77, 74.71)
df1 <- as.data.frame(cbind(per, age))
writexl::write_xlsx(df1, "output/df1.xlsx")

# Tabla de mortalidad -------
load("input/base.RData")
tab_mort <- tab_mort_func(base = base0)

tab_mort_b <-tab_mort %>% 
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

rm(tab_mort_b, tab_mort, base0)

#Gráficos -------------------
tab_mort_mx %>%
  filter(cve_geo == 1) %>% 
  filter(year %in% c(1970, 2000, 2019)) %>% 
  ggplot() +
  geom_line(aes(x = age, y = log(lx), color = factor(year), group = year)) +
  labs(caption = "Fuente: Conciliación Demográfica, CONAPO 2022",
       title = "Progresión función de supervivencia \n 1970, 2000 y 2019 en Aguascalientes", 
       x = "Edad",
       y = "lx",
       color = "Año") +  # Añade una etiqueta para la leyenda
  scale_color_manual(values = c("#D050F2", "#507CF2", "#F2507F")) +
  facet_wrap(~sex, labeller = labeller(sex = c("males" = "Hombres", "females" = "Mujeres", "both" = "Ambos"))) +
  theme_linedraw()+
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5),  # Título en negrita y más grande
    axis.title.y = element_text(face = "italic"),         # Etiqueta de y en cursivas
    strip.text = element_text(face = "bold")              # Etiquetas de facet_wrap en negrita
  )
ggsave("output/lx_plot.png", dpi = 3000)

tab_mort_mx %>% 
  filter(cve_geo == 1) %>% 
  filter(year %in% c(1970, 2000, 2019)) %>% 
  ggplot() +
  geom_line(aes(x = age, y = dx, color = factor(year), group = year)) +
  labs(caption = "Fuente: Conciliación Demográfica, CONAPO 2022",
       title = "Progresión función de supervivencia \n 1970, 2000 y 2019 en Aguascalientes", 
       x = "Edad",
       y = "lx",
       color = "Año") +  # Añade una etiqueta para la leyenda
  scale_color_manual(values = c("#D050F2", "#507CF2", "#F2507F")) +
  facet_wrap(~sex, labeller = labeller(sex = c("males" = "Hombres", "females" = "Mujeres", "both" = "Ambos"))) +
  theme_linedraw()+
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5),  # Título en negrita y más grande
    axis.title.y = element_text(face = "italic"),         # Etiqueta de y en cursivas
    strip.text = element_text(face = "bold")              # Etiquetas de facet_wrap en negrita
  )
ggsave("output/dx_plot.png", dpi = 3000)

#descomposición de arriaga-------

rm(base0,tab_mort)



###calculamos los cambios en la esperanza de vida en para la república mexicana
###y para los años 2000 y 2010

tab_mort_mx %>% 
  filter(year %in% c(2000, 2010), 
         cve_geo == 1,
         age == 0, 
         sex == "males") %>% 
  select(year, Ex) %>% 
  mutate(dif_ex = Ex-lag(Ex)) ##lag es para restas o sumas en la misma col

#descomposición de arriaga----------------------

desc_lt <- full_join(tab_mort %>% 
                       filter( sex == "males", 
                               year == 2000,
                               edo == "Aguascalientes") %>% 
                       select(age, lx, Lx, Tx), ########base1
                     tab_mort %>% 
                       filter( sex == "males", 
                               year == 2010,
                               edo == "Aguascalientes") %>% 
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
  na.omit() %>% 
  ggplot() +
  geom_bar(aes(x = age, y = delta_x, fill = age),
           stat = "identity", show.legend = FALSE) +
  scale_fill_viridis_c(option = "B") +
  scale_y_continuous(n.breaks = 20) +
  scale_x_continuous(n.breaks = 10, limits = c(-1, 110)) +
  labs(
    title = "Descomposición de Arriaga para los cambios en la mortalidad: \n Hombres, Aguascalientes 2000-2010", 
    caption = "Fuente: Elaboración Propia con Información de CONAPO",
    y = "Diferencias a cada edad",
    x = "Edad"
  ) +
  theme_linedraw() +  # Mantén solo uno de los temas
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),  # Título en negrita, centrado y más grande
    axis.title.y = element_text(face = "italic"),                      # Etiqueta del eje y en cursivas
    strip.text = element_text(face = "bold")                           # Etiquetas de facet_wrap en negrita (por si se añade facet_wrap)
  )

ggsave("output/delta_x.png", dpi = 3000)

#porcentual----
arriaga %>% 
  na.omit() %>% 
  ggplot() +
  geom_bar(aes(x = age, y = perc, fill = age),
           stat = "identity", show.legend = FALSE) +
  scale_fill_viridis_c(option = "B") +
  scale_y_continuous(n.breaks = 20) +
  scale_x_continuous(n.breaks = 10, limits = c(-1, 110)) +
  labs(
    title = "Descomposición de Arriaga para porcentaje de los cambios en la mortalidad: \n Hombres, Aguascalientes 2000-2010", 
    caption = "Fuente: Elaboración Propia con Información de CONAPO",
    y = "Porcentaje de Diferencias",
    x = "Edad"
  ) +
  theme_linedraw() +  # Mantén solo uno de los temas
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),  # Título en negrita, centrado y más grande
    axis.title.y = element_text(face = "italic"),                      # Etiqueta del eje y en cursivas
    strip.text = element_text(face = "bold")                           # Etiquetas de facet_wrap en negrita (por si se añade facet_wrap)
  )

ggsave("output/perc_arriaga.png", dpi = 3000)


arriaga %>% 
  summarise(ex1_ex2 = sum(delta_x, na.rm = T))



