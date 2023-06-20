##%######################################################%##
#                                                          #
####    ANÁLISIS TESIS DE DOCTORADO TATINA              ####
#                                                          #
##%######################################################%##

# librerias requeridas

library(tidyverse)
library(UnalData)
library(UnalR)
library(viridis)
library(readxl)
library(writexl)
library(scales)

# Parte 1. Análisis Demográfico ----

# Importar base de datos proyecciones poblacionales

Poblacion <- read_excel("Fuentes/Proy_Demográfica.xlsx", 
                            sheet = "Demografia",
                            guess_max = 60000)


# Evolución Población General (Serie de Tiempo)

Poblacion_Total <- Poblacion %>% 
                   summarise(Total = sum(Total), .by = c(YEAR, AREA)) %>% 
                   pivot_wider(names_from = AREA, values_from = Total) %>% 
                   rename(Cabecera = `Cabecera Municipal`, 
                   Resto = `Centros Poblados y Rural Disperso`)


# Serie General

ggplot(data = Poblacion_Total, aes(x = YEAR, y = Total, label = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(40000000,60000000))+
  ggtitle("Evolución Población Proyectada Para Colombia", subtitle = "Periodo 2020-2035\n")+
  ylab("\n Población Proyectada\n ")+
  xlab("Año")+
  annotate(geom="text", x=2020.5, y=49500000, 
           label= format(as.numeric(Poblacion_Total[1, 4]),big.mark=","), color="red")+
  annotate(geom="text", x=2034.5, y=57000000, 
           label=format(as.numeric(Poblacion_Total[nrow(Poblacion_Total), 4]),big.mark=","), color="red")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))


# Serie General * Rural y Cabecera

Poblacion_Grupos <- Poblacion_Total %>% pivot_longer(c(Cabecera, Resto) ,names_to = "Tipo", values_to = "TotalF")

ggplot(data = Poblacion_Grupos, aes(x = YEAR, y = TotalF, color = Tipo)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(10000000,45000000))+
  ggtitle("Evolución Población Proyectada Para Colombia", subtitle = "Periodo 2020-2035\n")+
  ylab("\n Población Proyectada\n ")+
  xlab("Año")+
  labs(colour = "Ubicación")+
  annotate(geom="text", x=2020.7, y=40500000, 
           label= format(as.numeric(Poblacion_Grupos[1, 4]),big.mark=","), color="red")+
  annotate(geom="text", x=2020.7, y=13500000, 
           label=format(as.numeric(Poblacion_Grupos[2, 4]),big.mark=","), color="blue")+
  annotate(geom="text", x=2034.5, y=40500000, 
           label= format(as.numeric(Poblacion_Grupos[nrow(Poblacion_Grupos)-1, 4]),big.mark=","), color="red")+
  annotate(geom="text", x=2034.5, y=12000000, 
           label=format(as.numeric(Poblacion_Grupos[nrow(Poblacion_Grupos), 4]),big.mark=","), color="blue")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))


# Poblacion 2023 por Departamentos

Pob_Dpto_2023 <- Poblacion %>% filter(YEAR == 2023, AREA == "Total") %>% 
                               summarise(Total = sum(Total), .by = c("COD_DEP", "DEP")) %>% 
                               mutate(Total_Mil = Total/1000)

# Mapa de calor población por Departamentos

Plot.Mapa(
  df       = Pob_Dpto_2023,
  depto    = COD_DEP,
  variable = Total_Mil,
  agregado = FALSE,
  tipo     = "Deptos",
  titulo   = "Población Colombia Proyectada por Departamentos",
  cortes   = c(0, 100, 500, 2000, 4000, Inf),
  colores  = c("#FED600", "#02D46E", "#006389", "#FA006E", "red"),
  colBorde = "#3A0F2D",
  estatico = TRUE,
  textSize = 10,
  opacidad = 0.6,
  estilo   = list(
    Style  = "Intervalo", Theme = 5, 
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs   = list(subtitle = "Año 2023\n", caption =  "(*) Por Mil Habitantes")
  )
)


# Poblacion 2023 por Municipios

Pob_Mpio_2023 <- Poblacion %>% 
                 filter(YEAR == 2023, AREA == "Total") %>% 
                 mutate(Total_Mil = Total/1000)

# Mapa de calor población por municipio

Plot.Mapa(
  df       = Pob_Mpio_2023,
  mpio     = COD_MPIO,
  variable = Total_Mil,
  agregado = FALSE,
  tipo     = "Mpios",
  titulo   = "Población Colombia Proyectada por Municipios\nAño 2023",
  naTo0    = FALSE,
  #centroideMapa = c("ANTIOQUIA"),
  cortes        = c(0, 10, 50, 200, 1000, Inf),  
  colores       = c("#ffffcc", "#10F235", "red", "yellow", "blue"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(caption = "(*) Por Mil Habitantes"),
    Text  = list(color = "#011532", size = 0)
  )
)

# Mapa Sí/No Población Cabecera vs Resto

Pob_Resto_2023 <- Poblacion %>% 
  filter(YEAR == 2023, AREA != "Total") %>% 
  pivot_wider(names_from = AREA, values_from = Total) %>% 
  rename(Cabecera = `Cabecera Municipal`, 
         Resto = `Centros Poblados y Rural Disperso`) %>% 
  mutate(Mayor = ifelse(Cabecera < Resto, 1, 0))

# Tabla 

table(Pob_Resto_2023$Mayor)

# Municipios con Cabecera < Resto

Pob_Rural_2023 <- Pob_Resto_2023 %>% filter(Mayor == 1)

# Mapa poblaciones rurales

Plot.Mapa(
  df       = Pob_Rural_2023,
  depto    = COD_DEP,
  mpio     = COD_MPIO,
  tipo     = "SiNoMpios",
 # centroideMapa = c("RISARALDA"),
  titulo   = "Municipios con mayor población en áreas rurales",
  colores  = c("red", "#10F235"),
  opacidad = 1,
  textSize = 0,
  limpio   = FALSE,
  estatico = TRUE,
  estilo   = list(
    Style = "SiNo", Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Labs  = list(subtitle = "Año 2023\n"),
    Legend = list(legend.position = "right", 
                  legend.direction = "horizontal")))

# Parte 2. Transito ----

Transito_Dpto <- read_excel("Fuentes/Transito.xlsx", 
                        sheet = "Departamentos",
                        guess_max = 50) %>% 
                 mutate(across(.cols = starts_with("Y"), .fns = ~round(.x*100, 1)))  


Transito_Mpio <- read_excel("Fuentes/Transito.xlsx", 
                            sheet = "Municipios",
                            guess_max = 50) %>% 
                 mutate(across(.cols = starts_with("Y"), .fns = ~round(.x*100, 1))) %>% 
                 mutate(Y2021_T30 = ifelse(Y2021 <= 30, 1, 0))

# Tabla transito menor al 30%

table(Transito_Mpio$Y2021_T30)

# Mapa de calor por departamentos

Plot.Mapa(
  df       = Transito_Dpto,
  depto     = COD_DEP,
  variable = Y2021,
  agregado = FALSE,
  tipo     = "Deptos",
  titulo   = "Tasa de Transito Inmmediato por Departamentos \nAño 2021",
  naTo0    = FALSE,
  #centroideMapa = c("ANTIOQUIA"),
  cortes        = c(0, 30, 40, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(caption = "(*) Tasa de Transito"),
    Text  = list(color = "#011532", size = 0)
  ))
  
# Mapa de calor por Municipios

Plot.Mapa(
  df       = Transito_Mpio,
  mpio     = COD_MPIO,
  variable = Y2021,
  agregado = FALSE,
  tipo     = "Mpios",
  titulo   = "Tasa de Transito Inmmediata por Municipios \nAño 2021",
  naTo0    = TRUE,
  #centroideMapa = c("TOLIMA"),
  cortes        = c(0, 30, 40, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(caption = "(*) Tasa de Transito"),
    Text  = list(color = "#011532", size = 0)
  ))



