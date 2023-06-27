# librerias requeridas

library(tidyverse)
library(UnalData)
library(UnalR)
library(viridis)
library(readxl)
library(writexl)
library(scales)

# Importar Datos
Poblacion <- read_excel("Fuentes/Proy_Demográfica.xlsx", 
                        sheet = "Demografia",
                        guess_max = 60000)


#############
# MAPA 1
#############

# Población Total por Municipios Proyectada 2023
Pob_Mpio_2023 <- Poblacion %>% 
  filter(YEAR == 2023, AREA == "Total")

# Mapa de calor/intervalo población por municipio

Plot.Mapa(
  df       = Pob_Mpio_2023,
  mpio     = COD_MPIO,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  titulo   = "Población Colombia Proyectada por Municipios\nAño 2023",
  naTo0    = FALSE,
  cortes        = c(0, 10000, 50000, 200000, 1000000, Inf), 
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
#############
# MAPA 2
#############

# Consolidado Sí/No Población Cabecera vs Resto

Pob_Rural_2023 <- Poblacion %>% 
  filter(YEAR == 2023, AREA != "Total") %>% 
  pivot_wider(names_from = AREA, values_from = Total) %>% 
  rename(Cabecera = `Cabecera Municipal`, 
         Resto = `Centros Poblados y Rural Disperso`) %>% 
  mutate(Mayor = ifelse(Cabecera < Resto, 1, 0))%>% 
  filter(Mayor == 1)

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

