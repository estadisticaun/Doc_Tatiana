##%######################################################%##
#                                                          #
####    ANÁLISIS TESIS DE DOCTORADO                     ####
#                                                          #
##%######################################################%##

# Librerias ----

library(tidyverse)
library(UnalData)
library(UnalR)
library(viridis)
library(readxl)
library(writexl)
library(scales)
library(cowplot)
library(stringr)
library(readr)
library(ggbrace)
library(gt)
library(gtExtras)
library(patchwork)

# Importar poblaciones ----

Poblacion <- read_excel("Fuentes/Proy_Demográfica.xlsx", 
                        sheet = "Demografia",
                        guess_max = 1000)


# Importar Base de Datos zonas rurales con 
# condiciones de difícil acceso a la educación superior 

Mun_DificilES <- read_excel("Fuentes/Municipios Dificiles ES.xlsx", 
                            sheet = "Hoja1",
                            guess_max = 60000)


# Importar bases de datos - Tendencias

Tendencias <- read_excel("Fuentes/Asp_Adm.xlsx", 
                         sheet = "General",
                         guess_max = 1000)

Departamentos <- read_excel("Fuentes/Asp_Adm.xlsx", 
                            sheet = "Departamentos",
                            guess_max = 1000) %>% 
  mutate(Total = `2010` + `2011` + `2012` + `2013` +
           `2014` + `2015` + `2016` + `2017` +
           `2018` + `2019` + `2020` + `2021`) %>% 
  select(c(Poblacion, COD_DEP, Dpto, Total))


p1721 <- read_excel("Fuentes/Asp_Adm.xlsx", 
                    sheet = "Pob17-21",
                    guess_max = 1000) %>% 
  filter(Dpto == "Total Nacional", Ano >= 2010) %>% 
  select(Year = Ano, Total = Pob1721) %>% 
  mutate(Poblacion = rep("Población 17 a 21 Años", 11))

# Poblaciones SNIES 2018-2021

Snies1821 <- read_excel("Fuentes/Asp_Adm.xlsx", 
                    sheet = "General_New",
                    guess_max = 600000)


# Cruce con municipios dificil acceso a la educación superior

Poblacion <- left_join(Poblacion, Mun_DificilES, by = "COD_MPIO") %>% 
             mutate(DificilES = ifelse(is.na(DificilES), "No", DificilES))

# Evolución Población General (Serie de Tiempo)


Poblacion_Total <- Poblacion %>% 
                   summarise(Total = sum(Total), .by = c(YEAR, AREA)) %>% 
                   pivot_wider(names_from = c(AREA), values_from = Total) %>% 
                   rename(Cabecera = `Cabecera Municipal`, 
                   Resto = `Centros Poblados y Rural Disperso`)


# Evolución Población Municipios difíciles ES

Poblacion_DificilES <- Poblacion %>% filter(AREA != "Total") %>% 
  summarise(Total = sum(Total), .by = c(YEAR, DificilES)) %>% 
  pivot_wider(names_from = DificilES, values_from = Total)


# Población General por años

Poblacion_Total <- left_join(Poblacion_Total, Poblacion_DificilES, by = "YEAR")


# Crear Tabla con tres poblaciones

Tres_pob <- Tendencias %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  pivot_wider(names_from = c(Modalidad), values_from = Total) %>% 
  mutate(Total = Oficial + Privado,
         Poblacion = ifelse(Poblacion == "Mpvez", "Primera Matrícula", Poblacion)) 


# Parte 1. Análisis Demográfico ----


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


# Serie General * Municipios dificil educación superior

Poblacion_Dificil <- Poblacion_Total %>% pivot_longer(c(No, Sí) ,names_to = "Tipo", values_to = "TotalF")


ggplot(data = Poblacion_Dificil, aes(x = YEAR, y = TotalF, color = Tipo)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(10000000,45000000))+
  ggtitle("Evolución Población Proyectada Para Municipios con Condiciones \n de Difícil Acceso a la Educación Superior en Colombia\n", subtitle = "Periodo 2020-2035\n")+
  ylab("\n Población Proyectada\n ")+
  xlab("Año")+
  labs(colour = "Ubicación")+
  annotate(geom="text", x=2020.7, y=41000000, 
           label= format(as.numeric(Poblacion_Dificil[1, 6]),big.mark=","), color="red")+
  annotate(geom="text", x=2020.7, y=13500000, 
           label=format(as.numeric(Poblacion_Dificil[2, 6]),big.mark=","), color="blue")+
  annotate(geom="text", x=2034.3, y=41000000, 
           label= format(as.numeric(Poblacion_Dificil[nrow(Poblacion_Dificil)-1, 6]),big.mark=","), color="red")+
  annotate(geom="text", x=2034.3, y=11500000, 
           label=format(as.numeric(Poblacion_Dificil[nrow(Poblacion_Dificil), 6]),big.mark=","), color="blue")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))


# Poblacion 2023 por Departamentos

Capital <- c(5001, 8001, 11001, 13001, 15001, 17001, 18001, 19001,
             20001, 23001, 27001, 41001, 44001, 47001, 50001, 52001,
             54001, 63001, 66001, 68001, 70001, 73001, 76001,81001,
             85001, 86001, 88001, 91001, 94001, 95001, 97001, 99001)

Pob_Dpto_2023 <- Poblacion %>% filter(YEAR == 2023, AREA == "Total") %>% 
                               mutate(Capital = case_when(COD_MPIO %in% Capital ~ "Sí",
                                                          TRUE ~ "No")) %>% 
                               summarise(Total = sum(Total), .by = c("COD_DEP", "DEP", "Capital")) %>% 
                               pivot_wider(names_from = Capital, values_from = Total, values_fill = 0) %>% 
                               mutate(Total = Sí + No,
                                      Total_Mil = Total/1000)

# Estadística de poblaciones en capitales y resto

Pob_Dpto_2023 %>% summarise(Total_Capital = sum(Sí),
                            Total_Resto = sum(No),
                            Total_General = sum(Total))

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
    Labs   = list(fill = "Total de Habitantes", subtitle = "Año 2023\n", caption =  "(*) Por Mil Habitantes")
  )
)


# Poblacion 2023 por Municipios

Pob_Mpio_2023 <- Poblacion %>% 
                 filter(YEAR == 2023, AREA == "Total") %>% 
                 mutate(Total_Mil = Total/1000, 
                        Grupos = case_when(Total <= 10000 ~ "< 10 mil",
                                           between(Total, 10000, 50000) ~ "Entre 10 y 50 mil",
                                           between(Total, 50001, 200000) ~ "Entre 50 y 200 mil",
                                           between(Total, 200000, 1000000) ~ "Entre 200 mil y 1 millón",
                                           TRUE ~ "Más de 1 millón")
                                           )

Pob_Mpio_2023 %>% summarise(Total = n(), .by = c(Grupos))

# Mapa de calor población por municipio

Plot.Mapa(
  df       = Pob_Mpio_2023,
  mpio     = COD_MPIO,
  #variable = Total_Mil,
  variable = Total,
  agregado = FALSE,
  zoomIslas = TRUE,
  tipo     = "Mpios",
  titulo   = "Población Colombia Proyectada por Municipios\nAño 2023",
  naTo0    = FALSE,
  #centroideMapa = c("ANTIOQUIA"),
  #cortes        = c(0, 10, 50, 200, 1000, Inf),  
  cortes        = c(0, 10000, 50000, 200000, 1000000, Inf), 
  colores       = c("#ffffcc", "#10F235", "red", "yellow", "blue"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total de Habitantes"),
    Text  = list(color = "#011532", size = 0)
  )
) -> listMaps

ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.31, y = 0.35, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.36, y = 0.38, width = 0.055)

ggsave("X.png", width = 12, height = 12)

# Consolidado Sí/No Población Cabecera vs Resto

Pob_Resto_2023 <- Poblacion %>% 
  filter(YEAR == 2023, AREA != "Total") %>% 
  pivot_wider(names_from = AREA, values_from = Total) %>% 
  rename(Cabecera = `Cabecera Municipal`, 
         Resto = `Centros Poblados y Rural Disperso`) %>% 
  mutate(Mayor = ifelse(Cabecera < Resto, 1, 0))

# Tabla 

table(Pob_Resto_2023$Mayor)

# Mapa municipios con Cabecera < Resto

Pob_Rural_2023 <- Pob_Resto_2023 %>% filter(Mayor == 1)

# Mapa poblaciones rurales

Plot.Mapa(
  df       = Pob_Rural_2023,
  depto    = COD_DEP,
  mpio     = COD_MPIO,
  tipo     = "SiNoMpios",
  zoomIslas = TRUE,
  SiNoLegend = c("Sí", "No"),
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
    Labs  = list(fill = "¿Municipio rural?", subtitle = "Año 2023\n"),
    Legend = list(legend.position = "bottom", 
                  legend.direction = "vertical")))-> listMaps

ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.31, y = 0.32, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.36, y = 0.35, width = 0.055)



# Mapas municipios con condiciones de difícil acceso a la educación superior

# Mapa Sí/No

Poblacion_Dif_SiNo <- Poblacion %>% filter(DificilES == "Sí", AREA == "Total", YEAR == 2023)

Plot.Mapa(
  df       = Poblacion_Dif_SiNo,
  depto    = COD_DEP,
  mpio     = COD_MPIO,
  tipo     = "SiNoMpios",
  zoomIslas = TRUE,
  # centroideMapa = c("RISARALDA"),
  titulo   = "Municipios con condiciones de difícil acceso \na la educación superior",
  colores  = c("#10F235", "red"),
  opacidad = 1,
  textSize = 0,
  limpio   = FALSE,
 # centroide = "TOLIMA",
  estatico = TRUE,
  SiNoLegend = c("Sí", "No"), 
  estilo   = list(
    Style = "SiNo", Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Labs  = list(fill = "¿Acceso dificil?", subtitle = "Año 2023\n"),
    Legend = list(legend.position = "bottom", 
                  legend.direction = "horizontal")))-> listMaps

ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.31, y = 0.29, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.36, y = 0.32, width = 0.055)

# Mapa calor total de habitantes.

# Parte 2. Transito ----

Transito_Dpto <- read_excel("Fuentes/Transito.xlsx", 
                        sheet = "Departamentos",
                        guess_max = 50) %>% 
                 mutate(across(.cols = starts_with("Y"), .fns = ~round(.x*100, 1))) 
                 
Transito_Mpio <- read_excel("Fuentes/Transito.xlsx", 
                            sheet = "Municipios",
                            guess_max = 50) %>% 
                 mutate(across(.cols = starts_with("Y"), .fns = ~round(.x*100, 1))) %>% 
                 mutate(Y2021_T30 = ifelse(Y2021 <= 30, 1, 0)) %>% 
                 mutate(Capital = case_when(COD_MPIO %in% Capital ~ Y2021,
                             TRUE ~ NA))


Transito_Capital <- Transito_Mpio %>% filter(!is.na(Capital)) %>% 
                    select(-c(Y2021_T30, Capital)) %>% 
                    rename(`2014` = Y2014, `2015` = Y2015, `2016` = Y2016,
                           `2017` = Y2017, `2018` = Y2018, `2019` = Y2019,
                           `2020` = Y2020, `2021` = Y2021) %>% 
                    pivot_longer(cols = c(`2014`:`2021`), names_to = "YEAR", values_to = "Tasa")  %>% 
                    mutate(YEAR = as.numeric(YEAR))
 

ggplot(data = Transito_Capital, aes(x = YEAR, y = Tasa, color = MPO)) +
  geom_point(size = 0)+
  geom_line() + 
  scale_y_continuous(limits = c(0,100)) 
                    
# Evolución Tasa de Transito Inmediato en Colombia

Evol_Transito <- Transito_Dpto %>% filter(DEP == "Colombia") %>%
                 rename(`2014` = Y2014, `2015` = Y2015, `2016` = Y2016,
                        `2017` = Y2017, `2018` = Y2018, `2019` = Y2019,
                        `2020` = Y2020, `2021` = Y2021) %>% 
                 pivot_longer(cols = `2014`:`2021`, names_to = "YEAR", values_to = "Tasa") %>% 
                 mutate(YEAR = as.numeric(YEAR))


ggplot(data = Evol_Transito, aes(x = YEAR, y = Tasa)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(limits = c(0,100))+
  ggtitle("Evolución Tasa de Transito Inmediata en Colombia", subtitle = "Periodo 2014-2021\n")+
  ylab("\n Tasa de Transito\n ")+
  xlab("Año") +
  annotate(geom="text", x=2014, y=42, 
           label= paste(as.numeric(Evol_Transito[1, ncol(Evol_Transito)]), "%"), color="red")+
  annotate(geom="text", x=2021, y=44, 
           label= paste(as.numeric(Evol_Transito[nrow(Evol_Transito), ncol(Evol_Transito)]), "%"), color="red")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Tabla transito menor al 30%

table(Transito_Mpio$Y2021_T30)



# Mapa de calor por departamentos

Plot.Mapa(
  df       = Transito_Dpto,
  depto     = COD_DEP,
  variable = Y2021,
  agregado = FALSE,
  tipo     = "Deptos",
  titulo   = "Tasa de Transito Inmmediato por Departamentos",
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
    Labs  = list(fill = "Tasa de transito (%)", subtitle = "Año 2021\n"),
    Text  = list(color = "#011532", size = 0)
  ))
  
# Mapa de calor por Municipios

Plot.Mapa(
  df       = Transito_Mpio,
  mpio     = COD_MPIO,
  variable = Y2021,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Tasa de Transito Inmmediata por Municipios",
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
    Labs  = list(fill = "Tasa de transito (%)", subtitle = "Año 2021\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps

ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.31, y = 0.29, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.36, y = 0.32, width = 0.055)

# Mapa de calor por capitales

Plot.Mapa(
  df       = Transito_Mpio,
  mpio     = COD_MPIO,
  variable = Capital,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Tasa de Transito Inmmediata en Capitales de \n Departamentos",
  naTo0    = TRUE,
  # centroideMapa = c("TOLIMA"),
  cortes        = c(0, 30, 40, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Tasa de transito (%)", subtitle = "Año 2021\n", caption = "NA: No Aplica"),
    Text  = list(color = "#011532", size = 0)
  ))-> listMaps

ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.33, y = 0.28, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.38, y = 0.31, width = 0.055)

# Parte 3. Inscritos ----

# Base de datos tendencias por poblaciones

Ten_Ins <- Tendencias %>% filter(Poblacion == "Aspirantes")
Ten_Adm <- Tendencias %>% filter(Poblacion == "Admitidos")
Ten_Mpvez <- Tendencias %>% filter(Poblacion == "Mpvez")

# Base de datos poblaciones por departamentos

Dep_Ins <- Departamentos %>% filter(Poblacion == "Aspirantes")
Dep_Adm <- Departamentos %>% filter(Poblacion == "Admitidos")
Dep_Mpvez <- Departamentos %>% filter(Poblacion == "Mpvez")


# Inicio Gráficos de Tendencias

# Serie Inscritos

Ten_Ins1 <- Ten_Ins %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  pivot_wider(names_from = c(Modalidad), values_from = Total) %>% 
  mutate(Total = Oficial + Privado) 

ggplot(data = Ten_Ins1, aes(x = Year, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,2500000))+
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Inscritos en Educación Superior en Colombia", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño") +
  annotate(geom="text", x=2010.5, y=500000, 
           label= format(as.numeric(Ten_Ins1[1, 6]),big.mark=","), color="red") +
  annotate(geom="text", x=2020.9, y=2250000, 
           label= format(as.numeric(Ten_Ins1[12, 6]),big.mark=","), color="red") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Por Sector

Ten_Ins %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,1700000))+
  scale_color_discrete(name = "Sector")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Inscritos en Colombia por Sector", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Por Sexo

Ten_Ins %>% filter(Variable == "Sexo") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,1500000))+
  scale_color_discrete(name = "Sexo")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Inscritos en Colombia por Sexo", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))


# Por Nivel

Ten_Ins %>% filter(Variable == "Nivel") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,1100000))+
  scale_color_discrete(name = "Nivel")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Inscritos en Colombia por Nivel de Formación", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom")

# Por Área

Ten_Ins %>% filter(Variable == "Area") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,1000000))+
  scale_color_discrete(name = "Área")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Inscritos en Colombia por Áreas del Conocimiento", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Por Departamentos

Plot.Mapa(
  df       = Dep_Ins,
  depto    = COD_DEP,
  variable = Total,
  agregado = FALSE,
  tipo     = "Deptos",
  titulo   = "Total de Inscritos en Colombia por Departamentos",
  cortes   = c(0, 10000, 100000, 500000, 1000000, Inf),
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
    Labs   = list(fill = "Total Aspirantes", subtitle = "Años 2010 a 2021\n")
  )
)

# Parte 4. Admitidos ----

# Inicio Gráficos de Tendencias

# Serie Admitidos

Ten_Adm1 <- Ten_Adm %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  pivot_wider(names_from = c(Modalidad), values_from = Total) %>% 
  mutate(Total = Oficial + Privado) 

  ggplot(data = Ten_Adm1, aes(x = Year, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,1500000))+
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Admitidos en Educación Superior en Colombia", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño") +
    annotate(geom="text", x=2010.1, y=270000, 
             label= format(as.numeric(Ten_Adm1[1, 6]),big.mark=","), color="red") +
    annotate(geom="text", x=2020.9, y=1250000, 
             label= format(as.numeric(Ten_Adm1[12, 6]),big.mark=","), color="red") +  
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Por Sector

Ten_Adm %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,750000))+
  scale_color_discrete(name = "Sector")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Admitidos en Colombia por Sector", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Por Sexo

Ten_Adm %>% filter(Variable == "Sexo") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,750000))+
  scale_color_discrete(name = "Sexo")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Admitidos en Colombia por Sexo", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))


# Por Nivel

Ten_Adm %>% filter(Variable == "Nivel") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,650000))+
  scale_color_discrete(name = "Nivel")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Admitidos en Colombia por Nivel de Fomración", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom")

# Por Área

Ten_Adm %>% filter(Variable == "Area") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,500000))+
  scale_color_discrete(name = "Área")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Admitidos en Colombia por Áreas del Conocimiento", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Por Departamentos

Plot.Mapa(
  df       = Dep_Adm,
  depto    = COD_DEP,
  variable = Total,
  agregado = FALSE,
  tipo     = "Deptos",
  titulo   = "Total de Admitidos en Colombia por Departamentos",
  cortes   = c(0, 5000, 50000, 200000, 300000, Inf),
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
    Labs   = list(fill = "Total Admitidos", subtitle = "Años 2010 a 2021\n")
  )
)

# Parte 5. Mpvz ----

# Inicio Gráficos de Tendencias

# Serie Primera Matricula

Ten_Mpvez1 <- Ten_Mpvez %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  pivot_wider(names_from = c(Modalidad), values_from = Total) %>% 
  mutate(Total = Oficial + Privado) 

  ggplot(data = Ten_Mpvez1, aes(x = Year, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,1100000))+
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Matriculados en Primer Curso en Colombia", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total Matriculados\n ")+
  xlab("\nAño") +
  annotate(geom="text", x=2010.1, y=200000, 
           label= format(as.numeric(Ten_Mpvez1[1, 6]),big.mark=","), color="red") +
  annotate(geom="text", x=2020.9, y=980000, 
           label= format(as.numeric(Ten_Mpvez1[12, 6]),big.mark=","), color="red") +  
    theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Por Sector

Ten_Mpvez %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,750000))+
  scale_color_discrete(name = "Sector")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Matriculados en Primer Curso en Colombia por Sector", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Matriculados en Primer Curso\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Por Sexo

Ten_Mpvez %>% filter(Variable == "Sexo") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,600000))+
  scale_color_discrete(name = "Sexo")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Matriculados en Primer Curso en Colombia por Sexo", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Matriculados en Primer Curso\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))


# Por Nivel

Ten_Mpvez %>% filter(Variable == "Nivel") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,550000))+
  scale_color_discrete(name = "Nivel")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Matriculados en Primer Curso en Colombia \npor Nivel de Formación", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Matriculados en Primer Curso\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom")

# Por Área

Ten_Mpvez %>% filter(Variable == "Area") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  ggplot(aes(x = Year, y = Total, color = Modalidad)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,400000))+
  scale_color_discrete(name = "Área")+
  # labs(colour = "Sector")+
  ggtitle("Evolución Total de Matriculados en Primer Curso en Colombia \npor Áreas del Conocimiento", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Matriculados en Primer Curso\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Por Departamentos

Plot.Mapa(
  df       = Dep_Mpvez,
  depto    = COD_DEP,
  variable = Total,
  agregado = FALSE,
  tipo     = "Deptos",
  titulo   = "Total de Matriculados en Primer Curso \nen Colombia por Departamentos",
  cortes   = c(0, 5000, 50000, 200000, 300000, Inf),
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
    Labs   = list(fill = "Total Matriculados en Primer Curso", subtitle = "Años 2010 a 2021\n")
  )
)

# Parte 6. Mpvz - Universitaria ----

Mpvez_Univer <- Tendencias %>% filter(Poblacion == "Mpvez",
                                      Modalidad == "Universitario") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year),
         Poblacion = ifelse(Poblacion == "Mpvez", "Primera Matrícula Universitaria", Poblacion)) %>% 
  select(Poblacion, Year, Total)

ggplot(data = Mpvez_Univer, aes(x = Year, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,600000))+
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Matriculados en Primer Curso de Universidad en Colombia", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total Matriculados \n ")+
  xlab("\nAño") +
  annotate(geom="text", x=2010, y=130000, 
           label= format(as.numeric(Mpvez_Univer[1, 3]),big.mark=","), color="red") +
  annotate(geom="text", x=2020.9, y=470000, 
           label= format(as.numeric(Mpvez_Univer[12, 3]),big.mark=","), color="red") +  
  
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))


# Parte 7. Asp-Adm-Mpvz ----

# Cruzar Poblaciones

Tres_pob <- bind_rows(Tres_pob, Mpvez_Univer)

# Gráfico de tres poblaciones

Tres_pob %>% 
  ggplot(aes(x = Year, y = Total, color = Poblacion)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  labs(title = "Evolución Poblaciones en Educación Superior en Colombia",
       subtitle = "Periodo 2010-2021\n",
       color = "Población",
       x = "\nAño",
       y = "\n Total \n")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom",
        legend.title = element_text("Población"))

# Tasa de Absorción en Educación Superior

Tasa_absorcion <- Tres_pob %>% select(Poblacion, Year, Total) %>% 
             filter(Poblacion %in% c('Aspirantes', "Admitidos", "Primera Matrícula", "Primera Matrícula Universitaria")) %>% 
             pivot_wider(names_from = Poblacion, values_from = Total) %>% 
             mutate(Admitidos  = Admitidos / Aspirantes,
                    `Primera Matrícula` = `Primera Matrícula` / Aspirantes ,
                    `Primera Matrícula Universitaria` = `Primera Matrícula Universitaria` / Aspirantes ) %>% 
             select(-Aspirantes) %>%
             pivot_longer(c(Admitidos, `Primera Matrícula`, `Primera Matrícula Universitaria`),
                          names_to = "Poblacion",
                          values_to = "Total")

ggplot(data = Tasa_absorcion, aes(x = Year, y = Total, color = Poblacion)) +
       geom_point(size = 3)+
       geom_line()+ 
       labs(title = "Evolución Tasa de Absorción en Educación Superior en Colombia",
       subtitle = "Periodo 2010-2021\n",
       color = "Población",
       x = "\nAño",
       y = "\n Tasa de Absorción \n")+
       scale_y_continuous(limits = c(0, 1),
                          labels = scales::percent)+
       scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  annotate(geom="text", x=2010, y=0.61, 
           label= scales::percent(as.numeric(Tasa_absorcion[1, 3])), 
           color="red") +
  annotate(geom="text", x=2010, y=0.37, 
           label= scales::percent(as.numeric(Tasa_absorcion[2, 3])), 
           color="#2d572c") +
  annotate(geom="text", x=2010, y=0.20, 
           label= scales::percent(as.numeric(Tasa_absorcion[3, 3])), 
           color="blue") +
  annotate(geom="text", x=2021, y=0.61, 
           label= scales::percent(as.numeric(Tasa_absorcion[34, 3])), 
           color="red") +
  annotate(geom="text", x=2021, y=0.37, 
           label= scales::percent(as.numeric(Tasa_absorcion[35, 3])), 
           color="#2d572c") +
  annotate(geom="text", x=2021, y=0.15, 
           label= scales::percent(as.numeric(Tasa_absorcion[36, 3])), 
           color="blue") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom",
        legend.title = element_text("Población"))

# Parte 8. Universidad 18-21 ----

# Base de datos de población universitaria

Universidad <- Snies1821 %>% filter(Formacion %in% c("Universitaria", "UNIVERSITARIA"))

Aspirantes <- Universidad %>% filter(Poblacion == "Inscritos")
Admitidos <- Universidad %>% filter(Poblacion == "Admitidos")
Mpvez <- Universidad %>% filter(Poblacion == "Mpvez")

# Aspirantes ----

# Serie de tiempo general

Ten_Ins <- Aspirantes %>% summarise(Total = sum(Total), .by = c(Ano)) 

ggplot(data= Ten_Ins, aes(x = Ano, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,1200000))+
  # scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Inscritos Formación Universitaria en Colombia", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño")+
  annotate(geom="text", x=2018.1, y=1100000, 
           label= format(as.numeric(Ten_Ins[1, 2]),big.mark=","), color="red") +
  annotate(geom="text", x=2020.95, y=1100000, 
           label= format(as.numeric(Ten_Ins[4, 2]),big.mark=","), color="red") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))
  
# Sector

Aspirantes %>% 
  rename(Sector = `Sector IES`) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sector)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Sector)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,650000))+
  scale_color_discrete(name = "Sector")+
  ggtitle("Evolución Inscritos en Formación Universitaria en Colombia por Sector", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Sexo

Aspirantes %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sexo)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Sexo)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,650000))+
  scale_color_discrete(name = "Sexo")+
  ggtitle("Evolución Inscritos en Formación Universitaria en Colombia por Sexo", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Metodología

Aspirantes %>% 
  summarise(Total = sum(Total), .by = c(Ano, Metodologia)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Metodologia)) +
  geom_point(size = 3)+
  geom_line() +
  scale_y_continuous(labels = comma, limits = c(0,1000000))+
  scale_color_discrete(name = "Metodología")+
  ggtitle("Evolución Inscritos en Formación Universitaria en Colombia por Metodología", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Admitidos ----

# Serie de tiempo general

Ten_Adm <- Admitidos %>% summarise(Total = sum(Total), .by = c(Ano)) 

ggplot(data= Ten_Adm, aes(x = Ano, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,650000))+
  # scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Admitidos a Formación Universitaria en Colombia", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño")+
  annotate(geom="text", x=2018.1, y=590000, 
           label= format(as.numeric(Ten_Adm[1, 2]),big.mark=","), color="red") +
  annotate(geom="text", x=2020.95, y=630000, 
           label= format(as.numeric(Ten_Adm[4, 2]),big.mark=","), color="red") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Sector

Admitidos %>% 
  rename(Sector = `Sector IES`) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sector)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Sector)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,500000))+
  scale_color_discrete(name = "Sector")+
  ggtitle("Evolución Admitidos en Formación Universitaria en Colombia por Sector", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Sexo

Admitidos %>% 
  mutate(Sexo1 = case_when(Sexo == "Hombre"~ "Hombre",
                           Sexo == "Mujer"~ "Mujer",
                           Sexo == "HOMBRE"~ "Hombre",
                           Sexo == "MUJER"~ "Mujer",
                           Sexo == "Sin Información" ~ "Sin Información")) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sexo1)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Sexo1)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,450000))+
  scale_color_discrete(name = "Sexo")+
  ggtitle("Evolución Admitidos en Formación Universitaria en Colombia por Sexo", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Metodología

Admitidos %>% 
  mutate(Metodologia1 = case_when(Metodologia == "Presencial" ~ "Presencial",
                                  Metodologia == "PRESENCIAL" ~ "Presencial",
                                  Metodologia == "Presencial-Dual" ~ "Presencial-Dual",
                                  Metodologia == "Presencial-Virtual" ~ "Presencial-Virtual",
                                  Metodologia == "Distancia (tradicional)" ~ "Distancia (tradicional)",
                                  Metodologia == "DISTANCIA (TRADICIONAL)" ~ "Distancia (tradicional)",
                                  Metodologia == "Distancia (virtual)" ~ "Distancia (virtual)",
                                  Metodologia == "DISTANCIA (VIRTUAL)" ~ "Distancia (virtual)")) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Metodologia1)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Metodologia1)) +
  geom_point(size = 3)+
  geom_line() +
  scale_y_continuous(labels = comma, limits = c(0,600000))+
  scale_color_discrete(name = "Metodología")+
  ggtitle("Evolución Admitidos en Formación Universitaria en Colombia por Metodología", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Mpvez ----

# Serie de tiempo general

Ten_Mpvez <- Mpvez %>% summarise(Total = sum(Total), .by = c(Ano)) 

ggplot(data= Ten_Mpvez, aes(x = Ano, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,650000))+
  # scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Matriculados Primera Vez en Formación\nUniversitaria en Colombia", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total Matriculados\n ")+
  xlab("\nAño")+
  annotate(geom="text", x=2018.1, y=470000, 
           label= format(as.numeric(Ten_Mpvez[1, 2]),big.mark=","), color="red") +
  annotate(geom="text", x=2020.95, y=480000, 
           label= format(as.numeric(Ten_Mpvez[4, 2]),big.mark=","), color="red") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Sector

Mpvez %>% 
  rename(Sector = `Sector IES`) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sector)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Sector)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,500000))+
  scale_color_discrete(name = "Sector")+
  ggtitle("Evolución Matriculados Primera Vez en Formación\nUniversitaria en Colombia por Sector", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total Matriculados\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Sexo

Mpvez %>% 
  mutate(Sexo1 = case_when(ID_Sexo == 1 ~ "Hombre",
                           ID_Sexo == 2 ~ "Mujer",
                           ID_Sexo == 0 ~ "Sin Información")) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Sexo1)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Sexo1)) +
  geom_point(size = 3)+
  geom_line()+
  scale_y_continuous(labels = comma, limits = c(0,450000))+
  scale_color_discrete(name = "Sexo")+
  ggtitle("Evolución Matriculados Primera Vez en Formación\nUniversitaria en Colombia por Sexo", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total Matriculados\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Metodología

Mpvez %>% 
  mutate(Metodologia1 = case_when(Metodologia == "Presencial" ~ "Presencial",
                                  Metodologia == "PRESENCIAL" ~ "Presencial",
                                  Metodologia == "Presencial-Dual" ~ "Presencial-Dual",
                                  Metodologia == "Presencial-Virtual" ~ "Presencial-Virtual",
                                  Metodologia == "Distancia (tradicional)" ~ "Distancia (tradicional)",
                                  Metodologia == "DISTANCIA (TRADICIONAL)" ~ "Distancia (tradicional)",
                                  Metodologia == "Distancia (virtual)" ~ "Distancia (virtual)",
                                  Metodologia == "DISTANCIA (VIRTUAL)" ~ "Distancia (virtual)")) %>% 
  summarise(Total = sum(Total), .by = c(Ano, Metodologia1)) %>% 
  ggplot(aes(x = Ano, y = Total, color = Metodologia1)) +
  geom_point(size = 3)+
  geom_line() +
  scale_y_continuous(labels = comma, limits = c(0,450000))+
  scale_color_discrete(name = "Metodología")+
  ggtitle("Evolución Matriculados Primera Vez en Formación\nUniversitaria en Colombia por Metodología", subtitle = "Periodo 2018-2021\n")+
  ylab("\n Total Matriculados\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Global ----


Universidad1 <- Universidad %>% summarise(Total = sum(Total), .by = c(Poblacion, Ano))
                ggplot(data = Universidad1, aes(x = Ano, y = Total, color = Poblacion))+
                geom_line()+
                geom_point()+
  labs(title = "Evolución Total Inscripciones, Admisiones y Matriculas\nPrimera Vez Formación Universitaria en Colombia",
       subtitle = "Periodo 2018-2021\n",
       color = "Población",
       x = "\nAño",
       y = "\n Total de Individuos \n")+
       scale_y_continuous(labels = comma, limits = c(0,1200000))+
       scale_color_discrete(breaks = c("Inscritos", "Admitidos", "Mpvez"),
                           labels = c("Inscritos", "Admitidos", "Matriculados Primera Vez"))+
    annotate(geom="text", x=2018.1, y=1050000, 
           label= format(as.numeric(Universidad1[1, 3]),big.mark=","), color="red") +
    annotate(geom="text", x=2020.9, y=1050000, 
           label= format(as.numeric(Universidad1[4, 3]),big.mark=","), color="red") +
    annotate(geom="text", x=2018.1, y=600000, 
                           label= format(as.numeric(Universidad1[5, 3]),big.mark=","), color="red") +
    annotate(geom="text", x=2020.9, y=650000, 
                           label= format(as.numeric(Universidad1[8, 3]),big.mark=","), color="red") +
    annotate(geom="text", x=2018.1, y=350000, 
                           label= format(as.numeric(Universidad1[9, 3]),big.mark=","), color="red") +
    annotate(geom="text", x=2020.9, y=350000, 
                           label= format(as.numeric(Universidad1[12, 3]),big.mark=","), color="red") +                
        theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom",
        legend.title = element_text("Población", face="bold"),
        legend.text = element_text(size = 12))

# Contexto Público vs Privado

Universidad2 <- Universidad %>% rename(Sector = `Sector IES`) %>% 
  summarise(Total = sum(Total), .by = c(Sector, Poblacion, Ano)) %>% 
  mutate(Sector1 = case_when(Sector == "Oficial" ~ "Oficial",
                             Sector == "OFICIAL" ~ "Oficial",
                             Sector == "Privada" ~ "Privada",
                             Sector == "PRIVADA" ~ "Privada"))
         
# Textos de las facetas

Text_Uni2 <- Universidad2 %>% filter(Ano %in% c(2018, 2021)) %>% 
             mutate(Year = ifelse(Ano == 2018, 2017.5, 2021.5))

# Gráfico

Uni2 <- ggplot(data = Universidad2, aes(x = Ano, y = Total, color = Poblacion))+
  geom_line()+
  geom_point()+
  labs(title = "Evolución Total Inscripciones, Admisiones y Matriculas Primera Vez Formación\nUniversitaria en Colombia por Sector de la Educación",
       subtitle = "Periodo 2018-2021\n",
       color = "Población",
       x = "\nAño",
       y = "\n Total de Individuos \n")+
  xlim(2017, 2022)+
  # scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022))+
  scale_y_continuous(labels = comma, limits = c(0,750000))+
  scale_color_discrete(breaks = c("Inscritos", "Admitidos", "Mpvez"),
                       labels = c("Inscritos", "Admitidos", "Matriculados Primera Vez"))+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom",
        legend.title = element_text("Población", size = 12, face="bold"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  facet_wrap(vars(Sector1))


Uni2 + geom_text(data = Text_Uni2,
                 aes(x = Year, y = Total,
                     label = format(Total, big.mark=",")))
              
# Parte 9. Análisis IES ----

# Crear bases de datos

IES <- Snies1821 %>% filter(Poblacion == "Mpvez", Formacion %in% c("Universitaria", "UNIVERSITARIA")) %>% 
                     select(COD_INTS:Municipio_IES, Total) %>% 
                     mutate(Caracter = case_when(Carácter == "Universidad" ~ "Universidad",
                                                 Carácter == "Institución Tecnológica" ~ "Institución Tecnológica",
                                                 Carácter == "Institución Universitaria/Escuela Tecnológica" ~ "Institución Universitaria/Escuela Tecnológica",
                                                 Carácter == "Institución Técnica Profesional" ~ "Institución Técnica Profesional",
                                                 Carácter == "UNIVERSIDAD" ~ "Universidad",
                                                 Carácter == "INSTITUCIÓN TECNOLÓGICA" ~ "Institución Tecnológica",
                                                 Carácter == "INSTITUCIÓN TÉCNICA PROFESIONAL" ~ "Institución Técnica Profesional",
                                                 Carácter == "INSTITUCIÓN UNIVERSITARIA/ESCUELA TECNOLÓGICA" ~ "Institución Universitaria/Escuela Tecnológica"
                                                 ),
                            `Sector IES` = ifelse(`Sector IES` == "OFICIAL", "Oficial", "Privada")) %>% 
                     summarise(Institucion = max( Institucion),
                               COD_INTS = max(COD_INTS),
                               Principal = min(Principal),
                               ID_Sector_IES = max(ID_Sector_IES),
                               `Sector IES` = max(`Sector IES`),
                               Acredita = max(Acredita, na.rm = TRUE),
                               ID_Caracter = max(ID_Caracter),
                               Caracter = max(Caracter),                              
                               Total = sum(Total),
                               .by = c(IES_PADRE))


IES_Seccional <- Snies1821 %>% filter(Poblacion == "Mpvez", Formacion %in% c("Universitaria", "UNIVERSITARIA")) %>% 
  select(COD_INTS:Municipio_IES, Total) %>%
  summarise(Institucion = max( Institucion),
            COD_DEP_IES = max(COD_DEP_IES),
            DPTO_IES = max(DPTO_IES),
            COD_MPIO_IES = max(COD_MPIO_IES),
            Municipio_IES = max(Municipio_IES),
            .by = c(IES_PADRE, COD_INTS))

# Tablas

# IES por Sector

IES_Sector <- IES %>% summarise(Total = n(), .by = c(`Sector IES`))
IES_Sector

IES_Sector_Mat <- IES %>% summarise(Total = sum(Total), .by = c(`Sector IES`))
IES_Sector_Mat

IES_Acredita <- IES %>% summarise(Total = n(), .by = c(Acredita))
IES_Acredita

IES_Acredita_Mat <- IES %>% summarise(Total = sum(Total), .by = c(Acredita))
IES_Acredita_Mat

IES_Caracter <- IES %>% summarise(Total = n(), .by = c(Caracter))
IES_Caracter

IES_Caracter_Mat <- IES %>% summarise(Total = sum(Total), .by = c(Caracter))
IES_Caracter_Mat

IES_Caracter_Sector <- IES %>% summarise(Total = n(), .by = c(`Sector IES`, Caracter))
IES_Caracter_Sector




# Gráficos

# Mapas IES con seccionales

IES_Seccional_Map <- IES_Seccional %>% 
  summarise(Total = n(), .by = c(COD_DEP_IES)) %>% 
  add_row(COD_DEP_IES  = 81, Total=0) %>% 
  add_row(COD_DEP_IES  = 99, Total=0) %>%
  add_row(COD_DEP_IES  = 94, Total=0) %>%
  add_row(COD_DEP_IES  = 95, Total=0) %>%
  add_row(COD_DEP_IES  = 97, Total=0) %>%
  add_row(COD_DEP_IES  = 91, Total=0) %>%
  add_row(COD_DEP_IES  = 88, Total=0) 
  
  
Plot.Mapa(
  df       = IES_Seccional_Map,
  depto    = COD_DEP_IES,
  variable = Total,
  agregado = FALSE,
  tipo     = "Deptos",
  naTo0     = TRUE,
  zoomIslas = TRUE,
  titulo   = "Total de Instituciones de Educación Superior\n(IES) en Colombia por Departamentos\n",
  cortes   = c(-1, 0, 1, 10, 50, Inf),
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
    Labs   = list(fill = "Total IES")
  )
)-> listMaps

ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres, x = 0.32, y = 0.735, width = 0.4)

# Caracter de las IES

Car_IES <- IES %>% summarise(Total = n(), .by = c(Caracter)) %>% 
  rename(Clase = Caracter) %>%
  mutate(Variable = "CARACTER",
         Clase = factor(Clase)) 
Gra1 <- Plot.Barras(
  datos     = Car_IES,
  categoria = "CARACTER",
  estatico = TRUE,
  vertical = FALSE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  titulo     = "Distribución Porcentual Total IES por tipología de las instituciones",
  labelEje   = "Porcentaje",
  colores   = RColorBrewer::brewer.pal(4, "Set1"),
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.5, color = "#000000")))

Gra1 + scale_y_continuous(limits = c(NA, 100))+
  theme(plot.title=element_text(hjust=0, size=12),
        axis.title=element_text(size=14),
        plot.caption=element_text(size=7),
        legend.text=element_text(size=9),
        axis.text = element_text(size=10))

# Caracter de las IES - Matriculados

Car_IES_Mat <- IES %>% summarise(Total = sum(Total), .by = c(Caracter)) %>% 
  rename(Clase = Caracter) %>%
  mutate(Variable = "CARACTER",
         Clase = factor(Clase)) 
Gra1 <- Plot.Barras(
  datos     = Car_IES_Mat,
  categoria = "CARACTER",
  estatico = TRUE,
  vertical = FALSE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  titulo     = "Distribución Total Matriculados Primera Vez en las IES\npor Tipología de las Instituciones",
  labelEje   = "Porcentaje",
  colores   = RColorBrewer::brewer.pal(4, "Set1"),
  estilo    = list(gg.Tema  = 5,
                   gg.Bar = list(width = 0.5, color = "#000000")))

Gra1 + scale_y_continuous(limits = c(NA, 100))+
  theme(plot.title=element_text(hjust=0, size=12),
        axis.title=element_text(size=14),
        plot.caption=element_text(size=7),
        legend.text=element_text(size=9),
        axis.text = element_text(size=10))

names(IES)

# Parte 10. Análisis Programas ----

PROGRAMAS <- Snies1821 %>% filter(Poblacion == "Mpvez", Formacion %in% c("Universitaria", "UNIVERSITARIA")) %>%
              select(Institucion, SNIES_PROGRA:Prog_Acredita, ID_Metodologia:NBC, COD_DEP_PRO:MUN_PRO, Total) %>% 
              filter(!is.na(SNIES_PROGRA)) %>% 
              summarise(Programa = max(Programa),
                        Institucion = max(Institucion),
                        Prog_Acredita = max(Prog_Acredita, na.rm = TRUE),
                        ID_Metodologia = max(ID_Metodologia),
                        Metodologia = max(Metodologia),
                        ID_Area_Con = max(ID_Area_Con),
                        Area_Con = max(Area_Con),
                        Id_Nucleo = max(Id_Nucleo),
                        NBC = max(NBC),
                        COD_DEP_PRO = max(COD_DEP_PRO),
                        DEP_Programa = max(DEP_Programa),
                        COD_MUN_PRO = max(COD_MUN_PRO),
                        MUN_PRO = max(MUN_PRO),
                        Total = sum(Total),
                        .by = c(SNIES_PROGRA)) %>% 
             mutate(Prog_Acredita = case_when(Prog_Acredita == "SI" ~ "Sí",
                                              Prog_Acredita == "NO" ~ "No",
                                              is.na(Prog_Acredita) == TRUE ~ "Sin Información"),
                    Metodologia = case_when(Metodologia == "Presencial" ~ "Presencial",
                                             Metodologia == "PRESENCIAL" ~ "Presencial",
                                             Metodologia == "Presencial-Dual" ~ "Presencial-Dual",
                                             Metodologia == "Presencial-Virtual" ~ "Presencial-Virtual",
                                             Metodologia == "Distancia (tradicional)" ~ "Distancia (tradicional)",
                                             Metodologia == "DISTANCIA (TRADICIONAL)" ~ "Distancia (tradicional)",
                                             Metodologia == "Distancia (virtual)" ~ "Distancia (virtual)",
                                             Metodologia == "DISTANCIA (VIRTUAL)" ~ "Distancia (virtual)"),
                    Area_Con = case_when(Area_Con == "INGENIERÍA, ARQUITECTURA, URBANISMO Y AFINES" ~ "Ingeniería, arquitectura, urbanismo y afines",
                                         Area_Con == "AGRONOMÍA, VETERINARIA Y AFINES" ~ "Agronomía, veterinaria y afines",              
                                         Area_Con == "BELLAS ARTES" ~ "Bellas artes",                                
                                         Area_Con == "CIENCIAS DE LA SALUD" ~ "Ciencias de la salud",                         
                                         Area_Con == "CIENCIAS SOCIALES Y HUMANAS" ~ "Ciencias sociales y humanas",                  
                                         Area_Con == "ECONOMÍA, ADMINISTRACIÓN, CONTADURÍA Y AFINES" ~ "Economía, administración, contaduría y afines",
                                         Area_Con == "MATEMÁTICAS Y CIENCIAS NATURALES" ~ "Matemáticas y ciencias naturales",
                                         Area_Con == "CIENCIAS DE LA EDUCACIÓN" ~ "Ciencias de la educación",      
                                         Area_Con == "Matemáticas y ciencias naturales" ~ "Matemáticas y ciencias naturales",   
                                         Area_Con == "Ciencias de la educación" ~ "Ciencias de la educación",                    
                                         Area_Con == "Bellas artes" ~ "Bellas artes",                                 
                                         Area_Con == "Economía, administración, contaduría y afines" ~ "Economía, administración, contaduría y afines",
                                         Area_Con == "Ingeniería, arquitectura, urbanismo y afines" ~ "Ingeniería, arquitectura, urbanismo y afines",
                                         Area_Con == "Ciencias sociales y humanas" ~ "Ciencias sociales y humanas",                 
                                         Area_Con == "Agronomía, veterinaria y afines" ~ "Agronomía, veterinaria y afines",             
                                         Area_Con == "Ciencias de la salud" ~ "Ciencias de la salud",                        
                                         Area_Con == "Sin clasificar" ~ "Sin clasificar",                              
                                         Area_Con == "No Aplica" ~ "Sin clasificar"),
                    NBC = str_to_sentence(NBC))


# Tabla Total Matriculados Pvez 18-21

sum(PROGRAMAS$Total)

# Tabla programas acreditados 

Progra_Acredita <- PROGRAMAS %>% summarise(Total = n(), .by = c(Prog_Acredita))
Progra_Acredita

Progra_Acredita_Mat <- PROGRAMAS %>% summarise(Total = sum(Total), .by = c(Prog_Acredita))
Progra_Acredita_Mat

# Gráficos

# Metodología

Met_Pro <- PROGRAMAS %>% summarise(Total = n(), .by = c(Metodologia)) %>% 
              rename(Clase = Metodologia) %>%
              mutate(Variable = "METODOLOGIA",
                     Clase = factor(Clase)) 
Gra1 <- Plot.Barras(
  datos     = Met_Pro,
  categoria = "METODOLOGIA",
  estatico = TRUE,
  #vertical = FALSE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  titulo     = "Distribución Total Programas Universitarios por Metodología de Formación",
  labelEje   = "Porcentaje",
  colores   = RColorBrewer::brewer.pal(5, "Set1"),
  estilo    = list(gg.Tema  = 5,
                   gg.Texto = list(subtitle = "Periodo 2018-2021")))

Gra1 + scale_y_continuous(limits = c(NA, 100))+
  theme(plot.title=element_text(hjust=0, size=12),
        axis.title=element_text(size=14),
        plot.caption=element_text(size=7),
        legend.text=element_text(size=9),
        axis.text = element_text(size=10))


# Metodología - Matriculados

Met_Pro_Mat <- PROGRAMAS %>% summarise(Total = sum(Total), .by = c(Metodologia)) %>% 
  rename(Clase = Metodologia) %>%
  mutate(Variable = "METODOLOGIA",
         Clase = factor(Clase)) 
Gra1 <- Plot.Barras(
  datos     = Met_Pro_Mat,
  categoria = "METODOLOGIA",
  estatico = TRUE,
  #vertical = FALSE,
  freqRelativa = TRUE,
  ordinal   = FALSE,
  titulo     = "Distribución Matriculados Primera Vez por Metodología de Formación",
  labelEje   = "Porcentaje",
  colores   = RColorBrewer::brewer.pal(5, "Set1"),
  estilo    = list(gg.Tema  = 5,
                   gg.Texto = list(subtitle = "Periodo 2018-2021")))

Gra1 + scale_y_continuous(limits = c(NA, 100))+
  theme(plot.title=element_text(hjust=0, size=12),
        axis.title=element_text(size=14),
        plot.caption=element_text(size=7),
        legend.text=element_text(size=9),
        axis.text = element_text(size=10))

# Áreas del Conocimiento

Area_Prog <- PROGRAMAS %>% summarise(Total = n(), .by = c(Area_Con)) %>% 
  rename(Clase = Area_Con) %>%
  mutate(Variable = "AREA")

Gra2 <- Plot.Barras(
  datos     = Area_Prog,
  categoria = "AREA",
  freqRelativa = TRUE,
  estatico = TRUE,
  vertical = FALSE,
  ordinal   = FALSE,
  titulo     = "Distribución de Programas Académicos Universitarios por Áreas del Conocimiento",
  labelEje   = "Porcentaje",
  colores   = RColorBrewer::brewer.pal(9, "Set1"),
  estilo    = list(gg.Tema  = 5,
                   gg.Texto = list(subtitle = "Periodo 2018-2021")))        

Gra2 + 
  scale_y_continuous(limits = c(NA, 50))+
  theme(plot.title=element_text(hjust=0, size=12),
        axis.title=element_text(size=14),
        plot.caption=element_text(size=7),
        legend.text=element_text(size=9),
        axis.text = element_text(size=10))


# Áreas del Conocimiento Matriculados

Area_Prog_Mat <- PROGRAMAS %>% summarise(Total = sum(Total), .by = c(Area_Con)) %>% 
  rename(Clase = Area_Con) %>%
  mutate(Variable = "AREA")

Gra2 <- Plot.Barras(
  datos     = Area_Prog_Mat,
  categoria = "AREA",
  freqRelativa = TRUE,
  estatico = TRUE,
  vertical = FALSE,
  ordinal   = FALSE,
  titulo     = "Distribución Matriculados Primera Vez Programas Universitarios\npor Áreas del Conocimiento",
  labelEje   = "Porcentaje",
  colores   = RColorBrewer::brewer.pal(9, "Set1"),
  estilo    = list(gg.Tema  = 5,
                   gg.Texto = list(subtitle = "Periodo 2018-2021")))        

Gra2 + 
  scale_y_continuous(limits = c(NA, 40))+
  theme(plot.title=element_text(hjust=0, size=12),
        axis.title=element_text(size=14),
        plot.caption=element_text(size=7),
        legend.text=element_text(size=9),
        axis.text = element_text(size=10))

# NBC

NBC_Prog <- PROGRAMAS %>% summarise(Total = n(), .by = c(NBC)) %>% 
  rename(Clase = NBC) %>%
  mutate(Variable = "NBC") %>% arrange(desc(Total))

NBC_Prog_Top10 <- PROGRAMAS %>% summarise(Total = n(), .by = c(NBC)) %>% 
  rename(Clase = NBC) %>%
  mutate(Variable = "NBC") %>% 
  arrange(desc(Total)) %>% 
  slice_max(Total, n= 10)
  
write_xlsx(NBC_Prog_Top10, "Datos/NBC_Prog_Top10.xlsx")

Gra2 <- Plot.Barras(
  datos     = NBC_Prog,
  categoria = "NBC",
  freqRelativa = TRUE,
  estatico = TRUE,
  vertical = FALSE,
  ordinal   = TRUE,
  titulo     = "Distribución de Programas Académicos Universitarios por Núcleos del Conocimiento",
  labelEje   = "Porcentaje"
  #colores   = RColorBrewer::brewer.pal(56, "Set1")
  )        

Gra2 + 
  scale_y_continuous(limits = c(NA, 30))+
  theme(plot.title=element_text(hjust=1, size=12),
        axis.title=element_text(size=14),
        plot.caption=element_text(size=7),
        legend.text=element_text(size=9),
        axis.text = element_text(size=10))


# NBC + Matriculados

NBC_Prog_Mat <- PROGRAMAS %>% summarise(Total = sum(Total), .by = c(NBC)) %>% 
  rename(Clase = NBC) %>%
  mutate(Variable = "NBC") %>% arrange(desc(Total))

Gra2 <- Plot.Barras(
  datos     = NBC_Prog_Mat,
  categoria = "NBC",
  freqRelativa = TRUE,
  estatico = TRUE,
  vertical = FALSE,
  ordinal   = TRUE,
  titulo     = "Distribución Matriculados Primera Vez Programas Universitarios por Núcleos del Conocimiento",
  labelEje   = "Porcentaje"
  #colores   = RColorBrewer::brewer.pal(56, "Set1")
)        

Gra2 + 
  scale_y_continuous(limits = c(NA, 30))+
  theme(plot.title=element_text(hjust=1, size=12),
        axis.title=element_text(size=14),
        plot.caption=element_text(size=7),
        legend.text=element_text(size=9),
        axis.text = element_text(size=10))

# Áreas y NBC TreeMap

Plot.Treemap(
  datos     = PROGRAMAS,
  variables = vars(Area_Con, NBC),
  estatico  = TRUE,
  estilo    = list(
    gg.fontsize.title = 12, gg.fontsize.labels = c(15, 9),
    gg.fontcolor.labels = c("#FFFFFF", "#212020"),
    gg.border.lwds = c(4, 2), gg.border.col = c("#636363", "white"),
    gg.lowerbound.cex.labels = 0.2, gg.overlap.labels = 0.1
  )
)  

# Top 20 programas académicos - General

Top_20 <- PROGRAMAS %>% summarise(Total = sum(Total), .by = c(SNIES_PROGRA, Programa, Prog_Acredita, Institucion, Metodologia)) %>% 
          slice_max(Total, n= 20)

write_xlsx(Top_20, "Datos/Top_20.xlsx")


# Top 20 programas académicos - 

Top_20_Presencial <- PROGRAMAS %>% filter(Metodologia == "Presencial") %>% 
  summarise(Total = sum(Total), .by = c(SNIES_PROGRA, Programa, Prog_Acredita, Institucion, Metodologia)) %>% 
  slice_max(Total, n= 20)

write_xlsx(Top_20_Presencial, "Datos/Top_20_Presencial.xlsx")

Top_n50 <- PROGRAMAS %>% summarise(Total = sum(Total), .by = c(SNIES_PROGRA, Programa, Prog_Acredita, Institucion, Metodologia)) %>% 
           filter(Total <= 50)


# Parte 11.Pilo Paga y Generación E ----

Pilo <- read_excel("Fuentes/Pilo_Excelencia_Equidad.xlsx", 
                        sheet = "Pilo",
                        guess_max = 1000) %>% 
        mutate(across(where(is.numeric), ~replace_na(., 0)))

Equidad <- read_excel("Fuentes/Pilo_Excelencia_Equidad.xlsx", 
                   sheet = "Equidad",
                   guess_max = 1000)%>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

Excelencia <- read_excel("Fuentes/Pilo_Excelencia_Equidad.xlsx", 
                      sheet = "Excelencia",
                      guess_max = 1000)%>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# Eliminar variables por poblaciones

Pilo <- Pilo %>% select(-c(COD_DEPTO, DEPTO_IES, IES))
Equidad <- Equidad %>% select(-c(COD_DEPTO, DEPTO_IES, IES))
Excelencia <- Excelencia %>% select(-c(COD_DEPTO, DEPTO_IES, IES))

# Crear base de datos única Programas
# Pilo Paga y Generación E

Pilo_GenE <- Pilo %>% full_join(Equidad) %>% 
                      full_join(Excelencia) %>% 
             mutate(across(everything(), ~ replace_na(.x, 0)),
                    `2019` = EQ20191 + EQ20192 + EX20191 + EX20192,
                    `2020` = EQ20201 + EQ20202 + EX20201 + EX20202,
                    `2021` = EQ20211 + EQ20212 + EX20211 + EX20212,
                    `2022` = EQ20221 + EQ20222 + EX20221 + EX20222,
                     Total = `2015` + `2016` + `2017` + `2018` + `2019`+
                             `2020` + `2021` + `2022`,
                    EQ2019 = EQ20191 + EQ20192,
                    EQ2020 = EQ20201 + EQ20202,
                    EQ2021 = EQ20211 + EQ20212,
                    EQ2022 = EQ20221 + EQ20222,
                    EX2019 = EX20191 + EX20192,
                    EX2020 = EX20201 + EX20202,
                    EX2021 = EX20211 + EX20212,
                    EX2022 = EX20221 + EX20222
                    ) %>% 
             relocate(c(`2019`:`2022`, EQ2019:EX2022), .after = `2018`)
             
# Base de Tendencia General y por Programas

Pilo_Exc_Equi <- Pilo_GenE %>% select(COD_IES:EX2022) %>% 
       pivot_longer(cols = c(`2015`:EX2022), 
                    names_to = "Year",
                    values_to = "Total") %>% 
       mutate(Programa = case_when(Year == c("2015", "2016", "2017", "2018")~ "Pilo Paga",
                                   str_detect(Year, "EX") == TRUE ~ "Excelencia",
                                   str_detect(Year, "EQ") == TRUE ~ "Equidad")) %>% 
       summarise(Total = sum(Total), .by = c(Year, Programa)) %>% 
       mutate(Ano = parse_number(Year))

# Agregar Categoría General

General_Pilos <- Pilo_Exc_Equi %>% 
                 filter(is.na(Programa) | Programa == "Pilo Paga") %>% 
                 mutate(Programa = case_when(Programa == "Pilo Paga" ~ "General",
                                             is.na(Programa) == TRUE ~ "General"))

Pilo_Exc_Equi <- bind_rows(Pilo_Exc_Equi, General_Pilos)

# Gráficos de Tendencias de Programas

# General

Pilo_Exc_Equi_Gen <- Pilo_Exc_Equi %>% filter(Programa == "General")

Pilo_Exc_Equi_Gen %>% 
  ggplot(aes(x = Ano, y = Total)) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0,100000))+
  scale_x_continuous(breaks = c(2015:2022))+
  ggtitle("Evolución Total Beneficiarios Programas Pilo Paga y Generación E", subtitle = "Periodo 2015-2022\n")+
  ylab("\n Total Beneficiarios\n ")+
  xlab("Año") +
  annotate(geom="text", x=2015, y=18000, label= Pilo_Exc_Equi_Gen[1, 3], color="red")+
  annotate(geom="text", x=2018, y=3000, label= Pilo_Exc_Equi_Gen[4, 3], color="red")+
  annotate(geom="text", x=2019, y=85000, label= Pilo_Exc_Equi_Gen[5, 3], color="red")+
  annotate(geom="text", x=2022, y=93000, label= Pilo_Exc_Equi_Gen[8, 3], color="red")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))


# General + Programas

# Base de datos

Pilo_Exc_Equi_Gen <- Pilo_Exc_Equi %>% filter(!is.na(Programa)) 
Pilo_Exc_Equi_Gen$Programa <- factor(Pilo_Exc_Equi_Gen$Programa, levels = c("Pilo Paga", "Equidad", "Excelencia", "General"))

#Gráfico

Pilo_Exc_Equi_Gen %>% 
  ggplot(aes(x = Ano, y = Total, color = Programa)) + 
  geom_line() +
  geom_point() +
  ylim(0,95000)+
  scale_color_manual(values = c("#E7B800", "#2E9FDF", "green", "red"))+
  geom_brace(aes(x=c(2015,2018), y = c(16000, 20000), label = "Programa Pilo Paga"), inherit.data=F, rotate=0, labelsize=5) +
  scale_y_continuous(limits = c(0,100000))+
  scale_x_continuous(breaks = c(2015:2022))+
  ggtitle("Evolución Total Beneficiarios Programas Pilo Paga y Generación E", subtitle = "Periodo 2015-2022\n")+
  ylab("\n Total Beneficiarios\n ")+
  xlab("Año")+
  annotate(geom="text", x=2015, y=6000, label= Pilo_Exc_Equi_Gen[13, 3], color="red")+
  annotate(geom="text", x=2018, y=3000, label= Pilo_Exc_Equi_Gen[16, 3], color="red")+
  annotate(geom="text", x=2019, y=85000, label= Pilo_Exc_Equi_Gen[17, 3], color="red")+
  annotate(geom="text", x=2022, y=93000, label= Pilo_Exc_Equi_Gen[20, 3], color="red")+
  annotate(geom="text", x=2019.3, y=70000, label= Pilo_Exc_Equi_Gen[5, 3], color="blue")+
  annotate(geom="text", x=2022, y=78000, label= Pilo_Exc_Equi_Gen[8, 3], color="blue")+
  annotate(geom="text", x=2019, y=8000, label= Pilo_Exc_Equi_Gen[9, 3], color="#2ca25f")+
  annotate(geom="text", x=2022, y=7000, label= Pilo_Exc_Equi_Gen[12, 3], color="#2ca25f")+
      theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# ANÁLISIS IES

# Base IES

IES_Final <- IES %>% select(-Total) %>% 
             mutate(Acredita = case_when(Acredita == "SI" ~ "Sí",
                                         Acredita == "NO" ~ "No",
                                         is.na(Acredita) == TRUE ~ "Sin Información"))

# Base Beneficiarios Programas

Pilo_Exc_Equi_IES <- Pilo_GenE %>% select(COD_IES:EX2022) %>% 
  pivot_longer(cols = c(`2015`:EX2022), 
               names_to = "Year",
               values_to = "Total") %>% 
  mutate(Programa = case_when(Year == c("2015", "2016", "2017", "2018")~ "Pilo Paga",
                              str_detect(Year, "EX") == TRUE ~ "Excelencia",
                              str_detect(Year, "EQ") == TRUE ~ "Equidad")) %>% 
  filter(!is.na(Programa)) %>% 
  mutate(Year = parse_number(Year))

# Cruzar base de Datos

Pilo_Exc_Equi_IES <- inner_join(Pilo_Exc_Equi_IES, IES_Final, 
                                by = join_by("COD_IES" == "IES_PADRE")) %>% 
                     mutate(Total = ifelse(Total == 0, NA, Total),
                            Caracter = factor(Caracter))

# Gráficos

# Gráfico General

Pilo_Exc_Equi_IES %>% ggplot(aes(x = as.factor(Year), y = Total)) +
  geom_boxplot(outlier.size = 0.4)+
  ggtitle("Distribución Total Beneficiarios Programas Pilo Paga y Generación E", subtitle = "Periodo 2015-2022\n")+
  ylab("\n Total Beneficiarios\n ")+
  xlab("Año")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Gráfico acotado con atípicos

Pilo_Exc_Equi_IES %>% ggplot(aes(x = as.factor(Year) , y = Total)) +
  geom_boxplot(outlier.size = 0.2)+
  scale_y_continuous(limits = c(0, 3000))+
  ggtitle("Distribución Total Beneficiarios Programas Pilo Paga y Generación E", subtitle = "Periodo 2015-2022\n")+
  ylab("\n Total Beneficiarios\n ")+
  xlab("Año")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# por Sector

# Gráfico
Pilo_Exc_Equi_IES %>% ggplot(aes(x = as.factor(Year) , y = Total, colour = `Sector IES`)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0, 3000)) +
  ggtitle("Distribución Total Beneficiarios Programas Pilo Paga y Generación E, por Sector", subtitle = "Periodo 2015-2022\n")+
  ylab("\n Total Beneficiarios\n ")+
  xlab("Año")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Tabla

Pilo_Exc_Equi_IES %>% summarise(Total = sum(Total, na.rm = TRUE), .by = c(Year, `Sector IES`)) %>% 
   rename(Sector = `Sector IES`) %>% 
  Tabla(rows = vars(Year), 
        pivotCat = Sector, 
        pivotVar = Total,
        encabezado  = "Sector de la educación",
        estadistico = "Suma",
        columnNames = c("Año", "Beneficiarios"),
        estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3), 
          Titulo = "Evolución Total de beneficiarios por sector")) %>% 
        tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
        cols_align(align = "center")


# por Instituciones Acreditadas

# Gráfico - Box plot
Pilo_Exc_Equi_IES %>% ggplot(aes(x = as.factor(Year) , y = Total, colour = Acredita)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(0, 2000)) +
  scale_color_discrete(name = "¿Acreditación?")+
  ggtitle("Distribución Total Beneficiarios Programas Pilo Paga y Generación E, por IES acreditadas", subtitle = "Diagrama de Cajas \nPeriodo 2015-2022\n")+
  ylab("\n Total Beneficiarios\n ")+
  xlab("Año")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Gráfico - violin
Pilo_Exc_Equi_IES %>% ggplot(aes(x = as.factor(Year) , y = Total, colour = Acredita)) +
  geom_violin()+
  scale_y_continuous(limits = c(0, 2000)) +
  scale_color_discrete(name = "¿Acreditación?")+
  ggtitle("Distribución Total Beneficiarios Programas Pilo Paga y Generación E, por IES acreditadas", subtitle = "Diagrama de Violín \nPeriodo 2015-2022\n")+
  ylab("\n Total Beneficiarios\n ")+
  xlab("Año")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))

# Tabla

Pilo_Exc_Equi_IES %>% summarise(Total = sum(Total, na.rm = TRUE), .by = c(Year, Acredita)) %>% 
  Tabla(rows = vars(Year), 
        pivotCat = Acredita, 
        pivotVar = Total,
        encabezado  = "Instituciones Acreditadas",
        estadistico = "Suma",
        columnNames = c("Año", "Beneficiarios"),
        estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3), 
          Titulo = "Evolución Total de beneficiarios por IES acreditadas")) %>% 
  tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
  cols_align(align = "center")

# por caracter de las IES

# Gráfico

Pilo_Exc_Equi_IES %>% filter(Year %in% c(2015:2018)) %>% 
  ggplot(aes(x = as.factor(Year) , y = Total, colour = Caracter)) +
  geom_boxplot() +
  scale_color_discrete(name = "Carácter de la IES")+
  ggtitle("Distribución Total Beneficiarios Programa Pilo Paga, por Carácter de las IES", subtitle = "Periodo 2015-2018\n")+
  ylab("\n Total Beneficiarios\n ")+
  xlab("Año")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom")


Pilo_Exc_Equi_IES %>% filter(Year %in% c(2019:2022)) %>% 
  ggplot(aes(x = as.factor(Year) , y = Total, colour = Caracter)) +
  geom_boxplot() +
  theme(legend.position="bottom")+
  scale_color_discrete(name = "Carácter de la IES")+
  ggtitle("Distribución Total Beneficiarios Programas Generación E, por Carácter de las IES", subtitle = "Periodo 2019-2022\n")+
  ylab("\n Total Beneficiarios\n ")+
  xlab("Año")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13))


# Tabla

Pilo_Exc_Equi_IES %>% summarise(Total = sum(Total, na.rm = TRUE), .by = c(Year, Caracter)) %>% 
  Tabla(rows = vars(Year), 
        pivotCat = Caracter, 
        pivotVar = Total,
        encabezado  = "Carácter de las instituciones",
        estadistico = "Suma",
        columnNames = c("Año", "Beneficiarios"),
        estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0.3, 3), 
          Titulo = "Evolución total de beneficiarios programas Pilo Paga y Generación E según el carácter de las IES")) %>% 
  tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
  cols_align(align = "center")

# Tablas Top 

# Universidades - Pilo Paga

Pilo_Universidad <- Pilo_Exc_Equi_IES %>% filter(Programa == "Pilo Paga", Caracter == "Universidad") %>% 
  rename(`¿Acreditada?` = Acredita)

# Top

Pilo_Universidad %>% summarise(Beneficios = sum(Total, na.rm = TRUE), 
                             .by = c(COD_IES, Institucion, `Sector IES`, `¿Acreditada?`)) %>%
  arrange(desc(Beneficios))  %>% 
  filter(Beneficios != 0) %>% 
  mutate(Orden = 1:n()) %>% 
  filter(Orden <=10 | between(Orden, n()-9 ,n())) %>% 
  #slice_max(Beneficios, n= 30) %>%
  select(-c(COD_IES, Orden)) %>% 
  rename(Sector = `Sector IES`) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0, 2), 
          Titulo = "Evolución Total de Beneficios Programa Pilo Paga en Universidades")) %>% 
  tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
  cols_align(align = "left", columns = "Institucion") %>% 
  tab_row_group(label = "Universidades con menos beneficios" ,rows = 11:20, id = "grupo") %>% 
  tab_row_group(label = "Universidades con más beneficios", rows = 1:10, id = "grupo1") %>% 
  tab_style(
    style = cell_fill(color = "#ffeda0"),
    locations = cells_row_groups(groups = "grupo")
  ) %>% 
  tab_style(
    style = cell_fill(color = "#ffeda0"),
    locations = cells_row_groups(groups = "grupo1")
  ) 

# Universidades - Generación E

Generacion_Universidad <- Pilo_Exc_Equi_IES %>% filter(Programa != "Pilo Paga", Caracter == "Universidad") %>% 
  rename(`¿Acreditada?` = Acredita)


# Top 10

Generacion_Universidad %>% summarise(Beneficios = sum(Total, na.rm = TRUE), 
                             .by = c(COD_IES, Institucion, `Sector IES`, `¿Acreditada?`)) %>%
  arrange(desc(Beneficios))  %>% 
  filter(Beneficios != 0) %>% 
  mutate(Orden = 1:n()) %>% 
  filter(Orden <=10 | between(Orden, n()-9 ,n())) %>% 
  #slice_max(Beneficios, n= 30) %>%
  select(-c(COD_IES, Orden)) %>% 
  rename(Sector = `Sector IES`) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0, 2), 
          Titulo = "Evolución Total de Beneficios Programa Generación E por Universidades")) %>% 
  tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
  cols_align(align = "left", columns = "Institucion") %>% 
  tab_row_group(label = "Universidades con menos beneficios" ,rows = 11:20, id = "grupo") %>% 
  tab_row_group(label = "Universidades con más beneficios", rows = 1:10, id = "grupo1") %>% 
  tab_style(
    style = cell_fill(color = "#ffeda0"),
    locations = cells_row_groups(groups = "grupo")
  ) %>% 
  tab_style(
    style = cell_fill(color = "#ffeda0"),
    locations = cells_row_groups(groups = "grupo1")
  ) 


# Instituciones Universitarias - Generación E

Generacion_IES <- Pilo_Exc_Equi_IES %>% filter(Programa != "Pilo Paga", Caracter == "Institución Universitaria/Escuela Tecnológica") %>% 
                  rename(`¿Acreditada?` = Acredita)


# Top 10

  Generacion_IES %>% summarise(Beneficios = sum(Total, na.rm = TRUE), 
                       .by = c(COD_IES, Institucion, `Sector IES`, `¿Acreditada?`)) %>%
  arrange(desc(Beneficios))  %>% 
    filter(Beneficios != 0) %>% 
    mutate(Orden = 1:n()) %>% 
    filter(Orden <=10 | between(Orden, n()-9 ,n())) %>% 
  #slice_max(Beneficios, n= 30) %>%
  select(-c(COD_IES, Orden)) %>% 
  rename(Sector = `Sector IES`) %>% 
    Tabla(estatico = TRUE,
          colorHead   = "#99d8c9",
          estilo      = list(
            Tema = 11, Padding = c(0, 2), 
            Titulo = "Evolución Total de Beneficios Programa Generación E Inst. Universitarias o Esc. Tecnológicas")) %>% 
    tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
    cols_align(align = "left", columns = "Institucion") %>% 
    tab_row_group(label = "Instituciones con menos beneficios" ,rows = 11:20, id = "grupo") %>% 
    tab_row_group(label = "Instituciones con más beneficios", rows = 1:10, id = "grupo1") %>% 
    tab_style(
      style = cell_fill(color = "#ffeda0"),
      locations = cells_row_groups(groups = "grupo")
    ) %>% 
    tab_style(
      style = cell_fill(color = "#ffeda0"),
      locations = cells_row_groups(groups = "grupo1")
    ) 

 
##  
# Instituciones Técnicas y Tecnológicas  
##
  
  # Instituciones Técnicas profesionales - Generación E
  
  Generacion_Tecnicas <- Pilo_Exc_Equi_IES %>% filter(Programa != "Pilo Paga", Caracter == "Institución Técnica Profesional") %>% 
    rename(`¿Acreditada?` = Acredita,
           Sector = `Sector IES`) %>% 
    summarise(Beneficios = sum(Total, na.rm = TRUE), 
            .by = c(COD_IES, Institucion, Sector, `¿Acreditada?`)) %>% 
    arrange(desc(Beneficios))
  
  
  # Instituciones Tecnológicas - Generación E
  
    Pilo_Exc_Equi_IES %>% filter(Programa != "Pilo Paga", Caracter == "Institución Tecnológica") %>% 
    rename(`¿Acreditada?` = Acredita,
           Sector = `Sector IES`) %>% 
    summarise(Beneficios = sum(Total, na.rm = TRUE), 
              .by = c(COD_IES, Institucion, Sector, `¿Acreditada?`)) %>% 
      arrange(desc(Beneficios)) %>% 
      bind_rows(Generacion_Tecnicas) %>% 
      select(-c(COD_IES)) %>% 
      Tabla(estatico = TRUE,
            colorHead   = "#99d8c9",
            estilo      = list(
              Tema = 11, Padding = c(0, 2), 
              Titulo = "Evolución Total de Beneficios Programas Generación E Instituciones Técnicas y Tecnológicas.")) %>% 
      tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
      cols_align(align = "left", columns = "Institucion") %>% 
      tab_row_group(label = "Instituciones Tecnológicas" ,rows = 1:3, id = "grupo") %>% 
      tab_row_group(label = "Instituciones Técnicas Profesionales", rows = 4:5, id = "grupo1") %>% 
      tab_style(
        style = cell_fill(color = "#ffeda0"),
        locations = cells_row_groups(groups = "grupo")
      ) %>% 
      tab_style(
        style = cell_fill(color = "#ffeda0"),
        locations = cells_row_groups(groups = "grupo1")
      ) 

# Programa Generación E - Excelencia
 
Pilo_Exc_Equi_IES %>% filter(Programa == "Excelencia") %>% 
      rename(`¿Acreditada?` = Acredita,
             Sector = `Sector IES`) %>% 
      summarise(Beneficios = sum(Total, na.rm = TRUE), 
                .by = c(COD_IES, Institucion, Sector, `¿Acreditada?`, Caracter)) %>% 
  arrange(desc(Beneficios)) %>% 
  filter(Beneficios!=0) %>% 
  mutate(Orden = 1:n()) %>% 
  filter(Orden <=10 | between(Orden, n()-9 ,n())) %>% 
  select(-c(COD_IES, Orden)) %>% 
  Tabla(estatico = TRUE,
        colorHead   = "#99d8c9",
        estilo      = list(
          Tema = 11, Padding = c(0, 2), 
          Titulo = "Evolución Total de Beneficios del Programa Excelencia por IES")) %>% 
  tab_source_note(source_note = "Fuente: Elaboracción propia") %>% 
  cols_align(align = "left", columns = "Institucion") %>% 
  tab_row_group(label = "Instituciones con menos beneficios" ,rows = 11:20, id = "grupo") %>% 
  tab_row_group(label = "Instituciones con más beneficios", rows = 1:10, id = "grupo1") %>% 
  tab_style(
    style = cell_fill(color = "#ffeda0"),
    locations = cells_row_groups(groups = "grupo")
  ) %>% 
  tab_style(
    style = cell_fill(color = "#ffeda0"),
    locations = cells_row_groups(groups = "grupo1")
  ) 

# Parte 12.Anális UNAL ----

# Base de datos municipios condicones difíciles acceso a ES

Mun_DificilES <- read_excel("Fuentes/Municipios Dificiles ES.xlsx", 
                            sheet = "Hoja1",
                            guess_max = 60000)

# Contexto UNAL ----

# Matriculados

# Tendencia general

Gen_Mat <- UnalData::Matriculados %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% 
  mutate(Periodo = factor(paste(YEAR, SEMESTRE, sep = "-"))) %>% 
  filter(YEAR >= 2010)

ggplot(data = Gen_Mat, aes(x = Periodo, y = Total, group=1)) +
  geom_point(size = 2)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,65000))+
  #ggtitle(, subtitle = "Periodo 2020-2035\n")+
  ylab("\n Total Matriculados\n ")+
  xlab("\nPeriodo")+
  labs(title = "Evolución Total de Estudiantes Matriculados en la Universidad Nacional de Colombia",
       subtitle = "Periodo 2010:2022")+
  annotate(geom="text", x="2010-1", y=42000,
           label= "     46.753", color="red")+
  annotate(geom="text", x="2022-2", y=61000,
           label= "57.090     ", color="red")+
   theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue", angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(face="bold", color="black", size=13))

# Nivel de formación

Gen_Mat_Nivel <- UnalData::Matriculados %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, TIPO_NIVEL)) %>% 
  mutate(Periodo = factor(paste(YEAR, SEMESTRE, sep = "-"))) %>% 
  filter(YEAR >= 2010)

ggplot(data = Gen_Mat_Nivel, aes(x = Periodo, y = Total, color = TIPO_NIVEL, group=TIPO_NIVEL)) +
  geom_point(size = 2)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,55000))+
  ylab("\n Total Matriculados\n ")+
  xlab("\nPeriodo")+
  labs(title = "Evolución Total de Estudiantes Matriculados por Nivel de Formación en la  \nUniversidad Nacional de Colombia",
       subtitle = "Periodo 2010:2022")+
  annotate(geom="text", x="2022-2", y=54000,
           label= "50.111     ", color="blue")+
  annotate(geom="text", x="2022-2", y=3500,
           label= "6.979     ", color="red")+
  scale_colour_discrete(name  ="Nivel de Formación")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue", angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position = "bottom")

# Modalidades postgrado

Gen_Mat_Modalidad <- UnalData::Matriculados %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE, NIVEL)) %>% 
  mutate(Periodo = factor(paste(YEAR, SEMESTRE, sep = "-")),
         NIVEL = ifelse(NIVEL == "Especialidades  médicas", "Especialidades médicas", NIVEL)) %>% 
  filter(YEAR >= 2010, !NIVEL %in% c("Pregrado", "Tecnología"))
 

ggplot(data = Gen_Mat_Modalidad, aes(x = Periodo, y = Total, color = NIVEL, group=NIVEL)) +
  geom_point(size = 2)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,7000))+
  ylab("\n Total Matriculados\n ")+
  xlab("\nPeriodo")+
  labs(title = "Evolución Total de Estudiantes Matriculados en Postgrado por Modalidad \nde Formación en la Universidad Nacional de Colombia",
       subtitle = "Periodo 2010:2022")+
  scale_colour_discrete(name  ="Modalidad de Formación")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue", angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(face="bold", color="black", size=13))

# Por departamentos

# Por municipios

# Graduados

# Tendencia general
# Nivel de formación
# Modalidades postgrado
# Por departamentos
# Por municipios

# Docentes y Funcionarios

# Docentes
# Tendencia general
# Máximo nivel de formación

# Funcionarios
# Tendencia general


# Inscritos ----

# Bases de datos

# Importar Base de Datos zonas rurales con 
# condiciones de difícil acceso a la educación superior 

Mun_DificilES <- read_excel("Fuentes/Municipios Dificiles ES.xlsx", 
                            sheet = "Hoja1",
                            guess_max = 60000)

# Base aspirantes pregrado UNAL

Asp_Pre <- UnalData::Aspirantes %>% 
  filter(YEAR >= 2010, 
        (TIPO_NIVEL == "Pregrado"  & is.na(MOD_INS) != TRUE & is.na(TIPO_INS) != TRUE)) 

# Base Agregada

Asp_Consolidada <- Agregar(datos = Asp_Pre,
                    formula = SEXO + ESTRATO + INS_SEDE_NOMBRE + 
                      ADMITIDO + TIPO_INS + MOD_INS + PAES + PEAMA ~ YEAR + SEMESTRE,
                    frecuencia = list(2010:2023, 1:2),
                    intervalo = list(c(2010, 1), c(2023, 2)),
                    textNA = "No Aplica",
                    ask = FALSE)

# Fase de Gráficos

# Serie de Tiempo General

Gen_Asp_Pre <- Asp_Pre %>% 
               summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% 
               mutate(Periodo = factor(paste(YEAR, SEMESTRE, sep = "-")))

ggplot(data = Gen_Asp_Pre, aes(x = Periodo, y = Total, group=1)) +
  geom_point(size = 2)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,80000))+
  #ggtitle(, subtitle = "Periodo 2020-2035\n")+
  ylab("\n Total de Aspirantes\n ")+
  xlab("\nPeriodo")+
  labs(title = "Evolución Total de Aspirantes a Pregrado en la Universidad Nacional de Colombia",
       subtitle = "Periodo 2010:2023")+
   annotate(geom="text", x="2019-1", y=80000,
            label= "75.681", color="red")+
   annotate(geom="text", x="2022-2", y=15000,
           label= "18.825", color="red")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue", angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(face="bold", color="black", size=13))


# Mapa - Por municipios de Residencia

Plot.Mapa(
  df       = Asp_Pre,
  depto    = COD_DEP_RES,
  mpio     = COD_CIU_RES,
  tipo     = "SiNoMpios",
  zoomIslas = FALSE,
  SiNoLegend = c("Sí", "No"),
  #centroideMapa = c("ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"),
  titulo   = "Municipios de Residencia con Aspirantes \na la Universidad Nacional de Colombia",
  colores  = c("red", "#10F235"),
  opacidad = 1,
  textSize = 0,
  limpio   = FALSE,
  estatico = TRUE,
  estilo   = list(
    Style = "SiNo", Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Labs  = list(fill = "¿Aspirantes?", subtitle = "Periodo 2010:2023\n"),
    Legend = list(legend.direction = "vertical")))

# Mapa - Por municipios de Residencia - 2023

Plot.Mapa(
  df       = Asp_Pre %>% filter(YEAR == 2023),
  depto    = COD_DEP_RES,
  mpio     = COD_CIU_RES,
  tipo     = "SiNoMpios",
  zoomIslas = FALSE,
  SiNoLegend = c("Sí", "No"),
  #centroideMapa = c("ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"),
  titulo   = "Municipios de Residencia con Aspirantes \na la Universidad Nacional de Colombia",
  colores  = c("red", "#10F235"),
  opacidad = 1,
  textSize = 0,
  limpio   = FALSE,
  estatico = TRUE,
  estilo   = list(
    Style = "SiNo", Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Labs  = list(fill = "¿Aspirantes?", subtitle = "Año 2023\n"),
    Legend = list(legend.direction = "vertical")))


# Tabla - Cobertura UNAL municipios en condiciones difíciles de absorción

# Crear base de datos

Asp_Pre_Unico <- Asp_Pre %>% filter(YEAR == 2023) %>% 
  summarise(Total = n(), .by = c(COD_CIU_RES)) %>% 
  mutate(Asp_UNAL = "Sí")

Municipios <- read_excel("Fuentes/Divipola.xlsx", 
                         sheet = "Hoja1",
                         guess_max = 1000) %>% 
              summarise(Municipio = max(Municipio), .by = c(COD_DEPT , COD_MUN ))

Municipios1 <- Municipios %>% 
               left_join(Asp_Pre_Unico, by = join_by(COD_MUN == COD_CIU_RES)) %>% 
               left_join(Mun_DificilES, by = join_by(COD_MUN == COD_MPIO)) %>% 
               mutate(Asp_UNAL = ifelse(is.na(Asp_UNAL), "No", Asp_UNAL),
                      DificilES = ifelse(is.na(DificilES), "No", DificilES)) %>% 
               filter(DificilES == "Sí")

message("Cobertura UNAL municipios difícil acceso ES")
table(Municipios1$Asp_UNAL)

# Sexo
# Serie de Tiempo

Asp_Consolidada %>% filter(Variable == "SEXO") %>%
                    pivot_wider(names_from = Clase, values_from = Total) %>% 
                    mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
                    select(-c(YEAR, SEMESTRE)) %>% 
                    pivot_longer(cols = -c(Variable, Periodo), names_to = "Sexo", values_to = "Total") %>% 
                    mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = Sexo, group = Sexo)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,45000))+
  ggtitle("Evolución Total Aspirantes a Pregrado en la Universidad Nacional de Colombia, \npor sexo", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Aspirantes\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
 # Transversal

Asp_Consolidada %>% 
  Plot.Barras(categoria = "SEXO",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 60),
              labelY = "Porcentaje de aspirantes (%)\n",
              labelX = "\nSexo",
              titulo = "Proporción de Aspirantes a Pregrado en la UNAL por Sexo",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))
  

# Estrato
# Serie de Tiempo

Asp_Consolidada %>% filter(Variable == "ESTRATO") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Estrato", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = Estrato, group = Estrato)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,50000))+
  ggtitle("Evolución Total Aspirantes a Pregrado en la Universidad Nacional de Colombia, \npor Estrato", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Aspirantes\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Asp_Consolidada %>% 
  Plot.Barras(categoria = "ESTRATO",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 60),
              labelY = "Porcentaje de aspirantes (%)\n",
              labelX = "\nEstrato",
              titulo = "Proporción de Aspirantes a Pregrado en la UNAL por Estrato",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Por Sedes
# Serie de Tiempo

Asp_Consolidada %>% filter(Variable == "INS_SEDE_NOMBRE", YEAR >= 2015) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Sede", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = Sede, group = Sede)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,55000))+
  ggtitle("Evolución Total Aspirantes a Pregrado en la Universidad Nacional de Colombia, \npor Sedes", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Aspirantes\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Asp_Consolidada %>% filter(YEAR >= 2015) %>% 
  Plot.Barras(categoria = "INS_SEDE_NOMBRE",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 75),
              vertical = FALSE,
              labelY = "\nPorcentaje de aspirantes (%)\n",
              labelX = "\nSede de la Universidad\n",
              titulo = "Proporción de Aspirantes a Pregrado en la UNAL por Sedes",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.7),
                            gg.Texto = list(subtitle = "Periodo 2015:2023",
                                            caption  = "No Aplica. Hace referencia a estudiantes inscritos\n a pregrado a la Universidad de manera global")))


# Modalidad de inscripción (REGULAR y ESPECIAL)
# Serie de tiempo

Asp_Consolidada %>% filter(Variable == "MOD_INS") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Modalidad", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Modalidad`, group = `Modalidad`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,80000))+
  ggtitle("Evolución Total Aspirantes a Pregrado en la Universidad Nacional de Colombia, \npor Modalidad de Iscripción", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Aspirantes\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Asp_Consolidada %>% 
  Plot.Barras(categoria = "MOD_INS",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 90),
              labelY = "Porcentaje de aspirantes (%)\n",
              labelX = "\nModalidad de Inscripción",
              titulo = "Proporción de Aspirantes a Pregrado en la UNAL por Modalidad de Inscripción",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Modalidad de inscripción (REGULAR, PAES, PEAMA)
# Serie de tiempo

Asp_Consolidada %>% filter(Variable == "TIPO_INS", Clase != "Regular") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Programa", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Programa`, group = `Programa`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,13000))+
  ggtitle("Evolución Total Aspirantes a Pregrado en la Universidad Nacional de Colombia, \npor Programa de Iscripción Especial", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Aspirantes\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Asp_Consolidada %>% filter(Clase != "Regular") %>% 
  Plot.Barras(categoria = "TIPO_INS",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 70),
              labelY = "Porcentaje de aspirantes (%)\n",
              labelX = "\nPrograma Especial",
              titulo = "Proporción de Aspirantes a Pregrado en la UNAL por Programas de Inscripción \nEspecial",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Modalidad de inscripción (PAES)
# Serie de tiempo

Asp_Consolidada %>% filter(Variable == "PAES", !Clase %in% c("No Aplica", "De La Paz")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Opciones PAES", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Opciones PAES`, group = `Opciones PAES`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,10000))+
  ggtitle("Evolución Total Aspirantes a Pregrado en la Universidad Nacional de Colombia, \nPrograma PAES", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Aspirantes\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        )

# Modalidad de inscripción (Indígenas y Víctimas)
# Serie de tiempo

Asp_Consolidada %>% filter(Variable == "PAES", Clase %in% c("Comunidades indígenas", "Victimas del conflicto armado interno en Colombia")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Opciones PAES", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Opciones PAES`, group = `Opciones PAES`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0, 9000))+
  ggtitle("Evolución Total Aspirantes a Pregrado en la Universidad Nacional de Colombia, \nPrograma PAES", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")


# Transversal

Asp_Consolidada %>% filter(Variable == "PAES", !Clase %in% c("No Aplica", "De La Paz")) %>%
  Plot.Barras(categoria = "PAES",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 40),
              labelY = "\nPorcentaje de aspirantes (%)\n",
              labelX = "\nOpciones Programa PAES\n",
              vertical = FALSE,
              titulo = "Proporción de Aspirantes a Pregrado en la UNAL del \nPrograma PAES",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Modalidad de inscripción (PEAMA)
# Serie de tiempo

Asp_Consolidada %>% filter(Variable == "PEAMA", !Clase %in% c("No Aplica")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Opciones PEAMA", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Opciones PEAMA`, group = `Opciones PEAMA`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,4000))+
  ggtitle("Evolución Total Aspirantes a Pregrado en la Universidad Nacional de Colombia, \nPrograma PEAMA", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Aspirantes\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Asp_Consolidada %>% filter(Variable == "PEAMA", !Clase %in% c("No Aplica")) %>%
  Plot.Barras(categoria = "PEAMA",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 40),
              labelY = "\nPorcentaje de aspirantes (%)",
              labelX = "\nOpciones PEAMA\n",
              vertical = FALSE,
              titulo = "Proporción de Aspirantes a Pregrado en la UNAL del \nPrograma PEAMA",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.6),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Mapa - Por municipios de Residencia Especial

Asp_Pre_Especial <- Asp_Pre %>% filter(MOD_INS == "Especial") %>% 
                    summarise(Total = n(), .by = c(COD_DEP_RES, COD_CIU_RES))


Plot.Mapa(
  df       = Asp_Pre_Especial,
  #dpto     = COD_DEP_RES,
  mpio     = COD_CIU_RES,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Municipios de Residencia de Aspirantes Inscritos \na Pregrado de Manera Especial en la Universidad \nNacional de Colombia",
  naTo0    = TRUE,
  #centroideMapa = c("TOLIMA"),
  cortes        = c(0, 20, 100, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total Aspirantes", subtitle = "Periodo 2010:2023\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps


ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.33, y = 0.25, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.38, y = 0.27, width = 0.055)

# Mapa - Por municipios de Residencia Especial - PAES

Asp_Pre_PAES <- Asp_Pre %>% filter(MOD_INS == "Especial", TIPO_INS == "PAES") %>% 
  summarise(Total = n(), .by = c(COD_DEP_RES, COD_CIU_RES))


Plot.Mapa(
  df       = Asp_Pre_PAES,
  mpio     = COD_CIU_RES,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Municipios de Residencia de Aspirantes Inscritos \na Pregrado del Programa PAES en la Universidad \nNacional de Colombia",
  naTo0    = TRUE,
  #centroideMapa = c("TOLIMA"),
  cortes        = c(0, 20, 100, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total Aspirantes PAES", subtitle = "Periodo 2010:2023\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps


ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.33, y = 0.25, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.38, y = 0.27, width = 0.055)

# Mapa - Por municipios de Residencia Especial - PEAMA

Asp_Pre_PEAMA <- Asp_Pre %>% filter(MOD_INS == "Especial", TIPO_INS == "PEAMA") %>% 
  summarise(Total = n(), .by = c(COD_DEP_RES, COD_CIU_RES))


Plot.Mapa(
  df       = Asp_Pre_PEAMA,
  mpio     = COD_CIU_RES,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Municipios de Residencia de Aspirantes Inscritos \na Pregrado del Programa PEAMA en la Universidad \nNacional de Colombia",
  naTo0    = TRUE,
  #centroideMapa = c("TOLIMA"),
  cortes        = c(0, 20, 100, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total Aspirantes PEAMA", subtitle = "Periodo 2010:2023\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps


ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.33, y = 0.25, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.38, y = 0.27, width = 0.055)


# Admitidos ----

# Base admitidos pregrado UNAL

Adm_Pre <- UnalData::Aspirantes %>% 
  filter(YEAR >= 2010,
         ADMITIDO == "Sí",
         (TIPO_NIVEL == "Pregrado"  & is.na(MOD_INS) != TRUE & is.na(TIPO_INS) != TRUE)) 

# Base Agregada

Adm_Consolidada <- Agregar(datos = Adm_Pre,
                           formula = SEXO + ESTRATO + INS_SEDE_NOMBRE + 
                             ADMITIDO + TIPO_INS + MOD_INS + PAES + 
                             PEAMA + AREAC_SNIES ~ YEAR + SEMESTRE,
                           frecuencia = list(2010:2023, 1:2),
                           intervalo = list(c(2010, 1), c(2023, 2)),
                           textNA = "No Aplica",
                           ask = FALSE)

# Fase de Gráficos

# Serie de Tiempo General

Gen_Adm_Pre <- Adm_Pre %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% 
  mutate(Periodo = factor(paste(YEAR, SEMESTRE, sep = "-")))

ggplot(data = Gen_Adm_Pre, aes(x = Periodo, y = Total, group=1)) +
  geom_point(size = 2)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,8000))+
  #ggtitle(, subtitle = "Periodo 2020-2035\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nPeriodo")+
  labs(title = "Evolución Total de Admitidos a Pregrado en la Universidad Nacional de Colombia",
       subtitle = "Periodo 2010:2023")+
  annotate(geom="text", x="2018-1", y=7700,
            label= "7.196", color="red")+
  annotate(geom="text", x="2013-2", y=4500,
           label= "5.034", color="red")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue", angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(face="bold", color="black", size=13))


# Mapa - Por municipios de Residencia

Plot.Mapa(
  df       = Adm_Pre,
  depto    = COD_DEP_RES,
  mpio     = COD_CIU_RES,
  tipo     = "SiNoMpios",
  zoomIslas = FALSE,
  SiNoLegend = c("Sí", "No"),
  #centroideMapa = c("ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"),
  titulo   = "Municipios de Residencia con Admitidos a la\nUniversidad Nacional de Colombia",
  colores  = c("red", "#10F235"),
  opacidad = 1,
  textSize = 0,
  limpio   = FALSE,
  estatico = TRUE,
  estilo   = list(
    Style = "SiNo", Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Labs  = list(fill = "¿Admitidos?", subtitle = "Periodo 2010:2023\n"),
    Legend = list(legend.direction = "vertical")))

# Mapa - Por municipios de Residencia - 2023

Plot.Mapa(
  df       = Adm_Pre %>% filter(YEAR == 2023),
  depto    = COD_DEP_RES,
  mpio     = COD_CIU_RES,
  tipo     = "SiNoMpios",
  zoomIslas = FALSE,
  SiNoLegend = c("Sí", "No"),
  #centroideMapa = c("ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"),
  titulo   = "Municipios de Residencia con Admitidos a la\nUniversidad Nacional de Colombia",
  colores  = c("red", "#10F235"),
  opacidad = 1,
  textSize = 0,
  limpio   = FALSE,
  estatico = TRUE,
  estilo   = list(
    Style = "SiNo", Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Labs  = list(fill = "¿Admitidos?", subtitle = "Año 2023\n"),
    Legend = list(legend.direction = "vertical")))


# Tabla - Cobertura UNAL municipios en condiciones difíciles de absorción

# Crear base de datos

Adm_Pre_Unico <- Adm_Pre %>% filter(YEAR == 2023) %>% 
  summarise(Total = n(), .by = c(COD_CIU_RES)) %>% 
  mutate(Asp_UNAL = "Sí")

Municipios <- read_excel("Fuentes/Divipola.xlsx", 
                         sheet = "Hoja1",
                         guess_max = 1000) %>% 
  summarise(Municipio = max(Municipio), .by = c(COD_DEPT , COD_MUN ))

Municipios1 <- Municipios %>% 
  left_join(Adm_Pre_Unico, by = join_by(COD_MUN == COD_CIU_RES)) %>% 
  left_join(Mun_DificilES, by = join_by(COD_MUN == COD_MPIO)) %>% 
  mutate(Asp_UNAL = ifelse(is.na(Asp_UNAL), "No", Asp_UNAL),
         DificilES = ifelse(is.na(DificilES), "No", DificilES)) %>% 
  filter(DificilES == "Sí")

message("Cobertura UNAL municipios difícil acceso ES")
table(Municipios1$Asp_UNAL)

# Sexo
# Serie de Tiempo

Adm_Consolidada %>% filter(Variable == "SEXO") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Sexo", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = Sexo, group = Sexo)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,6000))+
  ggtitle("Evolución Total Admitidos a Pregrado en la Universidad Nacional de Colombia, \npor Sexo", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Adm_Consolidada %>% 
  Plot.Barras(categoria = "SEXO",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 70),
              labelY = "Porcentaje de Admitidos (%)\n",
              labelX = "\nSexo",
              titulo = "Proporción de Admitidos a Pregrado en la UNAL por Sexo",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))


# Estrato
# Serie de Tiempo

Adm_Consolidada %>% filter(Variable == "ESTRATO") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Estrato", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = Estrato, group = Estrato)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,5000))+
  ggtitle("Evolución Total Admitidos a Pregrado en la Universidad Nacional de Colombia, \npor Estrato", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Adm_Consolidada %>% 
  Plot.Barras(categoria = "ESTRATO",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 60),
              labelY = "Porcentaje de Admitidos (%)\n",
              labelX = "\nEstrato",
              titulo = "Proporción de Admitidos a Pregrado en la UNAL por Estrato",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Por Sedes
# Serie de Tiempo

Adm_Consolidada %>% filter(Variable == "INS_SEDE_NOMBRE", YEAR >= 2015) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Sede", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = Sede, group = Sede)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,4000))+
  ggtitle("Evolución Total de Admitidos a Pregrado en la Universidad Nacional de Colombia, \npor Sedes", subtitle = "Periodo 2015-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Adm_Consolidada %>% filter(YEAR >= 2015) %>% 
  Plot.Barras(categoria = "INS_SEDE_NOMBRE",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 55),
              vertical = FALSE,
              labelY = "\nPorcentaje de Admitidos (%)",
              labelX = "Sede de la Universidad\n",
              titulo = "Proporción de Admitidos a Pregrado en la UNAL por Sedes",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.7),
                            gg.Texto = list(subtitle = "Periodo 2015:2023",
                                            caption  = "\nNo Aplica. Hace referencia a estudiantes admitidos\n a pregrado a la Universidad de manera global")))


# Modalidad de inscripción (REGULAR y ESPECIAL)
# Serie de tiempo

Adm_Consolidada %>% filter(Variable == "MOD_INS") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Modalidad", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Modalidad`, group = `Modalidad`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,7000))+
  ggtitle("Evolución Total Admitidos a Pregrado en la Universidad Nacional de Colombia, \npor Modalidad de Inscripción", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Adm_Consolidada %>% 
  Plot.Barras(categoria = "MOD_INS",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 90),
              labelY = "Porcentaje de Admitidos (%)\n",
              labelX = "\nModalidad de Inscripción",
              titulo = "Proporción de Admitidos a Pregrado en la UNAL por Modalidad de Inscripción",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Modalidad de inscripción (REGULAR, PAES, PEAMA)
# Serie de tiempo

Adm_Consolidada %>% filter(Variable == "TIPO_INS", Clase != "Regular") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Programa", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Programa`, group = `Programa`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,1100))+
  ggtitle("Evolución Total de Admitidos a Pregrado en la Universidad Nacional de Colombia, \npor Programa de Inscripción Especial", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Adm_Consolidada %>% filter(Clase != "Regular") %>% 
  Plot.Barras(categoria = "TIPO_INS",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 70),
              labelY = "Porcentaje de Admitidos (%)\n",
              labelX = "\nPrograma",
              titulo = "Proporción de Admitidos a Pregrado en la UNAL por \nPrograma de Inscripción Especial",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Modalidad de inscripción (PAES)
# Serie de tiempo

Adm_Consolidada %>% filter(Variable == "PAES", !Clase %in% c("No Aplica", "De La Paz")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Opciones PAES", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Opciones PAES`, group = `Opciones PAES`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0, 400))+
  ggtitle("Evolución Total de Admitidos a Pregrado en la Universidad Nacional de Colombia, \nPrograma PAES", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Modalidad de inscripción (Indígenas y Víctimas)
# Serie de tiempo

Adm_Consolidada %>% filter(Variable == "PAES", Clase %in% c("Comunidades indígenas", "Victimas del conflicto armado interno en Colombia")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Opciones PAES", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Opciones PAES`, group = `Opciones PAES`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0, 400))+
  ggtitle("Evolución Total de Admitidos a Pregrado en la Universidad Nacional de Colombia, \nPrograma PAES", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")

# Transversal

Adm_Consolidada %>% filter(Variable == "PAES", !Clase %in% c("No Aplica", "De La Paz")) %>%
  Plot.Barras(categoria = "PAES",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 40),
              labelY = "\nPorcentaje de Admitidos (%)\n",
              labelX = "\nOpciones Programa PAES\n",
              vertical = FALSE,
              titulo = "Proporción de Admitidos a Pregrado en la UNAL \nPrograma PAES",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Modalidad de inscripción (PEAMA)
# Serie de tiempo

Adm_Consolidada %>% filter(Variable == "PEAMA", !Clase %in% c("No Aplica")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Opciones PEAMA", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Opciones PEAMA`, group = `Opciones PEAMA`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,300))+
  ggtitle("Evolución Total de Admitidos a Pregrado en la Universidad Nacional de Colombia, \nPrograma PEAMA", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Adm_Consolidada %>% filter(Variable == "PEAMA", !Clase %in% c("No Aplica")) %>%
  Plot.Barras(categoria = "PEAMA",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 40),
              labelY = "\nPorcentaje de Admitidos (%)",
              labelX = "\nModalidades PEAMA\n",
              vertical = FALSE,
              titulo = "Proporción de Admitidos a Pregrado en la UNAL, \nPrograma PEAMA",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Mapa - Por municipios de Residencia Especial

Adm_Pre_Especial <- Adm_Pre %>% filter(MOD_INS == "Especial") %>% 
  summarise(Total = n(), .by = c(COD_DEP_RES, COD_CIU_RES))


Plot.Mapa(
  df       = Adm_Pre_Especial,
  #dpto     = COD_DEP_RES,
  mpio     = COD_CIU_RES,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Municipios de Residencia de Admitidos Inscritos \na Pregrado de Manera Especial en la Universidad \nNacional de Colombia",
  naTo0    = TRUE,
  #centroideMapa = c("TOLIMA"),
  cortes        = c(0, 20, 100, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total Admitidos", subtitle = "Periodo 2010:2023\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps


ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.31, y = 0.27, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.36, y = 0.29, width = 0.055)

# Mapa - Por municipios de Residencia Especial - PAES

Adm_Pre_PAES <- Adm_Pre %>% filter(MOD_INS == "Especial", TIPO_INS == "PAES") %>% 
  summarise(Total = n(), .by = c(COD_DEP_RES, COD_CIU_RES))


Plot.Mapa(
  df       = Adm_Pre_PAES,
  mpio     = COD_CIU_RES,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Municipios de Residencia de Admitidos \na Pregrado del Programa PAES en la UNAL",
  naTo0    = TRUE,
  #centroideMapa = c("NARIÑO"),
  cortes        = c(0, 10, 50, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total Admitidos PAES", subtitle = "Periodo 2010:2023\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps


ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.31, y = 0.27, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.36, y = 0.29, width = 0.055)

# Mapa - Por municipios de Residencia Especial - PEAMA

Adm_Pre_PEAMA <- Adm_Pre %>% filter(MOD_INS == "Especial", TIPO_INS == "PEAMA") %>% 
  summarise(Total = n(), .by = c(COD_DEP_RES, COD_CIU_RES))


Plot.Mapa(
  df       = Adm_Pre_PEAMA,
  mpio     = COD_CIU_RES,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Municipios de Residencia de Admitidos a \nPregrado del Programa PEAMA en la UNAL",
  naTo0    = TRUE,
  #centroideMapa = c("TOLIMA"),
  cortes        = c(0, 10, 50, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total Admitidos PEAMA", subtitle = "Periodo 2010:2023\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps


ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.31, y = 0.27, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.36, y = 0.29, width = 0.055)

#Por áreas del Conocimiento

# Serie de tiempo

Adm_Consolidada %>% filter(Variable == "AREAC_SNIES", !Clase %in% c("No Aplica", "Sin información")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Área SNIES", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Área SNIES`, group = `Área SNIES`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,3500))+
  ggtitle("Evolución Total de Admitidos a Pregrado en la Universidad Nacional de Colombia, \npor Áreas del Conocimiento", subtitle = "Periodo 2010-2023\n")+
  ylab("\n Admitidos\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Adm_Consolidada %>% filter(!Clase %in% c("No Aplica", "Sin información")) %>%
  Plot.Barras(categoria = "AREAC_SNIES",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 53),
              labelY = "\nPorcentaje de Admitidos (%)",
              labelX = "\nÁrea del Conocimiento SNIES\n",
              vertical = FALSE,
              titulo = "Proporción de Admitidos a Pregrado en la UNAL \npor Áreas del Conocimiento",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2023")))

# Matriculados PVEZ ----

# Base Matriculados Primera Vez pregrado UNAL

Mpvez_Pre <- UnalData::Matriculados %>% 
  filter(YEAR >= 2010,
         TIPO_NIVEL == "Pregrado", 
         MAT_PVEZ == "Sí") 


# Base Agregada

Mpvez_Consolidada <- Agregar(datos = Mpvez_Pre,
                           formula = SEXO + ESTRATO + SEDE_NOMBRE_MAT + 
                           + MOD_ADM + TIPO_ADM + PAES + PEAMA + 
                             AREAC_SNIES ~ YEAR + SEMESTRE,
                           frecuencia = list(2010:2022, 1:2),
                           intervalo = list(c(2010, 1), c(2022, 2)),
                           textNA = "No Aplica",
                           ask = FALSE)

# Fase de Gráficos

# Serie de Tiempo General

Gen_Mpvez_Pre <- Mpvez_Pre %>% 
  summarise(Total = n(), .by = c(YEAR, SEMESTRE)) %>% 
  mutate(Periodo = factor(paste(YEAR, SEMESTRE, sep = "-")))

ggplot(data = Gen_Mpvez_Pre, aes(x = Periodo, y = Total, group=1)) +
  geom_point(size = 2)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,8000))+
  #ggtitle(, subtitle = "Periodo 2020-2035\n")+
  ylab("\n Total de Matriculados Primera Vez\n ")+
  xlab("\nPeriodo")+
  labs(title = "Evolución Total de Matriculados Primera Vez en Pregrado \nUniversidad Nacional de Colombia",
       subtitle = "Periodo 2010:2022")+
  annotate(geom="text", x="2010-1", y=5500,
           label= "  5096", color="red")+
  annotate(geom="text", x="2022-2", y=4100,
           label= "4522", color="red")+
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue", angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_text(face="bold", color="black", size=13))

# Mapa - Por municipios de Residencia

Plot.Mapa(
  df       = Mpvez_Pre,
  depto    = COD_DEP_PROC,
  mpio     = COD_CIU_PROC,
  tipo     = "SiNoMpios",
  zoomIslas = FALSE,
  SiNoLegend = c("Sí", "No"),
  #centroideMapa = c("ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"),
  titulo   = "Municipios de Residencia con Matriculados Primera \nVez en la Universidad Nacional de Colombia",
  colores  = c("red", "#10F235"),
  opacidad = 1,
  textSize = 0,
  limpio   = FALSE,
  estatico = TRUE,
  estilo   = list(
    Style = "SiNo", Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Labs  = list(fill = "¿Matriculados \nPrimera Vez?", subtitle = "Periodo 2010:2022\n"),
    Legend = list(legend.direction = "vertical")))

# Mapa - Por municipios de Residencia - 2023

Plot.Mapa(
  df       = Mpvez_Pre %>% filter(YEAR == 2022),
  depto    = COD_DEP_PROC,
  mpio     = COD_CIU_PROC,
  tipo     = "SiNoMpios",
  zoomIslas = FALSE,
  SiNoLegend = c("Sí", "No"),
  #centroideMapa = c("ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA"),
  titulo   = "Municipios de Residencia con Matriculados Primera \nVez en la Universidad Nacional de Colombia",
  colores  = c("red", "#10F235"),
  opacidad = 1,
  textSize = 0,
  limpio   = FALSE,
  estatico = TRUE,
  estilo   = list(
    Style = "SiNo", Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Labs  = list(fill = "¿Matriculados \nPrimera Vez?", subtitle = "Año 2022\n"),
    Legend = list(legend.direction = "vertical")))


# Tabla - Cobertura UNAL municipios en condiciones difíciles de absorción

# Crear base de datos

Mpvez_Pre_Unico <- Mpvez_Pre %>% filter(YEAR == 2022) %>% 
  summarise(Total = n(), .by = c(COD_CIU_PROC)) %>% 
  mutate(Asp_UNAL = "Sí")

Municipios <- read_excel("Fuentes/Divipola.xlsx", 
                         sheet = "Hoja1",
                         guess_max = 1000) %>% 
  summarise(Municipio = max(Municipio), .by = c(COD_DEPT , COD_MUN ))

Municipios1 <- Municipios %>% 
  left_join(Mpvez_Pre_Unico, by = join_by(COD_MUN == COD_CIU_PROC)) %>% 
  left_join(Mun_DificilES, by = join_by(COD_MUN == COD_MPIO)) %>% 
  mutate(Asp_UNAL = ifelse(is.na(Asp_UNAL), "No", Asp_UNAL),
         DificilES = ifelse(is.na(DificilES), "No", DificilES)) %>% 
  filter(DificilES == "Sí")

message("Cobertura UNAL municipios difícil acceso ES")
table(Municipios1$Asp_UNAL)

# Sexo
# Serie de Tiempo

Mpvez_Consolidada %>% filter(Variable == "SEXO") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Sexo", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = Sexo, group = Sexo)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,4000))+
  ggtitle("Evolución Total Matriculados Primera Vez en Pregrado en la Universidad Nacional de Colombia, \npor Sexo", subtitle = "Periodo 2010-2022\n")+
  ylab("\n Matriculados Primera Vez\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Mpvez_Consolidada %>% 
  Plot.Barras(categoria = "SEXO",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 70),
              labelY = "Porcentaje de Matriculados Primera Vez (%)\n",
              labelX = "\nSexo",
              titulo = "Proporción de Matriculados Primera Vez en Pregrado en la UNAL por Sexo",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2022")))


# Estrato
# Serie de Tiempo

Mpvez_Consolidada %>% filter(Variable == "ESTRATO") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Estrato", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = Estrato, group = Estrato)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,4000))+
  ggtitle("Evolución Total Matriculados Primera Vez en Pregrado en la Universidad Nacional de Colombia, \npor Estrato", subtitle = "Periodo 2010-2022\n")+
  ylab("\n Matriculados Primera Vez\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Mpvez_Consolidada %>% 
  Plot.Barras(categoria = "ESTRATO",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 60),
              labelY = "Porcentaje de Matriculados Primera Vez (%)\n",
              labelX = "\nEstrato",
              titulo = "Proporción de Matriculados Primera Vez en Pregrado en la UNAL por Estrato",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2022")))

# Por Sedes
# Serie de Tiempo

Mpvez_Consolidada %>% filter(Variable == "SEDE_NOMBRE_MAT") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Sede", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = Sede, group = Sede)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,3500))+
  ggtitle("Evolución Total de Matriculados Primera Vez en Pregrado en la Universidad Nacional de Colombia, \npor Sedes", subtitle = "Periodo 2010-2022\n")+
  ylab("\n Matriculados Primera Vez\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Mpvez_Consolidada %>%  
  Plot.Barras(categoria = "SEDE_NOMBRE_MAT",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 55),
              vertical = FALSE,
              labelY = "\nPorcentaje de Matriculados Primera Vez (%)",
              labelX = "Sede de la Universidad\n",
              titulo = "Proporción de Matriculados Primera Vez en Pregrado en la UNAL por Sedes",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.7),
                            gg.Texto = list(subtitle = "Periodo 2010:2022")))


# Modalidad de inscripción (REGULAR y ESPECIAL)
# Serie de tiempo

Mpvez_Consolidada %>% filter(Variable == "MOD_ADM") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Modalidad", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Modalidad`, group = `Modalidad`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,5000))+
  ggtitle("Evolución Total Matriculados Primera Vez en Pregrado en la Universidad Nacional de Colombia, \npor Modalidad de Inscripción", subtitle = "Periodo 2010-2022\n")+
  ylab("\n Matriculados Primera Vez\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Mpvez_Consolidada %>% 
  Plot.Barras(categoria = "MOD_ADM",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 90),
              labelY = "Porcentaje de Matriculados Primera Vez (%)\n",
              labelX = "\nModalidad de Inscripción",
              titulo = "Proporción de Matriculados Primera Vez en Pregrado en la UNAL por Modalidad de Inscripción",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2022")))

# Modalidad de inscripción (REGULAR, PAES, PEAMA)
# Serie de tiempo

Mpvez_Consolidada %>% filter(Variable == "TIPO_ADM", Clase != "Regular") %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Programa", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Programa`, group = `Programa`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,800))+
  ggtitle("Evolución Total de Matriculados Primera Vez en Pregrado en la Universidad Nacional de Colombia, \npor Tipo de Inscripción Especial", subtitle = "Periodo 2010-2022\n")+
  ylab("\n Matriculados Primera Vez\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Mpvez_Consolidada %>% filter(Clase != "Regular") %>% 
  Plot.Barras(categoria = "TIPO_ADM",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 70),
              labelY = "Porcentaje de Matriculados Primera Vez (%)\n",
              labelX = "\nPrograma",
              titulo = "Proporción de Matriculados Primera Vez en Pregrado en la UNAL por \nTipo de Inscripción Especial",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2022")))

# Modalidad de inscripción (PAES)
# Serie de tiempo

Mpvez_Consolidada %>% filter(Variable == "PAES", !Clase %in% c("No Aplica", "De La Paz")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Opciones PAES", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Opciones PAES`, group = `Opciones PAES`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0, 300))+
  ggtitle("Evolución Total de Matriculados Primera Vez en Pregrado en la Universidad Nacional de Colombia, \nPrograma PAES", subtitle = "Periodo 2010-2022\n")+
  ylab("\n Matriculados Primera Vez\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Modalidad de inscripción (Indígenas y Víctimas)
# Serie de tiempo

Mpvez_Consolidada %>% filter(Variable == "PAES", Clase %in% c("Comunidades indígenas", "Victimas del conflicto armado interno en Colombia")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Opciones PAES", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Opciones PAES`, group = `Opciones PAES`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0, 300))+
  ggtitle("Evolución Total de Matriculados Primera Vez en Pregrado en la Universidad Nacional de Colombia, \nPrograma PAES", subtitle = "Periodo 2010-2022\n")+
  ylab("\n Matriculados Primera Vez\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom")

# Transversal

Mpvez_Consolidada %>% filter(Variable == "PAES", !Clase %in% c("No Aplica", "De La Paz")) %>%
  Plot.Barras(categoria = "PAES",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 40),
              labelY = "\nPorcentaje de Matriculados Primera Vez (%)\n",
              labelX = "\nOpciones Programa PAES\n",
              vertical = FALSE,
              titulo = "Proporción de Matriculados Primera Vez en Pregrado en la UNAL \nPrograma PAES",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2022")))

# Modalidad de inscripción (PEAMA)
# Serie de tiempo

Mpvez_Consolidada %>% filter(Variable == "PEAMA", !Clase %in% c("No Aplica")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Opciones PEAMA", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Opciones PEAMA`, group = `Opciones PEAMA`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,250))+
  ggtitle("Evolución Total de Matriculados Primera Vez en Pregrado en la Universidad Nacional de Colombia, \nPrograma PEAMA", subtitle = "Periodo 2010-2022\n")+
  ylab("\n Matriculados Primera Vez\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Mpvez_Consolidada %>% filter(Variable == "PEAMA", !Clase %in% c("No Aplica")) %>%
  Plot.Barras(categoria = "PEAMA",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 43),
              labelY = "\nPorcentaje de Matriculados Primera Vez (%)",
              labelX = "\nModalidades PEAMA\n",
              vertical = FALSE,
              titulo = "Proporción de Matriculados Primera Vez en Pregrado en la UNAL, \nPrograma PEAMA",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.55),
                            gg.Texto = list(subtitle = "Periodo 2010:2022")))

# Mapa - Por municipios de Residencia Especial

Mpvez_Pre_Especial <- Mpvez_Pre %>% filter(MOD_ADM == "Especial") %>% 
  summarise(Total = n(), .by = c(COD_DEP_PROC, COD_CIU_PROC))


Plot.Mapa(
  df       = Mpvez_Pre_Especial,
  #dpto     = COD_DEP_RES,
  mpio     = COD_CIU_PROC,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Municipios de Residencia de Matriculados Primera \nVez en Pregrado admitidos de Manera Especial en la \nUniversidad Nacional de Colombia",
  naTo0    = TRUE,
  #centroideMapa = c("TOLIMA"),
  cortes        = c(0, 20, 100, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total Matriculados Primera Vez", subtitle = "Periodo 2010:2022\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps


ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.33, y = 0.25, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.38, y = 0.27, width = 0.055)

# Mapa - Por municipios de Residencia Especial - PAES

Mpvez_Pre_PAES <- Mpvez_Pre %>% filter(MOD_ADM == "Especial", TIPO_ADM == "PAES") %>% 
  summarise(Total = n(), .by = c(COD_DEP_PROC, COD_CIU_PROC))


Plot.Mapa(
  df       = Mpvez_Pre_PAES,
  mpio     = COD_CIU_PROC,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Municipios de Residencia de Matriculados Primera \nVez a Pregrado del Programa PAES en la UNAL",
  naTo0    = TRUE,
  #centroideMapa = c("NARIÑO"),
  cortes        = c(0, 10, 50, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total Matriculados Primera Vez PAES", subtitle = "Periodo 2010:2022\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps


ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.31, y = 0.27, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.36, y = 0.29, width = 0.055)

# Mapa - Por municipios de Residencia Especial - PEAMA

Mpvez_Pre_PEAMA <- Mpvez_Pre %>% filter(MOD_ADM == "Especial", TIPO_ADM == "PEAMA") %>% 
  summarise(Total = n(), .by = c(COD_DEP_PROC, COD_CIU_PROC))


Plot.Mapa(
  df       = Mpvez_Pre_PEAMA,
  mpio     = COD_CIU_PROC,
  variable = Total,
  agregado = FALSE,
  tipo     = "Mpios",
  zoomIslas = TRUE,
  titulo   = "Municipios de Residencia de Matriculados Primera \nVez en Pregrado del Programa PEAMA en la UNAL",
  naTo0    = TRUE,
  #centroideMapa = c("TOLIMA"),
  cortes        = c(0, 10, 50, Inf),  
  colores       = c("red", "yellow", "#10F235"),
  estatico = TRUE,
  estilo   = list(
    Style = "Intervalo",  Theme = 5, anchoBorde = 0.2,
    labelX = "", 
    labelY = "",
    Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    Labs  = list(fill = "Total Matriculados Primera Vez PEAMA", subtitle = "Periodo 2010:2022\n"),
    Text  = list(color = "#011532", size = 0)
  )) -> listMaps


ggdraw() +
  draw_plot(listMaps$M_COL) +
  draw_plot(listMaps$M_SanAndres  , x = 0.33, y = 0.27, width = 0.060) +
  draw_plot(listMaps$M_Providencia, x = 0.38, y = 0.29, width = 0.055)

#Por áreas del Conocimiento

# Serie de tiempo

Mpvez_Consolidada %>% filter(Variable == "AREAC_SNIES", !Clase %in% c("No Aplica", "Sin información")) %>%
  pivot_wider(names_from = Clase, values_from = Total) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  select(-c(YEAR, SEMESTRE)) %>% 
  pivot_longer(cols = -c(Variable, Periodo), names_to = "Área SNIES", values_to = "Total") %>% 
  mutate(Total = ifelse(Total == 0, NA, Total)) %>% 
  ggplot(aes(x = Periodo, y = Total, color = `Área SNIES`, group = `Área SNIES`)) +
  geom_line() + geom_point()+
  scale_y_continuous(limits = c(0,3500))+
  ggtitle("Evolución Total de Matriculados Primera Vez en Pregrado en la Universidad Nacional de Colombia, \npor Áreas del Conocimiento", subtitle = "Periodo 2010-2022\n")+
  ylab("\n Matriculados Primera Vez\n ")+
  xlab("\n Periodo")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Transversal

Mpvez_Consolidada %>% filter(!Clase %in% c("No Aplica", "Sin información")) %>%
  Plot.Barras(categoria = "AREAC_SNIES",
              estatico = TRUE, 
              freqRelativa = TRUE,
              ylim = c(0, 53),
              labelY = "\nPorcentaje de Matriculados Primera Vez (%)",
              labelX = "\nÁrea del Conocimiento SNIES\n",
              vertical = FALSE,
              titulo = "Proporción de Matriculados Primera Vez en Pregrado en la UNAL \npor Áreas del Conocimiento",
              estilo = list(gg.Tema = 5, 
                            gg.Bar   = list(width = 0.5),
                            gg.Texto = list(subtitle = "Periodo 2010:2022")))


# Coberturas ----