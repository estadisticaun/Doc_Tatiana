##%######################################################%##
#                                                          #
####    ANÁLISIS TESIS DE DOCTORADO TATINA              ####
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
  draw_plot(listMaps$MapaCOL) +
  draw_plot(listMaps$MapaSanAndres, x = .32, y = .59, width = .15, height = .35)

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
                  legend.direction = "vertical")))


# Mapas municipios con condiciones de difícil acceso a la educación superior

# Mapa Sí/No

Poblacion_Dif_SiNo <- Poblacion %>% filter(DificilES == "Sí", AREA == "Total", YEAR == 2023)

Plot.Mapa(
  df       = Poblacion_Dif_SiNo,
  depto    = COD_DEP,
  mpio     = COD_MPIO,
  tipo     = "SiNoMpios",
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
                  legend.direction = "horizontal")))

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
  ))

# Mapa de calor por capitales

Plot.Mapa(
  df       = Transito_Mpio,
  mpio     = COD_MPIO,
  variable = Capital,
  agregado = FALSE,
  tipo     = "Mpios",
  titulo   = "Tasa de Transito Inmmediata en Capitales de \n Departamentos",
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
    Labs  = list(fill = "Tasa de transito (%)", subtitle = "Año 2021\n", caption = "NA: No Aplica"),
    Text  = list(color = "#011532", size = 0)
  ))


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

Ten_Ins %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  pivot_wider(names_from = c(Modalidad), values_from = Total) %>% 
  mutate(Total = Oficial + Privado) %>% 
ggplot(aes(x = Year, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,2500000))+
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Inscritos en Educación Superior en Colombia", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Inscritos\n ")+
  xlab("\nAño") +
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
  ggtitle("Evolución Total de Inscritos en Colombia por Nivel de Fomración", subtitle = "Periodo 2010-2021\n")+
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
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom")

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

Ten_Adm %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  pivot_wider(names_from = c(Modalidad), values_from = Total) %>% 
  mutate(Total = Oficial + Privado) %>% 
  ggplot(aes(x = Year, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,1500000))+
  scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Admitidos en Educación Superior en Colombia", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Admitidos\n ")+
  xlab("\nAño") +
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
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom")

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

Ten_Mpvez %>% filter(Variable == "Sector") %>% 
  pivot_longer(c(`2010`:`2021`) ,names_to = "Year", values_to = "Total") %>% 
  mutate(Year = as.numeric(Year)) %>% 
  pivot_wider(names_from = c(Modalidad), values_from = Total) %>% 
  mutate(Total = Oficial + Privado) %>% 
  ggplot(aes(x = Year, y = Total)) +
  geom_point(size = 3)+
  geom_line() + 
  scale_y_continuous(labels = comma, limits = c(0,1100000))+
scale_x_continuous(breaks = c(2010, 2013, 2016, 2019, 2021))+
  ggtitle("Evolución Total de Matriculados en Primer Curso en Colombia", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total Matriculados\n ")+
  xlab("\nAño") +
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
  ggtitle("Evolución Total de Matriculados en Primer Curso en Colombia por Nivel de Fomración", subtitle = "Periodo 2010-2021\n")+
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
  ggtitle("Evolución Total de Matriculados en Primer Curso en Colombia por Áreas del Conocimiento", subtitle = "Periodo 2010-2021\n")+
  ylab("\n Total de Matriculados en Primer Curso\n ")+
  xlab("\nAño") +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 12, colour = "blue"),
        axis.title = element_text(face="bold", color="black", size=13),
        legend.position="bottom")

# Por Departamentos

Plot.Mapa(
  df       = Dep_Mpvez,
  depto    = COD_DEP,
  variable = Total,
  agregado = FALSE,
  tipo     = "Deptos",
  titulo   = "Total de Matriculados en Primer Curso en Colombia por Departamentos",
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

  
