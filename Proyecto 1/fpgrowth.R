library(readxl)
library(arules)
library(ggplot2)
library(dplyr)
library(rstudioapi)

### EXPORTACIONES 2018 - 2019 - 2020 - 2021 - 2024

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# -
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## IMPORTAR ARCHIVO DE DICCIONARIO DE TERMINOS
ruta_dic <- "Exportaciones/diccionario.xlsx"

## IMPORTAR ARCHIVOS DE EXPORTACIONES 
datos_2018 <- read_excel("Exportaciones/bd-2018.xlsx")
datos_2019 <- read_excel("Exportaciones/bd-2019.xlsx")
datos_2020 <- read_excel("Exportaciones/bd-2020.xlsx")
datos_2021 <- read_excel("Exportaciones/bd-2021.xlsx")
datos_2024 <- read_excel("Exportaciones/bd-2024.xlsx")

history <- bind_rows(datos_2018,datos_2019,datos_2020,datos_2021,datos_2024)

history$SAC <- format(history$SAC, scientific = FALSE)

## SE REMUEVEN AQUELLAS ADUANAS QUE NO SE ENCUENTRAN DENTRO DEL CATALOGO DE ADUANAS
history <- subset(history, ADUANA < 100)

# MESES
history$MES <- factor(meses[history$MES], levels = meses)

# PAISES
paises <- read_excel(ruta_dic,sheet = "País")
history <- history %>%
  left_join(paises, by = c("PAIS" = "Código")) %>%
  mutate(PAIS = País) %>% 
  select(-País,-Continente)

# VIAS
vias <- read_excel(ruta_dic,sheet = "Vías")
history <- history %>%
  left_join(vias, by = c("VIA" = "Código")) %>%
  mutate(VIA = Vías) %>%
  select(-Vías)

# ADUANA
aduanas <- read_excel(ruta_dic, sheet = "Aduanas")
history <- history %>%
  left_join(aduanas, by = c("ADUANA" = "CÓDIGO")) %>%
  mutate(ADUANA = DESCRIPCIÓN) %>%
  select(-DESCRIPCIÓN)

# SAC
sac <- read_excel(ruta_dic, sheet = "SAC 6a.E")
sac <- sac %>%
  mutate(CÓDIGO = gsub("\\.","", CÓDIGO)) %>%
  mutate(CÓDIGO = sub("^0+","",CÓDIGO))
history <- history %>%
  left_join(sac, by = c("SAC" = "CÓDIGO"))


datos <- history[,c("MES","PAIS","ADUANA","VIA","VALOR","PESO")]

reglas <- fim4r(datos, method="fpgrowth", target ="rules", supp =.2, conf=.5)

