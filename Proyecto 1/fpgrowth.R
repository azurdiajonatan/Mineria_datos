library(readxl)
library(arules)
library(ggplot2)
library(dplyr)


### EXPORTACIONES 2018 - 2019 - 2020 - 2021 - 2024

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

## IMPORTAR ARCHIVO DE DICCIONARIO DE TERMINOS
ruta_dic <- "C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Proyecto 1\\Mineria\\Exportaciones\\diccionario.xlsx"

## IMPORTAR ARCHIVOS DE EXPORTACIONES 
datos_2024 <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Proyecto 1\\Mineria\\Exportaciones\\bd-2024.xlsx")

## SE REMUEVEN AQUELLAS ADUANAS QUE NO SE ENCUENTRAN DENTRO DEL CATALOGO DE ADUANAS
history <- subset(datos_2024, ADUANA < 100)



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

datos <- history[,c("MES","PAIS","ADUANA","VIA","VALOR","PESO")]


data.frame(1:ncol(datos), colnames(datos))
datos <- datos[, -1]

reglas <- fim4r(datos, method="fpgrowth", target ="rules", supp =.2, conf=.5)

