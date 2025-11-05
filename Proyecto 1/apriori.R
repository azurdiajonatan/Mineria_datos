library(readxl)
library(arules)
library(ggplot2)
library(dplyr)


### EXPORTACIONES 2018 - 2019 - 2020 - 2021 - 2024

## IMPORTAR ARCHIVOS DE EXPORTACIONES 
datos_2018 <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Proyecto 1\\Mineria\\Exportaciones\\bd-2018.xlsx")
datos_2019 <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Proyecto 1\\Mineria\\Exportaciones\\bd-2019.xlsx")
datos_2020 <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Proyecto 1\\Mineria\\Exportaciones\\bd-2020.xlsx")
datos_2021 <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Proyecto 1\\Mineria\\Exportaciones\\bd-2021.xlsx")
datos_2024 <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Proyecto 1\\Mineria\\Exportaciones\\bd-2024.xlsx")

## IMPORTAR ARCHIVO DE DICCIONARIO DE TERMINOS
ruta_dic <- "C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Proyecto 1\\Mineria\\Exportaciones\\diccionario.xlsx"

##  VARIABLES 
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")


# SE UNEN LOS GRUPOS DE DATOS EN UNA SOLA VARIABLES
history <- bind_rows(datos_2018,datos_2019,datos_2020,datos_2021,datos_2024)

## ASIGNACION DE VALORES

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


reglas <- apriori(history, parameter = list(support=0.2, confidence = 0.5)) 
