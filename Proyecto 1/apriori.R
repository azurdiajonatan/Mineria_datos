library(readxl)
library(arules)
library(ggplot2)
library(dplyr)
library(stringr)

### EXPORTACIONES 2018 - 2024

# Establecer en carpeta actual
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## IMPORTAR ARCHIVO DE DICCIONARIO DE TERMINOS
ruta_dic <- "Exportaciones/diccionario.xlsx"

## IMPORTAR ARCHIVOS DE EXPORTACIONES 
datos_2018 <- read_excel("Exportaciones/bd-2018.xlsx")
datos_2019 <- read_excel("Exportaciones/bd-2019.xlsx")
datos_2020 <- read_excel("Exportaciones/bd-2020.xlsx")
datos_2021 <- read_excel("Exportaciones/bd-2021.xlsx")
datos_2022 <- read_excel("Exportaciones/bd-2022.xlsx")
datos_2023 <- read_excel("Exportaciones/bd-2022.xlsx")
datos_2024 <- read_excel("Exportaciones/bd-2024.xlsx")

#Renombrar columnas para homologar con los demás archivos
datos_2022 <- datos_2022 %>%
  rename(
    ANYO = AÑO,
    VALOR = `MONTO EN DÓLARES`,
    PESO = `PESO KILOGRAMOS`,
    PAIS = PAÍS
  )  %>%
  select(ANYO,SAC,PAIS,ADUANA,VIA,VALOR,PESO)

datos_2023 <- datos_2023 %>%
  rename(
    ANYO = AÑO,
    VALOR = `MONTO EN DÓLARES`,
    PESO = `PESO KILOGRAMOS`,
    PAIS = PAÍS
  )  %>%
  select(ANYO,SAC,PAIS,ADUANA,VIA,VALOR,PESO)


datos_2018 <- select(datos_2018, -MES)
datos_2019 <- select(datos_2019, -MES)
datos_2020 <- select(datos_2020, -MES)
datos_2021 <- select(datos_2021, -MES)
datos_2024 <- select(datos_2024, -MES)


history <- bind_rows(datos_2018,datos_2019,datos_2020,datos_2021,datos_2022,datos_2023,datos_2024)

history$SAC <- format(history$SAC, scientific = FALSE)

## SE REMUEVEN AQUELLAS ADUANAS QUE NO SE ENCUENTRAN DENTRO DEL CATALOGO DE ADUANAS
history <- subset(history, ADUANA < 100)

# PAISES
paises <- read_excel(ruta_dic,sheet = "País")
history <- history %>%
  left_join(paises, by = c("PAIS" = "Código")) %>%
  mutate(PAIS = País) %>% 
  select(-País)

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

# Convertir SAC a carácter
history$SAC <- as.character(history$SAC)

# Convertir SAC a solamente padre
history$SAC <- str_sub(history$SAC,1,2)

capitulo <- read_excel(ruta_dic, sheet = "Detalle de capítulos")
capitulo$Capítulo <- as.character(capitulo$Capítulo) 
history <- history %>%
  left_join(capitulo, by = c("SAC" = "Capítulo"))


data_history <- history

data_history$VALOR <- cut(history$VALOR, breaks = c(1,5000,50000,500000,5000000, 95000000), 
                          labels = c("Cortas","Pequeñas","Medianas","Grandes","Muy Grandes"))

data_history$PESO <- cut(history$PESO, breaks = c(1,5000,50000,500000,5000000, 95000000), 
                         labels = c("xs","s","M","L","XL"))

View(data_history)

#Caso 1
data_case1 <- data_history[,c("Continente","SAC")]
reglas1 <- apriori(data_case1, parameter = list(support=0.02,confidence= 0.5))
inspect(reglas1)

data_case1_1 <- data_history[,c("Continente","VALOR")]
reglas1_1 <- apriori(data_case1_1, parameter = list(support=0.1,confidence= 0.5))
inspect(reglas1_1)

#Caso 2
data_case2 <- data_history %>%
  filter(Continente != "América") %>%
  select(Continente, VALOR, PESO)
reglas2 <- apriori(data_case2, parameter = list(support=0.2,confidence= 0.4))
inspect(reglas2)

data_case2_1 <- data_history %>%
  filter(Continente != "América") %>%
  select(Continente, SAC)
reglas2_1 <- apriori(data_case2_1, parameter = list(support=0.005,confidence= 0.7))
inspect(reglas2_1)

#CASO 3

data_case3 <- data_history %>%
  filter(PESO %in% c("L", "XL")) %>%
  select(PESO, PAIS, VIA)
reglas3 <- apriori(data_case3, parameter = list(support=0.1,confidence= 0.6))
inspect(reglas3)



# CASO 4

data_case4 <- data_history[,c("ANYO","VIA")]
reglas4 <- apriori(data_case4, parameter = list(support=0.01,confidence= 0.6))
inspect(reglas4)
