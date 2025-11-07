library(readxl)
library(arules)
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(stringr)

### EXPORTACIONES 2018 - 2019 - 2020 - 2021 - 2024

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Establecer en carpeta actual
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

# Convertir SAC a carácter
history$SAC <- as.character(history$SAC)

# Rellenar con ceros a la izquierda hasta 10 dígitos
history$SAC <- stringr::str_pad(history$SAC, width = 10, side = "left", pad = "0")

# Verificar
head(history$SAC)
nchar(history$SAC) # Debe devolver 10 para todos


sac <- read_excel(ruta_dic, sheet = "SAC 6a.E")
sac <- sac %>%
  mutate(CÓDIGO = gsub("\\.","", CÓDIGO)) %>%
  group_by(CÓDIGO) %>%
  summarise(DESCRIPCIÓN = first(DESCRIPCIÓN))

history <- history %>%
  mutate(SAC_PADRE = substr(SAC, 1, 4)) %>%
  left_join(sac, by = c("SAC_PADRE" = "CÓDIGO"))

          
data_history <- history

data_history$VALOR <- cut(history$VALOR, breaks = c(1,5000,50000,500000,5000000, 95000000), 
      labels = c("Cortas","Pequeñas","Medianas","Grandes","Muy Grandes"))

data_history$PESO <- cut(history$PESO, breaks = c(1,5000,50000,500000,5000000, 95000000), 
                    labels = c("xs","s","M","L","XL"))


# CASO 1
datos <- data_history[,c("ANYO","MES","PAIS","ADUANA","VIA","VALOR","PESO")]

reglas <- fim4r(datos, method="fpgrowth", target ="rules", supp =.2, conf=.5)

# CASO 2
datos_case2 <- data_history %>%
  filter(VALOR %in% c("Grandes","Muy Grandes"))

datos <- datos_case2[,c("ANYO","PAIS","SAC_PADRE")]

reglas_case2 <- fim4r(datos_case2, method="fpgrowth", target ="rules", supp =.1, conf=.5)


