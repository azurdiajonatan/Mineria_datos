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
datos_2023 <- read_excel("Exportaciones/bd-2023.xlsx")
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
    ANYO = ANYO,
    VALOR = `Monto_dolares`,
    PESO = `Peso_KG`,
    PAIS = `Codigo_País`,
    SAC = `Inciso_Arancelario`,
    VIA = `Codigo_Vía`,
    ADUANA = `Codigo_Aduana`
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

# Convertir SAC a carácter
history$SAC <- as.character(history$SAC)

# Convertir SAC a solamente padre
history$SAC <- str_sub(history$SAC,1,2)

capitulo <- read_excel(ruta_dic, sheet = "Detalle de capítulos")
capitulo$Capítulo <- as.character(capitulo$Capítulo) 
history <- history %>%
  left_join(capitulo, by = c("SAC" = "Capítulo"))

history$SAC <- as.numeric(history$SAC)

paises <- read_excel(ruta_dic,sheet = "País")
history <- history %>%
  left_join(paises, by = c("PAIS" = "Código")) %>%
  mutate(PAIS = País) %>% 
  select(-País)

data_history <- history

# ---------------------------------------------------------------------------------------

datos1 <- data_history[,c("ANYO","VIA")]

cluster <- kmeans(datos1, centers = 3)

ggplot(datos1, aes(x = VIA, y = ANYO, color = as.factor(cluster$cluster))) +
  geom_point() +
  geom_point(
    data = as.data.frame(cluster$centers),
    aes(x = VIA, y = ANYO),
    color = "black",
    size = 4,
    shape = 17
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Terrestre", "Aéreo", "Marítimo")
  ) +
  labs(
    title = "Via de transporte vs año",
    x = "Vía de transporte",
    y = "Año",
    color = "Cluster"
  ) +
  theme_minimal()
