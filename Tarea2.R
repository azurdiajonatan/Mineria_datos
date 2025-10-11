library(readxl)
library(arules)

datos <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Tarea 2\\graduados2023.xlsx")

reglas <- apriori(datos, parameter = list(support=0.2,confidence= 0.5))

data.frame(1:ncol(datos), colnames(datos))

# se remueve el anio
datos <- datos[,-1]

data.frame(1:ncol(datos), colnames(datos))

# se remueve pueblo pertenencia
datos <- datos[,-8]

data.frame(1:ncol(datos), colnames(datos))

reglas <- apriori(datos, parameter = list(support=0.2,confidence= 0.5))

inspect(reglas[0:28])

# Sexo - Mujer
datos_sexo_mujer <- subset(datos, Sexo == "Mujer" )

reglas_sexo_mujer <- apriori(datos_sexo_mujer, parameter = list(support = 0.4, confidence = 0.5))

inspect(reglas_sexo_mujer[0:13])

# Sector - publico

datos_sector_publico <- subset(datos, Sector == "Público" )

reglas_sector_publico <- apriori(datos_sector_publico, parameter = list(support = 0.3, confidence = 0.5))

inspect(reglas_sector_publico[0:17])

# Carrera - Ingeniería en Ciencias y Sistemas

datos_carrera <- subset(datos, CARRERA == "Ingeniería en Ciencias y Sistemas" )

datos_carrera_privado <- subset(datos, Sector == "Privado" )

reglas_carrera <- apriori(datos_carrera_privado, parameter = list(support = 0.4, confidence = 0.5))

inspect(reglas_carrera[0:8])
