library(readxl)
library(rpart)
library(rpart.plot)
library(dplyr)

datos <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Tarea 5\\base.xlsx")

datos <- datos %>% filter(AGR_EST_CIV != 9)

arbol <- rpart(AGR_EST_CIV ~
              AGR_EDAD +
              AGR_TRABAJA +
              AGR_ESCOLARIDAD,
              data = datos, method = "class")

rpart.plot(arbol, type = 2, extra = 0, under = TRUE, 
           fallen.leaves = TRUE, box.palette = "BuGn",
           main = "Agresor estado civil", cex = 0.5)

hecho <- data.frame(
  AGR_EDAD = c(25),
  AGR_TRABAJA = c(2),
  AGR_ESCOLARIDAD = c(49)
)

result <- predict(arbol, hecho, type = "prob")
result