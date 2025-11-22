# install.packages("randomForest")
library(readxl)
library(randomForest)

datos <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Tarea 6\\base.xlsx")

data <- datos[, c("AGR_EDAD","AGR_ESCOLARIDAD","AGR_TRABAJA","AGR_DEDICA","AGR_EST_CIV","AGR_NACIONAL")]

data <- subset(data, AGR_EST_CIV != 9 )
View(data)

data$AGR_EST_CIV <- as.factor(data$AGR_EST_CIV)

data <- na.omit(data)

set.seed(300)

data <- data[sample(1:nrow(data)),]

index <- sample(1:nrow(data),0.70*nrow(data))

train <- data[index,]
test <- data[-index,]

bosque <- randomForest(AGR_EST_CIV ~
                 AGR_EDAD +
                 AGR_TRABAJA +
                 AGR_ESCOLARIDAD + 
                 AGR_DEDICA +
                 AGR_NACIONAL,
               data = train, ntree = 200, mtry = 4)

prueba <- predict(bosque, test)
prueba

matriz <- table(test$AGR_EST_CIV, prueba)
matriz

preci <- sum(diag(matriz))/ sum(matriz)
preci

hecho <- data.frame(
  AGR_EDAD = c(25),
  AGR_TRABAJA = c(2),
  AGR_ESCOLARIDAD = c(49),
  AGR_DEDICA = c(6),
  AGR_NACIONAL = c(1)
)

result <- predict(bosque, hecho, type = "prob")
result

plot(bosque)