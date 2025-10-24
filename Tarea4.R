library(readxl)
library(arules)
library(ggplot2)

data <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Tarea 4\\datos.xlsx")

data_ag <- data[, c("AGR_ALFAB", "HEC_DEPTO", "AGR_EDAD", "AGR_ESCOLARIDAD", "AGR_EST_CIV", "AGR_TRABAJA", "AGR_DEDICA","AGR_SEXO")]

data_ag <- subset(data_ag, HEC_DEPTO == 1)
data_ag <- subset(data_ag, AGR_ESCOLARIDAD != 99)
data_ag <- subset(data_ag, AGR_EDAD != 99)
data_ag <- subset(data_ag, AGR_EST_CIV != 9)

data_ag[is.na(data_ag)] <- -1

cluster <- kmeans(data_ag, centers = 3)

ggplot(data_ag, aes(x = AGR_EST_CIV, y = AGR_EDAD, color =as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x = AGR_EST_CIV, y = AGR_EDAD), color="black", size=4, shape=17)+
  labs(title = "Agresores -> Estado civil vs Edad") +
  theme_minimal()


ggplot(data_ag, aes(x = AGR_SEXO, y = AGR_EDAD, color =as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x = AGR_SEXO, y = AGR_EDAD), color="black", size=4, shape=17)+
  labs(title = "Agresores -> Estado civil vs Edad") +
  theme_minimal()