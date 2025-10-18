library(readxl)
library(arules)

data <- read_excel("C:\\Users\\jonat\\Documents\\Maestria R\\Trimestre 2\\Tarea 3\\datos.xlsx")

data_fp <- data[, c("AGR_ALFAB", "HEC_DEPTO", "AGR_EDAD", "AGR_ESCOLARIDAD", "AGR_EST_CIV", "AGR_TRABAJA", "AGR_DEDICA")]

reglas_fp <- fim4r(data_fp, method="fpgrowth", target ="rules", supp =.2, conf=.5)

rf <- as(reglas_fp, "data.frame")

data_fp_ag <- subset(data_fp, AGR_TRABAJA != 9)
data_fp_ag <- subset(data_fp_ag, AGR_ALFAB == 2)
data_fp_ag <- subset(data_fp_ag, HEC_DEPTO != 1)


data.frame(1:ncol(data_fp_ag), colnames(data_fp_ag))
data_fp_ag <- data_fp_ag[, -1]
data_fp_ag <- data_fp_ag[, -3]

reglas_fp_ag <- fim4r(data_fp_ag, method="fpgrowth", target ="rules", supp =.2, conf=.5)

rf_ag <- as(reglas_fp_ag, "data.frame")



# CASO 2

data_2 <- data[, c("AGR_ALFAB" ,"HEC_DEPTO", "AGR_EDAD", "AGR_ESCOLARIDAD", "AGR_EST_CIV", "AGR_TRABAJA", "AGR_DEDICA")]

data_ag_2 <- subset(data_2, AGR_TRABAJA != 9)
data_ag_2 <- subset(data_ag_2, AGR_ALFAB == 1)
data_ag_2 <- subset(data_ag_2, HEC_DEPTO == 1)

data.frame(1:ncol(data_ag_2), colnames(data_ag_2))
data_ag_2 <- data_ag_2[, -1]
data_ag_2 <- data_ag_2[, -1]


reglas_2 <- fim4r(data_ag_2, method="fpgrowth", target ="rules", supp =.2, conf=.5)

rf_2 <- as(reglas_2, "data.frame")


# CASO

data_3 <- data[, c("HEC_DEPTO", "VIC_SEXO", "VIC_EDAD", "VIC_ESCOLARIDAD", "VIC_TRABAJA", "VIC_DEDICA")]

data_ag_3 <- subset(data_3, VIC_SEXO == 2)
data_ag_3 <- subset(data_ag_3, HEC_DEPTO == 1)
data.frame(1:ncol(data_ag_3), colnames(data_ag_3))
data_ag_3 <- data_ag_3[, -1]
data_ag_3 <- data_ag_3[, -1]

reglas_3 <- fim4r(data_ag_3, method="fpgrowth", target ="rules", supp =.2, conf=.5)

rf_3 <- as(reglas_3, "data.frame")


