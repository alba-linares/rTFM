# Cargar las librerías
library(readxl)

# Leer los archivos
setwd("D:/Escritorio/TFM/rTFM")
datos <-read_excel("Excel/MUCVA+SIOSE_areas_usos.xlsx", sheet = 1)
# head(datos)

datos[1:7] <- lapply(datos[1:7], as.factor)

datos_diferencia <- subset(datos, datos$ORIGEN=="DIFERENCIA")
modelo_lm <- lm(AREA_USOS~USOS_DEFINITIVOS+HUMEDAL, data=datos_diferencia)
summary(modelo_lm)
