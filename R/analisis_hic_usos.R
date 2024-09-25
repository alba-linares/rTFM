# OBJETIVOS
#En qué condiciones se han perdido hábitats
#Dónde se han perdido más hábitats (costa-interior, ENP-noENP e interacción)
#Variable respuesta: Presencia HIC (0/1), por ejemplo.


# Cargar las librerías
library(readxl)

# Leer los archivos
setwd("D:/Escritorio/TFM/rTFM")
datos <-read_excel("Cartografia/output/puntos_analisis/puntos_hic_zonas_naturales.xlsx", sheet = 1)
head(datos)

datos[2:9] <- lapply(datos[2:9], as.factor)
datos$CODIGO_UE2 <- lapply(datos$CODIGO_UE2, as.factor)
datos$CODIGO_UE3 <- lapply(datos$CODIGO_UE3, as.factor)
datos$CODIGO_UE4 <- lapply(datos$CODIGO_UE4, as.factor)
#datos[18:37] <- lapply(datos[18:37], as.factor)

lm_ENP_cost_int <- lm(datos$PRESENCIA_HIC~ENP*GRUPO_TIPO, contrasts=list(GRUPO_TIPO=contr.sum, ENP=contr.sum),data=datos)
summary(lm_ENP_cost_int)
# Sale diferencia significativa (***) en función de ENP
