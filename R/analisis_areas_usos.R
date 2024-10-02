# Cargar las librerías
library(readxl)

# Leer los archivos
setwd("D:/Escritorio/TFM/rTFM")
datos <-read_excel("Excel/MUCVA+SIOSE_areas_usos.xlsx", sheet = 1)
# head(datos)

datos[1:7] <- lapply(datos[1:7], as.factor)

datos_mucva <- subset(datos, datos$ORIGEN=="MUCVA1984")
datos_siose <- subset(datos, datos$ORIGEN=="SIOSE2020")
datos_diferencia <- subset(datos, datos$ORIGEN=="DIFERENCIA")
modelo_lm <- lm(AREA_USOS~USOS_DEFINITIVOS+HUMEDAL, data=datos_diferencia)
summary(modelo_lm)
#Esto quiere decir que en aquellos usos que tengan *, hay una diferencia significativa en el área entre 1984 y 2020.


datos_diferencia <- subset(datos, datos$ORIGEN=="DIFERENCIA")
modelo_lm <- lm(AREA_USOS~USOS_DEFINITIVOS+HUMEDAL, data=datos)
summary(modelo_lm)






# Áreas por usos 1984-2020 (SIOSE, MUCVA) ###############################################################
areas_usos_mucva<-as.data.frame(tapply(datos_mucva$AREA_USOS, INDEX=datos_mucva$USOS_DEFINITIVOS, FUN=sum))
colnames(areas_usos_mucva)<-"areas"
areas_usos_mucva

areas_usos_siose<-as.data.frame(tapply(datos_siose$AREA_USOS, INDEX=datos_siose$USOS_DEFINITIVOS, FUN=sum))
colnames(areas_usos_siose)<-"areas"
areas_usos_siose

areas_usos_dif<-as.data.frame(tapply(datos_diferencia$AREA_USOS, INDEX=datos_diferencia$USOS_DEFINITIVOS, FUN=sum))
colnames(areas_usos_dif)<-"areas"
areas_usos_dif

# %
total_area_siose <- sum(areas_usos_siose$areas) # Calcular el total de todas las áreas
areas_usos_siose <- areas_usos_siose %>%
  mutate(PORCENTAJE = (areas / total_area_siose) * 100) # Añadir una nueva columna con el porcentaje que representa cada uso del suelo
areas_usos_siose

total_area_mucva <- sum(areas_usos_mucva$areas) # Calcular el total de todas las áreas
areas_usos_mucva <- areas_usos_mucva %>%
  mutate(PORCENTAJE = (areas / total_area_mucva) * 100) # Añadir una nueva columna con el porcentaje que representa cada uso del suelo
areas_usos_mucva
