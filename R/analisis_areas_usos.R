# Cargar las librerías
library(readxl)
library(car) # Anova()
library(tidyr) # %>%
library(dplyr) # mutate()

# Leer los archivos
setwd("D:/Escritorio/TFM/rTFM")
datos <-read_excel("Excel/MUCVA+SIOSE_areas_usos.xlsx", sheet = 1)
# head(datos)

datos[1:7] <- lapply(datos[1:7], as.factor)

datos_mucva <- subset(datos, datos$ORIGEN=="MUCVA1984")
datos_siose <- subset(datos, datos$ORIGEN=="SIOSE2020")
datos_dif <- subset(datos, datos$ORIGEN=="DIFERENCIA")
modelo_lm_dif <- lm(AREA_USOS~USOS_DEFINITIVOS+HUMEDAL, data=datos_dif)

Anova(modelo_lm_dif)
summary(modelo_lm_dif)

#Esto quiere decir que en aquellos usos que tengan *, hay una diferencia significativa en el área entre 1984 y 2020.

#Usos del suelo con un área significativamente diferente en el año 2020 (SIOSE)
modelo_lm2020 <- lm(AREA_USOS_BUFFER~USOS_DEFINITIVOS+HUMEDAL, data=datos_siose)
summary(modelo_lm2020)
#Cambios del área de los usos del suelo dentro del humedal
modelo_lm_hum <- lm(datos$AREA_USOS_HUMEDAL~USOS_DEFINITIVOS+HUMEDAL, data=datos_dif)
summary(modelo_lm_hum)
#Cambios del área de los usos del suelo fuera del humedal
modelo_lm_buf <- lm(datos$AREA_USOS_BUFFER~USOS_DEFINITIVOS+HUMEDAL, data=datos)
summary(modelo_lm_buf)






# Áreas por usos 1984-2020 (SIOSE, MUCVA) ESPECÍFICOS #####################################################
areas_mucva_especifico<-as.data.frame(tapply(datos_mucva$AREA_USOS, INDEX=datos_mucva$USOS_DEFINITIVOS, FUN=sum))
colnames(areas_mucva_especifico)<-"areas"
areas_mucva_especifico

areas_siose_especifico<-as.data.frame(tapply(datos_siose$AREA_USOS, INDEX=datos_siose$USOS_DEFINITIVOS, FUN=sum))
colnames(areas_siose_especifico)<-"areas"
areas_siose_especifico

areas_dif_especifico<-as.data.frame(tapply(datos_dif$AREA_USOS, INDEX=datos_dif$USOS_DEFINITIVOS, FUN=sum))
colnames(areas_dif_especifico)<-"areas"
areas_dif_especifico

# PORCENTAJE %
total_area_siose <- sum(areas_siose_especifico$areas) # Calcular el total de todas las áreas
areas_siose_especifico <- areas_siose_especifico %>%
  mutate(PORCENTAJE = (areas / total_area_siose) * 100) # Añadir una nueva columna con el porcentaje que representa cada uso del suelo
areas_siose_especifico

total_area_mucva <- sum(areas_mucva_especifico$areas) # Calcular el total de todas las áreas
areas_mucva_especifico <- areas_mucva_especifico %>%
  mutate(PORCENTAJE = (areas / total_area_mucva) * 100) # Añadir una nueva columna con el porcentaje que representa cada uso del suelo
areas_mucva_especifico

# Esto creo que no tiene sentido hacerlo:
#total_area_dif <- sum(areas_dif_especifico$areas) # Calcular el total de todas las áreas
#areas_dif_especifico <- areas_dif_especifico %>%
#  mutate(PORCENTAJE = (areas / total_area_dif) * 100) # Añadir una nueva columna con el porcentaje que representa cada uso del suelo
#areas_dif_especifico

# Áreas por usos 1984-2020 (SIOSE, MUCVA) GENERALES #####################################################
areas_mucva_general<-as.data.frame(tapply(datos_mucva$AREA_USOS, INDEX=datos_mucva$USOS_GENERAL, FUN=sum))
colnames(areas_mucva_general)<-"areas"
areas_mucva_general

areas_siose_general<-as.data.frame(tapply(datos_siose$AREA_USOS, INDEX=datos_siose$USOS_GENERAL, FUN=sum))
colnames(areas_siose_general)<-"areas"
areas_siose_general

areas_dif_general<-as.data.frame(tapply(datos_dif$AREA_USOS, INDEX=datos_dif$USOS_GENERAL, FUN=sum))
colnames(areas_dif_general)<-"areas"
areas_dif_general

#Y POR HUMEDAL
areas_mucva_gen_x_hum <- datos_mucva %>%
  group_by(HUMEDAL, USOS_GENERAL) %>%  # Agrupa por humedal y uso general
  summarise(SUMA_AREAS = sum(AREA_USOS, na.rm = TRUE))  # Sumar las áreas por grupo
areas_mucva_gen_x_hum_usos <- pivot_wider(areas_mucva_gen_x_hum,names_from = USOS_GENERAL, values_from = SUMA_AREAS)

areas_siose_gen_x_hum <- datos_siose %>%
  group_by(HUMEDAL, USOS_GENERAL) %>%  # Agrupa por humedal y uso general
  summarise(SUMA_AREAS = sum(AREA_USOS, na.rm = TRUE))  # Sumar las áreas por grupo
areas_siose_gen_x_hum_usos <- pivot_wider(areas_siose_gen_x_hum,names_from = USOS_GENERAL, values_from = SUMA_AREAS)

areas_dif_gen_x_hum <- datos_dif %>%
  group_by(HUMEDAL, USOS_GENERAL) %>%  # Agrupa por humedal y uso general
  summarise(SUMA_AREAS = sum(AREA_USOS, na.rm = TRUE))  # Sumar las áreas por grupo
areas_dif_gen_x_hum_usos <- pivot_wider(areas_dif_gen_x_hum,names_from = USOS_GENERAL, values_from = SUMA_AREAS)

# GRÁFICO DE USOS GENERALES POR HUMEDAL ########################################
library(pheatmap)
# Estos gráficos representan los usos del suelo con un área significativamente distinta:
areas_siose_gen_x_hum_usos$HUMEDAL<-as.factor(areas_siose_gen_x_hum_usos$HUMEDAL)
pheatmap(areas_siose_gen_x_hum_usos[2:6],
         labels_row = areas_siose_gen_x_hum_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)

areas_mucva_gen_x_hum_usos$HUMEDAL<-as.factor(areas_mucva_gen_x_hum_usos$HUMEDAL)
pheatmap(areas_mucva_gen_x_hum_usos[2:6],
         labels_row = areas_mucva_gen_x_hum_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)

# Este gráfico representa los cambios de usos del suelo significativamente distintos:
areas_dif_gen_x_hum_usos$HUMEDAL<-as.factor(areas_dif_gen_x_hum_usos$HUMEDAL)
pheatmap(areas_dif_gen_x_hum_usos[2:6],
         labels_row = areas_dif_gen_x_hum_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)


################################################################################
################################################################################
# Ahora, el mismo análisis, pero para áreas de buffer y áreas de humedal por separado:


#ÁREA SOLO HUMEDAL (_H):

# Áreas por usos 1984-2020 (SIOSE, MUCVA) GENERALES (DENTRO DEL HUMEDAL) #####################################################
areas_mucva_general_H<-as.data.frame(tapply(datos_mucva$AREA_USOS_HUMEDAL, INDEX=datos_mucva$USOS_GENERAL, FUN=sum))
colnames(areas_mucva_general_H)<-"areas"
areas_mucva_general_H

areas_siose_general_H<-as.data.frame(tapply(datos_siose$AREA_USOS_HUMEDAL, INDEX=datos_siose$USOS_GENERAL, FUN=sum))
colnames(areas_siose_general_H)<-"areas"
areas_siose_general_H

areas_dif_general_H<-as.data.frame(tapply(datos_dif$AREA_USOS_HUMEDAL, INDEX=datos_dif$USOS_GENERAL, FUN=sum))
colnames(areas_dif_general_H)<-"areas"
areas_dif_general_H


#Y POR CADA HUMEDAL (SOLO DENTRO DEL HUMEDAL)
areas_mucva_gen_x_hum_H <- datos_mucva %>%
  group_by(HUMEDAL, USOS_GENERAL) %>%  # Agrupa por humedal y uso general
  summarise(SUMA_AREAS = sum(AREA_USOS_HUMEDAL, na.rm = TRUE))  # Sumar las áreas por grupo
areas_mucva_gen_x_hum_H_usos <- pivot_wider(areas_mucva_gen_x_hum_H,names_from = USOS_GENERAL, values_from = SUMA_AREAS)

areas_siose_gen_x_hum_H <- datos_siose %>%
  group_by(HUMEDAL, USOS_GENERAL) %>%  # Agrupa por humedal y uso general
  summarise(SUMA_AREAS = sum(AREA_USOS_HUMEDAL, na.rm = TRUE))  # Sumar las áreas por grupo
areas_siose_gen_x_hum_H_usos <- pivot_wider(areas_siose_gen_x_hum_H,names_from = USOS_GENERAL, values_from = SUMA_AREAS)

areas_dif_gen_x_hum_H <- datos_dif %>%
  group_by(HUMEDAL, USOS_GENERAL) %>%  # Agrupa por humedal y uso general
  summarise(SUMA_AREAS = sum(AREA_USOS_HUMEDAL, na.rm = TRUE))  # Sumar las áreas por grupo
areas_dif_gen_x_hum_H_usos <- pivot_wider(areas_dif_gen_x_hum_H,names_from = USOS_GENERAL, values_from = SUMA_AREAS)


# GRÁFICO DE USOS GENERALES POR HUMEDAL (SOLO DENTRO DE HUMEDAL) ########################################
library(pheatmap)
# Estos gráficos representan los usos del suelo (DENTRO DEL HUMEDAL):
areas_siose_gen_x_hum_H_usos$HUMEDAL<-as.factor(areas_siose_gen_x_hum_H_usos$HUMEDAL)
pheatmap(areas_siose_gen_x_hum_H_usos[2:6],
         main = "Área de los usos del suelo dentro del humedal en 2020",
         labels_row = areas_siose_gen_x_hum_H_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)

areas_mucva_gen_x_hum_H_usos$HUMEDAL<-as.factor(areas_mucva_gen_x_hum_H_usos$HUMEDAL)
pheatmap(areas_mucva_gen_x_hum_H_usos[2:6],
         main = "Área de los usos del suelo dentro del humedal en 1984",
         labels_row = areas_mucva_gen_x_hum_H_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)

# Este gráfico representa los cambios de usos del suelo (DENTRO DEL HUMEDAL):
areas_dif_gen_x_hum_H_usos$HUMEDAL<-as.factor(areas_dif_gen_x_hum_H_usos$HUMEDAL)
pheatmap(areas_dif_gen_x_hum_H_usos[2:6],
         main = "Cambios en el área de los usos del suelo dentro del humedal entre 1984-2020",
         labels_row = areas_dif_gen_x_hum_H_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)



################################################################################
#ÁREA SOLO BUFFER (_B):

# Áreas por usos 1984-2020 (SIOSE, MUCVA) GENERALES (EN BUFFER) #####################################################
areas_mucva_general_B<-as.data.frame(tapply(datos_mucva$AREA_USOS_BUFFER, INDEX=datos_mucva$USOS_GENERAL, FUN=sum))
colnames(areas_mucva_general_B)<-"areas"
areas_mucva_general_B

areas_siose_general_B<-as.data.frame(tapply(datos_siose$AREA_USOS_BUFFER, INDEX=datos_siose$USOS_GENERAL, FUN=sum))
colnames(areas_siose_general_B)<-"areas"
areas_siose_general_B

areas_dif_general_B<-as.data.frame(tapply(datos_dif$AREA_USOS_BUFFER, INDEX=datos_dif$USOS_GENERAL, FUN=sum))
colnames(areas_dif_general_B)<-"areas"
areas_dif_general_B

#Y POR CADA HUMEDAL (EN BUFFER)
areas_mucva_gen_x_hum_B <- datos_mucva %>%
  group_by(HUMEDAL, USOS_GENERAL) %>%  # Agrupa por humedal y uso general
  summarise(SUMA_AREAS = sum(AREA_USOS_BUFFER, na.rm = TRUE))  # Sumar las áreas por grupo
areas_mucva_gen_x_hum_B_usos <- pivot_wider(areas_mucva_gen_x_hum_B,names_from = USOS_GENERAL, values_from = SUMA_AREAS)

areas_siose_gen_x_hum_B <- datos_siose %>%
  group_by(HUMEDAL, USOS_GENERAL) %>%  # Agrupa por humedal y uso general
  summarise(SUMA_AREAS = sum(AREA_USOS_BUFFER, na.rm = TRUE))  # Sumar las áreas por grupo
areas_siose_gen_x_hum_B_usos <- pivot_wider(areas_siose_gen_x_hum_B,names_from = USOS_GENERAL, values_from = SUMA_AREAS)

areas_dif_gen_x_hum_B <- datos_dif %>%
  group_by(HUMEDAL, USOS_GENERAL) %>%  # Agrupa por humedal y uso general
  summarise(SUMA_AREAS = sum(AREA_USOS_BUFFER, na.rm = TRUE))  # Sumar las áreas por grupo
areas_dif_gen_x_hum_B_usos <- pivot_wider(areas_dif_gen_x_hum_B,names_from = USOS_GENERAL, values_from = SUMA_AREAS)

# GRÁFICO DE USOS GENERALES POR HUMEDAL (SOLO EN BUFFER) ########################################
library(pheatmap)
# Estos gráficos representan los usos del suelo EN BUFFER:
areas_siose_gen_x_hum_B_usos$HUMEDAL<-as.factor(areas_siose_gen_x_hum_B_usos$HUMEDAL)
pheatmap(areas_siose_gen_x_hum_B_usos[2:6],
         main = "Área de los usos del suelo fuera del humedal en 2020",
         labels_row = areas_siose_gen_x_hum_B_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)

areas_mucva_gen_x_hum_B_usos$HUMEDAL<-as.factor(areas_mucva_gen_x_hum_B_usos$HUMEDAL)
pheatmap(areas_mucva_gen_x_hum_B_usos[2:6],
         main = "Área de los usos del suelo fuera del humedal en 1984",
         labels_row = areas_mucva_gen_x_hum_B_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)

# Este gráfico representa los cambios de usos del suelo EN BUFFER:
areas_dif_gen_x_hum_B_usos$HUMEDAL<-as.factor(areas_dif_gen_x_hum_B_usos$HUMEDAL)
pheatmap(areas_dif_gen_x_hum_B_usos[2:6],
         main = "Cambios en el área de los usos del suelo fuera del humedal entre 1984-2020",
         labels_row = areas_dif_gen_x_hum_B_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         fontsize_number = 8)
