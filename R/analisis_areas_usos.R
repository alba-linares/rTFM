# Cargar las librerías
library(readxl)
library(car) # Anova()
library(tidyr) # %>%
library(dplyr) # mutate()

# Leer los archivos
setwd("D:/Escritorio/TFM/rTFM")
#setwd("D:/Escritorio/MASTER/TFM/rTFM")
datos <-read_excel("Excel/MUCVA+SIOSE_areas_usos.xlsx", sheet = 1)
# head(datos)

datos[1:7] <- lapply(datos[1:7], as.factor)

datos_mucva <- subset(datos, datos$ORIGEN=="MUCVA1984")
datos_siose <- subset(datos, datos$ORIGEN=="SIOSE2020")
datos_dif <- subset(datos, datos$ORIGEN=="DIFERENCIA")


# MODELOS LINEALES #############################################################################################################################################

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






# Áreas por usos 1984-2020 (SIOSE, MUCVA) ESPECÍFICOS ##########################################################################################################
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


# Áreas por usos 1984-2020 (SIOSE, MUCVA) GENERALES ############################################################################################################
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





# CÁLCULO DE PORCENTAJES PARA EL GRÁFICO #######################################################################################################################
library(dplyr)
############### PARA SIOSE######################################################################################################################################
# Agrupar por USOS_GENERAL y humedal, y sumar las áreas
areas_siose_gen_x_hum_percent <- datos_siose %>%
  group_by(USOS_GENERAL, HUMEDAL) %>%
  summarise(AREA_USOS = sum(AREA_USOS)) %>%
  ungroup()

# ESTO ES SI QUIERO EL PORCENTAJE TOTAL, NO POR HUMEDAL
#total_areas_siose_gen_x_hum_percent <- sum(areas_siose_gen_x_hum_percent$AREA_USOS) # Calcular el total de todas las áreas
#areas_siose_gen_x_hum_percent <- areas_siose_gen_x_hum_percent %>%
#  mutate(PORCENTAJE = (AREA_USOS / total_areas_siose_gen_x_hum_percent) * 100) # Añadir una nueva columna con el porcentaje

areas_siose_gen_x_hum_percent <- areas_siose_gen_x_hum_percent %>%
  group_by(HUMEDAL) %>%  # Agrupar por cada humedal
  mutate(TOTAL_AREA_HUMEDAL = sum(AREA_USOS)) %>%  # Calcular el total del área para cada humedal
  mutate(PORCENTAJE = (AREA_USOS / TOTAL_AREA_HUMEDAL) * 100) %>%  # Calcular el porcentaje para cada categoría de uso de suelo dentro del humedal
  ungroup()  # Desagrupar para evitar problemas en operaciones posteriores

# Separar los porcentajes en columnas de usos del suelo por humedal
areas_siose_gen_x_hum_percent$HUMEDAL<-as.factor(areas_siose_gen_x_hum_percent$HUMEDAL)
areas_siose_gen_x_hum_percent_usos <- pivot_wider(areas_siose_gen_x_hum_percent,names_from = USOS_GENERAL, values_from = PORCENTAJE)
areas_siose_gen_x_hum_percent_usos <- areas_siose_gen_x_hum_percent_usos %>%
  group_by(HUMEDAL) %>%
  summarise(
    total_agrario = sum(`Agrario`, na.rm = TRUE),
    total_agrario_invernaderos = sum(`Agrario invernaderos`, na.rm = TRUE),
    total_cobertura_vegetal = sum(`Cobertura vegetal y suelos`, na.rm = TRUE),
    total_urbano_infraestructuras = sum(`Urbano e infraestructuras`, na.rm = TRUE),
    total_zonas_humedas = sum(`Zonas humedas`, na.rm = TRUE)
  )

# Para SOLO humedal (_H)
areas_siose_gen_x_hum_percent <- datos_siose %>%
  group_by(USOS_GENERAL, HUMEDAL) %>%
  summarise(AREA_USOS_HUMEDAL = sum(AREA_USOS_HUMEDAL)) %>%
  ungroup()

#total_areas_siose_gen_x_hum_percent <- sum(areas_siose_gen_x_hum_percent$AREA_USOS_HUMEDAL) # Calcular el total de todas las áreas
#areas_siose_gen_x_hum_percent <- areas_siose_gen_x_hum_percent %>%
#  mutate(PORCENTAJE = (AREA_USOS_HUMEDAL / total_areas_siose_gen_x_hum_percent) * 100) # Añadir una nueva columna con el porcentaje

areas_siose_gen_x_hum_percent <- areas_siose_gen_x_hum_percent %>%
  group_by(HUMEDAL) %>%  # Agrupar por cada humedal
  mutate(TOTAL_AREA_HUMEDAL = sum(AREA_USOS_HUMEDAL)) %>%  # Calcular el total del área para cada humedal
  mutate(PORCENTAJE = (AREA_USOS_HUMEDAL / TOTAL_AREA_HUMEDAL) * 100) %>%  # Calcular el porcentaje para cada categoría de uso de suelo dentro del humedal
  ungroup()  # Desagrupar para evitar problemas en operaciones posteriores

# Separar los porcentajes en columnas de usos del suelo por humedal
areas_siose_gen_x_hum_percent$HUMEDAL<-as.factor(areas_siose_gen_x_hum_percent$HUMEDAL)
areas_siose_gen_x_hum_percent_usos_H <- pivot_wider(areas_siose_gen_x_hum_percent,names_from = USOS_GENERAL, values_from = PORCENTAJE)
areas_siose_gen_x_hum_percent_usos_H <- areas_siose_gen_x_hum_percent_usos_H %>%
  group_by(HUMEDAL) %>%
  summarise(
    total_AREA_USOS_HUMEDAL_agrario = sum(`Agrario`, na.rm = TRUE),
    total_agrario_invernaderos = sum(`Agrario invernaderos`, na.rm = TRUE),
    total_cobertura_vegetal = sum(`Cobertura vegetal y suelos`, na.rm = TRUE),
    total_urbano_infraestructuras = sum(`Urbano e infraestructuras`, na.rm = TRUE),
    total_zonas_humedas = sum(`Zonas humedas`, na.rm = TRUE)
  )

# Para SOLO buffer (_B)
areas_siose_gen_x_hum_percent <- datos_siose %>%
  group_by(USOS_GENERAL, HUMEDAL) %>%
  summarise(AREA_USOS_BUFFER = sum(AREA_USOS_BUFFER)) %>%
  ungroup()

#total_areas_siose_gen_x_hum_percent <- sum(areas_siose_gen_x_hum_percent$AREA_USOS_BUFFER) # Calcular el total de todas las áreas
#areas_siose_gen_x_hum_percent <- areas_siose_gen_x_hum_percent %>%
#  mutate(PORCENTAJE = (AREA_USOS_BUFFER / total_areas_siose_gen_x_hum_percent) * 100) # Añadir una nueva columna con el porcentaje

areas_siose_gen_x_hum_percent <- areas_siose_gen_x_hum_percent %>%
  group_by(HUMEDAL) %>%  # Agrupar por cada humedal
  mutate(TOTAL_AREA_HUMEDAL = sum(AREA_USOS_BUFFER)) %>%  # Calcular el total del área para cada humedal
  mutate(PORCENTAJE = (AREA_USOS_BUFFER / TOTAL_AREA_HUMEDAL) * 100) %>%  # Calcular el porcentaje para cada categoría de uso de suelo dentro del humedal
  ungroup()  # Desagrupar para evitar problemas en operaciones posteriores

# Separar los porcentajes en columnas de usos del suelo por humedal
areas_siose_gen_x_hum_percent$HUMEDAL<-as.factor(areas_siose_gen_x_hum_percent$HUMEDAL)
areas_siose_gen_x_hum_percent_usos_B <- pivot_wider(areas_siose_gen_x_hum_percent,names_from = USOS_GENERAL, values_from = PORCENTAJE)
areas_siose_gen_x_hum_percent_usos_B <- areas_siose_gen_x_hum_percent_usos_B %>%
  group_by(HUMEDAL) %>%
  summarise(
    total_agrario = sum(`Agrario`, na.rm = TRUE),
    total_agrario_invernaderos = sum(`Agrario invernaderos`, na.rm = TRUE),
    total_cobertura_vegetal = sum(`Cobertura vegetal y suelos`, na.rm = TRUE),
    total_urbano_infraestructuras = sum(`Urbano e infraestructuras`, na.rm = TRUE),
    total_zonas_humedas = sum(`Zonas humedas`, na.rm = TRUE)
  )


############### PARA MUCVA######################################################################################################################################
# Agrupar por USOS_GENERAL y humedal, y sumar las áreas
areas_mucva_gen_x_hum_percent <- datos_mucva %>%
  group_by(USOS_GENERAL, HUMEDAL) %>%
  summarise(AREA_USOS = sum(AREA_USOS)) %>%
  ungroup()

#total_areas_mucva_gen_x_hum_percent <- sum(areas_mucva_gen_x_hum_percent$AREA_USOS) # Calcular el total de todas las áreas
#areas_mucva_gen_x_hum_percent <- areas_mucva_gen_x_hum_percent %>%
#  mutate(PORCENTAJE = (AREA_USOS / total_areas_mucva_gen_x_hum_percent) * 100) # Añadir una nueva columna con el porcentaje

areas_mucva_gen_x_hum_percent <- areas_mucva_gen_x_hum_percent %>%
  group_by(HUMEDAL) %>%  # Agrupar por cada humedal
  mutate(TOTAL_AREA_HUMEDAL = sum(AREA_USOS)) %>%  # Calcular el total del área para cada humedal
  mutate(PORCENTAJE = (AREA_USOS / TOTAL_AREA_HUMEDAL) * 100) %>%  # Calcular el porcentaje para cada categoría de uso de suelo dentro del humedal
  ungroup()  # Desagrupar para evitar problemas en operaciones posteriores

# Separar los porcentajes en columnas de usos del suelo por humedal
areas_mucva_gen_x_hum_percent$HUMEDAL<-as.factor(areas_mucva_gen_x_hum_percent$HUMEDAL)
areas_mucva_gen_x_hum_percent_usos <- pivot_wider(areas_mucva_gen_x_hum_percent,names_from = USOS_GENERAL, values_from = PORCENTAJE)
areas_mucva_gen_x_hum_percent_usos <- areas_mucva_gen_x_hum_percent_usos %>%
  group_by(HUMEDAL) %>%
  summarise(
    total_agrario = sum(`Agrario`, na.rm = TRUE),
    total_agrario_invernaderos = sum(`Agrario invernaderos`, na.rm = TRUE),
    total_cobertura_vegetal = sum(`Cobertura vegetal y suelos`, na.rm = TRUE),
    total_urbano_infraestructuras = sum(`Urbano e infraestructuras`, na.rm = TRUE),
    total_zonas_humedas = sum(`Zonas humedas`, na.rm = TRUE)
  )

# Para SOLO humedal (_H)
areas_mucva_gen_x_hum_percent <- datos_mucva %>%
  group_by(USOS_GENERAL, HUMEDAL) %>%
  summarise(AREA_USOS_HUMEDAL = sum(AREA_USOS_HUMEDAL)) %>%
  ungroup()

#total_areas_mucva_gen_x_hum_percent <- sum(areas_mucva_gen_x_hum_percent$AREA_USOS) # Calcular el total de todas las áreas
#areas_mucva_gen_x_hum_percent <- areas_mucva_gen_x_hum_percent %>%
#  mutate(PORCENTAJE = (AREA_USOS / total_areas_mucva_gen_x_hum_percent) * 100) # Añadir una nueva columna con el porcentaje

areas_mucva_gen_x_hum_percent <- areas_mucva_gen_x_hum_percent %>%
  group_by(HUMEDAL) %>%  # Agrupar por cada humedal
  mutate(TOTAL_AREA_HUMEDAL = sum(AREA_USOS_HUMEDAL)) %>%  # Calcular el total del área para cada humedal
  mutate(PORCENTAJE = (AREA_USOS_HUMEDAL / TOTAL_AREA_HUMEDAL) * 100) %>%  # Calcular el porcentaje para cada categoría de uso de suelo dentro del humedal
  ungroup()  # Desagrupar para evitar problemas en operaciones posteriores

# Separar los porcentajes en columnas de usos del suelo por humedal
areas_mucva_gen_x_hum_percent$HUMEDAL<-as.factor(areas_mucva_gen_x_hum_percent$HUMEDAL)
areas_mucva_gen_x_hum_percent_usos_H <- pivot_wider(areas_mucva_gen_x_hum_percent,names_from = USOS_GENERAL, values_from = PORCENTAJE)
areas_mucva_gen_x_hum_percent_usos_H <- areas_mucva_gen_x_hum_percent_usos_H %>%
  group_by(HUMEDAL) %>%
  summarise(
    total_AREA_USOS_HUMEDAL_agrario = sum(`Agrario`, na.rm = TRUE),
    total_agrario_invernaderos = sum(`Agrario invernaderos`, na.rm = TRUE),
    total_cobertura_vegetal = sum(`Cobertura vegetal y suelos`, na.rm = TRUE),
    total_urbano_infraestructuras = sum(`Urbano e infraestructuras`, na.rm = TRUE),
    total_zonas_humedas = sum(`Zonas humedas`, na.rm = TRUE)
  )

# Para SOLO buffer (_B)
areas_mucva_gen_x_hum_percent <- datos_mucva %>%
  group_by(USOS_GENERAL, HUMEDAL) %>%
  summarise(AREA_USOS_BUFFER = sum(AREA_USOS_BUFFER)) %>%
  ungroup()

#total_areas_mucva_gen_x_hum_percent <- sum(areas_mucva_gen_x_hum_percent$AREA_USOS) # Calcular el total de todas las áreas
#areas_mucva_gen_x_hum_percent <- areas_mucva_gen_x_hum_percent %>%
#  mutate(PORCENTAJE = (AREA_USOS / total_areas_mucva_gen_x_hum_percent) * 100) # Añadir una nueva columna con el porcentaje

areas_mucva_gen_x_hum_percent <- areas_mucva_gen_x_hum_percent %>%
  group_by(HUMEDAL) %>%  # Agrupar por cada humedal
  mutate(TOTAL_AREA_HUMEDAL = sum(AREA_USOS_BUFFER)) %>%  # Calcular el total del área para cada humedal
  mutate(PORCENTAJE = (AREA_USOS_BUFFER / TOTAL_AREA_HUMEDAL) * 100) %>%  # Calcular el porcentaje para cada categoría de uso de suelo dentro del humedal
  ungroup()  # Desagrupar para evitar problemas en operaciones posteriores

# Separar los porcentajes en columnas de usos del suelo por humedal
areas_mucva_gen_x_hum_percent$HUMEDAL<-as.factor(areas_mucva_gen_x_hum_percent$HUMEDAL)
areas_mucva_gen_x_hum_percent_usos_B <- pivot_wider(areas_mucva_gen_x_hum_percent,names_from = USOS_GENERAL, values_from = PORCENTAJE)
areas_mucva_gen_x_hum_percent_usos_B <- areas_mucva_gen_x_hum_percent_usos_B %>%
  group_by(HUMEDAL) %>%
  summarise(
    total_agrario = sum(`Agrario`, na.rm = TRUE),
    total_agrario_invernaderos = sum(`Agrario invernaderos`, na.rm = TRUE),
    total_cobertura_vegetal = sum(`Cobertura vegetal y suelos`, na.rm = TRUE),
    total_urbano_infraestructuras = sum(`Urbano e infraestructuras`, na.rm = TRUE),
    total_zonas_humedas = sum(`Zonas humedas`, na.rm = TRUE)
  )


############### PARA LA DIFERENCIA##############################################################################################################################
# Agrupar por USOS_GENERAL y humedal, y sumar las áreas
# areas_dif_gen_x_hum_percent <- datos_dif %>%
#   group_by(USOS_GENERAL, HUMEDAL) %>%
    #   summarise(AREA_USOS = sum(AREA_USOS)) %>%
    #   ungroup()

# total_areas_dif_gen_x_hum_percent <- sum(areas_dif_gen_x_hum_percent$AREA_USOS) # Calcular el total de todas las áreas

# areas_dif_gen_x_hum_percent <- areas_dif_gen_x_hum_percent %>%
  #   mutate(PORCENTAJE = (AREA_USOS / total_areas_dif_gen_x_hum_percent) * 100) # Añadir una nueva columna con el porcentaje

# Separar los porcentajes en columnas de usos del suelo por humedal
  # areas_dif_gen_x_hum_percent$HUMEDAL<-as.factor(areas_dif_gen_x_hum_percent$HUMEDAL)
  # areas_dif_gen_x_hum_percent_usos <- pivot_wider(areas_dif_gen_x_hum_percent,names_from = USOS_GENERAL, values_from = PORCENTAJE)
# areas_dif_gen_x_hum_percent_usos <- areas_dif_gen_x_hum_percent_usos %>%
  #   group_by(HUMEDAL) %>%
  #   summarise(
  #     total_agrario = sum(`Agrario`, na.rm = TRUE),
  #     total_agrario_invernaderos = sum(`Agrario invernaderos`, na.rm = TRUE),
  #     total_cobertura_vegetal = sum(`Cobertura vegetal y suelos`, na.rm = TRUE),
  #     total_urbano_infraestructuras = sum(`Urbano e infraestructuras`, na.rm = TRUE),
  #     total_zonas_humedas = sum(`Zonas humedas`, na.rm = TRUE)
  #   )

# Como la diferencia en porcentaje sale muy extraña, la he vuelto a calcular:
M <- matrix (data=NA, 12, 6)
M<-as.data.frame(M) 

for (i in 1:nrow(areas_siose_gen_x_hum_percent_usos)) {
  for (j in 1:ncol(areas_siose_gen_x_hum_percent_usos)) {
    M[i, j] <- areas_siose_gen_x_hum_percent_usos[i, j] - areas_mucva_gen_x_hum_percent_usos[i, j]
  }
}

M[, 1] <- areas_siose_gen_x_hum_percent_usos[, 1]
colnames(M)<-colnames(areas_siose_gen_x_hum_percent_usos)

areas_dif_gen_x_hum_percent_usos <- M



# Para SOLO humedal:
M <- matrix (data=NA, 12, 6)
M<-as.data.frame(M) 

for (i in 1:nrow(areas_siose_gen_x_hum_percent_usos_H)) {
  for (j in 1:ncol(areas_siose_gen_x_hum_percent_usos_H)) {
    M[i, j] <- areas_siose_gen_x_hum_percent_usos_H[i, j] - areas_mucva_gen_x_hum_percent_usos_H[i, j]
  }
}

M[, 1] <- areas_siose_gen_x_hum_percent_usos_H[, 1]
colnames(M)<-colnames(areas_siose_gen_x_hum_percent_usos_H)

areas_dif_gen_x_hum_percent_usos_H <- M

# Para SOLO buffer:
M <- matrix (data=NA, 12, 6)
M<-as.data.frame(M) 

for (i in 1:nrow(areas_siose_gen_x_hum_percent_usos_B)) {
  for (j in 1:ncol(areas_siose_gen_x_hum_percent_usos_B)) {
    M[i, j] <- areas_siose_gen_x_hum_percent_usos_B[i, j] - areas_mucva_gen_x_hum_percent_usos_B[i, j]
  }
}

M[, 1] <- areas_siose_gen_x_hum_percent_usos_B[, 1]
colnames(M)<-colnames(areas_siose_gen_x_hum_percent_usos_B)

areas_dif_gen_x_hum_percent_usos_B <- M




# GRÁFICO DE USOS GENERALES POR HUMEDAL ########################################################################################################################
library(pheatmap)
# Estos gráficos representan los usos del suelo con un área significativamente distinta:
areas_siose_gen_x_hum_percent_usos$HUMEDAL<-as.factor(areas_siose_gen_x_hum_percent_usos$HUMEDAL)
colnames(areas_siose_gen_x_hum_percent_usos) <- colnames(areas_dif_gen_x_hum_usos)
pheatmap(areas_siose_gen_x_hum_percent_usos[2:6],
         main = "Área de los usos del suelo en 2020",
         labels_row = areas_siose_gen_x_hum_percent_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         cluster_cols = F,  # Mantiene el orden original de las columnas
         angle_col = 45,    # Inclina los nombres de las columnas 45 grados
         fontsize_number = 8)

colnames(areas_mucva_gen_x_hum_percent_usos) <- colnames(areas_dif_gen_x_hum_usos)
areas_mucva_gen_x_hum_percent_usos$HUMEDAL<-as.factor(areas_mucva_gen_x_hum_percent_usos$HUMEDAL)
pheatmap(areas_mucva_gen_x_hum_percent_usos[2:6],
         main = "Área de los usos del suelo en 1984",
         labels_row = areas_mucva_gen_x_hum_percent_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         cluster_cols = F,  # Mantiene el orden original de las columnas
         angle_col = 45,    # Inclina los nombres de las columnas 45 grados
         fontsize_number = 8)

# Este gráfico representa los cambios de usos del suelo:
colnames(areas_dif_gen_x_hum_percent_usos) <- colnames(areas_dif_gen_x_hum_usos)
areas_dif_gen_x_hum_percent_usos$HUMEDAL<-as.factor(areas_dif_gen_x_hum_percent_usos$HUMEDAL)
pheatmap(areas_dif_gen_x_hum_percent_usos[2:6],
         main = "Cambio en el área de los usos del suelo entre 1984-2020",
         labels_row = areas_dif_gen_x_hum_percent_usos$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black",
         cluster_cols = F,  # Mantiene el orden original de las columnas
         angle_col = 45,    # Inclina los nombres de las columnas 45 grados 
         fontsize_number = 8)



################################################################################################################################################################

# Ahora, el mismo análisis, pero para áreas de buffer y áreas de humedal por separado:

#ÁREA SOLO HUMEDAL (_H):########################################################################################################################################

# Áreas por usos 1984-2020 (SIOSE, MUCVA) GENERALES (DENTRO DEL HUMEDAL) #######################################################################################
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

# Voy a recalcular areas_dif
M <- matrix (data=NA, 12, 6)
M<-as.data.frame(M) 
for (i in 1:nrow(areas_siose_gen_x_hum_H_usos)) {
  for (j in 1:ncol(areas_siose_gen_x_hum_H_usos)) {
    M[i, j] <- areas_siose_gen_x_hum_H_usos[i, j] - areas_mucva_gen_x_hum_H_usos[i, j]
  }
}
M[, 1] <- areas_siose_gen_x_hum_H_usos[, 1]
colnames(M)<-colnames(areas_siose_gen_x_hum_H_usos)
areas_dif_gen_x_hum_usos_H <- M

# GRÁFICO DE USOS GENERALES POR HUMEDAL (SOLO DENTRO DE HUMEDAL) ###############################################################################################
library(pheatmap)
# Estos gráficos representan los usos del suelo (DENTRO DEL HUMEDAL):
colnames(areas_siose_gen_x_hum_percent_usos_H) <- colnames(areas_dif_gen_x_hum_usos)
areas_siose_gen_x_hum_percent_usos_H$HUMEDAL<-as.factor(areas_siose_gen_x_hum_percent_usos_H$HUMEDAL)
pheatmap(areas_siose_gen_x_hum_percent_usos_H[2:6],
         main = "Área de los usos del suelo dentro del humedal en 2020",
         labels_row = areas_siose_gen_x_hum_percent_usos_H$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         cluster_cols = F,  # Mantiene el orden original de las columnas
         angle_col = 45,    # Inclina los nombres de las columnas 45 grados
         fontsize_number = 8)

colnames(areas_mucva_gen_x_hum_percent_usos_H) <- colnames(areas_dif_gen_x_hum_usos)
areas_mucva_gen_x_hum_percent_usos_H$HUMEDAL<-as.factor(areas_mucva_gen_x_hum_percent_usos_H$HUMEDAL)
pheatmap(areas_mucva_gen_x_hum_percent_usos_H[2:6],
         main = "Área de los usos del suelo dentro del humedal en 1984",
         labels_row = areas_mucva_gen_x_hum_percent_usos_H$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black",
         cluster_cols = F,  # Mantiene el orden original de las columnas
         angle_col = 45,    # Inclina los nombres de las columnas 45 grados
         fontsize_number = 8)

# Este gráfico representa los cambios de usos del suelo (DENTRO DEL HUMEDAL):
colnames(areas_dif_gen_x_hum_percent_usos_H) <- colnames(areas_dif_gen_x_hum_usos)
areas_dif_gen_x_hum_percent_usos_H$HUMEDAL<-as.factor(areas_dif_gen_x_hum_percent_usos_H$HUMEDAL)
pheatmap(areas_dif_gen_x_hum_percent_usos_H[2:6],
         main = "Cambios en el área de los usos del suelo dentro del humedal entre 1984-2020",
         labels_row = areas_dif_gen_x_hum_percent_usos_H$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black",
         cluster_cols = F,  # Mantiene el orden original de las columnas
         angle_col = 45,    # Inclina los nombres de las columnas 45 grados
         fontsize_number = 8)



################################################################################################################################################################
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

# Voy a recalcular areas_dif
M <- matrix (data=NA, 12, 6)
M<-as.data.frame(M) 
for (i in 1:nrow(areas_siose_gen_x_hum_B_usos)) {
  for (j in 1:ncol(areas_siose_gen_x_hum_B_usos)) {
    M[i, j] <- areas_siose_gen_x_hum_B_usos[i, j] - areas_mucva_gen_x_hum_B_usos[i, j]
  }
}
M[, 1] <- areas_siose_gen_x_hum_B_usos[, 1]
colnames(M)<-colnames(areas_siose_gen_x_hum_B_usos)
areas_dif_gen_x_hum_usos_B <- M




# GRÁFICO DE USOS GENERALES POR HUMEDAL (SOLO EN BUFFER) #######################################################################################################
library(pheatmap)
# Estos gráficos representan los usos del suelo EN BUFFER:
colnames(areas_siose_gen_x_hum_percent_usos_B) <- colnames(areas_dif_gen_x_hum_usos)
areas_siose_gen_x_hum_percent_usos_B$HUMEDAL<-as.factor(areas_siose_gen_x_hum_percent_usos_B$HUMEDAL)
pheatmap(areas_siose_gen_x_hum_percent_usos_B[2:6],
         main = "Área de los usos del suelo fuera del humedal en 2020",
         labels_row = areas_siose_gen_x_hum_percent_usos_B$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         cluster_cols = F,  # Mantiene el orden original de las columnas
         angle_col = 45,    # Inclina los nombres de las columnas 45 grados
         fontsize_number = 8)

colnames(areas_mucva_gen_x_hum_percent_usos_B) <- colnames(areas_dif_gen_x_hum_usos)
areas_mucva_gen_x_hum_percent_usos_B$HUMEDAL<-as.factor(areas_mucva_gen_x_hum_percent_usos_B$HUMEDAL)
pheatmap(areas_mucva_gen_x_hum_percent_usos_B[2:6],
         main = "Área de los usos del suelo fuera del humedal en 1984",
         labels_row = areas_mucva_gen_x_hum_percent_usos_B$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         cluster_cols = F,  # Mantiene el orden original de las columnas
         angle_col = 45,    # Inclina los nombres de las columnas 45 grados
         fontsize_number = 8)

# Este gráfico representa los cambios de usos del suelo EN BUFFER:
colnames(areas_dif_gen_x_hum_percent_usos_B) <- colnames(areas_dif_gen_x_hum_usos)
areas_dif_gen_x_hum_percent_usos_B$HUMEDAL<-as.factor(areas_dif_gen_x_hum_percent_usos_B$HUMEDAL)
pheatmap(areas_dif_gen_x_hum_percent_usos_B[2:6],
         main = "Cambios en el área de los usos del suelo fuera del humedal entre 1984-2020",
         labels_row = areas_dif_gen_x_hum_percent_usos_B$HUMEDAL,
         display_numbers = TRUE,
         number_color = "black", 
         cluster_cols = F,  # Mantiene el orden original de las columnas
         angle_col = 45,    # Inclina los nombres de las columnas 45 grados
         fontsize_number = 8)

