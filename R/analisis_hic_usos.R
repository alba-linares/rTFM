# OBJETIVOS
#En qué condiciones se han perdido hábitats
#Dónde se han perdido más hábitats (costa-interior, ENP-noENP e interacción)
#Variable respuesta: Presencia HIC (0/1), por ejemplo.


# Cargar las librerías
library(readxl)
library(car) #Anova()
library(multcomp) # Tukey

# Leer los archivos
setwd("D:/Escritorio/TFM/rTFM")
datos <-read_excel("Cartografia/output/puntos_analisis/puntos_hic_zonas_naturales.xlsx", sheet = 1)
# head(datos)
# sum(as.numeric(is.na(datos$CODIGO_UE2))) #cantidad de NAs

# Categorizar como factor
datos[2:9] <- lapply(datos[2:9], as.factor)
datos$CODIGO_UE2 <- lapply(datos$CODIGO_UE2, as.factor)
datos$CODIGO_UE3 <- lapply(datos$CODIGO_UE3, as.factor)
datos$CODIGO_UE4 <- lapply(datos$CODIGO_UE4, as.factor)
datos[18:33] <- lapply(datos[18:33], as.factor)
datos$PRESENCIA_ZONA_NAT<- lapply(datos$PRESENCIA_ZONA_NAT, as.factor)
# sapply(datos[1:37], function(x) length(unique(x))) #nº de niveles

# Deshacer lista
datos$CODIGO_UE2<-unlist(datos$CODIGO_UE2)
datos$CODIGO_UE3<-unlist(datos$CODIGO_UE3)
datos$CODIGO_UE4<-unlist(datos$CODIGO_UE4)
datos$PRESENCIA_ZONA_NAT<-unlist(datos$PRESENCIA_ZONA_NAT)

pie(table(datos$GRUPO_TIPO))
pie(table(datos$PRESENCIA_ZONA_NAT))
pie(table(datos$PRESENCIA_HIC))
datos_costa <- subset(datos, datos$GRUPO_TIPO=="Costeros")
datos_interior <- subset(datos, datos$GRUPO_TIPO=="Interiores")
datos_ENP <- subset(datos, datos$ENP=="1")
datos_noENP <- subset(datos, datos$ENP=="0")

par(mfrow = c(2, 2), mar = c(5, 5, 2, 2)) 
pie(table(datos_ENP$PRESENCIA_ZONA_NAT), col = c("white","lightblue"))
pie(table(datos_ENP$PRESENCIA_HIC), col = c("lightblue", "white"))
pie(table(datos_noENP$PRESENCIA_ZONA_NAT), col = c("white","lightblue"))
pie(table(datos_noENP$PRESENCIA_HIC), col = c("lightblue","white"))



# Modelo lineal generalizado binomial ##########################################
modelo_glm <- glm(PRESENCIA_HIC~ENP+GRUPO_TIPO, family = binomial, data=datos)
Anova(modelo_glm)
summary(modelo_glm) #ENP pendiente negativa
# Sale diferencia significativa (***) en función de ENP

modelo_glm2 <- glm(PRESENCIA_HIC ~ PROPIEDAD+MUCVA_ZONAS_NAT_T_ESPECIFICAS+NOMBRE_HUM+PRESENCIA_ZONA_NAT, family = binomial, data = datos)
Anova(modelo_glm2)
# Sale diferencia significativa (***) en función de NOMBRE_HUM y MUCVA_ZONAS_NAT_T_ESPECIFICAS

#con interacción: * y contrasts=list(), también he eliminado PROPIEDAD, porque no era significativo
modelo_glm2_2 <- glm(PRESENCIA_HIC ~ M_ZN_T_ES*COD_IHA*PRESENCIA_ZONA_NAT, contrasts=list(M_ZN_T_ES=contr.sum,COD_IHA=contr.sum), family = binomial, data = datos)
Anova(modelo_glm2_2) #tarda mucho
#COD_IHA                                       8145.9   12     <2e-16 ***
#M_ZN_T_ES                                        305    8     <2e-16 ***
#No hay interacción

modelo_glm3 <- glm(PRESENCIA_HIC~datos$MUCVA_T_ES*SIOSE_T_ES, contrasts=list(MUCVA_T_ES=contr.sum,SIOSE_T_ES=contr.sum), family = binomial, data=datos)
Anova(modelo_glm3)


# Tukey
datos2<-datos[-5]
Tukey1<-glht(modelo_glm2, mcp(NOMBRE_HUM="Tukey")) #¿Por qué no me sale?
summary(Tukey1)

Tukey2<-glht(modelo_glm3, mcp(SIOSE_T_ES="Tukey")) #¿Por qué no me sale?
summary(Tukey2)



Tukey3<-glht(modelo_glm2, mcp(M_ZN_T_ES="Tukey"))
summary(Tukey3)

#¿Significado? 
#Salinas - Areas con fuertes procesos erosivos == 0                    *
#Salinas - Lagos y lagunas == 0                                        ***
#Playas, dunas y arenales - Marisma == 0                               ** 
#Salinas - Marisma == 0                                                ***
#VegetaciÃ³n con eucaliptos - Marisma == 0                                
#Vegetacion natural - Marisma == 0                                     ** 
#Vegetacion riparia - Marisma == 0                                     ** 
#Salinas - Playas, dunas y arenales == 0                               ***
#Vegetacion natural - Salinas == 0                                     ***
#Vegetacion riparia - Salinas == 0                                     * 