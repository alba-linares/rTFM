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
sum(as.numeric(is.na(datos$CODIGO_UE2))) #cantidad de NAs

# Categorizar como factor
datos[2:9] <- lapply(datos[2:9], as.factor)
datos$CODIGO_UE2 <- lapply(datos$CODIGO_UE2, as.factor)
datos$CODIGO_UE3 <- lapply(datos$CODIGO_UE3, as.factor)
datos$CODIGO_UE4 <- lapply(datos$CODIGO_UE4, as.factor)
datos[18:33] <- lapply(datos[18:33], as.factor)
sapply(datos[1:37], function(x) length(unique(x))) #nº de niveles

# Deshacer lista
datos$CODIGO_UE2<-unlist(datos$CODIGO_UE2)
datos$CODIGO_UE3<-unlist(datos$CODIGO_UE3)
datos$CODIGO_UE4<-unlist(datos$CODIGO_UE4)

# Modelos lineales
modelo_glm <- glm(PRESENCIA_HIC~ENP+GRUPO_TIPO, family = binomial, data=datos)
Anova(modelo_glm)
# Sale diferencia significativa (***) en función de ENP

modelo_glm2 <- glm(PRESENCIA_HIC ~ PROPIEDAD+M_ZN_T_ES+COD_IHA+PRESENCIA_ZONA_NAT, family = binomial, data = datos)
Anova(modelo_glm2)

modelo_glm2c <- glm(PRESENCIA_HIC ~ PROPIEDAD*M_ZN_T_ES*COD_IHA*PRESENCIA_ZONA_NAT, contrasts=list(PROPIEDAD=contr.sum, M_ZN_T_ES=contr.sum,COD_IHA=contr.sum), family = binomial, data = datos)
Anova(modelo_glm2c)
# Sale diferencia significativa (***) en función de COD_IHA y MUCVA_ZONAS_NATURALES_CLASES_ESPECÍFICAS

# Tukey
Tukey<-glht(modelo_glm2, mcp(M_ZN_T_ES="Tukey"))
summary(Tukey)

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