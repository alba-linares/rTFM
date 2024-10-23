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
datos <-read_excel("Excel/puntos_hic_zonas_naturales.xlsx", sheet = 1)
# head(datos)
# sum(as.numeric(is.na(datos$CODIGO_UE2))) #cantidad de NAs

# Categorizar como factor
#datos <- subset(datos, MUCVA_ZONAS_NAT_T_ESPECIFICAS != "Mares y océanos") #Porque no tienen HIC aunque deberían
datos[2:9] <- lapply(datos[2:9], as.factor)
datos$CODIGO_UE2 <- lapply(datos$CODIGO_UE2, as.factor)
datos$CODIGO_UE3 <- lapply(datos$CODIGO_UE3, as.factor)
datos$CODIGO_UE4 <- lapply(datos$CODIGO_UE4, as.factor)
datos[18:33] <- lapply(datos[18:33], as.factor)
datos$TIPO_DE_CAMBIO<- lapply(datos$TIPO_DE_CAMBIO, as.factor)
datos$PRESENCIA_ZONA_NAT<- lapply(datos$PRESENCIA_ZONA_NAT, as.factor)
datos$MUCVA_ZONAS_NAT_T_ESPECIFICAS<- lapply(datos$MUCVA_ZONAS_NAT_T_ESPECIFICAS, as.factor)
# sapply(datos[1:37], function(x) length(unique(x))) #nº de niveles

# Deshacer lista
datos$CODIGO_UE2<-unlist(datos$CODIGO_UE2)
datos$CODIGO_UE3<-unlist(datos$CODIGO_UE3)
datos$CODIGO_UE4<-unlist(datos$CODIGO_UE4)
datos$PRESENCIA_ZONA_NAT<-unlist(datos$PRESENCIA_ZONA_NAT)
datos$TIPO_DE_CAMBIO<-unlist(datos$TIPO_DE_CAMBIO)
datos$MUCVA_ZONAS_NAT_T_ESPECIFICAS<-unlist(datos$MUCVA_ZONAS_NAT_T_ESPECIFICAS)


# Modelo lineal generalizado binomial ##########################################
#¿Que esté protegido y su localización con respecto a la costa repercuten en si hay HIC en 2020 o no?
modelo_glm <- glm(PRESENCIA_HIC~FIGURA, family = binomial, data=datos)
Anova(modelo_glm)
summary(modelo_glm) #ENP pendiente negativa
# Sale diferencia significativa (***) en función de ENP
table(datos$PRESENCIA_HIC,datos$ENP)
barplot(table(datos$PRESENCIA_HIC,datos$FIGURA),
        las=2,
        legend.text = unique(c("Ausencia HIC","Presencia HIC")),
        args.legend = list(x = "top"),
        ylab="Puntos de análisis (nº)",
        main = "Presencia de HIC en función de la figura de protección")

barplot(table(datos$PRESENCIA_HIC,datos$ENP),
        las=2,
        legend.text = unique(c("Ausencia HIC","Presencia HIC")),
        args.legend = list(x = "topleft"),
        ylab="Puntos de análisis (nº)",
        main = "Presencia de HIC en función de la figura de protección")
        #arrows(data_summary$mean - data_summary$sd, data_summary$mean + data_summary$sd, angle = 90, code = 3, length = 0.1, col = "black"),
        #text(data_summary$mean + data_summary$sd + 0.02, labels = tukey_labels, col = "black")        # Añadir letras de Tukey

modelo_glm <- glm(PRESENCIA_ZONA_NAT~FIGURA, family = binomial, data=datos)
Anova(modelo_glm)
summary(modelo_glm) #

modelo_lm <- lm(TIPO_DE_CAMBIO ~ ENP, data=datos)
summary(modelo_lm) #

################################################################################
#library(dplyr)
#data_summary <- group_by(datos, FIGURA) %>%
  #summarise(mean=mean(PRESENCIA_HIC), sd=sd(PRESENCIA_HIC)) %>%
  #arrange(desc(mean))

#library(emmeans)
# Calcular los valores emmeans (estimaciones marginales) para FIGURA
#emmeans_res <- emmeans(modelo_glm, ~ FIGURA)
# Comparación de Tukey post-hoc con emmeans
#tukey_emmeans <- pairs(emmeans_res, adjust = "tukey")
# Mostrar las letras de Tukey
#cld_res <- cld(emmeans_res, Letters = letters)
#data_summary$Tukey <- cld_res$.group
#print(data_summary)
################################################################################


#¿El tipo de propiedad, el humedal o si hay zona natural en 1984 y el tipo de uso del suelo repercuten en si hay HIC en 2020 o no?
#...SIN INTERACCIÓN:
modelo_glm2 <- glm(PRESENCIA_HIC ~ MUCVA_ZONAS_NAT_T_ESPECIFICAS+NOMBRE_HUM+PRESENCIA_ZONA_NAT, family = binomial, data = datos)
Anova(modelo_glm2)
# Sale diferencia significativa (***) en función de NOMBRE_HUM y MUCVA_ZONAS_NAT_T_ESPECIFICAS

#...CON INTERACCIÓN: * y contrasts=list(), también he eliminado PROPIEDAD, porque no era significativo
modelo_glm2_2 <- glm(PRESENCIA_HIC ~ MUCVA_ZONAS_NAT_T_ESPECIFICAS*COD_IHA*PRESENCIA_ZONA_NAT, contrasts=list(MUCVA_ZONAS_NAT_T_ESPECIFICAS=contr.sum,COD_IHA=contr.sum), family = binomial, data = datos)
Anova(modelo_glm2_2) #tarda mucho
#COD_IHA                                       8145.9   12     <2e-16 ***
#M_ZN_T_ES                                        305    8     <2e-16 ***
#No hay interacción

barplot(table(datos$PRESENCIA_HIC,datos$MUCVA_ZONAS_NAT_T_ESPECIFICAS),
        legend.text = unique(datos$PRESENCIA_HIC),
        las=2,
        args.legend = list(x = "top"),
        ylab="Puntos de análisis (nº)",
        main = "Presencia de HIC en función de los usos del suelo en 1984")

# Verificar el número de niveles en cada variable
summary(datos$PROPIEDAD)  # Ejemplo para la variable PROPIEDAD
summary(datos$MUCVA_ZONAS_NAT_T_ESPECIFICAS)  # Otra variable categórica
summary(datos$NOMBRE_HUM)
summary(datos$PRESENCIA_ZONA_NAT)

#¿Los tipos de usos del suelo (específicos) 1984/2020 repercuten en si hay HIC en 2020 o no? ¿Cuáles repercuten?
#...CON INTERACCIÓN: * y contrasts=list()
modelo_glm3 <- glm(PRESENCIA_HIC~datos$MUCVA_C_ES*SIOSE_C_ES, contrasts=list(MUCVA_T_ES=contr.sum,SIOSE_T_ES=contr.sum), family = binomial, data=datos)
summary(modelo_glm3)

#...SIN INTERACCIÓN:
modelo_glm4 <- glm(PRESENCIA_HIC~datos$MUCVA_C_ES+SIOSE_C_ES, family = binomial, data=datos)
summary(modelo_glm4)

#¿Los tipos de usos del suelo (específicos) 1984/2020 repercuten en si hay HIC en 2020 o no? ¿Cuáles repercuten?
modelo_glm5 <- glm(PRESENCIA_HIC~datos$MUCVA_C_ES, family = binomial, data=datos)
summary(modelo_glm5)
modelo_glm5_2 <- glm(PRESENCIA_HIC~datos$SIOSE_C_ES, family = binomial, data=datos)
summary(modelo_glm5_2)
wider<-pivot_wider(as.data.frame(table(datos$PRESENCIA_HIC,datos$SIOSE_T_ES)), names_from=Var1, values_from = Freq)

#¿Los tipos de usos del suelo (generales) 1984/2020 repercuten en si hay HIC en 2020 o no?  ¿Cuáles repercuten?
modelo_glm6 <- glm(PRESENCIA_HIC~datos$MUCVA_C_GE, family = binomial, data=datos)
summary(modelo_glm6)
modelo_glm6_2 <- glm(PRESENCIA_HIC~datos$SIOSE_C_GE, family = binomial, data=datos)
summary(modelo_glm6_2)

#¿Buffer/Humedal repercute en si hay HIC en 2020 o no?
modelo_glm7 <- glm(PRESENCIA_HIC~ZONA, family = binomial, data=datos) #¿Está bien hecho?
Anova(modelo_glm7) 
summary(modelo_glm7)
#¿Esto quiere decir que hay diferencias significativas en la zona buffer en cuanto a la presencia de HIC, como se puede ver aquí:
table(datos$PRESENCIA_HIC, datos$ZONA)
#   Buffer Humedal
#0   1013     375
#1    496     196
#Se puede ver como hay menor presencia de HIC en el buffer
barplot(table(datos$PRESENCIA_HIC,datos$ZONA),
        legend.text = unique(datos$PRESENCIA_HIC),
        las=2,
        args.legend = list(x = "topright"),
        ylab="Puntos de análisis (nº)",
        main = "Presencia de HIC en función de la situación con respecto al humedal")

#De los que han cambiado, ¿qué hay ahora?:
datos_cambios <- subset(datos, datos$COMPARACION=="DISTINTO")
cambio_usos<-table(datos_cambios$SIOSE_T_ES)
as.data.frame(cambio_usos)
datos_cambios <- subset(datos, datos$COMPARACION=="DISTINTO")
cambio_usos_general<-table(datos_cambios$MUCVA_T_GE,datos_cambios$SIOSE_T_GE)
df_cambio_usos_general<-as.data.frame(cambio_usos_general)

modelo_glm7 <- glm(PRESENCIA_HIC~MUCVA_C_GE:SIOSE_C_GE, family = binomial, data=datos_cambios)
summary(modelo_glm7)

library(dplyr)
frecuencia_cambio_usos <- datos_cambios %>%
  group_by(MUCVA_C_GE, SIOSE_C_GE) %>%
  count()
View(as.data.frame(frecuencia_cambio_usos))

frecuencia_usos_HUM <- datos %>% #OJO, que lo he cambiado: datos_cambios -> datos
  group_by(NOMBRE_HUM,MUCVA_C_ES,MUCVA_T_ES, SIOSE_C_ES, SIOSE_T_ES) %>%
  count()
View(as.data.frame(frecuencia_usos_HUM))
write.table(frecuencia_usos_HUM, file="frecuencia_usos_HUM.txt", sep="\t", row.names=TRUE) 

#########
#########
#########
#########
#########

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






# Gráficos
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