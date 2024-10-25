# Cargar las librerías
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(multcomp) #glht() post-hoc y letras de significancia
library(emmeans) #letras de significancia
library(lme4) #glmer()


# Leer los archivos
setwd("D:/Escritorio/TFM/rTFM")
datos <-read_excel("Excel/Resultados_puntos_analisis.xlsx", range = "A1:X2081")
head(datos)

df_area_usos84 <-read_excel("Excel/Resultados_puntos_analisis.xlsx", sheet = 2)
df_area_usos20 <-read_excel("Excel/Resultados_puntos_analisis.xlsx", sheet = 3)

################################################################################
# Análisis exploratorio de los datos: gráficas #################################
pie(table(datos$CAMBIO))
pie(table(datos$MUCVA1984)) #Usos 1984
pie(table(datos$SIOSE2020)) #Usos 2020
#pie(table(datos$TIPO_DE_CAMBIO),names=c(0_0=="s",0_1=="q",1_0=="r",1_1=="w",)) #Usos 2020

# Gráfico de barras apiladas
ggplot(datos, aes(x = datos$MUCVA1984_GENERAL, fill = CAMBIO)) +
  geom_bar(position = "fill") +
  labs(title = "Comparación de usos del suelo entre 1984 y 2020", x = "Uso del suelo en 1984", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))


# Contar las frecuencias de cada clase de uso del suelo por humedal
tabla_frecuencia <- table(datos$NOMBRE_HUM, datos$MUCVA1984)

# Contar la frecuencia de cada clase por humedal
frecuencia_clase_humedal <- datos %>%
  group_by(NOMBRE_HUM, MUCVA1984) %>%
  count()

# Ver los resultados
head(frecuencia_clase_humedal)

################################################################################
# Gráfico de barras apiladas para mostrar la proporción de cada uso del suelo por humedal RELATIVO
#ggplot(datos, aes(x = MUCVA1984, fill = NOMBRE_HUM)) +
#  geom_bar(position = "fill") +
#  labs(title = "Proporción de usos del suelo en 1984 por humedal", 
#       x = "Uso del suelo", 
#       y = "Proporción") +
#  scale_fill_viridis_d(option = "H") + #B o H están bien, se distinguen los humedales
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0))

#ggplot(datos, aes(x = SIOSE2020, fill = NOMBRE_HUM)) +
#  geom_bar(position = "fill") +
#  labs(title = "Proporción de usos del suelo en la actualidad por humedal", 
#       x = "Uso del suelo", 
#       y = "Proporción") +
#  scale_fill_viridis_d(option = "H") + #B o H están bien, se distinguen los humedales
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))

# ABSOLUTO #####################################################################
#ggplot(datos, aes(x = MUCVA1984, fill = NOMBRE_HUM)) +
#  geom_bar(position = "stack") +  # Cambia "fill" por "stack" para contar valores absolutos
#  labs(title = "Número de hectáreas por uso del suelo en 1984 por humedal", 
#       x = "Uso del suelo", 
#       y = "Hectáreas") +
#  scale_fill_viridis_d(option = "H") + #B o H están bien, se distinguen los humedales
#  ylim(0, 800) +  # Establece el límite del eje y entre 0 y 800
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0))

#ggplot(datos, aes(x = SIOSE2020, fill = NOMBRE_HUM)) +
#  geom_bar(position = "stack") +  # Cambia "fill" por "stack" para contar valores absolutos
#  labs(title = "Número de hectáreas por uso del suelo en la actualidad por humedal", 
#       x = "Uso del suelo", 
#       y = "Hectáreas") +
#  scale_fill_viridis_d(option = "H") + #B o H están bien, se distinguen los humedales
#  ylim(0, 800) +  # Establece el límite del eje y entre 0 y 800
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))


################################################################################
################################################################################

result_area84 <- df_area_usos84 %>%
  group_by(df_area_usos84$`_USOS_DEFI`) %>%
  summarise(Total_Area = sum(AREA_POLIG))

result_area20 <- df_area_usos20 %>%
  group_by(`_USOS_DEFI`) %>%
  summarise(Total_Area = sum(AREA_POLIG))

################################################################################
################################################################################
# Agrupar por ZONA (BUFFER VS HUMEDAL) y CAMBIO (IGUAL VS DISTINTO), y contar las ocurrencias
comparacion_resumen <- datos %>%
  group_by(ZONA, CAMBIO) %>%
  summarize(count = n()) %>%
  ungroup()

print(comparacion_resumen)

# Calcular proporciones
comparacion_proporciones <- comparacion_resumen %>%
  group_by(ZONA) %>%
  mutate(proporcion = count / sum(count))

print(comparacion_proporciones)

# Gráfico de barras apiladas

ggplot(comparacion_resumen, aes(x = ZONA, y = count, fill = CAMBIO)) +
  geom_bar(stat = "identity", position = "fill") +  # Agregar position = "fill"
  labs(title = "Cambios de uso del suelo en función de situación con respecto al humedal",
       x = "Zona (dentro/fuera del humedal)", 
       y = "Porcentaje") +  # Cambiar la etiqueta del eje Y
  scale_fill_manual(values = c("0" = "honeydew3", "1" = "darkorchid1")) +
  scale_y_continuous(labels = scales::percent) +  # Mostrar porcentajes en el eje Y
  theme_minimal()


################################################################################
# Para evaluar los cambios específicos de uso del suelo ########################
# Crear una columna que muestre el cambio de uso del suelo
data <- datos %>%
  mutate(cambio_uso = paste(MUCVA1984_GENERAL, "a", SIOSE2020_GENERAL))

# Ver los primeros registros para confirmar
head(data)

# Agrupar por zona y tipo de cambio de uso del suelo, y contar las ocurrencias
cambio_uso_buf_hum <- data %>%
  group_by(ZONA, cambio_uso) %>%
  summarize(count = n()) %>%
  ungroup()

# Ver los resultados
print(cambio_uso_buf_hum)






# Resumen descriptivo
cambio_uso_buf_hum %>%
  arrange(desc(count))

# Crear tabla de contingencia
tabla_contingencia_bh <- table(data$ZONA, data$cambio_uso)

# Realizar prueba de Chi-cuadrado
chi_test <- chisq.test(tabla_contingencia_bh)
print(chi_test)

# Transformar la tabla de formato largo a formato ancho
tabla_ancha <- as.data.frame(tabla_contingencia_bh) %>%
  pivot_wider(names_from = Var1, values_from = Freq)

# Ver el resultado
print(tabla_ancha)


# Gráfico de barras horizontales ###############################################
# Filtrar para mostrar solo las 10 categorías más comunes
                    top_cambios <- cambio_uso_buf_hum %>%
                      top_n(10, count)
                    
# Gráfico de barras horizontales con las 10 categorías más comunes
                    ggplot(top_cambios, aes(x = count, y = reorder(cambio_uso, count))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                      labs(title = "Top 10 Cambios de Uso del suelo",
                           x = "Número de Cambios", y = "Cambio de Uso del suelo") +
                      theme_minimal()

# Definir un umbral para agrupar categorías menos frecuentes
umbral <- 5

# Crear una nueva categoría "Otros" para cambios poco frecuentes
cambio_uso_buf_hum_simplificado <- cambio_uso_buf_hum %>%
  mutate(cambio_uso = ifelse(count < umbral, "Otros", cambio_uso)) %>%
  group_by(cambio_uso) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  arrange(count)

# Gráfico de barras horizontales con categorías simplificadas
                    ggplot(cambio_uso_buf_hum_simplificado, aes(x = count, y = reorder(cambio_uso, count))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                      labs(title = "Cambios de Uso del suelo (Agrupados)",
                           x = "Número de Cambios", y = "Cambio de Uso del suelo") +
                      theme_minimal()


                    
# SOLO CAMBIOS (CAMBIO = DISTINTO) ########################################
# Filtrar los datos para obtener solo los cambios distintos ####################
data_distintos <- data %>%
  filter(CAMBIO == "1")
# Crear una columna que muestre el cambio de uso del suelo
data_distintos <- data_distintos %>%
  mutate(cambio_uso = paste(MUCVA1984, "a", SIOSE2020))
# Agrupar por tipo de cambio de uso del suelo y contar las ocurrencias
cambio_uso_buf_hum <- data_distintos %>%
  group_by(cambio_uso) %>%
  summarize(count = n()) %>%
  ungroup()

# Ver los resultados
print(cambio_uso_buf_hum)
# Gráfico de barras horizontales para cambios distintos
# Mostrar solo las 10 categorías más comunes
top_cambios <- cambio_uso_buf_hum %>%
  top_n(10, count)

# Gráfico de barras horizontales con las 10 categorías más comunes
                    ggplot(top_cambios, aes(x = count, y = reorder(cambio_uso, count))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                      labs(title = "Top 10 Cambios de Uso del suelo (DISTINTO)",
                           x = "Número de Cambios", y = "Cambio de Uso del suelo") +
                      theme_minimal()

# Definir un umbral para agrupar categorías menos frecuentes
umbral <- 5

# Crear una nueva categoría "Otros" para cambios poco frecuentes
cambio_uso_buf_hum_simplificado <- cambio_uso_buf_hum %>%
  mutate(cambio_uso = ifelse(count < umbral, "Otros", cambio_uso)) %>%
  group_by(cambio_uso) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  arrange(count)

# Gráfico de barras horizontales con categorías simplificadas
                    ggplot(cambio_uso_buf_hum_simplificado, aes(x = count, y = reorder(cambio_uso, count))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                      labs(title = "Cambios de Uso del suelo (DISTINTO, Agrupados)",
                           x = "Número de Cambios", y = "Cambio de Uso del suelo") +
                      theme_minimal()


################################################################################
# Crear una columna que muestre el cambio de uso del suelo
data <- datos %>%
  mutate(cambio_uso = paste(MUCVA1984, "a", SIOSE2020))

cambio_uso_cost_int <- data %>%
  group_by(GRUPO_TIPO, cambio_uso) %>%
  summarize(count = n()) %>%
  ungroup()

# Ver los resultados
print(cambio_uso_cost_int)

# Crear tabla de contingencia
tabla_contingencia_ci <- table(data$GRUPO_TIPO, data$cambio_uso)

# Transformar la tabla de formato largo a formato ancho
tabla_ancha_ci <- as.data.frame(tabla_contingencia_ci) %>%
  pivot_wider(names_from = Var1, values_from = Freq)

# Ver el resultado
head(tabla_ancha_ci)







################################################################################
################################################################################
################################################################################
# Filtrar datos dentro y fuera de humedales y costeros-interiores ##############
#dentro_humedales <- datos %>% filter(ZONA == "Humedal")
#fuera_humedales <- datos %>% filter(ZONA != "Humedal")

# Filtrar humedales costeros e interiores
#humedales_costeros <- dentro_humedales %>% filter(GRUPO_TIPO == "Costeros")
#humedales_interior <- dentro_humedales %>% filter(GRUPO_TIPO != "Costeros")

# Contar los registros en cada grupo
count_humedales <- datos %>%
  group_by(ZONA, GRUPO_TIPO,CAMBIO) %>%
  summarize(n = n())

print(count_humedales)


# Gráfico de barras para comparar las zonas
ggplot(count_humedales, aes(x = GRUPO_TIPO, y = n, fill = ZONA)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Zonas y Tipos de Humedales",
       x = "Tipo de Humedal", y = "Número de Registros") +
  theme_minimal()


################################################################################
# Según presión:
# Calcular la suma ponderada de la presión por ZONA y GRUPO_TIPO
suma_presion_cost_int <- datos %>%
  group_by(ZONA, GRUPO_TIPO) %>%
  summarise(balance1 = sum(CAMBIO_PRESION),
            n=n(),
            balance_presion = balance1*n)  # ¿Debería multiplicar el cambio de presión por el número de casos?

# GRÁFICO HUMEDAL/BUFFER + COSTEROS/INTERIORES + CAMBIO: IGUAL/DISTINTO
ggplot(suma_presion_cost_int, aes(x = interaction(ZONA, GRUPO_TIPO), y = balance_presion, fill = balance_presion)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Presión de los cambios de uso del suelo según situación con respecto al humedal y a la costa",
       x = "Situación con respecto al humedal y a la costa", 
       y = "Porcentaje") +
  scale_fill_manual(values = c("0" = "honeydew3", "1" = "darkorchid1")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
# GRÁFICO SUMA TOTAL DE PRESIONES (MULTIPLICANTO CAMBIO DE PRESIÓN POR NÚMERO DE CASOS)
ggplot(suma_presion_cost_int, aes(x = interaction(ZONA, GRUPO_TIPO), y = balance_presion, fill = balance_presion)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Presión de los cambios de uso del suelo según situación con respecto al humedal y a la costa",
       x = "Situación con respecto al humedal y a la costa", 
       y = "Balance de presión de los usos del suelo",
       fill = "Balance de presión") +  # Cambiar título de la leyenda
  scale_x_discrete(labels = c("Buffer.Costeros" = "Buffer-costeros", 
                              "Buffer.Interiores" = "Buffer-interiores",
                              "Humedal.Costeros" = "Humedal-costeros", 
                              "Humedal.Interiores" = "Humedal-interiores")) +  # Cambiar etiquetas del eje X
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +  # Escala de color continua
  theme_minimal()


################################################################################
# GRÁFICO HUMEDAL/BUFFER + COSTEROS/INTERIORES + BALANCE DE PRESIÓN SEPARADO EN + -
suma_presion_cost_int_2 <- datos %>%
  group_by(ZONA, GRUPO_TIPO) %>%
  summarise(suma_positivos = sum(CAMBIO_PRESION[CAMBIO_PRESION > 0], na.rm = TRUE),
            suma_negativos = sum(CAMBIO_PRESION[CAMBIO_PRESION < 0], na.rm = TRUE))  

ggplot(suma_presion_cost_int_2, aes(x = interaction(ZONA, GRUPO_TIPO))) +
  geom_bar(aes(y = suma_positivos, fill = "Positivos"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = suma_negativos, fill = "Negativos"), stat = "identity", position = "dodge") +
  labs(title = "Presión de los cambios de uso del suelo según situación con respecto al humedal y a la costa",
       x = "Situación con respecto al humedal y a la costa", 
       y = "Balance de presión de los usos del suelo",
       fill = "Balance de presión") +  # Cambiar título de la leyenda
  scale_x_discrete(labels = c("Buffer.Costeros" = "Buffer-costeros", 
                              "Buffer.Interiores" = "Buffer-interiores",
                              "Humedal.Costeros" = "Humedal-costeros", 
                              "Humedal.Interiores" = "Humedal-interiores")) +  # Cambiar etiquetas del eje X
  scale_fill_manual(values = c("Positivos" = "#04bcc4", "Negativos" = "#fc746c")) +  # Asignar colores específicos
  theme_minimal()



################################################################################
# GRÁFICO SUMA TOTAL DE PRESIONES (MULTIPLICANTO CAMBIO DE PRESIÓN POR NÚMERO DE CASOS)
# Calcular la suma ponderada de la presión por ZONA y ENP
suma_presion_ENP <- datos %>%
  group_by(ZONA, ENP) %>%
  summarise(balance1 = sum(CAMBIO_PRESION),
            n=n(),
            balance_presion = balance1*n)  # ¿Debería multiplicar el cambio de presión por el número de casos?

ggplot(suma_presion_ENP, aes(x = interaction(ZONA, ENP), y = balance_presion, fill = balance_presion)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Presión de los cambios de uso del suelo según situación con respecto al humedal y protección",
       x = "Situación con respecto al humedal y protección", 
       y = "Balance de presión de los usos del suelo",
       fill = "Balance de presión") +  # Cambiar título de la leyenda
  scale_x_discrete(labels = c("Buffer.0" = "Buffer-no protegido", 
                              "Buffer.1" = "Buffer-protegido",
                              "Humedal.0" = "Humedal-no protegido", 
                              "Humedal.1" = "Humedal-protegido")) +  # Cambiar etiquetas del eje X
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +  # Escala de color continua
  theme_minimal()

################################################################################
# GRÁFICO HUMEDAL/BUFFER + PROTEGIDOS/NO PROTEGIDOS + BALANCE DE PRESIÓN SEPARADO EN + -
suma_presion_ENP_2 <- datos %>%
  group_by(ZONA, ENP) %>%
  summarise(suma_positivos = sum(CAMBIO_PRESION[CAMBIO_PRESION > 0], na.rm = TRUE),
            suma_negativos = sum(CAMBIO_PRESION[CAMBIO_PRESION < 0], na.rm = TRUE))  # Multiplicación por n y suma condicional

# GRÁFICO HUMEDAL/BUFFER + PROTECCIÓN + BALANCE DE PRESIÓN SEPARADO EN + -
ggplot(suma_presion_ENP_2, aes(x = interaction(ZONA, ENP))) +
  geom_bar(aes(y = suma_positivos, fill = "Positivos"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = suma_negativos, fill = "Negativos"), stat = "identity", position = "dodge") +
  labs(title = "Presión de los cambios de uso del suelo según situación con respecto al humedal y protección",
       x = "Situación con respecto al humedal y protección", 
       y = "Balance de presión de los usos del suelo",
       fill = "Balance de presión") +  # Cambiar título de la leyenda
  scale_x_discrete(labels = c("Buffer.0" = "Buffer-no protegido", 
                              "Buffer.1" = "Buffer-protegido",
                              "Humedal.0" = "Humedal-no protegido", 
                              "Humedal.1" = "Humedal-protegido")) +  # Cambiar etiquetas del eje X
  scale_fill_manual(values = c("Positivos" = "#04bcc4", "Negativos" = "#fc746c")) +  # Asignar colores específicos
  theme_minimal()



################################################################################
                              # ESTANDARIZACIÓN
################################################################################
################################################################################
# MODELO Y GRÁFICO ESTANDARIZADO
# CAMBIO_PRESION ~ PROTEGIDO/NO PROTEGIDO * HUMEDAL/BUFFER
################################################################################
# datos$CAMBIO <- as.numeric(as.factor(datos$CAMBIO)) - 1  # Convertir a 0 y 1 si es necesario
datos$ENP <- as.factor(datos$ENP)
datos$ZONA <- as.factor(datos$ZONA)
# datos$CAMBIO_PRESION <- as.factor(datos$CAMBIO_PRESION)

# modelo_lm_pres_ENP_hb <- lm(CAMBIO_PRESION ~ ENP * ZONA, contrasts=list(ENP=contr.sum, ZONA=contr.sum), data = datos)
# Anova(modelo_lm_pres_ENP_hb)
# summary(modelo_lm_pres_ENP_hb)
# # Interacción significativa: de no protegido x humedal.
# 
# # Obtener las medias marginales estimadas y comparaciones
# emm1 <- emmeans(modelo_lm_pres_ENP_hb, ~ ENP * ZONA)
# # Aplicar comparación de letras de significancia
# letras_significancia1 <- cld(emm1, Letters = letters, adjust = "Bonferroni")
# # Mostrar el resultado
# print(letras_significancia1) #a a a b


# # MODELO LINEAL MIXTO
# modelo_lme_pres_ENP_hb <- lme(CAMBIO_PRESION ~ ENP * ZONA, random = ~ 1 | NOMBRE_HUM,data=datos)
# anova(modelo_lme_pres_ENP_hb)
# 
# # Obtener las medias marginales estimadas y comparaciones
# emm <- emmeans(modelo_lme_pres_ENP_hb, ~ ENP * ZONA)
# # Aplicar comparación de letras de significancia
# letras_significancia <- cld(emm, Letters = letters, adjust = "Bonferroni")
# # Mostrar el resultado
# print(letras_significancia)  #a ab ab b

# PRESION_BIN + MODELO MIXTO (NEGATIVA = 1, NO PRESIÓN O POSITIVA = 0)
modelo_glmer_pres_ENP_hb <- glmer(PRESION_BIN ~ ENP * ZONA + (1 | NOMBRE_HUM), family = binomial, data=datos)
Anova(modelo_glmer_pres_ENP_hb)
# Interacción significativa

# Obtener las estimaciones marginales
emmeans_result <- emmeans(modelo_glmer_pres_ENP_hb, ~ ENP * ZONA)

# Realizar comparaciones post-hoc y obtener letras de significancia
comparisons <- cld(emmeans_result, alpha = 0.05, Letters = letters)

# Mostrar resultados con letras de significancia
print(comparisons) # a b b b




# Cálculo de la suma de positivos, negativos y el número total de puntos
suma_presion_ENP_3 <- datos %>%
  group_by(ZONA, ENP) %>%
  summarise(
    #suma_positivos = sum(CAMBIO_PRESION[CAMBIO_PRESION > 0], na.rm = TRUE),
    #suma_negativos = abs(sum(CAMBIO_PRESION[CAMBIO_PRESION < 0], na.rm = TRUE)),
    # Contar la cantidad de valores positivos
    num_positivos = sum(CAMBIO_PRESION > 0, na.rm = TRUE),
    # Contar la cantidad de valores negativos
    num_negativos = sum(CAMBIO_PRESION < 0, na.rm = TRUE),
    total_puntos = n()  # Número total de puntos
  ) %>%
  # Calcular neutros
  mutate(sin_cambios = total_puntos - num_positivos - abs(num_negativos)) %>% 
  # Transformar a formato largo para apilamiento
  pivot_longer(cols = c(num_positivos, num_negativos,sin_cambios),
               names_to = "Categoria",
               values_to = "Valor") %>%
  # Calcular la proporción sobre el total
  mutate(Proporción = Valor / total_puntos) %>%
# Definir el orden de las categorías
mutate(Categoria = factor(Categoria, levels = c("num_positivos", "num_negativos","sin_cambios")))

# Gráfico apilado
ggplot(suma_presion_ENP_3, aes(x = interaction(ZONA, ENP), y = Proporción, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack") +  # Apilar las barras
  labs(title = "Proporción de la presión de los cambios de uso del suelo
       según situación con respecto al humedal y protección",
       x = "Situación con respecto al humedal y protección", 
       y = "Proporción (%)",
       fill = "Categoría de presión") +  # Cambiar título de la leyenda
  scale_x_discrete(labels = c("Buffer.0" = "Buffer-no protegido", 
                              "Buffer.1" = "Buffer-protegido",
                              "Humedal.0" = "Humedal-no protegido", 
                              "Humedal.1" = "Humedal-protegido")) +  # Cambiar etiquetas del eje X
  scale_fill_manual(values = c("num_positivos" = "#04bcc4", "num_negativos" = "#fc746c","sin_cambios" = "#D3D3D3"),  # Colores en el orden deseado
  labels=c("Cambios positivos", "Cambios negativos","Sin cambios"))  +
  theme(plot.title = element_text(hjust = 0.5, size = 13),  # Centrar y reducir el tamaño del título
        panel.background = element_rect(fill = "white", color = NA),  # Fondo blanco
        panel.grid.major = element_line(color = "gray90"),  # Líneas de cuadrícula principales en gris claro
        panel.grid.minor = element_line(color = "gray95"),   # Líneas de cuadrícula menores en un gris más claro
        axis.text.x = element_text(size = 12),
        plot.title.position = "panel",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12,face="bold")
  )# Centrar y reducir el tamaño del título
  

################################################################################
                       # MODELO Y GRÁFICO ESTANDARIZADO
            # CAMBIO_PRESION ~ INTERIOR/COSTERO * HUMEDAL/BUFFER
################################################################################
# PRESION_SIMPLIF
presion_COST_HB_xHUM <- datos %>%
  group_by(ZONA, GRUPO_TIPO, NOMBRE_HUM) %>%
  summarise(
    # Contar la cantidad de valores positivos
    num_positivos = sum(CAMBIO_PRESION > 0, na.rm = TRUE), #OTRA OPCIÓN: suma_positivos = sum(CAMBIO_PRESION[CAMBIO_PRESION > 0], na.rm = TRUE),
    # Contar la cantidad de valores negativos
    num_negativos = sum(CAMBIO_PRESION < 0, na.rm = TRUE),  #OTRA OPCIÓN: suma_negativos = abs(sum(CAMBIO_PRESION[CAMBIO_PRESION < 0], na.rm = TRUE)),
    total_puntos = n()  # Número total de puntos
  ) %>%
  mutate(sin_cambios = total_puntos - num_positivos - abs(num_negativos)) %>%   # Calcular ceros
  pivot_longer(cols = c(num_positivos, num_negativos,sin_cambios),  # Transformar a formato largo para apilamiento
               names_to = "Categoria",
               values_to = "Valor") %>%
  mutate(Proporcion = Valor / total_puntos) %>%  # Calcular la proporción sobre el total
  mutate(Categoria = factor(Categoria, levels = c("num_positivos", "num_negativos","sin_cambios")))  # Definir el orden de las categorías

presion_COST_HB_xHUM_wider <- pivot_wider(presion_COST_HB_xHUM, values_from = c(Valor, Proporcion), names_from = Categoria)

# # datos$CAMBIO <- as.numeric(as.factor(datos$CAMBIO)) - 1  # Convertir a 0 y 1 si es necesario
# datos$GRUPO_TIPO <- as.factor(datos$GRUPO_TIPO)
# datos$ZONA <- as.factor(datos$ZONA)
# # datos$CAMBIO_PRESION <- as.factor(datos$CAMBIO_PRESION)

# manova <- manova(cbind(Proporcion_num_positivos, Proporcion_sin_cambios, Proporcion_num_negativos) ~ (GRUPO_TIPO*ZONA), data = presion_COST_HB_xHUM_wider) 
# manova


# PRESION_BIN (NEGATIVA = 1, NO PRESIÓN O POSITIVA = 0)
# modelo_lm_pres_cost_hb <- glm(PRESION_BIN ~ GRUPO_TIPO * ZONA, contrasts=list(GRUPO_TIPO=contr.sum, ZONA=contr.sum), family=binomial, data = datos)
# Anova(modelo_lm_pres_cost_hb)
# summary(modelo_lm_pres_cost_hb)

# PRESION_BIN + MODELO MIXTO (NEGATIVA = 1, NO PRESIÓN O POSITIVA = 0)
modelo_glmer_pres_cost_hb <- glmer(PRESION_BIN ~ GRUPO_TIPO * ZONA + (1 | NOMBRE_HUM), family = binomial, data=datos)
Anova(modelo_glmer_pres_cost_hb)
# Interacción significativa

# Obtener las estimaciones marginales
emmeans_result <- emmeans(modelo_glmer_pres_cost_hb, ~ GRUPO_TIPO * ZONA)

# Realizar comparaciones post-hoc y obtener letras de significancia
comparisons <- cld(emmeans_result, alpha = 0.05, Letters = letters)

# Mostrar resultados con letras de significancia
print(comparisons)




# # MODELO LINEAL MIXTO
# modelo_lme_pres_cost_hb <- lme(CAMBIO_PRESION ~ GRUPO_TIPO * ZONA, random = ~ 1 | NOMBRE_HUM,data=datos)
# anova(modelo_lme_pres_cost_hb)

# Obtener las medias marginales estimadas y comparaciones
emm <- emmeans(modelo_lme_pres_cost_hb, ~ GRUPO_TIPO * ZONA)
# Aplicar comparación de letras de significancia
letras_significancia <- cld(emm, Letters = letters, adjust = "Bonferroni")
# Mostrar el resultado
print(letras_significancia)  #a a a b

# GRAFICO DE SOLO NEGATIVO
suma_presion_cost_int_4 <- datos %>%
  group_by(ZONA, GRUPO_TIPO) %>%
  summarise(
    # Contar la cantidad de valores positivos
    num_positivos = sum(PRESION_BIN == 0, na.rm = TRUE), #OTRA OPCIÓN: suma_positivos = sum(CAMBIO_PRESION[CAMBIO_PRESION > 0], na.rm = TRUE),
    # Contar la cantidad de valores negativos
    num_negativos = sum(PRESION_BIN == 1, na.rm = TRUE),  #OTRA OPCIÓN: suma_negativos = abs(sum(CAMBIO_PRESION[CAMBIO_PRESION < 0], na.rm = TRUE)),
    total_puntos = n()  # Número total de puntos
  ) %>%
  pivot_longer(cols = c(num_positivos, num_negativos),  # Transformar a formato largo para apilamiento
               names_to = "Categoria",
               values_to = "Valor") %>%
  mutate(Proporcion = Valor / total_puntos) %>%  # Calcular la proporción sobre el total
  mutate(Categoria = factor(Categoria, levels = c("num_positivos", "num_negativos")))  # Definir el orden de las categorías


ggplot(suma_presion_cost_int_4, aes(x = interaction(ZONA, GRUPO_TIPO), y = Proporcion, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack") +  # Apilar las barras
  labs(title = "Presión de los cambios de uso del suelo
       según situación con respecto al humedal y a la costa",
       x = "Situación con respecto al humedal y a la costa", 
       y = "Proporción (%)",
       fill = "Categoría de presión") +  # Cambiar título de la leyenda
  scale_x_discrete(labels = c("Buffer.Costeros" = "Buffer-costeros", 
                              "Buffer.Interiores" = "Buffer-interiores",
                              "Humedal.Costeros" = "Humedal-costeros", 
                              "Humedal.Interiores" = "Humedal-interiores")) +  # Cambiar etiquetas del eje X
  scale_fill_manual(values = c("num_positivos" = "#04bcc4", "num_negativos" = "#fc746c"),  # Colores en el orden deseado
                    labels=c("Sin presión", "Presión")) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),  # Centrar y reducir el tamaño del título
        panel.background = element_rect(fill = "white", color = NA),  # Fondo blanco
        panel.grid.major = element_line(color = "gray90"),  # Líneas de cuadrícula principales en gris claro
        panel.grid.minor = element_line(color = "gray95"),   # Líneas de cuadrícula menores en un gris más claro
        axis.text.x = element_text(size = 12),
        plot.title.position = "panel",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12,face="bold") # Centrar y reducir el tamaño del título
  )






################################################################################

# Cálculo de la suma de positivos, negativos y el número total de puntos
suma_presion_cost_int_3 <- datos %>%
  group_by(ZONA, GRUPO_TIPO) %>%
  summarise(
    # Contar la cantidad de valores positivos
    num_positivos = sum(CAMBIO_PRESION > 0, na.rm = TRUE), #OTRA OPCIÓN: suma_positivos = sum(CAMBIO_PRESION[CAMBIO_PRESION > 0], na.rm = TRUE),
    # Contar la cantidad de valores negativos
    num_negativos = sum(CAMBIO_PRESION < 0, na.rm = TRUE),  #OTRA OPCIÓN: suma_negativos = abs(sum(CAMBIO_PRESION[CAMBIO_PRESION < 0], na.rm = TRUE)),
    total_puntos = n()  # Número total de puntos
  ) %>%
  mutate(sin_cambios = total_puntos - num_positivos - abs(num_negativos)) %>%   # Calcular ceros
  pivot_longer(cols = c(num_positivos, num_negativos,sin_cambios),  # Transformar a formato largo para apilamiento
               names_to = "Categoria",
               values_to = "Valor") %>%
  mutate(Proporcion = Valor / total_puntos) %>%  # Calcular la proporción sobre el total
  mutate(Categoria = factor(Categoria, levels = c("num_positivos", "num_negativos","sin_cambios")))  # Definir el orden de las categorías

################################################################################

# Gráfico apilado
ggplot(suma_presion_cost_int_3, aes(x = interaction(ZONA, GRUPO_TIPO), y = Proporción, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack") +  # Apilar las barras
  labs(title = "Presión de los cambios de uso del suelo
       según situación con respecto al humedal y a la costa",
       x = "Situación con respecto al humedal y a la costa", 
       y = "Proporción (%)",
       fill = "Categoría de presión") +  # Cambiar título de la leyenda
  scale_x_discrete(labels = c("Buffer.Costeros" = "Buffer-costeros", 
                              "Buffer.Interiores" = "Buffer-interiores",
                              "Humedal.Costeros" = "Humedal-costeros", 
                              "Humedal.Interiores" = "Humedal-interiores")) +  # Cambiar etiquetas del eje X
  scale_fill_manual(values = c("num_positivos" = "#04bcc4", "num_negativos" = "#fc746c","sin_cambios" = "#D3D3D3"),  # Colores en el orden deseado
                    labels=c("Cambios positivos", "Cambios negativos","Sin cambios")) +
  theme(plot.title = element_text(hjust = 0.5, size = 13),  # Centrar y reducir el tamaño del título
        panel.background = element_rect(fill = "white", color = NA),  # Fondo blanco
        panel.grid.major = element_line(color = "gray90"),  # Líneas de cuadrícula principales en gris claro
        panel.grid.minor = element_line(color = "gray95"),   # Líneas de cuadrícula menores en un gris más claro
        axis.text.x = element_text(size = 12),
        plot.title.position = "panel",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12,face="bold") # Centrar y reducir el tamaño del título
  )











################################################################################
                            # ANÁLISIS MODELOS 
################################################################################
#Preg 1: cambios son similares/diferentes interior-costa y protegido-no protegido: graf barras <- % cambio y buffer-humedal + modelo mixto
datos$CAMBIO <- as.numeric(as.factor(datos$CAMBIO)) - 1  # Convertir a 0 y 1 si es necesario
datos$GRUPO_TIPO <- as.factor(datos$GRUPO_TIPO)
datos$ZONA <- as.factor(datos$ZONA)
# datos$CAMBIO_PRESION <- as.factor(datos$CAMBIO_PRESION)

modelo_glm_camb_cost_hb <- glm(CAMBIO ~ GRUPO_TIPO * ZONA, contrasts=list(GRUPO_TIPO=contr.sum, ZONA=contr.sum),family=binomial, data = datos)
Anova(modelo_glm_camb_cost_hb)
summary(modelo_glm_camb_cost_hb)
# Interacción significativa: de interior x humedal.

# Obtener las medias marginales estimadas y comparaciones
emm1 <- emmeans(modelo_glm_camb_cost_hb, ~ GRUPO_TIPO * ZONA)

# Aplicar comparación de letras de significancia
letras_significancia1 <- cld(emm1, Letters = letters, adjust = "bonferroni")
letras_significancia1




CAMBIOxcost_int_1 <- table(datos$CAMBIO,datos$GRUPO_TIPO)
# Convertir los valores en proporciones
CAMBIOxcost_int <- prop.table(CAMBIOxcost_int_1, margin = 2)  # Margen 2 asegura que sumen 1 por columna
rownames(CAMBIOxcost_int)<- c("Sin cambios", "Con cambios")

barplot(CAMBIOxcost_int,
        ylab = "Puntos de análisis (nº)",
        main = "Presencia de HIC en función de la situación con respecto al humedal",
        cex.main=0.9,
        col = c("mistyrose3", "palegreen3"),  # Colores para las barras
        ylim = c(0, max(CAMBIOxcost_int) * 1.2),  # Ajustar el límite del eje Y para dar espacio a la leyenda
        legend.text = c("Sin cambios", "Con cambios"),
        args.legend = list(x = "bottom",  # Ubicar la leyenda
                           horiz = TRUE,   # Poner la leyenda en horizontal
                           inset = c(0, -0.35),  # Ajustar la posición de la leyenda dentro del gráfico
                           cex = 0.9))     # Ajustar el tamaño del texto de la leyenda



  # CONTINUAR CON ANALISIS Y GRÁFICOS

################################################################################
# GRÁFICOS PLANTILLA
ggplot(datos.p, aes(x = wetland_or_buffer, y = ndvi_periods, fill = year_periods)) +
  geom_boxplot() +
  labs(title = "Relación entre NDVI y situación con respecto al humedal por periodos",
       x = "Humedal o buffer",
       y = "NDVI",
       fill = "Periodos de análisis") +
  theme_minimal() +
  scale_fill_manual(values = c("1999_2010" = "#DEB887", "2011_2021" = "#87CEFF")) +
  facet_wrap(~ year_periods)

ggplot(comparacion_resumen, aes(x = ZONA, y = count, fill = CAMBIO)) +
  geom_bar(stat = "identity") +
  labs(title = "Cambios de uso del suelo en función de situación con respecto al humedal",
       x = "Zona (dentro/fuera del humedal)", y = "Puntos de análisis (nº)") +
  scale_fill_manual(values = c("0" = "honeydew3", "1" = "darkorchid1")) +
  theme_minimal()

ggplot(count_humedales, aes(x = ZONA, y = GRUPO_TIPO, fill = CAMBIO)) +
  geom_bar(stat = "identity") +
  labs(title = "Cambios de uso del suelo en función de situación con respecto al humedal",
       x = "Zona (dentro/fuera del humedal)", y = "Puntos de análisis (nº)") +
  scale_fill_manual(values = c("0" = "honeydew3", "1" = "darkorchid1")) +
  theme_minimal()
