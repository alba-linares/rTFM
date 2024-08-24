# Cargar las librerías
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Leer los archivos
datos <-read_excel("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Resultados_puntos_analisis.xlsx", range = "A1:O2081")
View(datos)

df_area_usos84 <-read_excel("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Resultados_puntos_analisis.xlsx", sheet = 2)
df_area_usos20 <-read_excel("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Resultados_puntos_analisis.xlsx", sheet = 3)

################################################################################
# Análisis exploratorio de los datos: gráficas #################################
                    pie(table(datos$COMPARACION))
                    pie(table(datos$MUCVA__USO)) #Usos 1984
                    pie(table(datos$SIOSE__USO)) #Usos 2023

# Gráfico de barras apiladas
                    ggplot(datos, aes(x = datos$MUCVA__USO, fill = COMPARACION)) +
                      geom_bar(position = "fill") +
                      labs(title = "Comparación de Usos del Suelo entre 1984 y 2023", x = "Uso del Suelo en 1984", y = "Proporción") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))


# Contar las frecuencias de cada clase de uso del suelo por humedal
tabla_frecuencia <- table(datos$NOMBRE_HUM, datos$MUCVA__USO)

# Contar la frecuencia de cada clase por humedal
frecuencia_clase_humedal <- datos %>%
  group_by(NOMBRE_HUM, MUCVA__USO) %>%
  count()

# Ver los resultados
View(frecuencia_clase_humedal)

################################################################################
# Gráfico de barras apiladas para mostrar la proporción de cada uso del suelo por humedal RELATIVO
                    ggplot(datos, aes(x = MUCVA__USO, fill = NOMBRE_HUM)) +
                      geom_bar(position = "fill") +
                      labs(title = "Proporción de usos del suelo en 1984 por humedal", 
                           x = "Uso del suelo", 
                           y = "Proporción") +
                      scale_fill_viridis_d(option = "H") + #B o H están bien, se distinguen los humedales
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0))
                    
                    ggplot(datos, aes(x = SIOSE__USO, fill = NOMBRE_HUM)) +
                      geom_bar(position = "fill") +
                      labs(title = "Proporción de usos del suelo en la actualidad por humedal", 
                       x = "Uso del suelo", 
                       y = "Proporción") +
                      scale_fill_viridis_d(option = "H") + #B o H están bien, se distinguen los humedales
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))

# ABSOLUTO #####################################################################
                    ggplot(datos, aes(x = MUCVA__USO, fill = NOMBRE_HUM)) +
                      geom_bar(position = "stack") +  # Cambia "fill" por "stack" para contar valores absolutos
                      labs(title = "Número de hectáreas por uso del suelo en 1984 por humedal", 
                           x = "Uso del suelo", 
                           y = "Hectáreas") +
                      scale_fill_viridis_d(option = "H") + #B o H están bien, se distinguen los humedales
                      ylim(0, 800) +  # Establece el límite del eje y entre 0 y 800
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0))
                    
                    ggplot(datos, aes(x = SIOSE__USO, fill = NOMBRE_HUM)) +
                      geom_bar(position = "stack") +  # Cambia "fill" por "stack" para contar valores absolutos
                      labs(title = "Número de hectáreas por uso del suelo en la actualidad por humedal", 
                           x = "Uso del suelo", 
                           y = "Hectáreas") +
                      scale_fill_viridis_d(option = "H") + #B o H están bien, se distinguen los humedales
                      ylim(0, 800) +  # Establece el límite del eje y entre 0 y 800
                        theme_minimal() +
                      theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))


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
# Agrupar por ZONA (BUFFER VS HUMEDAL) y COMPARACION (IGUAL VS DISTINTO), y contar las ocurrencias
comparacion_resumen <- datos %>%
  group_by(ZONA, COMPARACION) %>%
  summarize(count = n()) %>%
  ungroup()

print(comparacion_resumen)

# Calcular proporciones
comparacion_proporciones <- comparacion_resumen %>%
  group_by(ZONA) %>%
  mutate(proporcion = count / sum(count))

print(comparacion_proporciones)

# Gráfico de barras apiladas
                    ggplot(comparacion_proporciones, aes(x = ZONA, y = proporcion, fill = COMPARACION)) +
                      geom_bar(stat = "identity") +
                      labs(title = "Comparación de Cambios de Uso del Suelo Dentro y Fuera de Humedales",
                           x = "Zona (Dentro/Fuera del Humedal)", y = "Proporción") +
                      scale_y_continuous(labels = scales::percent_format()) +
                      theme_minimal()

################################################################################
# Para evaluar los cambios específicos de uso del suelo ########################
# Crear una columna que muestre el cambio de uso del suelo
data <- datos %>%
  mutate(cambio_uso = paste(MUCVA__USO, "a", SIOSE__USO))

# Ver los primeros registros para confirmar
head(data)

# Agrupar por zona y tipo de cambio de uso del suelo, y contar las ocurrencias
cambio_uso_buf_hum <- data %>%
  group_by(ZONA, cambio_uso) %>%
  summarize(count = n()) %>%
  ungroup()

# Ver los resultados
print(cambio_uso_buf_hum)







# Gráfico de barras apiladas por tipo de cambio
                    ggplot(cambio_uso_buf_hum, aes(x = ZONA, y = count, fill = cambio_uso)) +
                      geom_bar(stat = "identity") +
                      labs(title = "Cambios de Uso del Suelo Dentro y Fuera de Humedales",
                           x = "Zona (Dentro/Fuera del Humedal)", y = "Número de Cambios") +
                      theme_minimal()
                    
# Gráfico de barras agrupadas
                    ggplot(cambio_uso_buf_hum, aes(x = cambio_uso, y = count, fill = ZONA)) +
                      geom_bar(stat = "identity", position = "dodge") +
                      labs(title = "Cambios Específicos de Uso del Suelo por Zona",
                           x = "Cambio de Uso del Suelo", y = "Número de Cambios") +
                      theme_minimal() +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
                    ggplot(cambio_uso_buf_hum, aes(x = count, y = reorder(cambio_uso, count))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                      labs(title = "Cambios de Uso del Suelo",
                           x = "Número de Cambios", y = "Cambio de Uso del Suelo") +
                      theme_minimal()
                    
# Filtrar para mostrar solo las 10 categorías más comunes
                    top_cambios <- cambio_uso_buf_hum %>%
                      top_n(10, count)
                    
# Gráfico de barras horizontales con las 10 categorías más comunes
                    ggplot(top_cambios, aes(x = count, y = reorder(cambio_uso, count))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                      labs(title = "Top 10 Cambios de Uso del Suelo",
                           x = "Número de Cambios", y = "Cambio de Uso del Suelo") +
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
                      labs(title = "Cambios de Uso del Suelo (Agrupados)",
                           x = "Número de Cambios", y = "Cambio de Uso del Suelo") +
                      theme_minimal()


# SOLO CAMBIOS (COMPARACION = DISTINTO) ########################################
# Filtrar los datos para obtener solo los cambios distintos ####################
data_distintos <- data %>%
  filter(COMPARACION == "DISTINTO")
# Crear una columna que muestre el cambio de uso del suelo
data_distintos <- data_distintos %>%
  mutate(cambio_uso = paste(MUCVA__USO, "a", SIOSE__USO))
# Agrupar por tipo de cambio de uso del suelo y contar las ocurrencias
cambio_uso_buf_hum <- data_distintos %>%
  group_by(cambio_uso) %>%
  summarize(count = n()) %>%
  ungroup()

# Ver los resultados
print(cambio_uso_buf_hum)
# Gráfico de barras horizontales para cambios distintos
                    ggplot(cambio_uso_buf_hum, aes(x = count, y = reorder(cambio_uso, count))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                      labs(title = "Cambios de Uso del Suelo (DISTINTO)",
                           x = "Número de Cambios", y = "Cambio de Uso del Suelo") +
                      theme_minimal()

# Mostrar solo las 10 categorías más comunes
top_cambios <- cambio_uso_buf_hum %>%
  top_n(10, count)

# Gráfico de barras horizontales con las 10 categorías más comunes
                    ggplot(top_cambios, aes(x = count, y = reorder(cambio_uso, count))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                      labs(title = "Top 10 Cambios de Uso del Suelo (DISTINTO)",
                           x = "Número de Cambios", y = "Cambio de Uso del Suelo") +
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
                      labs(title = "Cambios de Uso del Suelo (DISTINTO, Agrupados)",
                           x = "Número de Cambios", y = "Cambio de Uso del Suelo") +
                      theme_minimal()


#
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
View(tabla_ancha_ci)










################################################################################
# Filtrar datos dentro y fuera de humedales y costeros-interiores ##############
dentro_humedales <- datos %>% filter(ZONA == "Humedal")
fuera_humedales <- datos %>% filter(ZONA != "Humedal")

# Filtrar humedales costeros e interiores
humedales_costeros <- dentro_humedales %>% filter(GRUPO_TIPO == "Costeros")
humedales_interior <- dentro_humedales %>% filter(GRUPO_TIPO != "Costeros")

# Contar los registros en cada grupo
count_humedales <- datos %>%
  group_by(ZONA, GRUPO_TIPO) %>%
  summarize(n = n())

print(count_humedales)

# Gráfico de barras para comparar las zonas
                    ggplot(count_humedales, aes(x = GRUPO_TIPO, y = n, fill = ZONA)) +
                      geom_bar(stat = "identity", position = "dodge") +
                      labs(title = "Comparación de Zonas y Tipos de Humedales",
                           x = "Tipo de Humedal", y = "Número de Registros") +
                      theme_minimal()

