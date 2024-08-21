library(readxl)
datos <-read_excel("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Resultados_puntos_analisis.xlsx", range = "A1:O2081")
?ggplot
summary(datos)

pie(table(datos$COMPARACION))
pie(table(datos$MUCVA__USO)) #Usos 1984
pie(table(datos$SIOSE__USO)) #Usos 2023


library(ggplot2)

# Gráfico de barras apiladas
ggplot(datos, aes(x = datos$MUCVA__USO, fill = COMPARACION)) +
  geom_bar(position = "fill") +
  labs(title = "Comparación de Usos del Suelo entre 1984 y 2023", x = "Uso del Suelo en 1984", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=0))

library(dplyr)
# Contar las frecuencias de cada clase de uso del suelo por humedal
tabla_frecuencia <- table(datos$NOMBRE_HUM, datos$MUCVA__USO)

library(dplyr)

# Contar la frecuencia de cada clase por humedal
frecuencia_clase_humedal <- datos %>%
  group_by(NOMBRE_HUM, MUCVA__USO) %>%
  count()

# Ver los resultados
View(frecuencia_clase_humedal)



# Gráfico de barras apiladas para mostrar la proporción de cada uso del suelo por humedal
ggplot(datos, aes(x = MUCVA__USO, fill = NOMBRE_HUM)) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de Usos del Suelo en 1984 y Actualidad por Humedal", 
       x = "Uso del Suelo", 
       y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

