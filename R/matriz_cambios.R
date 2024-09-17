# Cargar las librerías
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Leer los archivos
datos <-read_excel("D:/Escritorio/MASTER/TFM/rTFM/Resultados_puntos_analisis.xlsx", range = "A1:O2081")
head(datos)

df_area_usos84 <-read_excel("D:/Escritorio/MASTER/TFM/rTFM/Resultados_puntos_analisis.xlsx", sheet = 2)
df_area_usos20 <-read_excel("D:/Escritorio/MASTER/TFM/rTFM/Resultados_puntos_analisis.xlsx", sheet = 3)

################################################################################
# Matriz de doble entrada con los cambios
# Crear una tabla de contingencia para los cambios de uso del suelo
matriz_cambios_general <- table(datos$MUCVA1984_GENERAL, datos$SIOSE2020_GENERAL)
matriz_cambios <- table(datos$MUCVA1984, datos$SIOSE2020)

# Ver la matriz
print(matriz_cambios)

library(ggplot2)

# Convertir la matriz de cambios a un data frame para usar ggplot
df_matriz <- as.data.frame(as.table(matriz_cambios))

# Crear el gráfico de calor con números
ggplot(df_matriz, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +  # Color blanco para separar las celdas
  geom_text(aes(label = Freq), color = "black", size = 4) +  # Colocar los números sobre las celdas
  scale_fill_gradient(low = "white", high = "cyan4") +  # Gradiente de color
  labs(x = "Uso del Suelo en 1984", y = "Uso del Suelo en 2020", fill = "Frecuencia") +
  theme_minimal()

#OTRO MÉTODO: TABLA DE CONTINGENCIA (https://r-coder.com/tabla-contingencia-r/)
tabla <- prop.table(matriz_cambios, margin = 1)
tabla_2 <- prop.table(tabla, margin = 2)
tabla_2 
tabla_prop <- prop.table(tabla)
tabla_prop
tabla_3 <- addmargins(tabla_prop * 100)
tabla_3
