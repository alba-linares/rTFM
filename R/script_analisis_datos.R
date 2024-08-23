library(readxl)
datos <-read_excel("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Resultados_puntos_analisis.xlsx", range = "A1:O2081")
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

library(dplyr)
df_area_usos84 <-read_excel("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Resultados_puntos_analisis.xlsx", sheet = 2)
df_area_usos20 <-read_excel("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Resultados_puntos_analisis.xlsx", sheet = 3)

result_area84 <- df_area_usos84 %>%
  group_by(df_area_usos84$`_USOS_DEFI`) %>%
  summarise(Total_Area = sum(AREA_POLIG))

result_area20 <- df_area_usos20 %>%
  group_by(`_USOS_DEFI`) %>%
  summarise(Total_Area = sum(AREA_POLIG))
