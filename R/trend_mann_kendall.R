#vignette("ggplot2-specs")

#library
library(funtimes)
library(Kendall)


# Data
setwd("D:/Escritorio/TFM/rTFM") # ¡Recuerda hacer setwd("D:/Escritorio/TFM/rTFM") en el portátil!
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone01b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone02b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone03b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone04b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone05b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone06b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone07b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone08b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone09b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone10b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone11b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone12b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone13b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone14b.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_buffer_lswi/lswi_zone15b.csv", sep=",", dec = ".")

data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone01.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone02.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone03.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone04.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone05.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone06.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone07.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone08.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone09.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone10.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone11.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone12.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone13.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone14.csv", sep=",", dec = ".")
data<-read.delim("Google Earth Engine/zones_humedal_lswi/lswi_zone15.csv", sep=",", dec = ".")

#Option A #library(funtimes)
notrend_test(data$LSWI)
notrend_test(data$LSWI,test='MK') # Usa esta función
#notrend_test(data$LSWI,test='WAVK', factor.length = "adaptive.selection")
plot(data$Year,data$LSWI, type='l') # Pinta siempre los resultados
# apply LM to check
summary(lm(data$LSWI~data$Year))

#Option B# Sólo quería probar otra forma pero da el mismo resultado #library(Kendall)
MannKendall(data$LSWI)

#################
#Examples test

# test (source: https://cran.r-project.org/web/packages/funtimes/vignettes/trendtests.html)
# set.seed(777)
# n <- 100
# Time <- c(1:n)
# X0 <- arima.sim(list(order = c(1, 0, 0), ar = 0.5), n = n, n.start = 100, sd = 0.5)
# X1 <- 2*Time/n + X0
# X11<-as.numeric(X1) # convert TS to a vector of numbers

# plot(X11)
# notrend_test(X11)
# end test

# Example (source: https://rdrr.io/cran/Kendall/man/MannKendall.html)
# data(PrecipGL)
# plot(PrecipGL)
# MannKendall(PrecipGL)

################################################################################
                          # Mann-Kendall espacial #
################################################################################
library(here)

# Install the required packages
# install.packages("zyp")
# install.packages("terra")
# install.packages("spatialEco")

# Load required packages

library(spatialEco)
library(terra)

# Create a multiband terra SpatRaster object with 10 layers

# Definir años
years <- 1999:2021

# Definir la ruta base
base_path <- "D:/Escritorio/TFM/rTFM/Google Earth Engine/zones_lswi/humedal/"

# Definir subrutas para cada zona
paths <- c(
  "zone01h_albufera_honda/",
  "zone02h_albufera_nueva/",
  "zone03h_barranco_del_agua/",
  "zone04h_canada_de_las_norias/",
  "zone05h_cola_del_embalse_del_negratin/",
  "zone06h_charcones_de_punta_entinas/",
  "zone07h_humedales_de_baza/",
  "zone08h_laguna_de_la_gravera/",
  "zone09h_rambla_morales/",
  "zone10h_ribera_de_la_algaida/",
  "zone11h_rio_antas/",
  "zone12h_salinas_de_cabo_de_gata/",
  "zone13h_salar_de_los_canos/",
  "zone14h_salinas_de_cerrillos/",
  "zone15h_saladar_del_margen/"
)

# Nombres de las zonas (corrigiendo zone13h)
zones <- c("zone01h","zone02h","zone03h",
           "zone04h","zone05h","zone06h",
           "zone07h","zone08h","zone09h",
           "zone10h","zone11h","zone12h",
           "zone13h","zone14h","zone15h")

# Crear una lista para almacenar los datos de cada zona
r_zones <- list()

# Loop para cargar los archivos de cada zona sin cambiar el directorio de trabajo
for (i in 1:length(paths)) {
  # Cargar los archivos usando lapply, construyendo la ruta completa
  r_zones[[zones[i]]] <- lapply(years, function(year) {
    # Crear la ruta completa del archivo TIFF
    file_path <- paste0(base_path, paths[i], zones[i], "_lswi_", year, ".tif")
    
    # Cargar el archivo .tif
    rast(file_path)
  })
}

# Ahora tienes todos los archivos cargados en r_zones


# Ahora tienes una lista con los datos cargados para cada zona
# r_zones[["zone01h"]] contiene los archivos de la primera zona
# r_zones[["zone03h"]] contiene los archivos de la segunda zona


# Y ahora paso a rast() cada elemento de esa lista:
r_zone01h <- rast(r_zones$zone01h)
r_zone02h <- rast(r_zones$zone02h)
r_zone03h <- rast(r_zones$zone03h)
r_zone04h <- rast(r_zones$zone04h)
r_zone05h <- rast(r_zones$zone05h)
r_zone06h <- rast(r_zones$zone06h)
r_zone07h <- rast(r_zones$zone07h)
r_zone08h <- rast(r_zones$zone08h)
r_zone09h <- rast(r_zones$zone09h)
r_zone10h <- rast(r_zones$zone10h)
r_zone11h <- rast(r_zones$zone11h)
r_zone12h <- rast(r_zones$zone12h)
r_zone13h <- rast(r_zones$zone13h)
r_zone14h <- rast(r_zones$zone14h)
r_zone15h <- rast(r_zones$zone15h)

# Por ejemplo, r_zone02h se ve así, como 15 gráficas, una para cada año, en ese humedal:
plot(r_zone06h)


# Source: https://www.pmassicotte.com/posts/2022-04-28-changing-spatial-resolution-of-a-raster-with-terra/

# Aggregate the raster using 8 pixels within the horizontal and the vertical directions
r <- list(r_zone01h,r_zone02h, r_zone03h,
          r_zone04h,r_zone05h,r_zone06h,
          r_zone07h,r_zone08h,r_zone09h,
          r_zone10h,r_zone11h,r_zone12h,
          r_zone13h,r_zone14h,r_zone15h)


r8 <- aggregate(r, fact = 8) # approx. higher than 200m resolution
writeRaster(r8,'input_lswi_ts_scale8.tif',overwrite=T)









# Lista de los objetos raster
r_list <- list(r_zone01h,r_zone02h,r_zone03h,
               r_zone04h,r_zone05h,r_zone06h,
               r_zone07h,r_zone08h,r_zone09h,
               r_zone10h,r_zone11h,r_zone12h,
               r_zone13h,r_zone14h,r_zone15h)

# Loop para agregar y escribir raster para cada zona
setwd("D:/Escritorio/TFM/rTFM/R/resample_r8") # Para exportar los .tif a esta carpeta específica
for (i in 1:length(r_list)) {
  # Obtener el raster de la zona actual
  r <- r_list[[i]]
  
  # Agregar con un factor de 8 (mayor resolución aprox. 200m)
  r8 <- aggregate(r, fact = 8)
  
  # Escribir el raster resultante en un archivo TIFF
  output_filename <- paste0("input_", zones[i], "_lswi_ts_scale8.tif")
  writeRaster(r8, output_filename, overwrite = TRUE)
  
  # Imprimir mensaje de progreso
  print(paste("Archivo guardado:", output_filename))
}
setwd("D:/Escritorio/TFM/rTFM") # Vuelvo al setwd de antes

mk <- raster.kendall(r8, method="none")

plot(mk)



















#GRÁFICO LSWI HUMEDALES ########################################################
# Tendencia del LSWI en el tiempo por humedal ##################################
library(ggplot2)

# Tendencia del LSWI en el tiempo separado por humedal
ggplot(datos, aes(x = year, y = lswi, color = wetland_name)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tendencia del LSWI en el tiempo por humedal",
       x = "Año", y = "LSWI") +
  facet_wrap(~ wetland_name, scales = "free_y") + 
  theme_minimal()


# GRÁFICO CON R^2 AJUSTADO #####################################################
library(ggplot2)
library(dplyr)
library(readxl)
#library(broom) # Necesario para la función glance()

datos <-read_excel("Google Earth Engine/LSWI_zones_hum_buf.xlsx")
# Calcular el R^2 ajustado para cada humedal y unir con los datos originales
datos_con_r2 <- datos %>%
  group_by(wetland_name) %>%
  do(modelo = lm(lswi ~ year, data = .)) %>%
  mutate(adj_r2 = summary(modelo)$adj.r.squared) %>%
  ungroup() %>%
  left_join(datos, by = "wetland_name")

# Gráfico con R^2 ajustado
ggplot(datos_con_r2, aes(x = year, y = lswi, color = wetland_name)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tendencia del LSWI en el tiempo por Humedal",
       x = "Año", y = "LSWI") +
  facet_wrap(~ wetland_name, scales = "free_y") + 
  theme_minimal() +
  geom_text(data = datos_con_r2 %>% distinct(wetland_name, adj_r2), 
            aes(x = Inf, y = Inf, label = paste("R² adj: ", round(adj_r2, 3))),
            hjust = 1.1, vjust = 1.1, inherit.aes = FALSE, size = 3)

# GRAFICO CON LINEAS DE BUFFER Y DE HUMEDAL SEPARADAS
datos$wetland_or_buffer = recode(datos$wetland_or_buffer,
                                    "wetland" = "Humedal",
                                    "buffer" = "Buffer")


datos_con_r2 <- datos %>%
  group_by(wetland_name, wetland_or_buffer) %>%
  summarise(adj_r2 = summary(lm(lswi ~ year, data = .))$adj.r.squared) %>%
  ungroup()

nombre_humedales_correctos <- c(
  "albufera_honda" = "Albufera Honda",
  "albufera_nueva" = "Albufera Nueva",
  "barranco_del_agua" = "Barranco del Agua",
  "canada_de_las_norias" = "Cañada de las Norias",
  "cola_del_embalse_del_negratin" = "Cola del embalse del Negratín",
  "charcones_de_punta_entinas" = "Charcones de Punta-Entinas",
  "humedales_de_baza" = "Humedales de Baza",
  "laguna_de_la_gravera" = "Laguna de la Gravera",
  "rambla_morales" =  "Rambla Morales",
  "ribera_de_la_algaida" = "Ribera de la Algaida",
  "rio_antas" = "Río Antas",
  "salinas_de_cabo_de_gata" = "Salinas de Cabo de Gata",
  "salar_de_los_canos" = "Salar de los Canos",
  "salinas_de_cerrillos" = "Salinas de Cerrillos",
  "saladar_del_margen" = "Saladar del Margen")
  
# Crear el gráfico con líneas separadas por "wetland_or_buffer"
ggplot(datos, aes(x = year, y = lswi, color = wetland_or_buffer)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Humedal" = "blue", "Buffer" = "orange")) +
  labs(title = "Tendencia del índice LSWI en el tiempo por humedal",
       x = "Año", y = "LSWI", color = "Tipo de área") +
  facet_wrap(~ wetland_name, scales = "free_y", labeller = labeller(wetland_name = nombre_humedales_correctos) 
) + 
  coord_cartesian(ylim = c(-0.05, 0.12)) +
  
  theme_minimal() +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  theme(plot.title = element_text(hjust=0.5))
  #geom_text(data = datos_con_r2, 
    #         aes(x = Inf, y = Inf, label = paste("R² adj: ", round(adj_r2, 3))),
    #        hjust = 1.1, vjust = 1.1, inherit.aes = FALSE, size = 3)



# Explicación del código: ######################################################
#aes(x = year, y = lswi, color = wetland_name): Establece el año en el eje x y el LSWI en el eje y, y colorea las líneas según el nombre del humedal.
#geom_line(): Dibuja las líneas de LSWI a lo largo del tiempo.
#geom_smooth(method = "lm", se = FALSE): Añade una línea de tendencia (regresión lineal) para cada humedal sin mostrar el intervalo de confianza (al usar se = FALSE).
#facet_wrap(~ wetland_name, scales = "free_y"): Crea un gráfico separado para cada humedal. scales = "free_y" permite que cada gráfico tenga su propio rango en el eje y, adaptado a los valores de LSWI de cada humedal.
#theme_minimal(): Aplica un tema minimalista al gráfico para mejorar la claridad visual.


