#library
library(funtimes)
library(Kendall)

# Data
data<-read.delim('D:/Escritorio/MASTER/TFM/rTFM/R/LSWI_zones/lswi_zone15.csv', sep=",", dec = ".")

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

install.packages(c("raster", "rgdal", "sp"))
library(raster)
library(rgdal)
library(sp)

# Leer un único archivo raster
raster1 <- raster("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/Mann-Kendall/zone01b_tau.tif")
raster2 <- raster("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/Mann-Kendall/zone02b_tau.tif")
raster3 <- raster("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/Mann-Kendall/zone03b_tau.tif")

library(raster)

# Definir el directorio donde se encuentran los archivos
directory <- "C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/Mann-Kendall/"

# Crear un vector para almacenar todos los nombres de archivos
file_names <- c()

# Rellenar el vector con los nombres de archivos desde zone01b_tau.tif a zone15b_tau.tif
for (i in 1:15) {
  file_names <- c(file_names, paste0(directory, "zone", sprintf("%02d", i), "b_tau.tif"))
  file_names <- c(file_names, paste0(directory, "zone", sprintf("%01d", i), "_tau.tif"))
}

# Cargar todos los archivos raster en una lista
raster_list <- lapply(file_names, raster)


# Calcular estadísticas básicas para un único raster
mean_value <- cellStats(raster1, stat='mean')
median_value <- cellStats(raster1, stat='median')
sd_value <- cellStats(raster1, stat='sd')

# Crear una lista para almacenar las estadísticas
stats_list <- list()

# Calcular estadísticas para cada raster en la lista
for (i in 1:length(raster_list)) {
  raster_data <- raster_list[[i]]
  
  # Calcular estadísticas
  mean_value <- cellStats(raster_data, stat='mean')
  sd_value <- cellStats(raster_data, stat='sd')
  min_value <- cellStats(raster_data, stat='min')
  max_value <- cellStats(raster_data, stat='max')
  
  # Guardar estadísticas en una lista
  stats_list[[i]] <- data.frame(
    file_name = file_names[i],
    mean = mean_value,
    sd = sd_value,
    min = min_value,
    max = max_value
  )
}

# Combinar todos los resultados en un solo data frame
all_stats <- do.call(rbind, stats_list)

# Ver las estadísticas
print(all_stats)

as.data.frame(stats_list) %>%
  pivot_longer(names_to = Var1, values_to = Freq)
