#vignette("ggplot2-specs")

#library
library(funtimes)
library(Kendall)

# Data
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone01b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone02b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone03b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone04b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone05b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone06b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone07b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone08b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone09b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone10b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone11b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone12b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone13b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone14b.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone15b.csv", sep=",", dec = ".")

data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone01.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone02.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone03.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone04.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone05.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone06.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone07.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone08.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone09.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone10.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone11.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone12.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone13.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone14.csv", sep=",", dec = ".")
data<-read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone15.csv", sep=",", dec = ".")

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



########

datalist<-list(read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone01b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone02b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone03b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone04b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone05b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone06b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone07b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone08b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone09b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone10b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone11b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone12b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone13b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone14b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_buffer/lswi_zone15b.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone01.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone02.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone03.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone04.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone05.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone06.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone07.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone08.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone09.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone10.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone11.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone12.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone13.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone14.csv", sep=",", dec = "."),
            read.delim("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/zones_humedal/lswi_zone15.csv", sep=",", dec = "."))

# Inicializar las variables que almacenarán los resultados
n <- 30  # Número de archivos que esperas procesar
intercept_lm <- numeric(n)
slope_lm <- numeric(n)
p_value_slope <- numeric(n)
r_squared <- numeric(n)
adj_r_squared <- numeric(n)

tau_mk_test_2 <- numeric(n)
p_value_mk_test_2 <- numeric(n)


for (i in 1:length(datalist)) {
  data <- datalist[[i]]
  
  # Asegurarte de que LSWI es numérico y un vector
  data$LSWI <- as.numeric(as.character(data$LSWI))
  
  if (any(is.na(data$LSWI))) {
    data <- na.omit(data)
  }
  
  # Realizar los análisis
  if (is.vector(data$LSWI) && !is.matrix(data$LSWI)) {
    nt_test <- notrend_test(data$LSWI)
    mk_test <- notrend_test(data$LSWI, test='MK')
    lm_model <- lm(LSWI ~ Year, data = data)
    summary_lm <- summary(lm_model)
    mk_test_2 <- MannKendall(data$LSWI)
    
    intercept_lm[i] <- summary_lm$coefficients[1, "Estimate"]
    slope_lm[i] <- summary_lm$coefficients[2, "Estimate"]
    p_value_slope[i] <- summary_lm$coefficients[2, "Pr(>|t|)"]
    r_squared[i] <- summary_lm$r.squared
    adj_r_squared[i] <- summary_lm$adj.r.squared
    
    tau_mk_test_2[i] <- mk_test_2$tau
    p_value_mk_test_2[i] <- mk_test_2$sl
  } else {
    print(paste("Error en la iteración", i, ": LSWI no es un vector univariado."))
  }
}

# Verificar los resultados
results_df <- data.frame(
  intercept_lm,
  slope_lm,
  p_value_slope,
  r_squared,
  adj_r_squared,
  tau_mk_test_2,
  p_value_mk_test_2
)

rownames(results_df)<-c(c(paste(rep("zone",15),sprintf("%02d", 1:15),"b",sep = "")),c(paste(rep("zone",15),sprintf("%02d", 1:15),sep = "")))
results_df <- format(results_df, scientific = 5)
print(results_df)

setwd("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/R")
write.table(results_df, file ="Mann-Kendall_lm_lswi_results.csv", append=TRUE, sep="\t", row.names = F, col.names=F)

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














#GRÁFICO LSWI HUMEDALES ########################################################
library(ggplot2)

# Tendencia del LSWI en el tiempo separado por humedal
ggplot(datos, aes(x = year, y = lswi, color = wetland_name)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tendencia del LSWI en el tiempo por Humedal",
       x = "Año", y = "LSWI") +
  facet_wrap(~ wetland_name, scales = "free_y") + 
  theme_minimal()




# GRÁFICO CON R^2 AJUSTADO #####################################################
library(ggplot2)
library(dplyr)
library(readxl)
#library(broom) # Necesario para la función glance()

datos <-read_excel("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/LSWI_zones_hum_buf.xlsx")
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


