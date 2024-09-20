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
# Install the required packages

install.packages("zyp")
install.packages("terra")
install.packages("spatialEco")

# Load required packages

library(spatialEco)
library(terra)














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


