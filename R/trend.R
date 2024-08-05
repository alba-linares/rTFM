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