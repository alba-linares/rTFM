library(readxl)
library(lme4)
library(ez)

datos <-read_excel("C:/Users/VE-UGR-0208/Desktop/TFM/rTFM/Google Earth Engine/LSWI_zones_hum_buf.xlsx")

modelo <-lmer(lswi ~ location + year, data=datos)

#ANOVA de medidas repetidas####################################################################################################################
#Asunciones: esfericidad y el resto
demo1 <- within(datos, (year <-factor(year)))
demo1 <- within(demo1, (zone <-factor(zone)))
demo1 <- within(demo1, (location <-factor(location)))
demo1 <- within(demo1, (wetland_name <-factor(wetland_name)))
lmer(lswi ~ location + conservation + wetland_or_buffer + environmental_protection + conservation + sup_ha + (1 | year), data = datos)
# A este se le comprueba todas las asunciones menos la de esfericidad
ezANOVA(data = demo1, dv=.(lswi), wid=.(zone), within=.(year), between=.(location), detailed=TRUE) # A este se le comprueba la asunción de esfericidad

#vd = variable dependiente
#wid = es la réplica o factor aleatorio entre réplicas (acuario, montaña, laguna...)
#within = variable interna dentro del diseño que se debe al tiempo
#between = entre qué queremos comparar: factor
#detailed = para que me dé todas las salids, incluyendo análisis de esfericidad




shapiro.test(residuals(modelo2)) 			#NORMALIDAD
lillie.test(residuals(modelo2)) 			#NORMALIDAD LILLIEFORS
bptest(modelo2)		            			#HOMOCEDASTICIDAD
leveneTest(dep~factor1, data=dataset, center="median") 	#HOMOCEDASTICIDAD
resettest(modelo2)	          			#LINEALIDAD
outlierTest(modelo2)         				#OUTLIER
vif(modelo2)                 				#REDUNDANCIA





