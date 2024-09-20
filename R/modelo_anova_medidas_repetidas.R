library(readxl)
library(lme4)
library(ez)

setwd("D:/Escritorio/TFM/rTFM")
datos <-read_excel("Google Earth Engine/LSWI_zones_hum_buf.xlsx")



#ANOVA de medidas repetidas####################################################################################################################
#Asunciones: esfericidad y el resto
datos <- datos %>%
  mutate(zone_converged = paste(datos$zone, "'(", datos$wetland_or_buffer, ")'"))

demo1 <- within(datos, (year <-factor(year)))
demo1 <- within(demo1, (zone <-factor(zone$converged)))
demo1 <- within(demo1, (location <-factor(location)))
demo1 <- within(demo1, (wetland_name <-factor(wetland_name)))
#lmer(lswi ~ location + (1 | year), data = demo1)
anova(modelo<-lmer(lswi ~ location*year + (1 | zone_converged), data=demo1))

# A este se le comprueba todas las asunciones menos la de esfericidad
ezANOVA(data = demo1, dv=.(lswi), wid=.(zone_converged), within=.(year), between=.(location), detailed=TRUE) # A este se le comprueba la asunción de esfericidad
#dv: dependence variable -> Zooplancton
#b. wid: factor aleatorio -> acuario
#c. within: variabilidad interna dentro del diseño que se debe al tiempo
#d. between: factor
#e. detailed: para que me de todas las salidas, incluyendo en análisis de esfericidad.

#vd = variable dependiente
#wid = es la réplica o factor aleatorio entre réplicas (acuario, montaña, laguna...)
#within = variable interna dentro del diseño que se debe al tiempo
#between = entre qué queremos comparar: factor
#detailed = para que me dé todas las salids, incluyendo análisis de esfericidad

###################################### si lmer()
library(emmeans)
lsm <- lsmeans(LSWI, ~ toxico)
lsm <- lsmeans(ay7, ~ toxico * time)
summary(pairs(lsm), type = "response")

#ANOVA########################################################################################################################
lm(dep ~ factor1 * factor2, data=)
summary(modelo)
Anova(modelo, type=3)

############################### ¿Se ve que no hay interacción? Entonces...
lm(dep ~ factor1 + factor2, data=)
summary(modelo)
Anova(modelo, type=2)

#POST-HOC
TukeyHSD() #si aov()
###################################### si lm()
library(multcomp) 
Tukey<-glht(modelo, mcp(datos$lswi="Tukey"))
summary(Tukey)
#PARTICIÓN DE LA VARIANZA
calc.relimp(modelo, type=c("lmg", "first", "last"))




shapiro.test(residuals(modelo2)) 			#NORMALIDAD
lillie.test(residuals(modelo2)) 			#NORMALIDAD LILLIEFORS
bptest(modelo2)		            			#HOMOCEDASTICIDAD
leveneTest(dep~factor1, data=dataset, center="median") 	#HOMOCEDASTICIDAD
resettest(modelo2)	          			#LINEALIDAD
outlierTest(modelo2)         				#OUTLIER
vif(modelo2)                 				#REDUNDANCIA





