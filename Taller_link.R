
# Cargar Librerias
library(rgeos)
library(sp)
library(maptools)
library(car)
library(geoR)
library(gstat)
library(gdata)
library(readxl)
library(dplyr)


# Cargar datos
datosdf = read.table("RNSC_Antioquia_2017_2018.txt", sep = "\t", dec = ",", header = T)

datosdf$periodo <- as.Date(datosdf$Fecha, "%d/%m/%Y")
datosdf$anio <- format(datosdf$periodo, "%Y")
datosdf$mes <- format(datosdf$periodo, "%m")

#datosdf <- datosdf %>% filter(anio==2017)

datossp <- datosdf
str(datosdf)
head(datosdf)
#attach(datosdf)

#Importación de la capa geográfica a R.
Antioquia = readShapePoly("./ANTIOQUIA/Antioquia.shp")
xy = SpatialPoints(datosdf[c("Longitud", "Latitud")])	#Puntos de los radares

datosdf$Magnitud.Ml

hist(datosdf$Magnitud.Ml, breaks = 16)
summary(datosdf$Magnitud.Ml)

# Estructura espacial

coordinates(datossp) <- c("Longitud", "Latitud")
class(datossp)

#Gráfico de la capa y los sismos 
plot(Antioquia)
points(xy, pch = 3, cex = 0.3, col = "red")
title(main="Sismicidad Diaria en Antioquia 2017-2018 (Escala de Magnitud Local)")

plot(Antioquia)
points(datossp, asp = 1, cex = 4 * datossp$Magnitud.Ml/max(datossp$Magnitud.Ml),pch = 1, col="green") 
title(main="Sismicidad Diaria en Antioquia 2017-2018 (Escala de Magnitud Local)")

# Análisis de estacionariedad		

scatterplot(Magnitud.Ml~Longitud, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=datosdf)
#linea verde hace referencia al modelo lineal, se observa una leve tendencia 

scatterplot(Magnitud.Ml~Latitud, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=datosdf)
# lo mismo se ve en la latitud (tendencia)
# Claramente se observa en los garficos que la media de la maginitud Local no es constante 
# Sobre la regi?n de observaci?n
# Es necesario remover esta dependencia

#### Lo siguiente es ajustar un modelo de la Magnitud Local en f?ncion de la Lat y Long ###
# Modelo en términos de las direcciones
# Utilizo un modelo cuadrático en las direcciones con un stepwise
modelo1 = lm(Magnitud.Ml ~ Longitud + Latitud + I(Longitud * Latitud) + I(Longitud^2) 
             + I(Latitud^2), data = datosdf)
anova(modelo1)
summary(step(modelo1))

#Ajuste del modelo seleccionado anteriormente 
modelo2 = lm(Magnitud.Ml ~ Longitud + Latitud + 
               I(Longitud^2) + I(Latitud^2), data = datosdf)
anova(modelo2)
summary(modelo2)


#Gráficos sobre los residuales del modelo ajustado 2
par(mfrow = c(1, 3))
hist(modelo2$ res, freq = FALSE, main = "", xlab = "Residuales", ylab = "Frecuencia")
curve(dnorm(x, mean(modelo2$res), sd(modelo2$res)), add = T)
boxplot(modelo2$res)
qqPlot(modelo2$res, ylab = "Magnitud ML")
title(main=list("Graficos descriptivos Residuales Sismicidad Diaria en Antioquia 2017-2018 (Escala de Magnitud Local)", cex=2,col="black", font=3), outer=T,line=-2)
par(mfrow = c(1, 1))

#Modelo de sgundo orden es estacionario, se trabaja con los residuales del modelo
modelo3=lm(modelo2$res ~ Longitud + Latitud + I(Longitud * Latitud) + 
             I(Longitud^2) + I(Latitud^2), data = datosdf)
summary(modelo3)
Anova(modelo3)

scatterplot(modelo2$res~Longitud, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=datosdf)
scatterplot(modelo2$res~Latitud, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=datosdf)

# variograma

# Se construye el semivariograma sobre los residuales del modelo ajustado 2
datos2 <- data.frame(Longitud = datosdf$Longitud, Latitud = datosdf$Latitud, res = modelo2$res)

datos2sp <- datos2
coordinates(datos2sp) <- c("Longitud", "Latitud")
class(datos2sp)

#VARIOGRAMA EMPÍRICO O EXPERIMENTAL

ve <- variogram(res ~ 1, datos2sp) 
ve

plot(ve, plot.numbers = T, asp=1)



    

                
                