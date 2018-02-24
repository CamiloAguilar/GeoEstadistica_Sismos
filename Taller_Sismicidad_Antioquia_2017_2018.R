##################################################################
############ 			Cargue de librerias		##############
##################################################################

#install.packages("rgeos","sp","maptools","car","geoR","gstat")
library(rgeos)
library(sp)
library(maptools)
library(car)
library(geoR)
library(gstat)
library(gdata)
library(readxl)
library(dplyr)
##################################################################
############ 		Importación de datos y capa		##############
##################################################################

#Directorio de trabajo

#setwd("D:/Documents/MEA/ESTADISTICA ESPACIAL/TALLER/Sismicidad/2017")
datosdf = read.table("RNSC_Antioquia_2017_2018.txt", sep = "\t", dec = ",", header = T)
datosdf$periodo <- as.Date(datosdf$Fecha, "%d/%m/%Y")
datosdf$anio <- format(datosdf$periodo, "%Y")
datosdf$mes <- format(datosdf$periodo, "%m")
datosdf <- datosdf %>% filter(mes=="06")

datossp <- datosdf

#Importación de la capa geográfica a R.
Antioquia = readShapePoly("./ANTIOQUIA/Antioquia.shp")
xy = SpatialPoints(datosdf[c("Longitud", "Latitud")])	#Puntos de los radares

##################################################################
############ 			Análisis gráfico			##############
##################################################################

#Gráfico de la capa y los sismos 
plot(Antioquia)
points(xy, pch = 3, cex = 0.3, col = "red")
title(main="Sismicidad Diaria en Antioquia 2017-2018 (Escala de Magnitud Local)")

#Análisis descriptivo para la Escala de Magnitud Local
par(mfrow = c(1, 3))
hist(datosdf$Magnitud.Ml, freq = FALSE, main = "", xlab = "Magnitud.Ml", ylab = "Frecuencia")
curve(dnorm(x, mean(datosdf$Magnitud.Ml), sd(datosdf$Magnitud.Ml)), add = T)
boxplot(datosdf$Magnitud.Ml)
qqPlot(datosdf$Magnitud.Ml, ylab = "Magnitud Local")
title(main=list("Graficos descriptivos Sismicidad Diaria en Antioquia 2017-2018 (Escala de Magnitud Local)", 
                cex=2,col="black", font=3), outer=T,line=-2)
par(mfrow = c(1, 1))

limites=c(min(datosdf$Magnitud.Ml), quantile(datosdf$Magnitud.Ml,probs = c(0.2, 0.4, 0.6, 0.8),type = 5), 
          max(datosdf$Magnitud.Ml))
##Transformar por lo atipicos 

coordinates(datossp) = ~ Longitud + Latitud
spplot(datossp, "Magnitud.Ml", cuts = limites)
### Aparentemente no es estacionaria porque los puntos no estan mezclados aleatoriamente 
### Existe dependencia espacial!


##################################################################
############ 		Análisis de estacionariedad		##############
##################################################################

##############Gráficos contra las direcciones################
# Como se observa el proceso no es estacionario  
# tambien se observa alguna tendencia 
# los eventos sismologicos dada con magnitud Local (Richter) no son estacionario
# ya que los puntos de colores no estan mezclados aleatoriamente como se muestra 
# en el anterior grafico

scatterplot(Magnitud.Ml~Longitud, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=datosdf)## prec contra long, se observa una tendencia 
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

# ES ESTACIONARIO


# Gráficos contra las direcciones para los residuales
# En los graficos de dispersi?n no se oberva tendencia alguna
# Lo que indica estacionariedad en los datos analizados 
scatterplot(modelo2$res~Longitud, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=datosdf)
scatterplot(modelo2$res~Latitud, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, span=0.5, data=datosdf)
# se observa  valores atipicos, es posible controlar con Dummy 


#*******************************************************
# 3. Modelo a sentimiento		####
#*******************************************************

# Se construye el semivariograma sobre los residuales del modelo ajustado 2
datos2 <- data.frame(Longitud = datosdf$Longitud, Latitud = datosdf$Latitud, res = datosdf$Magnitud.Ml)

# Objeto de tipo geodata para el calculo del semivariograma
geo = as.geodata(datos2, coords.col = 1:2, data.col = 3)
class(geo)
dup.coords(geo) # No hay duplicados

# variog para estimar semivariograma
var = variog(geo, max.dist = 1.5, direction = "omnidirectional")
# semivariograma de los residuos, ya que la Maginitud Local es estacionaria, entonces 
# removemos la tendencia, aunque la dependencia entre las obsevaciones se evidencia
# debido al crecimiento presente en el semivariograma, lo que indica que los residuos 
# tienen estructura de dependencia espacial, es decir, existen rezagos de dependencia   
# lo que indica un problema a solucionar

plot(var)


# Estimación variograma por lat y lon
#var_lat = variog(geo, direction = 90, unit.angle="degre", max.dist = 2.9)
#var_lon = variog(geo, direction = 0, unit.angle="degre", max.dist = 2.9)

# graficamos los semivariogramas
# Se observa que la variación no depende de la dirección
plot(var,main="Semivariograma de residuos", xlim=c(0, 1.7), type="o", ylim=c(0,0.5))
#points(var_lat$u, var_lat$v, col=2, type="o")
#points(var_lon$u, var_lon$v, col=3, type="o")
#legend("bottomright",c("Omnidir","Latitud","Longitud"), col=1:3,pch=1, inset=0.03, box.lwd=0)

#plot(var,main="Semivariograma")
#plot(var,main="Semivariograma",type="l")

#Ajuste de modelos al semivariograma
ev=eyefit(var)
ev

#Se especifica: un modelo circular
#Kriging ordinario, type="OK"
#Modelo para pronostico
#sigmasq valor 0.272761875503621 es la meseta
#phi valor 1.1209345602207 es el rango
#teusq valor 0.0340952344379526 nugget para el efecto pepita
#practicalrange valor 1.1209345602207

##################################################################
############ 			Ajuste de modelos			##############
##################################################################

#Estimacion del modelo de semivarianza
#Asignando valores iniciales

mod1=variofit(var,ini=ev,weights="equal")
mod1

#Minimos cuadrados ponderados
mod2=variofit(var,ini=ev,weights="npairs")
mod2

#Minimos cuadrados ponderados
mod3=variofit(var,ini=ev,weights="cressie")
mod3


plot(var)
lines(mod1, max.dist = 3.5, col = 1)
lines(mod2, max.dist = 3.5, col = 2)
lines(mod3, max.dist = 3.5, col = 3)

legend("bottomright",legend = c("MCO", "MCP - npairs", "MCP - cressie"),
       col = 1:5, lwd = 2, inset = .03)


# Los modelos MCP - npair y MCP - cressie aparentemente se ajustan más a los datos

##################################################################
############ 			Validación cruzada			##############
###########       con kriging ordinario   ##############
###########       sobre los residuales    ##############
##################################################################
### utilizo la valizaci?n cruzada sobre los reiduales para comprara los modelos 
### en geo estan guardados los residuales 

cruzada1=xvalid(geo,model=mod1,reestimate = F)
cruzada2=xvalid(geo,model=mod2,reestimate = F)
cruzada3=xvalid(geo,model=mod3,reestimate = F)

sqrt(mean(cruzada1$error^2))
sqrt(mean(cruzada2$error^2))
sqrt(mean(cruzada3$error^2))
# Usar el modelo 3

### Validaci?n cruzada para la Magnitud Local 
### cambiar de geoR a gstat
### Se crea un objeto de tipo gstat para utilizarlo en el kriging
### valid cruzada sobre la Nagnitud Local y o residuales 

library(gstat)
mod1_1 <- as.vgm.variomodel(mod3)
class(mod3)
class(mod1_1)
#mod1 es el modelo en la libreria geoR
#mod1_1 es el mismo modelo pero de la libreria gstat

##################################################################
############ 			Kriging universal			##############
##################################################################

## INTERPOLACI?N  

poligonos=polygons(Antioquia)

#Puntos para hacer interpolaci?n (1000)
muestra = spsample(poligonos, n = 10000, "regular")
muestra1=data.frame(muestra)
plot(muestra)
names(muestra1) = c("Longitud", "Latitud")
gridded(muestra1) = c("Longitud", "Latitud")
#Para cuadricular la muestra generada, porque se ha generado de forma regular

#Kri universal, la dif es la expresion para la media del modelo

krig_u <- krige(Magnitud.Ml ~ Longitud + I(Longitud * Latitud) +
              I(Longitud^2) + I(Latitud^2), datossp, muestra1, model = mod1_1)
head(krig_u)


#kriging universal sobre la Magitud Local.
head(krig_u$var1.pred)
head(krig_u$var1.var)

#Mapa para la Magnitud Local
spplot(krig_u, c("var1.pred"), main = "Kriging Universal para la Magnitud Local", 
       contour = T, labels = T, pretty = TRUE, col = "black", col.regions = terrain.colors(100))

#Con algunas opciones distintas
spplot(krig_u, c("var1.pred"), main = "Kriging Universal para la Magnitud Local", contour = FALSE, labels = FALSE, pretty = F, col = "black", col.regions = terrain.colors(100))
spplot(krig_u, c("var1.var"), main = "Mapa para las varianzas de predicción", contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))
#mapa para las varaiznas de predicci?n 

#Para visualizar los puntos de las estaciones
li = list("sp.polygons", Antioquia)
pts = list("sp.points", datossp, pch = 3, col = "black", cex = 0.2)
spplot(krig_u, c("var1.pred"), main = "Kriging Universal para la precipitación", sp.layout = list(li, pts), contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))

##################################################################
############ 			Kriging ordinario			##############
##################################################################

krig_ord=krige(formula=Magnitud.Ml ~ 1,datossp,muestra1,model=mod1_1)

#Mapa para la Magnitud Local
spplot(krig_ord, c("var1.pred"), main = "Kriging Ordinario para la Magnitud Local", contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))
#esos parches significan que hay algo por revisar 
#(el interpolador es decir Kriking ordinadrio no es buea idea)

spplot(krig_ord, c("var1.var"), main = "Mapa para las varianzas de Magnitud Local?n", contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))
## tambien se observan modificaciones (mas regiones con varianzas altas)

#Para visualizar los puntos de las estaciones
li = list("sp.polygons", Antioquia)
pts = list("sp.points", datossp, pch = 3, col = "black", cex = 0.2)
spplot(krig_ord, c("var1.pred"), main = "Kriging ordinario para la Magnitud Local", sp.layout = list(li, pts), contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))


