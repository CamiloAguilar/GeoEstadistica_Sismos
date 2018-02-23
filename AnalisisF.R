##################################################################
############    Cargue de librerias  ##############
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
#library(xlsx)
##################################################################
############   Importación de datos y capa  ##############
##################################################################

#Directorio de trabajo

#setwd("D:/A MEA USTA/ESTADISTICA ESPACIAL/TRABAJO 1/SISMOS/ANTIOQUIA")
datosdf = read.table("RNSC_Antioquia_2017_2018.txt", sep = "\t", dec = ",", header = T)
datossp = read.table("RNSC_Antioquia_2017_2018.txt", sep = "\t", dec = ",", header = T)
str(datosdf)
head(datosdf)
attach(datosdf)
#Importación de la capa geográfica a R.
Antioquia = readShapePoly("./ANTIOQUIA/Antioquia.shp")
xy = SpatialPoints(datosdf[c("Longitud", "Latitud")]) #Puntos de los radares

##################################################################
############    Análisis gráfico   ##############
##################################################################

#Gráfico de la capa y los sismos 
plot(Antioquia)
points(xy, pch = 3, cex = 0.3, col = "red")
title(main="Sismicidad en Antioquia Detectada por las Estaciones")

#Análisis descriptivo para la Escala de Magnitud Local
par(mfrow = c(1, 3))
hist(datosdf$Magnitud.Ml, freq = FALSE, main = "", xlab = "Magnitud.Ml", ylab = "Frecuencia")
curve(dnorm(x, mean(datosdf$Magnitud.Ml), sd(datosdf$Magnitud.Ml)), add = T)
boxplot(datosdf$Magnitud.Ml)
qqPlot(datosdf$Magnitud.Ml, ylab = "Magnitud Local")
title(main=list("Graficos descriptivos Sismicidad en Antioquia (Escala de Magnitud Local)", cex=2,col="black", font=3), outer=T,line=-2)

limites=c(min(datosdf$Magnitud.Ml), quantile(datosdf$Magnitud.Ml,probs = c(0.2, 0.4, 0.6, 0.8),type = 5), max(datosdf$Magnitud.Ml))

coordinates(datossp) = ~ Longitud + Latitud
spplot(datossp, "Magnitud.Ml", cuts = limites)


##################################################################
############   Análisis de estacionariedad  ##############
##################################################################

scatterplot(Magnitud.Ml~Longitud, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, 
            span=0.5, data=datosdf)## prec contra long, se observa una tendencia 

scatterplot(Magnitud.Ml~Latitud, reg.line=lm, smooth=TRUE, spread=TRUE, boxplots=FALSE, 
            span=0.5, data=datosdf)

modelo1 = lm(Magnitud.Ml ~ Longitud + Latitud + I(Longitud * Latitud) + I(Longitud^2) + I(Latitud^2), data = datosdf)
summary(modelo1)
summary(step(modelo1))

#Gráficos sobre los residuales del modelo 1
par(mfrow = c(1, 3))
hist(modelo1$res, freq = FALSE, main = "", xlab = "Residuales", ylab = "Frecuencia")
curve(dnorm(x, mean(modelo1$res), sd(modelo1$res)), add = T)
boxplot(modelo1$res)
qqPlot(modelo1$res, ylab = "Magnitud ML")
title(main=list("Graficos descriptivos M1 Residuales Sismicidad en Antioquia (Escala de Magnitud Local)", cex=2,col="black", font=3), outer=T,line=-2)

#Ajuste del modelo seleccionado ### a los residuales 
modelo2 = lm(Magnitud.Ml ~ Longitud + Latitud + I(Longitud * Latitud) +
               I(Longitud^2) + I(Latitud^2), data = datosdf)
anova(modelo2)
summary(modelo2)

#Ajuste modelo anterior
modelo2 = lm(Magnitud.Ml ~ Longitud +
               I(Longitud^2) + I(Latitud^2), data = datosdf)
anova(modelo2)
summary(modelo2)


#Gráficos sobre los residuales del modelo ajustado
par(mfrow = c(1, 3))
hist(modelo2$ res, freq = FALSE, main = "", xlab = "Residuales", ylab = "Frecuencia")
curve(dnorm(x, mean(modelo2$res), sd(modelo2$res)), add = T)
boxplot(modelo2$res)
qqPlot(modelo2$res, ylab = "Magnitud ML")
title(main=list("Graficos descriptivos Residuales Sismicidad en Antioquia (Escala de Magnitud Local)", cex=2,col="black", font=3), outer=T,line=-2)

#Modelo de sgundo orden
modelo3=lm(modelo2$res ~ Longitud + Latitud + I(Longitud * Latitud) + 
             I(Longitud^2) + I(Latitud^2), data = datosdf)
summary(modelo3)
Anova(modelo3)

#Gráficos contra las direcciones para los residuales



##################################################################
############    Modelo a sentimiento  ##############
##################################################################

#Ahora, se construye el semivariograma sobre los residuales del modelo ajustado
datos2=data.frame(Longitud,Latitud,res=modelo2$res)

#Creando objeto de tipo geodata para el calculo del semivariograma
geo = as.geodata(datos2, coords.col = 1:2, data.col = 3)
var = variog(geo, max.dist = 3,direction = "omnidirectional")

#dup.coords(geo)
#geo.nuevo = jitterDupCoords(geo,max=0.01)
#dup.coords(geo.nuevo)
#coordinates(geo.nuevo) = ~ Longitud + Latitud
#variog para estimar semivariograma

par(mfrow = c(1, 1))
plot(var)
var = variog(geo,direction = "omnidirectional",width = 1,max.dist=3.5)


#Ajuste de modelos al semivariograma
#Aca se puede "jugar" con varios ajustes
ev=eyefit(var)
ev

# Modelos
mod1=variofit(var,ini=ev,weights="equal")
mod1
#M?nimos cuadrados ponderados
mod2=variofit(var,ini=ev,weights="npairs")
mod2
#M?nimos cuadrados ponderados
mod3=variofit(var,ini=ev,weights="cressie")
mod3

# Ajuste
plot(var,xlab="Distancia")
lines(mod1, max.dist = 3, col = 1)
lines(mod2, max.dist = 3, col = 2)
lines(mod3, max.dist = 3, col = 3)
legend("bottomright",legend = c("MCO", "MCP - npairs", "MCP - cressie"),
       col = 1:5, lwd = 2, inset = .03, cex=0.5)
title(main=list("Modelo Lineal",cex=1,col="black", 
                font=3), outer=T,line=-2)

mod1_1 <- as.vgm.variomodel(mod1)

# Interpolaci?n

poligonos=polygons(Antioquia)
muestra = spsample(poligonos, n = 100, "regular")
plot(muestra)
muestra1=data.frame(muestra)
names(muestra1) = c("Longitud", "Latitud")
gridded(muestra1) = c("Longitud", "Latitud")


#Para cuadricular la muestra generada! porque se ha generado de forma regular
krig_u=krige(formula= res ~ 1,geo,muestra1,model=mod1_1)

#kriging universal sobre la precipitación.
head(krig_u$var1.pred) # Pronosticos
head(krig_u$var1.var) # Varianza de predicci?n

#Mapa para la precipitación
spplot(krig_u, c("var1.pred"), main = "Kriging Universal para la precipitación", contour = T, labels = T, pretty = TRUE, col = "black", col.regions = terrain.colors(100))

#Con algunas opciones distintas
spplot(krig_u, c("var1.pred"), main = "Kriging Universal para la precipitación", contour = FALSE, labels = FALSE, pretty = F, col = "black", col.regions = terrain.colors(100))
spplot(krig_u, c("var1.var"), main = "Mapa para las varianzas de predicción", contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))

#Para visualizar los puntos de las estaciones
li = list("sp.polygons", cundinamarca)
pts = list("sp.points", datossp, pch = 3, col = "black", cex = 0.2)
spplot(krig_u, c("var1.pred"), main = "Kriging Universal para la precipitación", sp.layout = list(li, pts), contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))


##################################################################
############ 			Kriging ordinario			##############
##################################################################
krig_ord=krige(formula=prec ~ 1,datossp,muestra1,model=mod1_1)

#Mapa para la precipitación, Parches, se?ales de que hay que revisar algo
spplot(krig_ord, c("var1.pred"), main = "Kriging Ordinario para la precipitación", contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))
spplot(krig_ord, c("var1.var"), main = "Mapa para las varianzas de predicción", contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))


#Para visualizar los puntos de las estaciones
li = list("sp.polygons", cundinamarca)
pts = list("sp.points", datossp, pch = 3, col = "black", cex = 0.2)
spplot(krig_ord, c("var1.pred"), main = "Kriging ordinario para la precipitación", sp.layout = list(li, pts), contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))

spplot(krig_ord, c("var1.var"), main = "Kriging ordinario para la precipitación", sp.layout = list(li, pts), contour = FALSE, labels = FALSE, pretty = TRUE, col = "black", col.regions = terrain.colors(100))


