# Paquetes
library(readxl)
library(ggplot2)
library(vars)  
library(AER) 
library(forecast)

# Cargando datos
DJI <- read.csv("datos/DJI.csv")
DORM <- read.csv("datos/DORM.csv")

x<-DORM$DORM.Close
y<-DJI$DJI.Close

datos<-cbind(x,y)

DORM.ts = ts((log(DORM$DORM.Close)), star = c(2015,7), end = c(2020,12), frequency = 12)
autoplot(DORM.ts)
ndiffs(DORM.ts)
diff.DORM.ts = diff(DORM.ts, 1)
plot(diff.DORM.ts)

DJI.ts = ts((log(DJI$DJI.Close)), star = c(2015,7), end = c(2020,12), frequency = 12)
autoplot(DJI.ts)
ndiffs(DJI.ts)
diff.DJI.ts = diff(DJI.ts, 1)
plot(diff.DJI.ts)

#eliminamos los valores perdidos de las series de tiempo
d.DIJ=na.locf(diff.DJI.ts)
d.DORM=na.locf(diff.DORM.ts)


# Creamos la matriz de datos
data<-ts.union(d.DIJ,d.DORM)

# Grafico de los datos
autoplot(data, main = "Datos", xlab = "Periodos (Mensuales)")

# Estimacion VAR en forma reducida
var_est <- VAR(y= data, p=1)

coef(var_est)


# Seleccion de rezagos
VARselect(y = data, lag.max = 7, type = c("const"))
modelo <- VAR(y = data, p = 1)
summary(modelo,equation="x")
summary(modelo,equation="y")

#Prueba de Estacionariedad
roots(modelo)
###La comprobación de la estabilidad no indica que nuestro modelo este mal.

#Analisis de Autocorrelacion en los Residuales
plot(modelo, names="y")
plot(modelo, names="x")

dev.off()
acf(residuals(modelo)[,1])

#Diagnostico de ruido blanco 
serial.test (var_est, lags.pt=5, type= "PT.asymptotic")
# p-value = 0.2193 aceptamos Ho, no hay autocorrelacion.

#Prueba de Normalidad de los Residuales
normality.test(modelo, multivariate.only=FALSE)

# Pruebas de causalidad de Granger
causality(var_est, cause = "d.DIJ")
causality(var_est, cause = "d.DORM")

# FIR: 
# impulso = DIR, respuesta = DORM
irf_i_y = irf(var_est, impulse = "d.DIJ", response = "d.DORM", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = DIJ, respuesta = DORM", lwd=2)

irf_i_x = irf(var_est, impulse = "d.DORM", response = "d.DIJ", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_x, main = "impulso = DORM, respuesta = DIJ", lwd=2)

# Prediccion
var_est_pred <- predict(var_est, n.ahead = 4, ci = 0.95)
var_est_pred

plot(var_est_pred)

