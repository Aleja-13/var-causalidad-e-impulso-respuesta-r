# Paquetes
library(readxl)
library(ggplot2)
library(vars)  
library(AER) 
library(forecast)

# Cargando datos
TSLA <- read.csv("datos/TSLA.csv")
V <- read.csv("datos/V.csv")


Datos<-cbind(TSLA$TSLA.Close,V$V.Close)

#Presentamos como una serie de tiempo
series<-ts(Datos,star = c(2015,7), end = c(2020,12), frequency = 12)

#Graficamos las series de tiempo
ts.plot(series, xlab="Tiempo",col=c(1,2))
### Se observa una pequeña tendencia, aplicamos una diferencia para estabilizarla. 
ts.plot(diff(series),xlab="Tiempo", col=c(1,2))


# Identificacion del Numero de Retardos a Incluir en el VAR
VARselect(diff(series),lag.max=10,type="const")

# Estimación del Modelo VAR
modelo<-VAR(diff(series),p=1,type=c("const"))
modelo

summary(modelo,equation="x")
summary(modelo,equation="y")

#Diagnostico de ruido blanco 
serial.test(modelo,lags.pt=5)
# p-value = 0.4255 aceptamos Ho, no hay autocorrelacion.

# Pruebas de causalidad de Granger
grangertest(diff(V1) ~ diff(V2),order=1, data=as.data.frame(Datos))

grangertest(diff(V2)~diff(V1),order=1, data=as.data.frame(Datos))
### Segun los resultados el crecimiento mensual del TSLA causa Granger el crecimiento Mensual de V, al menos en nuestro periodo de estimación. Asi mismo, el crecimiento mensual de V causa Granger el crecimiento mensual de TSLA.
### En este caso los p-value son cero con lo que podemos rechazar la hipótesis nula, es decir, podemos rechazar que no existe una relación entre TSLA y V.
