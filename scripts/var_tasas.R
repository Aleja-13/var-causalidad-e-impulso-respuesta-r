# Paquetes
library(readxl)
library(ggplot2)
library(vars)  
library(AER) 
library(forecast)

# Cargando datos
tasa.de.referencia <- read.csv2("datos/tasa de referencia.csv", header=FALSE)
tasa.desempleo <- read.csv2("datos/tasa desempleo.csv", header=FALSE)
tasa.inflacion <- read.csv2("datos/INFLACION.csv", header=FALSE)

## Los ordenamos como serie de tiempo
referencia.ts = ts((log(tasa.de.referencia$V2)), star = c(2010,1), end = c(2020,12), frequency = 12)
plot(referencia.ts)
ndiffs(referencia.ts)
diff.referencia.ts = diff(referencia.ts, 1)
plot(diff.referencia.ts)

desempleo.ts = ts((log(tasa.desempleo$V2)), star = c(2010,1), end = c(2020,12), frequency = 12)
plot(desempleo.ts)
ndiffs(desempleo.ts)
diff.desempleo.ts = diff(desempleo.ts, 1)
plot(diff.desempleo.ts)

inflacion.ts = ts((log(tasa.inflacion$V2)), star = c(2010,1), end = c(2020,12), frequency = 12)
plot(inflacion.ts)
ndiffs(inflacion.ts)
diff.inflacion.ts = diff(inflacion.ts, 1)
plot(diff.inflacion.ts)

#eliminamos los valores perdidos de las series de tiempo
d.des=na.locf(diff.desempleo.ts)
d.inf=na.locf(diff.inflacion.ts)
d.ref=na.locf(diff.referencia.ts)

# Creamos la matriz de datos
data<-ts.union(d.inf,d.des,d.ref)

# Grafico de los datos
plot(data, main = "Datos Perú", xlab = "Periodos (Mensuales)")

# Estimacion VAR en forma reducida
var_est <- VAR(y= data, p=1)

coef(var_est)


# Seleccion de rezagos
VARselect(y = data, lag.max = 7, type = c("const"))

var_est <- VAR(y = data, p = 5)

# Estabilidad del VAR

barplot(roots(var_est), ylim = c(0,1), 
        main = "Estabilidad: Raices",
        names.arg = as.character(1:15))
abline(h = 1, col = "red", lty = 2, lwd=2)


# Pruebas de causalidad de Granger
causality(var_est, cause = "d.des")
causality(var_est, cause = "d.inf")
causality(var_est, cause = "d.ref")

# FIR: 
# impulso = producto, respuesta = Inflacion
irf_i_y = irf(var_est, impulse = "d.des", response = "d.inf", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = Desempleo, respuesta = Inflacion", lwd=2)


# impulso = producto, respuesta = tasa de referencia
irf_i_pi = irf(var_est, impulse = "d.des", response = "d.ref", 
               n.ahead = 12, ortho = TRUE)
plot(irf_i_pi, main = "impulso = Desempleo, respuesta = Tasa DE referencia", lwd=2)

# Analisis de descomposicion de la varianza del error de prediccion:
desc_var <- fevd(var_est, n.ahead = 12)

plot(desc_var, xlab = c("Horizonte","Horizonte","Horizonte"), 
     ylab = c("Porcentaje","Porcentaje","Porcentaje"), 
     legend = c("Desempleo","Inflacion","tasa de referencia"),
     main = c("Desempleo","Inflacion","tasa de referencia"))

# Prediccion
var_est_pred <- predict(var_est, n.ahead = 4, ci = 0.95)
var_est_pred

plot(var_est_pred)

