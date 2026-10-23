# Paquetes
library(readxl)
library(ggplot2)
library(vars)  
library(AER) 
library(forecast)


# Cargando datos
TIPO.DE.CAMBIO <- read.csv("datos/TIPO DE CAMBIO.csv", header=FALSE)
INFLACION <- read.csv("datos/INFLACION.csv", header=FALSE)
PIB <- read.csv("datos/PIB.csv", header=FALSE)
IGBLV <- read.csv("datos/IGBLV.csv", header=FALSE)
dev.off()
## Los ordenamos como serie de tiempo
TIPO.DE.CAMBIO.ts = ts((log(TIPO.DE.CAMBIO$V2)), star = c(2009,1), end = c(2019,4), frequency = 4)
plot(TIPO.DE.CAMBIO.ts)
ndiffs(TIPO.DE.CAMBIO.ts)
d_TIPO.DE.CAMBIO.ts = diff(TIPO.DE.CAMBIO.ts, 1)
plot(d_TIPO.DE.CAMBIO.ts)

inflacion.ts = ts((log(INFLACION$V2)), star = c(2009,1), end = c(2019,4), frequency = 4)
plot(inflacion.ts)
ndiffs(inflacion.ts)
d_inflacion.ts = diff(inflacion.ts, 1)
plot(d_inflacion.ts)


IGBLV.ts = ts((log(IGBLV$V2)), star = c(2009,1), end = c(2019,4), frequency = 4)
plot(IGBLV.ts)
ndiffs(IGBLV.ts)



PIB.ts = ts((log(PIB$V2)), star = c(2009,1), end = c(2019,4), frequency = 4)
plot(PIB.ts)
ndiffs(PIB.ts)
d_PIB.ts = diff(PIB.ts, 1)
plot(d_PIB.ts)

# Creamos la matriz de datos
data<-ts.union(PIB.ts,IGBLV.ts,inflacion.ts,TIPO.DE.CAMBIO.ts)

# Grafico de los datos
plot(data, main = "Datos Macroeconomicos Perú", xlab = "Periodos (Trimestres)",
     col = "blue", lwd=2)

# Estimacion VAR en forma reducida
var_est <- VAR(y= data, p=1)

coef(var_est)

# SelecciÃ³n de rezagos
VARselect(y = data, lag.max = 7, type = c("const"))

var_est <- VAR(y = data, p = 7)

# Estabilidad del VAR

barplot(roots(var_est), ylim = c(0,1), 
        main = "Estabilidad: Raices",
        names.arg = as.character(1:28))
abline(h = 1, col = "red", lty = 2, lwd=2)


# Pruebas de causalidad de Granger
causality(var_est, cause = "PIB.ts")
causality(var_est, cause = "inflacion.ts")
causality(var_est, cause = "IGBLV.ts")
causality(var_est, cause = "TIPO.DE.CAMBIO.ts")

# FIR: 
# impulso = producto, respuesta = Inflacion
irf_i_y = irf(var_est, impulse = "PIB.ts", response = "inflacion.ts", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = PIB, respuesta = Inflacion", lwd=2)

# impulso = producto, respuesta = tasa
irf_i_pi = irf(var_est, impulse = "PIB.ts", response = "IGBLV.ts", 
               n.ahead = 12, ortho = TRUE)
plot(irf_i_pi, main = "impulso = PIB, respuesta = Tasa IGBVL", lwd=2)

# impulso = producto, respuesta = tasa de cambio
irf_i_pi = irf(var_est, impulse = "PIB.ts", response = "TASA.DE.CAMBIO.ts", 
               n.ahead = 12, ortho = TRUE)
plot(irf_i_pi, main = "impulso = PIB, respuesta = Tasa DE CAMBIO", lwd=2)

# Analisis de descomposicion de la varianza del error de prediccion:
desc_var <- fevd(var_est, n.ahead = 12)

plot(desc_var, xlab = c("Horizonte","Horizonte","Horizonte"), 
     ylab = c("Porcentaje","Porcentaje","Porcentaje"), 
     legend = c("PIB","Inflacion","IGBLV"),
     main = c("PIB", "Tasa de Inflacion", "IGBLV"))

# Prediccion
var_est_pred <- predict(var_est, n.ahead = 4, ci = 0.95)
var_est_pred

plot(var_est_pred)

