# Paquetes
library(readxl)
library(ggplot2)
library(vars)  
library(AER) 
library(forecast)

#Cargando datos
PBI <- read_excel("datos/PIB%mensual.xls")
PBI<- PBI$PBI
CREDITOS<- read_excel("datos/CRÉDITOS (MILLONES Soles).xlsx")
CREDITOS<-CREDITOS$`Créditos (millones S)`
INFLACION<- read_excel("datos/inflacion.xlsx")
INFLACION<-INFLACION$Inflación
IGLBV<- read.csv("datos/IGBLV.csv", header=FALSE)
IGLBV<-IGLBV$V2
INVERSION<- read_excel("datos/Inversión privada Directa.xlsx")
INVERSION<-INVERSION$`Pasivos - Inversión Directa (porcentaje del PBI)`
TIPO.DE.CAMBIO<- read.csv("datos/TIPO DE CAMBIO.csv", header=FALSE)
TDC<-TIPO.DE.CAMBIO$V2
EXPORTACIONES <- read_excel("datos/Exportaciones.xlsx")
EXPORTACIONES<-EXPORTACIONES$`Exportaciones (Millones $)`
IMPORTACIONES <- read_excel("datos/Importaciones.xlsx")
IMPORTACIONES<-IMPORTACIONES$Importaciones


#2.1. Graficos de las series originales.

PBI.ts <- ts(PBI, frequency = 12, star = c(2010,1), end = c(2020,12))
autoplot(PBI.ts, frequency =12, xlab = "Tiempo", ylab = "PBI", main = "Comportamiento de PBI")

CRE.ts <- ts(log(CREDITOS), frequency = 12, star = c(2010,1), end = c(2020,12))
autoplot(CRE.ts, frequency =12, xlab = "Tiempo", ylab = "CREDITOS", main = "Comportamiento de CREDITOS")

TDC.ts <- ts(log(TDC), frequency = 12, star = c(2010,1), end = c(2020,12))
autoplot(TDC.ts, frequency =12, xlab = "Tiempo", ylab = "TIPO DE CAMBIO", main = "Comportamiento de TIPO DE CAMBIO")

EXP.ts <- ts(log(EXPORTACIONES), frequency = 12, star = c(2010,1), end = c(2020,12))
autoplot(EXP.ts, frequency =12, xlab = "Tiempo", ylab = "EXPORTACIONES", main = "Comportamiento de EXPORTACIONES")

IMP.ts <- ts(log(IMPORTACIONES), frequency = 12, star = c(2010,1), end = c(2020,12))
autoplot(IMP.ts, frequency =12, xlab = "Tiempo", ylab = "IMPORTACIONES", main = "Comportamiento de IMPORTACIONES")

INF.ts <- ts(INFLACION, frequency = 12, star = c(2010,1), end = c(2020,12))
autoplot(INF.ts, frequency =12, xlab = "Tiempo", ylab = "INFLACION", main = "Comportamiento de INFLACION")

IGLBV.ts <- ts(log(IGLBV), frequency = 12, star = c(2010,1), end = c(2020,12))
autoplot(IGLBV.ts, frequency =12, xlab = "Tiempo", ylab = "IGLBV", main = "Comportamiento de IGLBV")

INV.ts <- ts(log(INVERSION), frequency = 12, star = c(2010,1), end = c(2020,12))
autoplot(INV.ts, frequency =12, xlab = "Tiempo", ylab = "INVERSION", main = "Comportamiento de INVERSIÓN")

#2.2. Pruebas de estacionariedad (formales e informales)
### informales
ggtsdisplay(PBI.ts, main = "PBI") 
ggtsdisplay(CRE.ts, main = "Créditos") 
ggtsdisplay(TDC.ts, main = "Tipo de Cambio")
ggtsdisplay(INF.ts, main = "Inflación")
ggtsdisplay(INV.ts, main = "Inversión") 
ggtsdisplay(IMP.ts, main = "Importaciones")
ggtsdisplay(EXP.ts, main = "Exportaciones")
ggtsdisplay(IGLBV.ts, main = "IGLBV")

### formales
# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER
adf.test(PBI.ts,alternative = "stationary")     #  p-value = 0.01 ... ES ESTACIONARIA
adf.test(INV.ts,alternative = "stationary")     #  p-value = 0.3749 ... NO ES ESTACIONARIA
adf.test(IMP.ts,alternative = "stationary")     #  p-value = 0.01 ... ES ESTACIONARIA
adf.test(EXP.ts,alternative = "stationary")     #  p-value = 0.01 ... NO ES ESTACIONARIA
adf.test(TDC.ts,alternative = "stationary")     #  p-value = 0.8875 ... NO ES ESTACIONARIA
adf.test(IGLBV.ts,alternative = "stationary")   #  p-value = 0.99 ... NO ES ESTACIONARIA
adf.test(CRE.ts,alternative = "stationary")     #  p-value = 0.99 ... NO ES ESTACIONARIA
adf.test(INF.ts,alternative = "stationary")     #  p-value = 0.01 ... ES ESTACIONARIA


#Realizamos la diferenciacion para estabilizar las series

ndiffs(CRE.ts)
CRE.diff<-diff(CRE.ts, 1)
ggtsdisplay(CRE.diff, main = "Créditos") 
adf.test(CRE.diff,alternative = "stationary")

ndiffs(INV.ts)
INV.diff<-diff(INV.ts, 1)
ggtsdisplay(INV.diff, main = "Inversion") 
adf.test(INV.diff,alternative = "stationary")

ndiffs(TDC.ts)
TDC.diff<-diff(TDC.ts, 2)
ggtsdisplay(TDC.diff, main = "Tipo de cambio") 
adf.test(TDC.diff,alternative = "stationary")
#eliminamos los valores perdidos de las series de tiempo
d.CRE=na.locf(CRE.diff)
d.TDC=na.locf(TDC.diff)
d.INV=na.locf(INV.diff)

#2.3 Analisis de cointegracion 

####PBI ~ Credito
data<-ts.union(PBI.ts,CRE.ts)

# Grafico de los datos
plot(data, main = "Datos Perú", xlab = "Periodos (Mensuales)")

# Estimacion VAR en forma reducida
var_est1 <- VAR(y= data, p=1)
coef(var_est1)

#2.4 Seleccion de rezagos
VARselect(y = data, lag.max = 7, type = c("const"))

var_est1 <- VAR(y = data, p = 2)

#2.5 Validacion del modelo
#Estabilidad del VAR

barplot(roots(var_est1), ylim = c(0,1), 
        main = "Estabilidad: Raices",
        names.arg = as.character(1:4))
abline(h = 1, col = "red", lty = 2, lwd=2)

# Pruebas de causalidad de Granger
causality(var_est1, cause = "PBI.ts")
causality(var_est1, cause = "CRE.ts")

#2.6 FIR: 
# impulso =PBI, respuesta = Creditos
irf_i_y = irf(var_est1, impulse = "PBI.ts", response = "CRE.ts", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = PBI, respuesta = Creditos", lwd=2)


#2.8 Analisis de descomposicion de la varianza del error de prediccion:
desc_var1 <- fevd(var_est1, n.ahead = 6)

plot(desc_var1, xlab = c("Horizonte","Horizonte"), 
     ylab = c("Porcentaje","Porcentaje"), 
     legend = c("PBI","Creditos"),
     main = c("PIB","Creditos"))

#2.9 Descomposición histórica 
plot(decompose(data)) 


####PBI ~ Tipo de Cambio
data1<-ts.union(PBI.ts,TDC.ts)

# Grafico de los datos
plot(data1, main = "Datos Perú", xlab = "Periodos (Mensuales)")

# Estimacion VAR en forma reducida
var_est2 <- VAR(y= data1, p=1)
coef(var_est2)

# Seleccion de rezagos
VARselect(y = data1, lag.max = 7, type = c("const"))

var_est2 <- VAR(y = data1, p = 2)

# Estabilidad del VAR
par(mfrow=c(1,1))
barplot(roots(var_est2), ylim = c(0,1), 
        main = "Estabilidad: Raices",
        names.arg = as.character(1:4))
abline(h = 1, col = "red", lty = 2, lwd=2)


# Pruebas de causalidad de Granger
causality(var_est2, cause = "PBI.ts")
causality(var_est2, cause = "TDC.ts")

# FIR: 
# impulso =PBI, respuesta = Tipo de Cambio
irf_i_y = irf(var_est1, impulse = "PBI.ts", response = "TDC.ts", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = PBI, respuesta = Tipo de Cambio", lwd=2)


# Analisis de descomposicion de la varianza del error de prediccion:
desc_var2 <- fevd(var_est2, n.ahead = 6)

plot(desc_var2, xlab = c("Horizonte","Horizonte"), 
     ylab = c("Porcentaje","Porcentaje"), 
     legend = c("PBI","Tipo de cambio"),
     main = c("PIB","Tipo de cambio"))

#2.9 Descomposición histórica 
plot(decompose(data1))

####PBI ~ Exportaciones
data2<-ts.union(PBI.ts,EXP.ts)

# Grafico de los datos
plot(data2, main = "Datos Perú", xlab = "Periodos (Mensuales)")

# Estimacion VAR en forma reducida
var_est3 <- VAR(y= data2, p=1)
coef(var_est3)

# Seleccion de rezagos
VARselect(y = data2, lag.max = 7, type = c("const"))

var_est3 <- VAR(y = data2, p = 7)

# Estabilidad del VAR
par(mfrow=c(1,1))
barplot(roots(var_est3), ylim = c(0,1), 
        main = "Estabilidad: Raices",
        names.arg = as.character(1:14))
abline(h = 1, col = "red", lty = 2, lwd=2)


# Pruebas de causalidad de Granger
causality(var_est3, cause = "PBI.ts")
causality(var_est3, cause = "EXP.ts")

# FIR: 
# impulso =PBI, respuesta = Tipo de Cambio
irf_i_y = irf(var_est3, impulse = "PBI.ts", response = "TDC.ts", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = PBI, respuesta = Exportaciones", lwd=2)


# Analisis de descomposicion de la varianza del error de prediccion:
desc_var3 <- fevd(var_est3, n.ahead = 6)

plot(desc_var3, xlab = c("Horizonte","Horizonte"), 
     ylab = c("Porcentaje","Porcentaje"), 
     legend = c("PBI","Exportaciones"),
     main = c("PIB","Exportaciones"))

#2.9 Descomposición histórica 
plot(decompose(data2)) 

####PBI ~ Inversiones
data3<-ts.union(PBI.ts,INV.ts)

# Grafico de los datos
plot(data3, main = "Datos Perú", xlab = "Periodos (Mensuales)")

# Estimacion VAR en forma reducida
var_est4 <- VAR(y= data3, p=1)
coef(var_est4)

# Seleccion de rezagos
VARselect(y = data3, lag.max = 7, type = c("const"))

var_est4 <- VAR(y = data3, p = 3)

# Estabilidad del VAR
par(mfrow=c(1,1))
barplot(roots(var_est4), ylim = c(0,1), 
        main = "Estabilidad: Raices",
        names.arg = as.character(1:6))
abline(h = 1, col = "red", lty = 2, lwd=2)


# Pruebas de causalidad de Granger
causality(var_est4, cause = "PBI.ts")
causality(var_est4, cause = "INV.ts")

# FIR: 
# impulso =PBI, respuesta = Inversiones
irf_i_y = irf(var_est4, impulse = "PBI.ts", response = "INV.ts", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = PBI, respuesta = Inversiones", lwd=2)


# Analisis de descomposicion de la varianza del error de prediccion:
desc_var4 <- fevd(var_est4, n.ahead = 6)

plot(desc_var4, xlab = c("Horizonte","Horizonte"), 
     ylab = c("Porcentaje","Porcentaje"), 
     legend = c("PBI","Inversiones"),
     main = c("PIB","Inversiones"))

#2.9 Descomposición histórica 
plot(decompose(data3)) 

####PBI ~ Importaciones
data4<-ts.union(PBI.ts,IMP.ts)

# Grafico de los datos
plot(data4, main = "Datos Perú", xlab = "Periodos (Mensuales)")

# Estimacion VAR en forma reducida
var_est5 <- VAR(y= data4, p=1)
coef(var_est5)

# Seleccion de rezagos
VARselect(y = data4, lag.max = 7, type = c("const"))

var_est5 <- VAR(y = data4, p = 5)

# Estabilidad del VAR
par(mfrow=c(1,1))
barplot(roots(var_est5), ylim = c(0,1), 
        main = "Estabilidad: Raices",
        names.arg = as.character(1:10))
abline(h = 1, col = "red", lty = 2, lwd=2)


# Pruebas de causalidad de Granger
causality(var_est5, cause = "PBI.ts")
causality(var_est5, cause = "IMP.ts")

# FIR: 
# impulso =PBI, respuesta = Importaciones
irf_i_y = irf(var_est5, impulse = "PBI.ts", response = "IMP.ts", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = PBI, respuesta = Importaciones", lwd=2)


# Analisis de descomposicion de la varianza del error de prediccion:
desc_var5 <- fevd(var_est5, n.ahead = 6)

plot(desc_var5, xlab = c("Horizonte","Horizonte"), 
     ylab = c("Porcentaje","Porcentaje"), 
     legend = c("PBI","Importaciones"),
     main = c("PIB","Importaciones"))

#2.9 Descomposición histórica 
plot(decompose(data4))

####PBI ~ Inflacion
data5<-ts.union(PBI.ts,INF.ts)

# Grafico de los datos
plot(data5, main = "Datos Perú", xlab = "Periodos (Mensuales)")

# Estimacion VAR en forma reducida
var_est6 <- VAR(y= data5, p=1)
coef(var_est6)

# Seleccion de rezagos
VARselect(y = data5, lag.max = 7, type = c("const"))

var_est6 <- VAR(y = data5, p = 3)

# Estabilidad del VAR
par(mfrow=c(1,1))
barplot(roots(var_est6), ylim = c(0,1), 
        main = "Estabilidad: Raices",
        names.arg = as.character(1:6))
abline(h = 1, col = "red", lty = 2, lwd=2)


# Pruebas de causalidad de Granger
causality(var_est6, cause = "PBI.ts")
causality(var_est6, cause = "INF.ts")

# FIR: 
# impulso =PBI, respuesta = Inflacion
irf_i_y = irf(var_est6, impulse = "PBI.ts", response = "INF.ts", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = PBI, respuesta = Inflacion", lwd=2)


# Analisis de descomposicion de la varianza del error de prediccion:
desc_var6 <- fevd(var_est6, n.ahead = 6)
desc_var6
plot(desc_var6, xlab = c("Horizonte","Horizonte"), 
     ylab = c("Porcentaje","Porcentaje"), 
     legend = c("PBI","Inflacion"),
     main = c("PIB","Inflacion"))

#2.9 Descomposición histórica 
plot(decompose(data5))

####PBI ~ IGLBV
data6<-ts.union(PBI.ts,IGLBV.ts)

# Grafico de los datos
plot(data6, main = "Datos Perú", xlab = "Periodos (Mensuales)")

# Estimacion VAR en forma reducida
var_est7 <- VAR(y= data6, p=1)
coef(var_est6)

# Seleccion de rezagos
VARselect(y = data6, lag.max = 7, type = c("const"))

var_est7 <- VAR(y = data6, p = 6)

# Estabilidad del VAR
par(mfrow=c(1,1))
barplot(roots(var_est7), ylim = c(0,1), 
        main = "Estabilidad: Raices",
        names.arg = as.character(1:12))
abline(h = 1, col = "red", lty = 2, lwd=2)


# Pruebas de causalidad de Granger
causality(var_est7, cause = "PBI.ts")
causality(var_est7, cause = "IGLBV.ts")

# FIR: 
# impulso =PBI, respuesta = Inflacion
irf_i_y = irf(var_est7, impulse = "PBI.ts", response = "IGLLBV.ts", 
              n.ahead = 12, ortho = TRUE)
plot(irf_i_y, main = "impulso = PBI, respuesta = IGLBV", lwd=2)


# Analisis de descomposicion de la varianza del error de prediccion:
desc_var7 <- fevd(var_est7, n.ahead = 6)
desc_var7
plot(desc_var7, xlab = c("Horizonte","Horizonte"), 
     ylab = c("Porcentaje","Porcentaje"), 
     legend = c("PBI","IGLBV"),
     main = c("PIB","IGLBV"))

#2.9 Descomposición histórica 
plot(decompose(data6))
