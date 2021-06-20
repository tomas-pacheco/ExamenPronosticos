## Importamos algunas de las librer?as que vamos a usar.

library(xts)
library(ggplot2)
library(devtools)
library(ggfortify)
library(dplyr)


###############################################################################

# PREGUNTA M: ¿Tenemos que diferenciar las reservas? 



###############################################################################




## Seteamos el directorio. Prueba.``

dir <- "G:\\Mi unidad\\UdeSA\\Pronósticos\\Final\\Data\\dep"
dir <- "C:\\Users\\Abi\\Downloads"

setwd(dir)

# Definimos la paleta de colores.

colores <- c("#00ABC5", "#f7941c", "#edf71c", "#ff3c84","#f75144")

# Abrimos la base de datos.

data <- read.csv("Data_Final_PF2.csv")
data <- data[,-2]
data <- na.omit(data)
rownames(data) <- data$time

options(scipen=999)


colnames(data)
data <- data[,-c(12,13,14,18,19,20,21,22,23)]


# Construimos nuevas variables 

#Las muertes en Argentina respecto de las muertes en el resto del mundo 

data$muertes.arg.rel <- data$muertosarg/data$muertesmundo

# Ponemos cero donde hay NA

data$muertes.arg.rel<-ifelse(is.na(data$muertes.arg.rel),0,data$muertes.arg.rel)

#Los casos en Argentina respecto de los casos en el resto del mundo 

data$casos.arg.rel <- data$casosarg/data$casosmundo

#, Ponemos cero donde hay NA

data$casos.arg.rel<-ifelse(is.na(data$casos.arg.rel),0,data$casos.arg.rel)


# Borramos las variables 'nominales' del resto del mundo, el sentimiento de alberto fernandez sin 
# el suavizado, y las variables del sentimiento de mercado de Twitter 

data <- data[, -c(2,15,16,17,18)]

# Ahora generamos un objeto de series de tiempo para cada una de las variables.

for (i in colnames(data)[-(1)]){
    assign(i, xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d")) )
}


# Grafico de nuestra serie de interes.

autoplot(sentsmooth, ts.colour = colores[1]) + 
  ggtitle("Evolución sentimental de Alberto Fernández") + 
  xlab("Tiempo") + 
  ylab("Sentimiento") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# VER 

# Grafico mercado sintetico  (lo dejamos para el event study)

#autoplot(sent_trends, ts.colour = colores[2]) + 
  #ggtitle("Evolución de nuestro 'synthetic market'") + 
  #xlab("Tiempo") + 
  #ylab("Sentimiento") + 
  #theme_minimal() + 
  #theme(legend.position = "none",
  #      plot.title = element_text(hjust = 0.5))


# Creamos una función que devuelve el estadístico con su significatividad 

stars <- function(estadistico, criticalvector){
  if (estadistico < criticalvector[1]){
    return(paste(round(estadistico,2), "***", sep = "")) 
  }else{
    if (estadistico < criticalvector[2]){
      return(paste(round(estadistico,2), "**", sep = ""))
    }else{
      if (estadistico < criticalvector[3]){
        return(paste(round(estadistico,2), "*", sep = ""))
      }else{
        return(paste(round(estadistico,2), "", sep = ""))
      }
    }
  }
}


stars2 <- function(estadistico, criticalvector){
  if (estadistico > criticalvector[1]){
    return(paste(round(estadistico,2), "***", sep = "")) 
  }else{
    if (estadistico > criticalvector[2]){
      return(paste(round(estadistico,2), "**", sep = ""))
    }else{
      if (estadistico > criticalvector[3]){
        return(paste(round(estadistico,2), "*", sep = ""))
      }else{
        return(paste(round(estadistico,2), "", sep = ""))
      }
    }
  }
}


library(urca)
library(stargazer)

results <- matrix(nrow = 16,ncol = 4, NA)
r = 1
for (i in colnames(data)[-(1)]){
  results[r,1] <- i
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- na.omit(series)
  test <- ur.df(series, type = c("none"))
  results[r,2] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.df(series, type = c("trend"))
  results[r,3] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.df(series, type = c("drift"))
  results[r,4] <- stars(test@teststat[1], test@cval[1,])
  r = r+1
}
colnames(results) <- c("Variable", "None","Trend","Drift")


# Descargamos la tabla 

stargazer(results , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

results1 <- matrix(nrow = 25,ncol = 3, NA)
r = 1
for (i in colnames(data)[-(1)]){
  results1[r,1] <- i
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- na.omit(series)
  test <- ur.pp(series, type = c("Z-tau"), model=c("constant"), lags=c("long"))
  results1[r,2] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.pp(series, type = c("Z-tau"), model=c("trend"), lags=c("long"))
  results1[r,3] <- stars(test@teststat[1], test@cval[1,])
  r = r+1
}
colnames(results1) <- c("Variable", "Constant", "Trend")

# Descargamos la tabla 

library(stargazer)

stargazer(results1 , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

results2 <- matrix(nrow = 25,ncol = 3, NA)
r = 1
for (i in colnames(data)[-(1)]){
  results2[r,1] <- i
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- na.omit(series)
  test <- ur.kpss(series, type = c("mu"), lags=c("short"))
  results2[r,2] <- stars2(test@teststat[1], test@cval[1,])
  test <- ur.kpss(series, type = c("tau"), lags=c("short"))
  results2[r,3] <- stars2(test@teststat[1], test@cval[1,])
  r = r+1
}
colnames(results2) <- c("Variable", "Constant", "Trend")

stargazer(results2 , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

# Definimos la ventana in-sample y out-of-sample

in.sample <- data[1:425,]

out.of.sample <- data[426:483,]


# Estimacion de los modelos 

# Cargamos la libreria

library(forecast)

#Estimamos un modelo ARIMA 

arima.1 <- auto.arima(in.sample[,2])

summary(arima.1)

arima.to.table <- arima(in.sample[,2],order = c(5,0,1))

# Validaremos el modelo chequeando que los residuos sean ruido blanco.

act1 <- checkresiduals(arima.1, lag = 13)

test <- round(act1$p.value, 2)

# Como H0 es ausencia de autocorrelación y no rechazamos la hipótesis
# podemos decir que los residuos no están correlacionados.
# Modelo valido, pval = 0.1405

# Estimamos un modelo ETS


ets.1 <- ets(in.sample[,2])

# Verificamos que los residuos sean RB.

act2 <- Box.test(ets.1$residuals, lag = 13, type = c("Ljung-Box"))

test2 <- round(act2$p.value, 2)

# Como H0 es ausencia de autocorrelación y rechazamos la hipótesis
# podemos decir que los residuos están correlacionados.
# Modelo no valido, pval = 0.00000411

# Estimamos un modelo ARIMAX 

# Usaremos el sentimiento de Alberto Fernandez 
# como variable explicada e introduciremos al resto de nuestras variables
# como regresores exogenos

# Utilizamos el mismo modelo ARIMAX seleccionado previamente.


arimax.1 <- Arima(in.sample[,2], order = c(5,0,1),
                     xreg = as.matrix(in.sample[,3:length(in.sample)]))

arimax.to.table <- arima(in.sample[,2],order = c(5,0,1),
                         xreg = as.matrix(in.sample[,4:length(in.sample)]))


act3 <- Box.test(arimax.1$residuals, lag = 13, type = c("Ljung-Box"))

test3 <- round(act3$p.value, 2)

# El p valor nos da 0.6, con lo cual validamos el modelo 

# Estimamos un modelo ADL 

library(ARDL)
library(dynlm)

# Creamos las dummies 

data$mes <- substr(data$time, 6,7)
num <- as.data.frame(table(substr(data$time, 6,7)))[,1]

for (i in num){
  var.names <- paste("mes",i, sep = "")
  data[var.names] <- ifelse(data$mes == i,1,0)
}

data_1 <- subset(data, select = -c(mes))

data.in.sample.dum <- data_1[1:425,-c(1)]

# Seleccionamos el orden del ADL 

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + casosarg + muertosarg +
                            vacunasarg + maxtemp + mintemp + muertes.arg.rel + casos.arg.rel |
                            mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                            mes07 + mes08 + mes09 +  mes10 + mes11, 
                          data = data.in.sample.dum, max_order = 5)

# Ahora estimamos el modelo:

adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                 casosarg + muertosarg + vacunasarg + maxtemp + 
                 mintemp + muertes.arg.rel + casos.arg.rel|
                 mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                 mes07 + mes08 + mes09 +  mes10 + mes11, 
               data = data.in.sample.dum, order = as.vector(order.adl.dl$best_order))

# Estimamos el ADL pero con la función dynlm

adl <- adl.dl$full_formula
adl.1 <- dynlm(adl, data = adl.dl$data)

# Comprobamos que tiene los mismos coeficientes

identical(adl.dl$coefficients, adl.1$coefficients)

# Test

act4 <- Box.test(adl.1$residuals, lag = 13, type = c("Ljung-Box"))
test4 <- round(act4$p.value, 4)

# el p valor nos da 0.0083

model <- lm(rnorm(100,0,1) ~ rnorm(100,20,3))

stargazer(arima.to.table, arimax.to.table, model, adl.1,
          align = TRUE, 
          dep.var.labels = c("SentIndex", "SentIndex", "SentIndex", "SentIndex"),
          omit = c("mes01", "mes02", "mes03", "mes04",
                   "mes05", "mes06", "mes07", "mes08",
                   "mes09", "mes10", "mes11"),
          keep.stat = c("n", "ll", "rsq"),
          no.space = TRUE,
          type = "latex")



# Estimamos un modelo MCE

# VER: creo que lo deberiamos sacar porque no tiene sentido probar cointegracion entre variables
# I(0) 

library(egcm)

# Evaluamos cointegración con el test de E&G entre la variable sentsmooth y las demás variables consideradas 

# Creamos una función para mostrar la significatividad de los p valores 

stars3<-function(x){
  if (x < 0.01){
    return(paste(x,"***", sep = ""))
  }else{
    if (x < 0.05){
    return(paste(x,"**", sep = ""))
  }else{
    if (x < 0.1){
    return(paste(x,"*", sep = ""))  
  }else{
    return(paste(x,"", sep = ""))
  }
}
}
}


Test.cointEG <- matrix(nrow = 13,ncol = 2, NA)
r=1
for (i in 1:13) {
  Test.cointEG[,1] <- colnames(data)[3:15]
  j <- egcm(in.sample[,2], in.sample[,2+i], urtest = 'adf', i1test = 'adf')
  Test.cointEG[i,2]<-stars3(round(j[["r.p"]],4))
}

colnames(Test.cointEG) <- c("Variable", "p valor")

# Descargamos la tabla de los test de E&G

stargazer(Test.cointEG, no.space = TRUE, type = "latex")


# Modelo de corrección de errores.


library(ecm)

ecm(sentsmooth, twfav, lags = 1)

summary(lm(sentsmooth ~ twfav))


# Estimamos un modelo VAR 

library(vars)

#nos quedamos con el período in sample que tiene todas las variables estacionarias 

reservas.est <- diff(in.sample[,5])

dolar.est <- diff(in.sample[,8])

in.sample.d <- cbind(in.sample[-1,-c(5,8)],reservas.est, dolar.est)

#[,3_17]

in.sample.d <- in.sample.d[,2:15]

var.d <- VARselect(in.sample.d, type ="const")

var.d$selection


# El orden de rezagos óptimos, de acuerdo al criterio de FPE, es 6.
# Estimamos:

# VER: si estimamos el var con constante, tendencia, las dos o ninguna

var.dl <- VAR(in.sample.d, p = 6, type = "const")

# Par

serial.test(var.dl) 


# Rechazamos la hipótesis nula de que los residuos están
# incorrelacionados. Por lo tanto, no podemos validar el modelo. 

# VER: lo del VEC lo podriamos eliminar porque las series son I(0)

# Estimamos un modelo VEC 

#Estimamos un modelo VAR en niveles 

# El orden de rezagos óptimos, de acuerdo al criterio de FPE, es 1.
# Estimamos:

var.l <- VAR(in.sample[,3:16], p = 1, type = "both", season = 12)

# Ahora, corremos el test de normalidad en los residuos:

normality.test(var.l, multivariate.only = TRUE)

jct.t.e <-ca.jo(in.sample[,c(3,4,5)],type = "eigen", ecdet = "trend", 
              spec = "transitory", K=2)

summary(jct.t.e)

jct.t.t <-ca.jo(in.sample[,c(3,4,5)],type = "trace", ecdet = "trend", 
              spec = "transitory", K=2)

summary(jct.t.t)

# Estan las 3 cointegradas. Resultados de ambos tests.


# Ahora, un FAVAR.

PCA1 <- prcomp(in.sample[,3:15], scale =TRUE) 

# Veremos los autovalores para evaluar qué cantidad de componentes utilizaremos

PCA1$sdev^2

# Como hay 4 componentes cuyo autovalor es superior a 1, los usaremos.

PC <- scale(in.sample[,3:15])%*%PCA1$rotation

# Ahora estimamos el modelo FAVAR.

favar.data <- cbind(in.sample[,2], PC[,1:4])

VARselect(favar.data, lag.max = 10, type = c("both"))

favar<- VAR(favar.data, p= 8, type = "both")
summary(favar)

# Correlacion

serial.test(favar) 

favar$varresult$X

stargazer(var.dl$varresult$sentsmooth, favar$varresult$X,
          align = TRUE, 
          dep.var.labels = c("SentIndex", "SentIndex"),
          omit = c("mes01", "mes02", "mes03", "mes04",
                   "mes05", "mes06", "mes07", "mes08",
                   "mes09", "mes10", "mes11", "sd1", 
                   "sd2", "sd3"),
          keep.stat = c("n", "ll", "rsq"),
          no.space = TRUE,
          type = "latex")




## PRONOSTICOS 

#Esquema fijo 

# h=1 

data1 <- ts(data, frequency = 365, start = c(2019,12))

pr.f.h1 <- ts(matrix(0, 58, 5), frequency = 365, start=c(2020,11))
colnames(pr.f.h1) <- c("ARIMA", "ARIMAX", "ADL", "ETS","VAR")
h<-1
for(i in 1:58){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = h)
  pr.f.h1[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h1[i,2] <- forecast2$mean[h]
  
  #ETS 
  f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f1, n.ahead=h)
  pr.f.h1[i,3] <- pre$mean[h]
  
}

#ADL 

#Seleccionamos el orden del ADL 

data_1.1<-ts(data_1,frequency = 365, start = c(2019,12))

data_dum.1<-data_1.1[1:425,-c(1,27)]

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + muertes.arg.rel + casos.arg.rel|
                            mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                            mes07 + mes08 + mes09 +  mes10 + mes11, 
                          data = data_dum.1, max_order = 8)


h<-1

count <- 2
for(i in 1:58){
  temp2 <-window(data_1.1[,-c(1,27)],start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
          
#el orden del ADL es el conseguido al aplicar el modelo al período in sample 
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel|
                   mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                   mes07 + mes08 + mes09 +  mes10 + mes11, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = temp2)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.f.h1[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

library(forecast)

library(vars)

# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8,16,17,18,19,20,21,22,23,24,25,26,27,28)], reservas.est, dolar.est)

h<-1

for(i in 1:58){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  f5 <- VAR(temp2, p = 6, type = "trend")
  forecast1<- forecast(f5)
  pr.f.h1[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Grafico de los pronosticos con esquema fijo y h=1 

# VER: los colores no se si quedan muuy bien 

colores <- c("#00ABC5","#cfb0b4" ,"#ff3c84","#f7941c", "#edf71c")


autoplot(ts.union(out.of.sample[,2], pr.f.h1[,1], pr.f.h1[,2],pr.f.h1[,3],pr.f.h1[,4],pr.f.h1[,5]), size = 1) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5]))+
  ggtitle("Pronósticos fijos con h = 1") +
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# ESQUEMA RECURSIVO 

data1 <- ts(data, frequency = 365, start = c(2019,12))

pr.rec.h1 <- ts(matrix(0, 58, 5), frequency = 365, start=c(2020,11))
colnames(pr.rec.h1) <- c("ARIMA", "ARIMAX", "ADL", "ETS","VAR")
h<-1
for(i in 1:58){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rec.h1[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rec.h1[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f1, n.ahead=h)
  pr.rec.h1[i,3] <- pre$mean[h]
  
}


#ADL 

data_1.1<-ts(data_1,frequency = 365, start = c(2019,12))

data_dum.1<-data_1.1[1:425,-c(1,27)]

h<-1
count <- 2
for(i in 1:58){
  temp2 <-window(data_1.1[,-c(1,27)], start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp","muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                              mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                              mes07 + mes08 + mes09 +  mes10 + mes11, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                   mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                   mes07 + mes08 + mes09 +  mes10 + mes11, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rec.h1[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8,16,17,18,19,20,21,22,23,24,25,26,27,28)], reservas.est, dolar.est)

h<-1

for(i in 1:58){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5)
  pr.rec.h1[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos recursivos y h=1 

autoplot(ts.union(out.of.sample[,2], pr.rec.h1[,1], pr.rec.h1[,2],pr.rec.h1[,3],pr.rec.h1[,4],pr.rec.h1[,5]), size = 1.1) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"),
                     values = c("black", "sienna2", "palegreen3","#00AFBB", colores[2], colores[1])) +
  ggtitle("Pronósticos recursivos con h = 1") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))



# ESQUEMA ROLLING 


data1 <- ts(data, frequency = 365, start = c(2019,12))

pr.rol.h1 <- ts(matrix(0, 58, 5), frequency = 365, start=c(2020,11))
colnames(pr.rec.h1) <- c("ARIMA", "ARIMAX", "ADL", "ETS","VAR")
h<-1
for(i in 1:58){
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rol.h1[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h1[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f1, n.ahead=h)
  pr.rol.h1[i,3] <- pre$mean[h]
  
}


#ADL 

data_1.1<-ts(data_1,frequency = 365, start = c(2019,12))

data_dum.1<-data_1.1[1:425,-c(1,27)]

h<-1
count <- 2
for(i in 1:58){
  temp2 <-window(data_1.1[,-c(1,27)], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                              mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                              mes07 + mes08 + mes09 +  mes10 + mes11, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                   mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                   mes07 + mes08 + mes09 +  mes10 + mes11, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rol.h1[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8,16,17,18,19,20,21,22,23,24,25,26,27,28)], reservas.est, dolar.est)

h<-1

for(i in 1:58){ 
  temp2<-window(data.diff, start = 2019.033 + (i-1)/365, end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5)
  pr.rol.h1[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos rolling y h=1 

autoplot(ts.union(out.of.sample[,2], pr.rol.h1[,1], pr.rol.h1[,2],pr.rol.h1[,3],pr.rol.h1[,4],pr.rol.h1[,5]), size = 1.3) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"),
                     values = c("black", "sienna2", "palegreen3","#00AFBB", colores[2], colores[1])) +
  ggtitle("Pronósticos rolling con h = 1") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


#### h = 2


#Esquema fijo 

# h=2

# con lo cual, pierdo 1 observación

data1 <- ts(data, frequency = 365, start = c(2019,12))

pr.f.h2 <- ts(matrix(0, 57, 5), frequency = 365, start=c(2020,11))
colnames(pr.f.h2) <- c("ARIMA", "ARIMAX", "ADL", "ETS","VAR")
h<-2
for(i in 1:57){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = h)
  pr.f.h2[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h2[i,2] <- forecast2$mean[h]
  
  #ETS 
  f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f1, n.ahead=h)
  pr.f.h2[i,3] <- pre$mean[h]
  
}

#ADL 

#Seleccionamos el orden del ADL 

data_1.1<-ts(data_1,frequency = 365, start = c(2019,12))

data_dum.1<-data_1.1[1:425,-c(1,27)]

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + muertes.arg.rel + casos.arg.rel|
                            mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                            mes07 + mes08 + mes09 +  mes10 + mes11, 
                          data = data_dum.1, max_order = 8)


h<-2

count <- 2
for(i in 1:57){
  temp2 <-window(data_1.1[,-c(1,27)],start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
  
  #el orden del ADL es el conseguido al aplicar el modelo al período in sample 
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel|
                   mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                   mes07 + mes08 + mes09 +  mes10 + mes11, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = temp2)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.f.h2[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

library(forecast)

library(vars)

# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8,16,17,18,19,20,21,22,23,24,25,26,27,28)], reservas.est, dolar.est)

h<-2

for(i in 1:57){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  f5 <- VAR(temp2, p = 6, type = "trend")
  forecast1<- forecast(f5)
  pr.f.h2[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Grafico de los pronosticos con esquema fijo y h=2

# VER: los colores no se si quedan muuy bien 

colores <- c("#00ABC5","#cfb0b4" ,"#ff3c84","#f7941c", "#edf71c")

# eliminamos las primeras 6 observaciones del período out of sample debido a que la cantidad 
# de pasos adelante que hicimos los pronósticos 

autoplot(ts.union(out.of.sample[2:58,2], pr.f.h2[,1], pr.f.h2[,2],pr.f.h2[,3],pr.f.h2[,4],pr.f.h2[,5]), size = 1) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5]))+
  ggtitle("Pronósticos fijos con h = 2") +
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# ESQUEMA RECURSIVO 

data1 <- ts(data, frequency = 365, start = c(2019,12))

pr.rec.h2 <- ts(matrix(0, 57, 5), frequency = 365, start=c(2020,11))
colnames(pr.rec.h2) <- c("ARIMA", "ARIMAX", "ADL", "ETS","VAR")
h<-2
for(i in 1:57){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rec.h2[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rec.h2[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f1, n.ahead=h)
  pr.rec.h2[i,3] <- pre$mean[h]
  
}


#ADL 

data_1.1<-ts(data_1,frequency = 365, start = c(2019,12))

data_dum.1<-data_1.1[1:425,-c(1,27)]

h<-2
count <- 2
for(i in 1:57){
  temp2 <-window(data_1.1[,-c(1,27)], start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp","muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                              mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                              mes07 + mes08 + mes09 +  mes10 + mes11, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                   mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                   mes07 + mes08 + mes09 +  mes10 + mes11, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rec.h2[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8,16,17,18,19,20,21,22,23,24,25,26,27,28)], reservas.est, dolar.est)

h<-2

for(i in 1:57){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5)
  pr.rec.h2[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos recursivos y h=1 

autoplot(ts.union(out.of.sample[2:58,2], pr.rec.h2[,1], pr.rec.h2[,2],pr.rec.h2[,3],pr.rec.h2[,4],pr.rec.h2[,5]), size = 1.1) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"),
                     values = c("black", "sienna2", "palegreen3","#00AFBB", colores[2], colores[1])) +
  ggtitle("Pronósticos recursivos con h = 2") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# ESQUEMA ROLLING 


data1 <- ts(data, frequency = 365, start = c(2019,12))

pr.rol.h2 <- ts(matrix(0, 57, 5), frequency = 365, start=c(2020,11))
colnames(pr.rol.h2) <- c("ARIMA", "ARIMAX", "ADL", "ETS","VAR")
h<-2
for(i in 1:57){
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rol.h2[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h2[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f1, n.ahead=h)
  pr.rol.h2[i,3] <- pre$mean[h]
  
}

#ADL 

data_1.1<-ts(data_1,frequency = 365, start = c(2019,12))

data_dum.1<-data_1.1[1:425,-c(1,27)]

h<-2
count <- 2
for(i in 1:57){
  temp2 <-window(data_1.1[,-c(1,27)], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                              mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                              mes07 + mes08 + mes09 +  mes10 + mes11, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                   mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                   mes07 + mes08 + mes09 +  mes10 + mes11, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rol.h2[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8,16,17,18,19,20,21,22,23,24,25,26,27,28)], reservas.est, dolar.est)

h<-2

for(i in 1:57){ 
  temp2<-window(data.diff, start = 2019.033 + (i-1)/365, end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5)
  pr.rol.h2[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos rolling y h=1 

autoplot(ts.union(out.of.sample[2:58,2], pr.rol.h2[,1], pr.rol.h2[,2],pr.rol.h2[,3],pr.rol.h2[,4],pr.rol.h2[,5]), size = 1.3) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"),
                     values = c("black", "blue", "palegreen3","#00AFBB", colores[2], colores[1])) +
  ggtitle("Pronósticos rolling con h = 1") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))









#### h = 7


#Esquema fijo 

# h=7 

# con lo cual, pierdo 6 observaciones 

data1 <- ts(data, frequency = 365, start = c(2019,12))

pr.f.h7 <- ts(matrix(0, 52, 5), frequency = 365, start=c(2020,11))
colnames(pr.f.h7) <- c("ARIMA", "ARIMAX", "ADL", "ETS","VAR")
h<-7
for(i in 1:52){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = 7)
  pr.f.h7[i,1] <- forecast$mean[7]
  
  #ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h7[i,2] <- forecast2$mean[h]
  
  #ETS 
  f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f1, n.ahead=h)
  pr.f.h7[i,3] <- pre$mean[h]
  
}


# VER: el ARIMA queda horrible, capaz hay algo mal 

# como perdemos las 6 primeras observaciones vamos a tener 52 pronósticos en vez de 58

#ADL 

#Seleccionamos el orden del ADL 

data_1.1<-ts(data_1,frequency = 365, start = c(2019,12))

data_dum.1<-data_1.1[1:425,-c(1,27)]

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + muertes.arg.rel + casos.arg.rel|
                            mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                            mes07 + mes08 + mes09 +  mes10 + mes11, 
                          data = data_dum.1, max_order = 8)


h<-7

count <- 2
for(i in 1:52){
  temp2 <-window(data_1.1[,-c(1,27)],start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
  
  #el orden del ADL es el conseguido al aplicar el modelo al período in sample 
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel|
                   mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                   mes07 + mes08 + mes09 +  mes10 + mes11, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = temp2)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.f.h7[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

library(forecast)

library(vars)

# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8,16,17,18,19,20,21,22,23,24,25,26,27,28)], reservas.est, dolar.est)

h<-7

for(i in 1:52){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  f5 <- VAR(temp2, p = 6, type = "trend")
  forecast1<- forecast(f5)
  pr.f.h7[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Grafico de los pronosticos con esquema fijo y h=1 

# VER: los colores no se si quedan muuy bien 

colores <- c("#00ABC5","#cfb0b4" ,"#ff3c84","#f7941c", "#edf71c")

# eliminamos las primeras 6 observaciones del período out of sample debido a que la cantidad 
# de pasos adelante que hicimos los pronósticos 

autoplot(ts.union(out.of.sample[7:58,2], pr.f.h7[,1], pr.f.h7[,2],pr.f.h7[,3],pr.f.h7[,4],pr.f.h7[,5]), size = 1) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5]))+
  ggtitle("Pronósticos fijos con h = 7") +
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# ESQUEMA RECURSIVO 

data1 <- ts(data, frequency = 365, start = c(2019,12))

pr.rec.h7 <- ts(matrix(0, 52, 5), frequency = 365, start=c(2020,11))
colnames(pr.rec.h7) <- c("ARIMA", "ARIMAX", "ADL", "ETS","VAR")
h<-7
for(i in 1:52){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rec.h7[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rec.h7[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f1, n.ahead=h)
  pr.rec.h7[i,3] <- pre$mean[h]
  
}


#ADL 

data_1.1<-ts(data_1,frequency = 365, start = c(2019,12))

data_dum.1<-data_1.1[1:425,-c(1,27)]

h<-7 
count <- 2
for(i in 1:52){
  temp2 <-window(data_1.1[,-c(1,27)], start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp","muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                              mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                              mes07 + mes08 + mes09 +  mes10 + mes11, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                   mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                   mes07 + mes08 + mes09 +  mes10 + mes11, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rec.h7[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8,16,17,18,19,20,21,22,23,24,25,26,27,28)], reservas.est, dolar.est)

h<-7

for(i in 1:52){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5)
  pr.rec.h7[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos recursivos y h=1 

autoplot(ts.union(out.of.sample[7:58,2], pr.rec.h7[,1], pr.rec.h7[,2],pr.rec.h7[,3],pr.rec.h7[,4],pr.rec.h7[,5]), size = 1.1) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"),
                     values = c("black", "sienna2", "palegreen3","#00AFBB", colores[2], colores[1])) +
  ggtitle("Pronósticos recursivos con h = 7") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# ESQUEMA ROLLING 


data1 <- ts(data, frequency = 365, start = c(2019,12))

pr.rol.h7 <- ts(matrix(0, 52, 5), frequency = 365, start=c(2020,11))
colnames(pr.rol.h7) <- c("ARIMA", "ARIMAX", "ADL", "ETS","VAR")
h<-7
for(i in 1:52){
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rol.h7[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h7[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f1, n.ahead=h)
  pr.rol.h7[i,3] <- pre$mean[h]
  
}


#ADL 

data_1.1<-ts(data_1,frequency = 365, start = c(2019,12))

data_dum.1<-data_1.1[1:425,-c(1,27)]

h<-7
count <- 2
for(i in 1:52){
  temp2 <-window(data_1.1[,-c(1,27)], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                              mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                              mes07 + mes08 + mes09 +  mes10 + mes11, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                   mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                   mes07 + mes08 + mes09 +  mes10 + mes11, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rol.h7[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-c(1,5,8,16,17,18,19,20,21,22,23,24,25,26,27,28)], reservas.est, dolar.est)

h<-7

for(i in 1:52){ 
  temp2<-window(data.diff, start = 2019.033 + (i-1)/365, end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5)
  pr.rol.h7[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos rolling y h=1 

autoplot(ts.union(out.of.sample[7:58,2], pr.rol.h7[,1], pr.rol.h7[,2],pr.rol.h7[,3],pr.rol.h7[,4],pr.rol.h7[,5]), size = 1.3) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"),
                     values = c("black", "sienna2", "palegreen3","#00AFBB", colores[2], colores[1])) +
  ggtitle("Pronósticos rolling con h = 1") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))



#### h = 14

# VER: creo que van a quedar horribles, porque los que son 7 pasos adelante son horribles

########### BAGGING ########################

# Realizamos bagging con los modelos arima, ets, var, adl con esquema fijo y h=1

# ARIMA 

library(quantmod)

# Hago las series bootstrap de todas las variables

var.bag <- ts(matrix(0, 500, 15000), frequency = 365, start=c(2020,11))


# VER: quiero hacer que itere la matriz en la que se guarda, por ejemplo, si i=2 
#que guarde en una matriz las series bootstrap del sentimiento de alberto 

for (i in 2:15) {
  a<-bld.mbb.bootstrap(data[,i], 100) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  var.bag.i<-ts(a, frequency = 365, start = c(2019,12))
}

data.bag <- ts(a, frequency = 365, start = c(2019,12))

h<-1
for(i in 1:58){
  temp<-window(data.bag[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = h)
  pr.f.h1[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h1[i,2] <- forecast2$mean[h]
  
  #ETS 
  f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f1, n.ahead=h)
  pr.f.h1[i,3] <- pre$mean[h]
  
}






fcst.boot.arima1 <- as.data.frame(fcst.boot.arima)

plot(fcst.boot.arima1[,1])

fcst.bagged.arima2 <- rowMeans(fcst.boot.arima1)%>% ts(start=c(2019,12), frequency = 365) 














