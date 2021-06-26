## Importamos algunas de las librer?as que vamos a usar.

library(xts)
library(ggplot2)
library(devtools)
library(ggfortify)
library(dplyr)


###############################################################################

# PREGUNTA M: ¿Tenemos que diferenciar las reservas? 

# buscar VER

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


# Borramos las variables 'nominales' del resto del mundo, el sentimiento de Alberto Fernandez sin 
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

PC.is <- scale(in.sample[,3:15])%*%PCA1$rotation


### prueba 

PC <- scale(data[,3:15])%*%PCA1$rotation


# Ahora estimamos el modelo FAVAR.

favar.data <- cbind(in.sample[,2], PC.is[,1:4])

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
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.f.h1 <- ts(matrix(0, 58, 6), frequency = 365, start=c(2020,11))
colnames(pr.f.h1) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR", "FAVAR")
h<-1
for(i in 1:58){
  temp <- window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <- window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = h)
  pr.f.h1[i,1] <- forecast$mean[h]
  
  # ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h1[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f3, n.ahead=h)
  pr.f.h1[i,3] <- pre$mean[h]
  
  # FAVAR
  f4 <- VAR(cbind(temp, temp3), p = 6, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.f.h1[i,6] <- forecast4$forecast$temp$mean[h]
}

# ADL 

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
  forecast1<- forecast(f5, h=h)
  pr.f.h1[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}

write.csv(cbind(out.of.sample[,1:2], pr.f.h1), "pr.f.h1.csv")

# Grafico de los pronosticos con esquema fijo y h=1 

# VER: los colores no se si quedan muuy bien #941cf7

colores <- c("#00ABC5","#cfb0b4" ,"#ff3c84","#FF7F32", "#edf71c", "#941cf7")


autoplot(ts.union(out.of.sample[,2], pr.f.h1[,1], pr.f.h1[,2],pr.f.h1[,3],pr.f.h1[,4],pr.f.h1[,5], pr.f.h1[,6]), size = 0.7) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5], colores[6]), 
                     ncol(2))+
  ggtitle("Pronósticos fijos con h = 1") +
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.85,0.80), plot.title = element_text(hjust = 0.5))

# VER prueba de graficos 

out.of.sample.prueba <- ts(out.of.sample[,2], frequency = 365, start = c(2020,11))

autoplot(out.of.sample.prueba, size=1)+
  autolayer(pr.f.h1[,1])

# fin de prueba 

# ESQUEMA RECURSIVO 

data1 <- ts(data, frequency = 365, start = c(2019,12))

PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rec.h1 <- ts(matrix(0, 58, 6), frequency = 365, start=c(2020,11))
colnames(pr.rec.h1) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")
h<-1
for(i in 1:58){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rec.h1[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=auto.arima(temp), newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rec.h1[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rec.h1[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rec.h1[i,6] <- forecast4$forecast$temp$mean[h] 
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
                              mintemp + muertes.arg.rel + casos.arg.rel|
                              mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                              mes07 + mes08 + mes09 +  mes10 + mes11, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel|
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
  a<-VARselect(data.diff, lag.max = 13, type = "const")$selection[1]
  f5 <- VAR(temp2, p = a, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rec.h1[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos recursivos y h=1 

autoplot(ts.union(out.of.sample[,2], pr.rec.h1[,1], pr.rec.h1[,2],pr.rec.h1[,3],pr.rec.h1[,4],pr.rec.h1[,5], pr.rec.h1[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"),
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5], colores[6])) +
  ggtitle("Pronósticos recursivos con h = 1") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))



# ESQUEMA ROLLING 

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rol.h1 <- ts(matrix(0, 58, 6), frequency = 365, start=c(2020,11))
colnames(pr.rec.h1) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")
h<-1
for(i in 1:58){
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rol.h1[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=auto.arima(temp), newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h1[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rol.h1[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rol.h1[i,6] <- forecast4$forecast$temp$mean[h] 
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
  a<-VARselect(data.diff, lag.max = 13, type = "const")$selection[1]
  f5 <- VAR(temp2, p = a, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rol.h1[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos rolling y h=1 

autoplot(ts.union(out.of.sample[,2], pr.rol.h1[,1], pr.rol.h1[,2],pr.rol.h1[,3],pr.rol.h1[,4],pr.rol.h1[,5], pr.rol.h1[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"),
                     values = c("black", colores[1], colores[2],colores[3], colores[4], colores[5], colores[6])) +
  ggtitle("Pronósticos rolling con h = 1") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


#### h = 2


#Esquema fijo 

# h=2

# con lo cual, pierdo 1 observación

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.f.h2 <- ts(matrix(0, 57, 6), frequency = 365, start=c(2020,11))
colnames(pr.f.h2) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")
h<-2
for(i in 1:57){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)

  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = h)
  pr.f.h2[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h2[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f3, n.ahead=h)
  pr.f.h2[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.f.h2[i,6] <- forecast4$forecast$temp$mean[h]
  
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
  forecast1<- forecast(f5, h=h)
  pr.f.h2[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Grafico de los pronosticos con esquema fijo y h=2


# eliminamos las primeras 6 observaciones del período out of sample debido a que la cantidad 
# de pasos adelante que hicimos los pronósticos 

autoplot(ts.union(out.of.sample[2:58,2], pr.f.h2[,1], pr.f.h2[,2],pr.f.h2[,3],pr.f.h2[,4],pr.f.h2[,5], pr.f.h2[,6]), size = 0.7) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5], colores[6]))+
  ggtitle("Pronósticos fijos con h = 2") +
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# ESQUEMA RECURSIVO 

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rec.h2 <- ts(matrix(0, 57, 6), frequency = 365, start=c(2020,11))
colnames(pr.rec.h2) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR", "FAVAR")
h<-2
for(i in 1:57){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)

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
  pre <- forecast(f3, n.ahead=h)
  pr.rec.h2[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rec.h2[i,6] <- forecast4$forecast$temp$mean[h]
  
  
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
  forecast1<- forecast(f5, h=h)
  pr.rec.h2[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos recursivos y h=2

autoplot(ts.union(out.of.sample[2:58,2], pr.rec.h2[,1], pr.rec.h2[,2],pr.rec.h2[,3],pr.rec.h2[,4],pr.rec.h2[,5], pr.rec.h2[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"),
                     values =  c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5], colores[6])) +
  ggtitle("Pronósticos recursivos con h = 2") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# ESQUEMA ROLLING 


data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rol.h2 <- ts(matrix(0, 57, 6), frequency = 365, start=c(2020,11))
colnames(pr.rol.h2) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")
h<-2
for(i in 1:57){
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)

  # ARIMA 
  f1 <- auto.arima(temp)
  forecast <- forecast(f1,h = h)
  pr.rol.h2[i,1] <- forecast$mean[h]
  
  #ARIMAX
  f2 <- Arima(temp,model=auto.arima(temp), newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h2[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rol.h2[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rol.h2[i,6] <- forecast4$forecast$temp$mean[h]
  
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
  forecast1<- forecast(f5, h=h)
  pr.rol.h2[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos rolling y h=2

autoplot(ts.union(out.of.sample[2:58,2], pr.rol.h2[,1], pr.rol.h2[,2],pr.rol.h2[,3],pr.rol.h2[,4],pr.rol.h2[,5], pr.rol.h2[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"),
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5], colores[6])) +
  ggtitle("Pronósticos rolling con h = 1") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


#### h = 7

#Esquema fijo 

# con lo cual, pierdo 6 observaciones 

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.f.h7 <- ts(matrix(0, 52, 6), frequency = 365, start=c(2020,11))
colnames(pr.f.h7) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")
h<-7
for(i in 1:52){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)

  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = 7)
  pr.f.h7[i,1] <- forecast$mean[7]
  
  #ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h7[i,2] <- forecast2$mean[h]
  
  #ETS 
  f3 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f3, n.ahead=h)
  pr.f.h7[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.f.h7[i,6] <- forecast4$forecast$temp$mean[h]
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
  forecast1<- forecast(f5, h=h)
  pr.f.h7[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Grafico de los pronosticos con esquema fijo y h=7


# eliminamos las primeras 6 observaciones del período out of sample debido a que la cantidad 
# de pasos adelante que hicimos los pronósticos 

autoplot(ts.union(out.of.sample[7:58,2], pr.f.h7[,1], pr.f.h7[,2], pr.f.h7[,3], pr.f.h7[,4], pr.f.h7[,5], pr.f.h7[,6]), size = 0.7) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5], colores[6]))+
  ggtitle("Pronósticos fijos con h = 7") +
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# ESQUEMA RECURSIVO 

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rec.h7 <- ts(matrix(0, 52, 6), frequency = 365, start=c(2020,11))
colnames(pr.rec.h7) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")
h<-7
for(i in 1:52){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)

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
  pre <- forecast(f3, n.ahead=h)
  pr.rec.h7[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rec.h7[i,6] <- forecast4$forecast$temp$mean[h]
  
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
  forecast1<- forecast(f5, h=h)
  pr.rec.h7[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos recursivos y h=7

autoplot(ts.union(out.of.sample[7:58,2], pr.rec.h7[,1], pr.rec.h7[,2],pr.rec.h7[,3],pr.rec.h7[,4],pr.rec.h7[,5], pr.rec.h7[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"),
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5], colores[6])) +
  ggtitle("Pronósticos recursivos con h = 7") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto") +
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# ESQUEMA ROLLING 


data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rol.h7 <- ts(matrix(0, 52, 6), frequency = 365, start=c(2020,11))
colnames(pr.rol.h7) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR", "FAVAR")
h<-7
for(i in 1:52){
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)

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
  pre <- forecast(f3, n.ahead=h)
  pr.rol.h7[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rol.h7[i,6] <- forecast4$forecast$temp$mean[h]
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
  forecast1<- forecast(f5, h=h)
  pr.rol.h7[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
}


# Gráfico con los pronósticos rolling y h=7

autoplot(ts.union(out.of.sample[7:58,2], pr.rol.h7[,1], pr.rol.h7[,2],pr.rol.h7[,3],pr.rol.h7[,4],pr.rol.h7[,5], pr.rol.h7[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","ADL","VAR"),
                     values = c("#4b4b4b", colores[1], colores[2],colores[3], colores[4], colores[5], colores[6])) +
  ggtitle("Pronósticos rolling con h = 7") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


########### BAGGING ########################

# Realizamos bagging con los modelos arima, ets, var, adl con esquema fijo 

# h = 1

# ARIMA 

library(forecast)

library(quantmod)

set.seed(444)
for (i in 2:15) {
  a <- bld.mbb.bootstrap(data[,i], 1000) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("data.bag.",i,sep = ""),ts(a, frequency = 365, start = c(2019,12)))
}

for (i in 1:4) {
  b <- bld.mbb.bootstrap(PC[,i], 1000) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("PC.bag.",i,sep = ""),ts(b, frequency = 365, start = c(2019,12)))
}

########## NO CORRERRRRR
pr.f.h1.b <- matrix(nrow=58,ncol=5, NA)




########### CORRER A PARTIR DE ACAAA!!!!!!







h<-1
for(i in 1:58){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {
  if (j == 500){print(j)}
  temp <-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
  data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j])
  temp2 <- window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)

    # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = h)
  pr.arima[1,j] <- forecast$mean[h]
  
  # ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.arimax[1,j] <- forecast2$mean[h]
  
  # ETS 
  f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f1, n.ahead=h)
  pr.ets[1,j] <- pre$mean[h]
  

  } 
  
  pr.f.h1.b[i,1] <- mean(pr.arima)
  pr.f.h1.b[i,2] <- mean(pr.arimax)
  pr.f.h1.b[i,3] <- mean(pr.ets)
  print(i)
}

pr.f.h1.b <- ts(pr.f.h1.b, frequency = 365, start = c(2019,12))

# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)


h<-1

for(i in 1:58){
  for (j in 1:1000) {
  temp<-window(data.bag.2[-1,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
  data.var.bag <- cbind(data.bag.2[-1,j],data.bag.3[-1,j],diff(data.bag.4[,j]),data.bag.5[-1,j],data.bag.6[-1,j],data.bag.7[-1,j],diff(data.bag.8[,j]),data.bag.9[-1,j],data.bag.10[-1,j],data.bag.11[-1,j],data.bag.12[-1,j],data.bag.13[-1,j],data.bag.14[-1,j],data.bag.15[-1,j])
  temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
  f5 <- VAR(data.var.bag, p = 6, type = "trend")
  forecast1<- forecast(f5)
  pr.f.h1.b[i,5] <- forecast1$forecast$data1..1...c.1..5..8..16..17..18..19..20..21..22..23..24..25...sentsmooth$mean[h]
  }
}


# Realizamos los pronósticos para el FAVAR 

h<-1
for(i in 1:58){
    pr.favar <- matrix(nrow=1,ncol=50,NA)
  for (j in 1:50){
    if (j == 500){print(j)}
    temp <-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3[,1]), p = 6, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
  } 
  
  pr.f.h1.b[i,4] <- mean(pr.favar,na.rm = TRUE)
  print(i)
}

autoplot(ts.union(out.of.sample[,2], pr.f.h1.b[,1], pr.f.h1.b[,2], pr.f.h1.b[,3], pr.f.h1[,4], pr.f.h1[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS", "FAVAR, VAR"),
                     values = c("black", colores[1], colores[2], colores[3], colores[4])) +
  ggtitle("Pronósticos bagging h=1, esquema fijo") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))

# Exportamos los ponósticos 

write.csv(pr.f.h1.b, "pr.f.h1.b.csv", row.names = FALSE)

# h = 2 

pr.f.h2.b <- matrix(nrow=57,ncol=5, NA)

h<-2
for(i in 1:57){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <-  matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {
    if (j == 500){print(j)}
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],
                             data.bag.6[,j],data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],
                             data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                             data.bag.13[,j],data.bag.14[,j],data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model=arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = 6, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
  } 
  pr.f.h2.b[i,1] <- mean(pr.arima)
  pr.f.h2.b[i,2] <- mean(pr.arimax)
  pr.f.h2.b[i,3] <- mean(pr.ets)
  pr.f.h2.b[i,4] <- mean(pr.favar)
  print(i)
}

pr.f.h2.b <- ts(pr.f.h2.b, frequency = 365, start = c(2019,12))

# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

h<-2

for(i in 1:57){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000){
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],
                                    data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],
                                    data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                                    data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]),
                              start = c(2019,12), end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])),
                             start = c(2019,12), end = 2020.195 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,], var.bag.ex.diff), p = 6, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.f.h2.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}

# Realizamos el gráfico de los pronósticos 

autoplot(ts.union(out.of.sample[2:58,2], pr.f.h2.b[,1], pr.f.h2.b[,2], pr.f.h2.b[,3], pr.f.h2.b[,4], pr.f.h2.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS", "FAVAR", "VAR"),
                     values = c("black", colores[1], colores[2], colores[3], colores[4])) +
  ggtitle("Pronósticos baggind h=2, esquema fijo") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


write.csv(pr.f.h2.b, "pr.f.h2.b.csv", row.names = FALSE)


# h = 7 

pr.f.h7.b <- matrix(nrow=52,ncol=5, NA)

h<-7
for(i in 1:52){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],
                             data.bag.6[,j],data.bag.7[,j],data.bag.8[,j],
                             data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                             data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model=arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=auto.arima(temp),xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = 6, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  pr.f.h7.b[i,1] <- mean(pr.arima)
  pr.f.h7.b[i,2] <- mean(pr.arimax)
  pr.f.h7.b[i,3] <- mean(pr.ets)
  pr.f.h7.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}



# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)


h <- 7

for(i in 1:52){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],
                                    data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],
                                    data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                                    data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = c(2019,12), end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.195 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = 6, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.f.h7.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}
pr.f.h7.b <- ts(pr.f.h7.b, frequency = 365, start = c(2019,12))


# Realizamos el gráfico de los pronósticos 

autoplot(ts.union(out.of.sample[7:58,2], pr.f.h7.b[,1], pr.f.h7.b[,2], pr.f.h7.b[,3], pr.f.h7.b[,4], pr.f.h7.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","FAVAR", "VAR"),
                     values = c("black", colores[1], colores[2], colores[3], colores[4])) +
  ggtitle("Pronósticos bagging h=7, esquema fijo") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))

write.csv(pr.f.h7.b, "pr.f.h7.b.csv", row.names = FALSE)

# Realizamos bagging con los modelos arima, ets, var, adl con esquema recursivo 

# h = 1

# ARIMA 

library(forecast)
library(quantmod)

set.seed(444)
for (i in 2:15) {
  a <- bld.mbb.bootstrap(data[,i], 1000) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("data.bag.",i,sep = ""),ts(a, frequency = 365, start = c(2019,12)))
}
for (i in 1:4) {
  b <- bld.mbb.bootstrap(PC[,i], 1000) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("PC.bag.",i,sep = ""),ts(b, frequency = 365, start = c(2019,12)))
}



pr.rec.h1.b <- matrix(nrow=58,ncol=5, NA)

h<-1

for(i in 1:58){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {
    temp <-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <- window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],
                          PC.bag.4[,j]), start = c(2019,12), end = 2020.195 + (i-1)/365)
    # ARIMA 
    f1 <- auto.arima(temp)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=auto.arima(temp),xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    a <- VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
    f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rec.h1.b[i,1] <- mean(pr.arima)
  pr.rec.h1.b[i,2] <- mean(pr.arimax)
  pr.rec.h1.b[i,3] <- mean(pr.ets)
  pr.rec.h1.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

pr.rec.h1.b <- ts(pr.rec.h1.b, frequency = 365, start = c(2019,12))


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

h<-1

for(i in 1:58){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), start = c(2019,12), end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.195 + (i-1)/365)
    a <- VARselect(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), lag.max = 13, type = "const")$selection[1]
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = a, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rec.h1.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


write.csv(pr.rec.h1.b, "pr.rec.h1.b.csv", row.names = FALSE)


autoplot(ts.union(out.of.sample[,2], pr.rec.h1.b[,1], pr.rec.h1.b[,2], pr.rec.h1.b[,3], pr.rec.h1[,4], pr.rec.h1[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS", "FAVAR, VAR"),
                     values = c("black", colores[1], colores[2], colores[3], colores[4])) +
  ggtitle("Pronósticos bagging h=1, esquema recursivo") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))

# gráficos que comparen ets bagged contra ets 


# h = 2 

pr.rec.h2.b <- matrix(nrow=57,ncol=5, NA)

h<-2
for(i in 1:57){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <-  matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j],
                             data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                             data.bag.13[,j],data.bag.14[,j],data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    f1 <- auto.arima(temp)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    #ARIMAX
    f2 <- Arima(temp,model=auto.arima(temp),xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    #ETS 
    f1 <- ets(temp)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    a <- VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
    f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rec.h2.b[i,1] <- mean(pr.arima)
  pr.rec.h2.b[i,2] <- mean(pr.arimax)
  pr.rec.h2.b[i,3] <- mean(pr.ets)
  pr.rec.h2.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

pr.rec.h2.b <- ts(pr.rec.h2.b, frequency = 365, start = c(2019,12))


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

h<-2

for(i in 1:57){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j], 
                                    data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],
                                    data.bag.14[,j],data.bag.15[,j]), start = c(2019,12), end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.195 + (i-1)/365)
    a <- VARselect(cbind(data.var.bag.ex[-1,],var.bag.ex.diff),lag.max = 13, type = "trend")$selection[1]
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = a, type = "trend")
    pr.var[1,j]<- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rec.h2.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}

write.csv(pr.rec.h2.b, "pr.rec.h2.b.csv", row.names = FALSE)

# Realizamos el gráfico de los pronósticos 


autoplot(ts.union(out.of.sample[2:58,2], pr.rec.h2.b[,1], pr.rec.h2.b[,2], pr.rec.h2.b[,3], pr.rec.h2.b[,4], pr.rec.h2.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS", "FAVAR", "VAR"),
                     values = c("black", colores[1], colores[2], colores[3], colores[4])) +
  ggtitle("Pronósticos bagging h=2, esquema recursivo") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# h = 7 

pr.rec.h7.b <- matrix(nrow=52,ncol=5, NA)

h<-1
for(i in 1:52){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j],
                             data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                             data.bag.13[,j],data.bag.14[,j],data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]), 
                    start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    f1 <- auto.arima(temp)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    #ARIMAX
    f2 <- Arima(temp,model=auto.arima(temp),xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    #ETS 
    f1 <- ets(temp)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    a <- VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
    f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rec.h7.b[i,1] <- mean(pr.arima)
  pr.rec.h7.b[i,2] <- mean(pr.arimax)
  pr.rec.h7.b[i,3] <- mean(pr.ets)
  pr.rec.h7.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

pr.rec.h7.b <- ts(pr.rec.h7.b, frequency = 365, start = c(2019,12))


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)


h <- 7

for(i in 1:52){
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],
                                    data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                                    data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = c(2019,12), end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.195 + (i-1)/365)
    a <- VARselect(cbind(data.var.bag.ex[-1,], var.bag.ex.diff), 
                   lag.max = 13, type = "trend")$selection[1]
    f5 <- VAR(cbind(data.var.bag.ex[-1,], var.bag.ex.diff), p = a, type = "trend")
    pr.var[1,j]<- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rec.h7.b[i,5]
  print(paste("VAMOS:", i, "PERIODOS"))
}


write.csv(pr.rec.h7.b, "pr.rec.h7.b.csv", row.names = FALSE)

# Realizamos el gráfico de los pronósticos 

autoplot(ts.union(out.of.sample[7:58,2], pr.rec.h7.b[,1], pr.rec.h7.b[,2], pr.rec.h7.b[,3], pr.rec.h7.b[,4], pr.rec.h7.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","FAVAR", "VAR"),
                     values = c("black", colores[1], colores[2], colores[3], colores[4])) +
  ggtitle("Pronósticos bagging h=7, esquema recursivo") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))



# Realizamos bagging con los modelos arima, ets, var, adl con esquema rolling

# h = 1

# ARIMA 

library(forecast)
library(quantmod)

set.seed(444)

for (i in 2:15) {
  a <- bld.mbb.bootstrap(data[,i], 1000) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("data.bag.",i,sep = ""),ts(a, frequency = 365, start = c(2019,12)))
}

for (i in 1:4) {
  b <- bld.mbb.bootstrap(PC[,i], 1000) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("PC.bag.",i,sep = ""),ts(b, frequency = 365, start = c(2019,12)))
}

pr.rol.h1.b <- matrix(nrow=58,ncol=5, NA)

h<-1
for(i in 1:58){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  
  for (j in 1:1000) {
    temp <-window(data.bag.2[,j], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <- window(data.var.bag.ex, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    # ARIMA 
    f1 <- auto.arima(temp)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=auto.arima(temp),xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    a <- VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
    f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rol.h1.b[i,1] <- mean(pr.arima)
  pr.rol.h1.b[i,2] <- mean(pr.arimax)
  pr.rol.h1.b[i,3] <- mean(pr.ets)
  pr.rol.h1.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

pr.rol.h1.b <- ts(pr.rol.h1.b, frequency = 365, start = c(2019,12))


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)


h<-1

for(i in 1:58){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],
                                    data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],
                                    data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                                    data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    a <- VARselect(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), lag.max = 13, type = "const")$selection[1]
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = a, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rol.h1.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


write.csv(pr.rol.h1.b, "pr.rol.h1.b.csv", row.names = FALSE)

autoplot(ts.union(out.of.sample[,2], pr.rol.h1.b[,1], pr.rol.h1.b[,2], pr.rol.h1.b[,3], pr.rol.h1[,4], pr.rol.h1[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS", "FAVAR, VAR"),
                     values = c("black", colores[1], colores[2], colores[3], colores[4])) +
  ggtitle("Pronósticos bagging h=1, esquema rolling") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))

# gráficos que comparen ets bagged contra ets 


# h = 2 

pr.rol.h2.b <- matrix(nrow=57,ncol=5, NA)

h<-2
for(i in 1:57){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <-  matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {

        temp<-window(data.bag.2[,j], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],
                             data.bag.6[,j],data.bag.7[,j],data.bag.8[,j],
                             data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                             data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]), 
                    start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    f1 <- auto.arima(temp)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    #ARIMAX
    f2 <- Arima(temp,model=auto.arima(temp),xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    #ETS 
    f1 <- ets(temp)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    a <- VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
    f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rol.h2.b[i,1] <- mean(pr.arima)
  pr.rol.h2.b[i,2] <- mean(pr.arimax)
  pr.rol.h2.b[i,3] <- mean(pr.ets)
  pr.rol.h2.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

pr.rol.h2.b <- ts(pr.rol.h2.b, frequency = 365, start = c(2019,12))


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)

h<-2

for(i in 1:57){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000){
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],
                                    data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                                    data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    a <- VARselect(cbind(data.var.bag.ex[-1,],var.bag.ex.diff),lag.max = 13, type = "trend")$selection[1]
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = a, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rol.h2.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


write.csv(pr.rol.h2.b, "pr.rol.h2.b.csv", row.names = FALSE)


# Realizamos el gráfico de los pronósticos 


autoplot(ts.union(out.of.sample[2:58,2], pr.rol.h2.b[,1], pr.rol.h2.b[,2], pr.rol.h2.b[,3], pr.rol.h2.b[,4], pr.rol.h2.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS", "FAVAR", "VAR"),
                     values = c("black", colores[1], colores[2], colores[3], colores[4])) +
  ggtitle("Pronósticos bagging h=2, esquema rolling") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))


# h = 7 

for (i in 2:15) {
  a <- bld.mbb.bootstrap(data[,i], 1000) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("data.bag.",i,sep = ""),ts(a, frequency = 365, start = c(2019,12)))
}


pr.rol.h7.b <- matrix(nrow=52,ncol=5, NA)

h<-1
for(i in 1:52){
  pr.arima <- matrix(nrow=1,ncol=1000,NA)
  pr.arimax <- matrix(nrow=1,ncol=1000,NA)
  pr.ets <- matrix(nrow=1,ncol=1000,NA)
  pr.favar <- matrix(nrow=1,ncol=1000,NA)
  
  for (j in 1:1000) {
    temp<-window(data.bag.2[,j], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]), start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    f1 <- auto.arima(temp)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    #ARIMAX
    f2 <- Arima(temp,model=auto.arima(temp),xreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    #ETS 
    f1 <- ets(temp)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    a <- VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
    f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    
    print(j)
  } 
  
  pr.rol.h7.b[i,1] <- mean(pr.arima)
  pr.rol.h7.b[i,2] <- mean(pr.arimax)
  pr.rol.h7.b[i,3] <- mean(pr.ets)
  pr.rol.h7.b[i,4] <- mean(pr.favar, na.rm = TRUE)
  print(i)
}

pr.rol.h7.b <- ts(pr.rol.h7.b, frequency = 365, start = c(2019,12))


# Realizamos los pronósticos con el VAR 

# Construimos la serie diferenciando las variables que son I(1)


h <- 7

for(i in 1:52){
  pr.var <- matrix(nrow=1,ncol=1000,NA)
  for (j in 1:1000) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],
                                    data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                                    data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    a <- VARselect(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), lag.max = 13, type = "trend")$selection[1]
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = 6, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rol.h7.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}


write.csv(pr.rol.h7.b, "pr.rol.h7.b.csv", row.names = FALSE)


# Realizamos el gráfico de los pronósticos 

autoplot(ts.union(out.of.sample[7:58,2], pr.rol.h7.b[,1], pr.rol.h7.b[,2], pr.rol.h7.b[,3], pr.rol.h7.b[,4], pr.rol.h7.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ETS","FAVAR", "VAR"),
                     values = c("black", colores[1], colores[2], colores[3], colores[4])) +
  ggtitle("Pronósticos bagging h=7, esquema rolling") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))



### MEDIDAS DE ACCURACY 

AC<-matrix(NA,33,6)
colnames(AC) <- c("Modelo", "MAPE", "MAE", "RMSE", "Estadístico","P-valor")
AC2<-matrix(NA,33,1)


# TABLA 1

#medidas de accuracy y test de DM de los pronosticos con esquema fijo y h=1

AC[1,1]<-"ARIMA fijo" 
AC2[1,1]<-"1"
AC[1,2:4]<-round(accuracy(pr.f.h1[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[2,1]<-"ARIMAX fijo" 
AC2[2,1]<-"1"
AC[2,2:4]<-round(accuracy(pr.f.h1[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[3,1]<-"ETS fijo" 
AC2[3,1]<-"1"
AC[3,2:4]<-round(accuracy(pr.f.h1[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[4,1]<- "ADL fijo" 
AC2[4,1]<-"1"
AC[4,2:4]<-round(accuracy(pr.f.h1[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[5,1]<- "VAR fijo"
AC2[5,1]<-"1"
AC[5,2:4]<-round(accuracy(pr.f.h1[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[6,1]<- "FAVAR fijo" 
AC2[6,1]<-"1"
AC[6,2:4]<-round(accuracy(pr.f.h1[,6], out.of.sample[,2])[c(2:3,5)],4)

# medidas de accuracy de los pronósticos con esquema rolling y h=1

AC[7,1]<-"ARIMA rolling" 
AC2[7,1]<-"1"
AC[7,2:4]<-round(accuracy(pr.rol.h1[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[8,1]<-"ARIMAX rolling" 
AC2[8,1]<-"1"
AC[8,2:4]<-round(accuracy(pr.rol.h1[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[9,1]<-"ETS rolling" 
AC2[9,1]<-"1"
AC[9,2:4]<-round(accuracy(pr.rol.h1[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[10,1]<- "ADL rolling" 
AC2[10,1]<-"1"
AC[10,2:4]<-round(accuracy(pr.rol.h1[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[11,1]<- "VAR rolling"
AC2[11,1]<-"1"
AC[11,2:4]<-round(accuracy(pr.rol.h1[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[12,1]<- "FAVAR rolling" 
AC2[12,1]<-"1"
AC[12,2:4]<-round(accuracy(pr.rol.h1[,6], out.of.sample[,2])[c(2:3,5)],4)


#  medidas de accuracy de los pronósticos con esquema recursivo y h=1


AC[13,1]<-"ARIMA recursivo" 
AC2[13,1]<-"1"
AC[13,2:4]<-round(accuracy(pr.rec.h1[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[14,1]<-"ARIMAX recursivo" 
AC2[14,1]<-"1"
AC[14,2:4]<-round(accuracy(pr.rec.h1[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[15,1]<-"ETS recursivo" 
AC2[15,1]<-"1"
AC[15,2:4]<-round(accuracy(pr.rec.h1[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[16,1]<- "ADL recursivo" 
AC2[16,1]<-"1"
AC[16,2:4]<-round(accuracy(pr.rec.h1[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[17,1]<- "VAR recursivo"
AC2[17,1]<-"1"
AC[17,2:4]<-round(accuracy(pr.rec.h1[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[18,1]<- "FAVAR recursivo" 
AC2[18,1]<-"1"
AC[18,2:4]<-round(accuracy(pr.rec.h1[,6], out.of.sample[,2])[c(2:3,5)],4)


# medidas de accuracy de los pronósticos con esquema fijo, h = 1 y series boots


AC[19,1]<-"ARIMA fijo bagged" 
AC2[19,1]<-"1"
AC[19,2:4]<-round(accuracy(pr.f.h1.b[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[20,1]<-"ARIMAX fijo bagged" 
AC2[20,1]<-"1"
AC[20,2:4]<-round(accuracy(pr.f.h1.b[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[21,1]<-"ETS fijo bagged" 
AC2[21,1]<-"1"
AC[21,2:4]<-round(accuracy(pr.f.h1.b[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[22,1]<- "VAR fijo bagged" 
AC2[22,1]<-"1"
AC[22,2:4]<-round(accuracy(pr.f.h1.b[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[23,1]<- "FAVAR fijo bagged"
AC2[23,1]<-"1"
AC[23,2:4]<-round(accuracy(pr.rec.h1.b[,5], out.of.sample[,2])[c(2:3,5)],4)

# medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots

AC[24,1]<-"ARIMA recursivo bagged" 
AC2[24,1]<-"1"
AC[24,2:4]<-round(accuracy(pr.rec.h1.b[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[25,1]<-"ARIMAX recursivo bagged" 
AC2[25,1]<-"1"
AC[25,2:4]<-round(accuracy(pr.rec.h1.b[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[26,1]<-"ETS recursivo bagged" 
AC2[26,1]<-"1"
AC[26,2:4]<-round(accuracy(pr.rec.h1.b[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[27,1]<- "VAR recursivo bagged" 
AC2[27,1]<-"1"
AC[27,2:4]<-round(accuracy(pr.rec.h1.b[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[28,1]<- "FAVAR recursivo bagged"
AC2[28,1]<-"1"
AC[28,2:4]<-round(accuracy(pr.rec.h1.b[,5], out.of.sample[,2])[c(2:3,5)],4)

# medidas de accuracy de los pronósticos con esquema rolling, h = 1 y series boots


AC[29,1]<-"ARIMA rolling bagged" 
AC2[29,1]<-"1"
AC[29,2:4]<-round(accuracy(pr.rol.h1.b[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[30,1]<-"ARIMAX rolling bagged" 
AC2[30,1]<-"1"
AC[30,2:4]<-round(accuracy(pr.rol.h1.b[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[31,1]<-"ETS rollingbagged" 
AC2[31,1]<-"1"
AC[31,2:4]<-round(accuracy(pr.rol.h1.b[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[32,1]<- "VAR rolling bagged" 
AC2[32,1]<-"1"
AC[32,2:4]<-round(accuracy(pr.rol.h1.b[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[33,1]<- "FAVAR rolling bagged"
AC2[33,1]<-"1"
AC[33,2:4]<-round(accuracy(pr.rol.h1.b[,5], out.of.sample[,2])[c(2:3,5)],4)


# TABLA 2 

#medidas de accuracy y test de DM de los pronosticos con esquema fijo y h=2

AC1<-matrix(NA,33,6)
colnames(AC) <- c("Modelo", "MAPE", "MAE", "RMSE", "Estadístico","P-valor")
AC3<-matrix(NA,33,1)

AC1[1,1]<-"ARIMA fijo" 
AC3[1,1]<-"2"
AC1[1,2:4]<-round(accuracy(pr.f.h2[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[2,1]<-"ARIMAX fijo" 
AC3[2,1]<-"2"
AC1[2,2:4]<-round(accuracy(pr.f.h2[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[3,1]<-"ETS fijo" 
AC3[3,1]<-"2"
AC1[3,2:4]<-round(accuracy(pr.f.h2[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[4,1]<- "ADL fijo" 
AC3[4,1]<-"2"
AC1[4,2:4]<-round(accuracy(pr.f.h2[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[5,1]<- "VAR fijo"
AC3[5,1]<-"2"
AC1[5,2:4]<-round(accuracy(pr.f.h2[,5], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[6,1]<- "FAVAR fijo" 
AC3[6,1]<-"2"
AC1[6,2:4]<-round(accuracy(pr.f.h2[,6], out.of.sample[2:58,2])[c(2:3,5)],4)

# medidas de accuracy de los pronósticos con esquema rolling y h=2

AC1[7,1]<-"ARIMA rolling" 
AC3[7,1]<-"2"
AC1[7,2:4]<-round(accuracy(pr.rol.h2[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[8,1]<-"ARIMAX rolling" 
AC3[8,1]<-"2"
AC1[8,2:4]<-round(accuracy(pr.rol.h2[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[9,1]<-"ETS rolling" 
AC3[9,1]<-"2"
AC1[9,2:4]<-round(accuracy(pr.rol.h2[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[10,1]<- "ADL rolling" 
AC3[10,1]<-"2"
AC1[10,2:4]<-round(accuracy(pr.rol.h2[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[11,1]<- "VAR rolling"
AC3[11,1]<-"2"
AC1[11,2:4]<-round(accuracy(pr.rol.h2[,5], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[12,1]<- "FAVAR rolling" 
AC3[12,1]<-"2"
AC1[12,2:4]<-round(accuracy(pr.rol.h2[,6], out.of.sample[2:58,2])[c(2:3,5)],4)


#  medidas de accuracy de los pronósticos con esquema recursivo y h=2


AC1[13,1]<-"ARIMA recursivo" 
AC3[13,1]<-"2"
AC1[13,2:4]<-round(accuracy(pr.rec.h2[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[14,1]<-"ARIMAX recursivo" 
AC3[14,1]<-"2"
AC1[14,2:4]<-round(accuracy(pr.rec.h2[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[15,1]<-"ETS recursivo" 
AC3[15,1]<-"2"
AC1[15,2:4]<-round(accuracy(pr.rec.h2[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[16,1]<- "ADL recursivo" 
AC3[16,1]<-"2"
AC1[16,2:4]<-round(accuracy(pr.rec.h2[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[17,1]<- "VAR recursivo"
AC3[17,1]<-"2"
AC1[17,2:4]<-round(accuracy(pr.rec.h2[,5], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[18,1]<- "FAVAR recursivo" 
AC3[18,1]<-"2"
AC1[18,2:4]<-round(accuracy(pr.rec.h2[,6], out.of.sample[2:58,2])[c(2:3,5)],4)


# medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots


AC1[19,1]<-"ARIMA fijo bagged" 
AC3[19,1]<-"2"
AC1[19,2:4]<-round(accuracy(pr.f.h2.b[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[20,1]<-"ARIMAX fijo bagged" 
AC3[20,1]<-"2"
AC1[20,2:4]<-round(accuracy(pr.f.h2.b[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[21,1]<-"ETS fijo bagged" 
AC3[21,1]<-"2"
AC1[21,2:4]<-round(accuracy(pr.f.h2.b[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[22,1]<- "VAR fijo bagged" 
AC3[22,1]<-"2"
AC1[22,2:4]<-round(accuracy(pr.f.h2.b[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[23,1]<- "FAVAR fijo bagged"
AC3[23,1]<-"2"
AC1[23,2:4]<-round(accuracy(pr.rec.h2.b[,5], out.of.sample[2:58,2])[c(2:3,5)],4)

# medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots

AC1[24,1]<-"ARIMA recursivo bagged" 
AC3[24,1]<-"2"
AC1[24,2:4]<-round(accuracy(pr.rec.h2.b[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[25,1]<-"ARIMAX recursivo bagged" 
AC3[25,1]<-"2"
AC1[25,2:4]<-round(accuracy(pr.rec.h2.b[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[26,1]<-"ETS recursivo bagged" 
AC3[26,1]<-"2"
AC1[26,2:4]<-round(accuracy(pr.rec.h2.b[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[27,1]<- "VAR recursivo bagged" 
AC3[27,1]<-"2"
AC1[27,2:4]<-round(accuracy(pr.rec.h2.b[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[28,1]<- "FAVAR recursivo bagged"
AC3[28,1]<-"2"
AC1[28,2:4]<-round(accuracy(pr.rec.h2.b[,5], out.of.sample[2:58,2])[c(2:3,5)],4)

# medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots


AC1[29,1]<-"ARIMA rolling bagged" 
AC3[29,1]<-"2"
AC1[29,2:4]<-round(accuracy(pr.rol.h2.b[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[30,1]<-"ARIMAX rolling bagged" 
AC3[30,1]<-"2"
AC1[30,2:4]<-round(accuracy(pr.rol.h2.b[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[31,1]<-"ETS rollingbagged" 
AC3[31,1]<-"2"
AC1[31,2:4]<-round(accuracy(pr.rol.h2.b[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[32,1]<- "VAR rolling bagged" 
AC3[32,1]<-"2"
AC1[32,2:4]<-round(accuracy(pr.rol.h2.b[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[33,1]<- "FAVAR rolling bagged"
AC3[33,1]<-"2"
AC1[33,2:4]<-round(accuracy(pr.rol.h2.b[,5], out.of.sample[2:58,2])[c(2:3,5)],4)



# TABLA 3

#medidas de accuracy y test de DM de los pronosticos con esquema fijo y h=7

AC4<-matrix(NA,33,6)
colnames(AC) <- c("Modelo", "MAPE", "MAE", "RMSE", "Estadístico","P-valor")
AC5<-matrix(NA,33,1)

AC4[1,1]<-"ARIMA fijo" 
AC5[1,1]<-"2"
AC4[1,2:4]<-round(accuracy(pr.f.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[2,1]<-"ARIMAX fijo" 
AC5[2,1]<-"2"
AC4[2,2:4]<-round(accuracy(pr.f.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[3,1]<-"ETS fijo" 
AC5[3,1]<-"2"
AC4[3,2:4]<-round(accuracy(pr.f.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[4,1]<- "ADL fijo" 
AC5[4,1]<-"2"
AC4[4,2:4]<-round(accuracy(pr.f.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[5,1]<- "VAR fijo"
AC5[5,1]<-"2"
AC4[5,2:4]<-round(accuracy(pr.f.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[6,1]<- "FAVAR fijo" 
AC5[6,1]<-"2"
AC4[6,2:4]<-round(accuracy(pr.f.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# medidas de accuracy de los pronósticos con esquema rolling y h=2

AC4[7,1]<-"ARIMA rolling" 
AC5[7,1]<-"2"
AC4[7,2:4]<-round(accuracy(pr.rol.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[8,1]<-"ARIMAX rolling" 
AC5[8,1]<-"2"
AC4[8,2:4]<-round(accuracy(pr.rol.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[9,1]<-"ETS rolling" 
AC5[9,1]<-"2"
AC4[9,2:4]<-round(accuracy(pr.rol.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[10,1]<- "ADL rolling" 
AC5[10,1]<-"2"
AC4[10,2:4]<-round(accuracy(pr.rol.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[11,1]<- "VAR rolling"
AC5[11,1]<-"2"
AC4[11,2:4]<-round(accuracy(pr.rol.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[12,1]<- "FAVAR rolling" 
AC5[12,1]<-"2"
AC4[12,2:4]<-round(accuracy(pr.rol.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)


#  medidas de accuracy de los pronósticos con esquema recursivo y h=2


AC4[13,1]<-"ARIMA recursivo" 
AC5[13,1]<-"2"
AC4[13,2:4]<-round(accuracy(pr.rec.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[14,1]<-"ARIMAX recursivo" 
AC5[14,1]<-"2"
AC4[14,2:4]<-round(accuracy(pr.rec.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[15,1]<-"ETS recursivo" 
AC5[15,1]<-"2"
AC4[15,2:4]<-round(accuracy(pr.rec.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[16,1]<- "ADL recursivo" 
AC5[16,1]<-"2"
AC4[16,2:4]<-round(accuracy(pr.rec.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[17,1]<- "VAR recursivo"
AC5[17,1]<-"2"
AC4[17,2:4]<-round(accuracy(pr.rec.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[18,1]<- "FAVAR recursivo" 
AC5[18,1]<-"2"
AC4[18,2:4]<-round(accuracy(pr.rec.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)


# medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots


AC4[19,1]<-"ARIMA fijo bagged" 
AC5[19,1]<-"2"
AC4[19,2:4]<-round(accuracy(pr.f.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[20,1]<-"ARIMAX fijo bagged" 
AC5[20,1]<-"2"
AC4[20,2:4]<-round(accuracy(pr.f.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[21,1]<-"ETS fijo bagged" 
AC5[21,1]<-"2"
AC4[21,2:4]<-round(accuracy(pr.f.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[22,1]<- "VAR fijo bagged" 
AC5[22,1]<-"2"
AC4[22,2:4]<-round(accuracy(pr.f.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[23,1]<- "FAVAR fijo bagged"
AC5[23,1]<-"2"
AC4[23,2:4]<-round(accuracy(pr.rec.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots

AC4[24,1]<-"ARIMA recursivo bagged" 
AC5[24,1]<-"2"
AC4[24,2:4]<-round(accuracy(pr.rec.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[25,1]<-"ARIMAX recursivo bagged" 
AC5[25,1]<-"2"
AC4[25,2:4]<-round(accuracy(pr.rec.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[26,1]<-"ETS recursivo bagged" 
AC5[26,1]<-"2"
AC4[26,2:4]<-round(accuracy(pr.rec.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[27,1]<- "VAR recursivo bagged" 
AC5[27,1]<-"2"
AC4[27,2:4]<-round(accuracy(pr.rec.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[28,1]<- "FAVAR recursivo bagged"
AC5[28,1]<-"2"
AC4[28,2:4]<-round(accuracy(pr.rec.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots


AC4[29,1]<-"ARIMA rolling bagged" 
AC5[29,1]<-"2"
AC4[29,2:4]<-round(accuracy(pr.rol.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[30,1]<-"ARIMAX rolling bagged" 
AC5[30,1]<-"2"
AC4[30,2:4]<-round(accuracy(pr.rol.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[31,1]<-"ETS rollingbagged" 
AC5[31,1]<-"2"
AC4[31,2:4]<-round(accuracy(pr.rol.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[32,1]<- "VAR rolling bagged" 
AC5[32,1]<-"2"
AC4[32,2:4]<-round(accuracy(pr.rol.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[33,1]<- "FAVAR rolling bagged"
AC5[33,1]<-"2"
AC4[33,2:4]<-round(accuracy(pr.rol.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)








