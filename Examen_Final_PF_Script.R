## Importamos algunas de las librer?as que vamos a usar.

library(xts)
library(ggplot2)
library(devtools)
library(ggfortify)
library(dplyr)

## Seteamos el directorio. Prueba.``

dir <- "G:\\Mi unidad\\UdeSA\\Pronósticos\\Final\\Data\\dep"
dir <- "C:\\Users\\Abi\\Downloads"

setwd(dir)

# Definimos la paleta de colores.

colores <- c("#00ABC5", "#f7941c")

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




# Borramos las variables 'nominales' del resto del mundo

data <- data[, -c(15,16)]

# Ahora generamos un objeto de series de tiempo para cada una de las variables.

for (i in colnames(data)[-(1:2)]){
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

# Grafico mercado sintetico

autoplot(sent_trends, ts.colour = colores[2]) + 
  ggtitle("Evolución de nuestro 'synthetic market'") + 
  xlab("Tiempo") + 
  ylab("Sentimiento") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


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
for (i in colnames(data)[-(1:2)]){
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
for (i in colnames(data)[-(1:2)]){
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
for (i in colnames(data)[-(1:2)]){
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

arima.1 <- auto.arima(in.sample[,3])

summary(arima.1)

arima.to.table <- arima(in.sample[,3],order = c(5,0,1))

# Validaremos el modelo chequeando que los residuos sean ruido blanco.

act1 <- checkresiduals(arima.1, lag = 13)

test <- round(act1$p.value, 2)

# Como H0 es ausencia de autocorrelación y no rechazamos la hipótesis
# podemos decir que los residuos no están correlacionados.
# Modelo valido, pval = 0.1405

# Estimamos un modelo ETS


ets.1 <- ets(in.sample[,3])

# Verificamos que los residuos sean RB.

act2 <- Box.test(ets.1$residuals, lag = 13, type = c("Ljung-Box"))

test2 <- round(act2$p.value, 2)

# Como H0 es ausencia de autocorrelación y rechazamos la hipótesis
# podemos decir que los residuos están correlacionados.
# Modelo no valido, pval = 0.0000

# Estimamos un modelo ARIMAX 

# Usaremos el sentimiento de Alberto Fernandez 
# como variable explicada e introduciremos al resto de nuestras variables
# como regresores exogenos

# Utilizamos el mismo modelo ARIMAX seleccionado previamente.


arimax.1 <- Arima(in.sample[,3], order = c(5,0,1),
                     xreg = as.matrix(in.sample[,4:length(in.sample)]))

arimax.to.table <- arima(in.sample[,3],order = c(5,0,1),
                         xreg = as.matrix(in.sample[,4:length(in.sample)]))


act3 <- Box.test(arimax.1$residuals, lag = 13, type = c("Ljung-Box"))

test3 <- round(act3$p.value, 2)

# El p valor nos da 0.6, con lo cual validamos el modelo 

# PREGUNTA M: ¿Tenemos que diferenciar las reservas? 




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

data.in.sample.dum <- data_1[1:425,-c(1,2,15,30)]

# Seleccionamos el orden del ADL 

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + casosarg + muertosarg +
                            vacunasarg + maxtemp + mintemp + sent_trends + muertes.arg.rel + casos.arg.rel |
                            mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                            mes07 + mes08 + mes09 +  mes10 + mes11, 
                          data = data.in.sample.dum, max_order = 5)

# Ahora estimamos el modelo:

adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                 casosarg + muertosarg + vacunasarg + maxtemp + 
                 mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
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
  Test.cointEG[,1] <- colnames(data)[4:16]
  j <- egcm(in.sample[,3], in.sample[,3+i], urtest = 'adf', i1test = 'adf')
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

reservas.est <- diff(in.sample[,6])

dolar.est <- diff(in.sample[,9])

sentiment.trend.est <-diff(in.sample[,15])

in.sample.d <- cbind(in.sample[-1,-c(6,9,15)],reservas.est, dolar.est, sentiment.trend.est)

#[,3_17]

in.sample.d <- in.sample.d[,3:17]

var.d <- VARselect(in.sample.d, type ="const", season = 12)

var.d$selection


# El orden de rezagos óptimos, de acuerdo al criterio de FPE, es 1.
# Estimamos:

var.dl <- VAR(in.sample.d, p = 6, type = "const", season = 12)

# Par

serial.test(var.dl) 


# Rechazamos la hipótesis nula de que los residuos están
# incorrelacionados. Por lo tanto, no podemos validar el modelo. 


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

PCA1 <- prcomp(in.sample[,4:16], scale =TRUE) 

# Veremos los autovalores para evaluar qué cantidad de componentes utilizaremos

PCA1$sdev^2

# Como hay 6 componentes cuyo autovalor es superior a 1, los usaremos.

PC <- scale(in.sample[,4:16])%*%PCA1$rotation

# Ahora estimamos el modelo FAVAR.

favar.data <- cbind(in.sample[,3], PC[,1:4])

VARselect(favar.data, lag.max = 10, type = c("both"), season = 4)

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

data1<-ts(data, frequency = 365, start = c(2019,12))

pr.f.h1 <- ts(matrix(0, 58, 4), frequency = 365, start=c(2020,11))
colnames(pr.f.h1) <- c("ARIMA", "ARIMAX", "ADL", "ETS")
h<-1
for(i in 1:58){
  temp<-window(data1[,3], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,4:17], start = c(2019,12), end = 2020.195 + (i-1)/365)
  
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

data_dum.1<-data_1.1[1:425,-c(1,2,15,30)]

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
                            mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                            mes07 + mes08 + mes09 +  mes10 + mes11, 
                          data = data_dum.1, max_order = 8)


h<-1

count <- 2
for(i in 1:58){
  temp2 <-window(data_1.1[,-c(1,2,15,30)],start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "sent_trends", "muertes.arg.rel", "casos.arg.rel",     
                       "mes01", "mes02", "mes03", "mes04", "mes05", "mes06",
                       "mes07", "mes08", "mes09", "mes10", "mes11")
          
#el orden del ADL es el conseguido al aplicar el modelo al período in sample 
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + sent_trends + muertes.arg.rel + casos.arg.rel|
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




# ESQUEMA RECURSIVO  (recursivo)

h<-1
count <- 2
for(i in 1:58){
  temp2 <-window(data_1.1[,-c(1,2,30)],start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "sentiment_trend_orig",
                       "sent_trends", "muertes.arg.rel", "casos.arg.rel",     
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
  pr.adl[i,1] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# Grafico 

autoplot(ts.union(out.of.sample[,3], pr.f.h1[,1], pr.f.h1[,2], pr.adl), size = 1.3) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX","ADL rec"),
                     values = c("black", "sienna2", "palegreen3","#00AFBB")) +
  ggtitle("Pronósticos fijos con h = 1") + 
  xlab("Tiempo") + ylab("Sentimiento Alberto")+
  theme_minimal() +  
  theme(legend.position = c(0.1,0.80), plot.title = element_text(hjust = 0.5))




