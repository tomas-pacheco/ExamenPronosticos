
# Importamos algunas de las librerías que vamos a usar.

library(devtools)

options(scipen=999)

# Seteamos el directorio.

dir <- "G:\\Mi unidad\\UdeSA\\Pronósticos\\Final\\Data\\dep"
dir <- "C:\\Users\\Abi\\Downloads"

setwd(dir)

# Definimos la paleta de colores que vamos a usar.

colores <- c("#00ABC5","#cfb0b4" ,"#ff3c84","#FF7F32", "#edf71c", "#941cf7")

colores2 <- c("#f5fa7b", "#3cffb7", "#ffdaaa", "#fa66f3",
              "#f84a42","#9e9d9c", "#c3f842", "#3cff56")

nuestra.paleta <- function(){
  set.seed(444)
  plot(rnorm(100,0,1), rnorm(100,0,1)+1)
  abline(h = 4, col = colores[1], lwd = 5)
  abline(h = 3, col = colores[2], lwd = 5)
  abline(h = 2, col = colores[3], lwd = 5)
  abline(h = 1, col = colores[4], lwd = 5)
  abline(h = 0, col = colores[5], lwd = 5)
  abline(h = -1, col = colores[6], lwd = 5)
}

# Abrimos la base de datos.

data <- read.csv("Data_Final_PF2.csv")
data <- data[,-2]
data <- na.omit(data)
rownames(data) <- data$time
colnames(data)
data <- data[,-c(12,13,14,18,19,20,21,22,23)]

setwd("C:\\Users\\Tomas\\Desktop\\Pronosticos_Final\\Script\\Graficos")

# Construimos las nuevas variables.

# Las muertes en Argentina respecto de las muertes en el resto del mundo. 

data$muertes.arg.rel <- data$muertosarg/data$muertesmundo

# Ponemos cero donde hay NA (porque no había pandemia).

data$muertes.arg.rel<-ifelse(is.na(data$muertes.arg.rel),0,data$muertes.arg.rel)

# Los casos en Argentina respecto de los casos en el resto del mundo.

data$casos.arg.rel <- data$casosarg/data$casosmundo

# Ponemos cero donde hay NA.

data$casos.arg.rel<-ifelse(is.na(data$casos.arg.rel),0,data$casos.arg.rel)

# Borramos las variables 'nominales' del resto del mundo, el sentimiento de Alberto Fernandez sin 
# el suavizado, y las variables del sentimiento de mercado de Twitter. 

data <- data[, -c(2,15,16,17,18)]

# Ahora generamos un objeto de series de tiempo para cada una de las variables.

library(xts)

for (i in colnames(data)[-(1)]){
    assign(i, xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d")) )
}

# Graficamos nuestra serie de interés.

library(ggplot2)
library(ggfortify)
library(forecast)

autoplot(sentsmooth, ts.colour = colores[1]) + 
  ggtitle("Evolución del sentimiento de Alberto Fernández") + 
  xlab("Tiempo") + 
  ylab("Sentimiento") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Tiempo", 
       y = "Sentimiento", 
       title = "Evolución del sentimiento de Alberto Fernández",
       caption = "Fuente: elaboración propia")

ggsave(file="Sentimiento_AF.eps", width=6.5, height=4, dpi=300)

# Graficamos las demás variables que consideramos relevantes. 

autoplot(twfav, ts.colour = colores[2]) + 
  ggtitle("Evolución de los favoritos que recibe AF en Twitter") + 
  xlab("Tiempo") + 
  ylab("Cantidad de favoritos") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia") 

ggsave(file="FAVs.eps", width=6.5, height=4, dpi=300)

autoplot(twret, ts.colour = colores[3]) + 
  ggtitle("Evolución de los retweets que recibe AF en Twitter") + 
  xlab("Tiempo") + 
  ylab("Cantidad de retweets") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia") 

ggsave(file="RTs.eps", width=6.5, height=4, dpi=300)


autoplot(reservasbcra, ts.colour = colores2[7]) + 
  ggtitle("Evolución de las reservas del BCRA") + 
  xlab("Tiempo") + 
  ylab("Miles de millones de dólares") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del BCRA") 

ggsave(file="ReservasBCRA.eps", width=6.5, height=4, dpi=300)

autoplot(tasaint, ts.colour = colores[4]) + 
  ggtitle("Evolución de la tasa de interés") + 
  xlab("Tiempo") + 
  ylab("Tasa") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del BCRA") 

ggsave(file="TasaInt.eps", width=6.5, height=4, dpi=300)

autoplot(basemon, ts.colour = colores[5]) + 
  ggtitle("Evolución de la base monetaria") + 
  xlab("Tiempo") + 
  ylab("Miles de millones de pesos") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del BCRA") 

ggsave(file="BaseMon.eps", width=6.5, height=4, dpi=300)

autoplot(tcdolar, ts.colour = colores2[8]) + 
  ggtitle("Evolución del tipo de cambio") + 
  xlab("Tiempo") + 
  ylab("Pesos por dólar") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos de Yahoo Finance")

ggsave(file="Dolar.eps", width=6.5, height=4, dpi=300)

autoplot(casosarg[51:483], ts.colour = colores[6]) + 
  ggtitle("Evolución de los de casos de COVID-19 en Argentina") + 
  xlab("Tiempo") + 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud") 

ggsave(file="CasosArg.eps", width=6.5, height=4, dpi=300)

autoplot(muertosarg[51:483], ts.colour = colores2[1]) + 
  ggtitle("Evolución de las de muertes por COVID-19 en Argentina") + 
  xlab("Tiempo") + 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud") 

ggsave(file="MuertesArg.eps", width=6.5, height=4, dpi=300)


autoplot(vacunasarg[51:483], ts.colour = colores2[2]) + 
  ggtitle("Evolución de la cantidad de vacunas aplicadas en Argentina") + 
  xlab("Tiempo") + 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud") 

ggsave(file="Vacunas.eps", width=6.5, height=4, dpi=300)

autoplot(maxtemp, ts.colour = colores2[3]) + 
  ggtitle("Temperatura máxima en Buenos Aires") + 
  xlab("Tiempo") + 
  ylab("Grados centígrados") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos de World Weather Online") 

ggsave(file="TempMax.eps", width=6.5, height=4, dpi=300)

autoplot(mintemp, ts.colour = colores2[4]) + 
  ggtitle("Temperatura mínima en Buenos Aires") + 
  xlab("Tiempo") + 
  ylab("Grados centígrados") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos de World Weather Online")

ggsave(file="TempMin.eps", width=6.5, height=4, dpi=300)

autoplot(muertes.arg.rel[51:483], ts.colour = colores2[5]) + 
  ggtitle("Muertes por COVID-19 en Arg. relativas al resto del mundo") + 
  xlab("Tiempo") + 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud y Our World in Data") 

ggsave(file="MuertesRel.eps", width=6.5, height=4, dpi=300)

autoplot(casos.arg.rel[51:483], ts.colour = colores2[6]) + 
  ggtitle("Casos de COVID-19 en Arg. relativos al resto del mundo)")+ 
  xlab("Tiempo")+ 
  ylab("Cantidad") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) + 
  labs(caption = "Fuente: elaboración propia con datos del Ministerio de Salud y Our World in Data") 

ggsave(file="CasosRel.eps", width=6.5, height=4, dpi=300)


# Creamos una función que devuelve el estadístico con su significatividad. 

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

# Hacemos un loop para calcular los test de raíz unitaria para cada una de las variables.

library(urca)
library(stargazer)

results <- matrix(nrow = 16,ncol = 4, NA)
r <- 1
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
  r <- r+1
}
colnames(results) <- c("Variable", "None","Trend","Drift")

# Agregamos a mano aquellas variables que tenemos que diferenciar.
r <- 15
for (i in c("reservasbcra", "tcdolar")){
  results[r,1] <- paste("L1", i, sep = " ")
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- diff(series)
  series <- na.omit(series)
  test <- ur.df(series, type = c("none"))
  results[r,2] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.df(series, type = c("trend"))
  results[r,3] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.df(series, type = c("drift"))
  results[r,4] <- stars(test@teststat[1], test@cval[1,])
  r <- r+1
}
# Exportamos la tabla.

stargazer(results , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

# Hacemos el test de Phillips-Perron.

results1 <- matrix(nrow = 16,ncol = 3, NA)
r <- 1
for (i in colnames(data)[-(1)]){
  results1[r,1] <- i
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- na.omit(series)
  test <- ur.pp(series, type = c("Z-tau"), model=c("constant"), lags=c("long"))
  results1[r,2] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.pp(series, type = c("Z-tau"), model=c("trend"), lags=c("long"))
  results1[r,3] <- stars(test@teststat[1], test@cval[1,])
  r <- r+1
}
colnames(results1) <- c("Variable", "Constant", "Trend")

# Diferenciamos y agregamos a nuestra tabla.

r <- 15
for (i in c("reservasbcra", "tcdolar")){
  results1[r,1] <- paste("L1", i, sep = " ")
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- diff(series)
  series <- na.omit(series)
  test <- ur.pp(series, type = c("Z-tau"), model=c("constant"), lags=c("long"))
  results1[r,2] <- stars(test@teststat[1], test@cval[1,])
  test <- ur.pp(series, type = c("Z-tau"), model=c("trend"), lags=c("long"))
  results1[r,3] <- stars(test@teststat[1], test@cval[1,])
  r <- r+1
}
# Exportamos la tabla.

stargazer(results1 , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

# Por último, hacemos el test KPSS.

results2 <- matrix(nrow = 14,ncol = 3, NA)
r <- 1
for (i in colnames(data)[-(1)]){
  results2[r,1] <- i
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- na.omit(series)
  test <- ur.kpss(series, type = c("mu"), lags=c("short"))
  results2[r,2] <- stars2(test@teststat[1], test@cval[1,])
  test <- ur.kpss(series, type = c("tau"), lags=c("short"))
  results2[r,3] <- stars2(test@teststat[1], test@cval[1,])
  r <- r+1
}
colnames(results2) <- c("Variable", "Constant", "Trend")

stargazer(results2 , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

# Definimos la ventana in-sample y out-of-sample.

in.sample <- data[1:425,]

out.of.sample <- data[426:483,]

## Estimación de los modelos ##

# Estimamos un modelo ARIMA. 

arima.1 <- auto.arima(in.sample[,2])

summary(arima.1)

arima.to.table <- arima(in.sample[,2],order = c(5,0,1))


# Validaremos el modelo chequeando que los residuos sean ruido blanco.

act1 <- checkresiduals(arima.1, lag = 13)

test <- round(act1$p.value, 2)

# Como H0 es ausencia de autocorrelación y no rechazamos la hipótesis
# podemos decir que los residuos no están correlacionados.
# Modelo valido, pval = 0.1405

# Estimamos un modelo ETS.

ets.1 <- ets(in.sample[,2])

# Verificamos que los residuos sean RB.

act2 <- Box.test(ets.1$residuals, lag = 13, type = c("Ljung-Box"))

test2 <- round(act2$p.value, 2)

# Como H0 es ausencia de autocorrelación y rechazamos la hipótesis
# podemos decir que los residuos están correlacionados.
# Modelo no valido, pval = 0.000004111937.

# Estimamos un modelo ARIMAX 

# Usaremos el sentimiento de Alberto Fernández. 
# como variable explicada e introduciremos al resto de nuestras variables
# como regresores exógenos.

# Utilizamos el mismo modelo ARIMA seleccionado previamente.

arimax.1 <- Arima(in.sample[,2], order = c(5,0,1),
                     xreg = as.matrix(in.sample[,3:length(in.sample)]))

arimax.to.table <- arima(in.sample[,2],order = c(5,0,1),
                         xreg = as.matrix(in.sample[,4:length(in.sample)]))

act3 <- Box.test(arimax.1$residuals, lag = 13, type = c("Ljung-Box"))

test3 <- round(act3$p.value, 2)

# El p-valor nos da 0.6359, con lo cual validamos el modelo.

# Estimamos un modelo ADL. 

library(ARDL)
library(dynlm)

# Creamos las dummies. 

# Seleccionamos el orden del ADL. 

order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + casosarg + muertosarg +
                            vacunasarg + maxtemp + mintemp + muertes.arg.rel + casos.arg.rel, 
                          data = in.sample[,-1], max_order = 5)


# Ahora estimamos el modelo.

adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                 casosarg + muertosarg + vacunasarg + maxtemp + 
                 mintemp + muertes.arg.rel + casos.arg.rel, 
               data = in.sample[,-1], order = as.vector(order.adl.dl$best_order))

# Estimamos el ADL pero con la función dynlm.

adl <- adl.dl$full_formula
adl.1 <- dynlm(adl, data = adl.dl$data)

# Comprobamos que tiene los mismos coeficientes.

identical(adl.dl$coefficients, adl.1$coefficients)

# Test.

act4 <- Box.test(adl.1$residuals, lag = 13, type = c("Ljung-Box"))
test4 <- round(act4$p.value, 4)

# El p-valor nos da 0.0083.

model <- lm(rnorm(100,0,1) ~ rnorm(100,20,3))

stargazer(arima.to.table, arimax.to.table, model, adl.1,
          align = TRUE, 
          dep.var.labels = c("SentIndex", "SentIndex", "SentIndex", "SentIndex"),
          keep.stat = c("n", "ll", "rsq"),
          no.space = TRUE,
          type = "latex")

# Continuamos estimando un modelo VAR.

library(vars)

# Nos quedamos con el período in-sample que tiene todas las variables estacionarias.

reservas.est <- diff(in.sample[,5])

dolar.est <- diff(in.sample[,8])

in.sample.d <- cbind(in.sample[-1,-c(5,8)],reservas.est, dolar.est)

in.sample.d <- in.sample.d[,2:15]

# Elegimos el orden.

var.d <- VARselect(in.sample.d, type ="trend")

var.d$selection

# El orden de rezagos óptimos, de acuerdo al criterio de FPE, es 6.

# Estimamos:

# VER: si estimamos el var con constante, tendencia, las dos o ninguna

var.dl <- VAR(in.sample.d, p = 6, type = "trend")

# Vemos si podemos validar el modelo.

serial.test(var.dl) 

# Rechazamos la hipótesis nula de que los residuos están
# incorrelacionados. Por lo tanto, no podemos validar el modelo. 

# Ahora lo que haremos es interpretar económicamente al modelo VAR.

# Comenzaremos estimando funciones de impulso respuesta.

variables.var <- colnames(in.sample.d)

set.seed(444)

sent.fir <- irf(var.dl, impulse = variables.var, 
              response = "sentsmooth", n.ahead = 7,
              ortho = FALSE, runs = 1000)

# Hacemos una función que devuelve un df para cada una de las covariables.

fir.cases <- function(variable){
  t <- as.data.frame(cbind(seq(0,7,1), sent.fir$irf[[variable]],
                           sent.fir$Lower[[variable]],
                           sent.fir$Upper[[variable]]))
  colnames(t) <- c("day","irf", "lower", "upper")
  return(t)
}

# Graficamos.

ggplot(fir.cases("twfav") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.01,0.01)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[1])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[1],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[1], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Cantidad de Favoritos") + 
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("twret") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.01,0.01)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[2])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[2],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[2], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Cantidad de Favoritos") + 
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e1 <- ggplot(fir.cases("tasaint") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.025,0.025)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[3])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[3],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[3], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de tasa de interés") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("basemon") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.025,0.025)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[4])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[4],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[4], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Base Monetaria") + 
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("casosarg") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.025,0.025)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[5])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[5],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[5], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Casos COVID-19 en Argentina") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e2 <- ggplot(fir.cases("muertosarg") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.0025,0.0025)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[6])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[6],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[6], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de muertes") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("vacunasarg") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.02,0.02)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[1])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[1],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[1], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Vacunas Aplicadas")+
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("maxtemp") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.02,0.02)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[1])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[1],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[1], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Temperatura Máxima") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e6 <- ggplot(fir.cases("mintemp") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.02,0.02)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[2])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[2],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[2], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de temp. mínima") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e3 <- ggplot(fir.cases("casos.arg.rel") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-6,6)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[3])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[3],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[3], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de casos relativos") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")


e4 <- ggplot(fir.cases("muertes.arg.rel") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-3.5,3)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[4])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[4],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[4], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de muertes relativas")+
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

ggplot(fir.cases("reservas.est") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.005,0.005)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[5])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[5],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[5], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF de Reservas del BCRA") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

e5 <- ggplot(fir.cases("dolar.est") , aes(y = irf, x = day)) +
  theme_bw() + 
  xlim(0,7) +
  ylim(-0.01,0.01)+
  geom_hline(aes(yintercept = 0), size = 1, color = colores[6])+
  geom_line(aes(y = upper , x = day), size = 1, color = colores[6],
            linetype = "dashed") +
  geom_line(aes(y = irf, x = day), size = 1, color = "black") +
  geom_line(aes(y = lower, x = day), size = 1, color = colores[6], 
            linetype = "dashed")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("IRF del tipo de cambio") +
  labs(x = "95 % Bootstrap CI, 1000 runs", 
       y = "Sentimiento del presidente", 
       caption = "Fuente: elaboración propia")

# Exportamos las que tienen resultados interesantes.

library(gridExtra)

grid.arrange(e2, e3, e4, nrow = 1, ncol = 3)

ggsave(file="FirEpid.eps", width=6.5, height=4, dpi=300)

grid.arrange(e1, e5,e6 ,nrow = 1, ncol = 3)

ggsave(file="FirResto.eps", width=6.5, height=4, dpi=300)

# Descomposición de la varianza.

var.decomp <- fevd(var.dl, n.ahead=7)

library(reshape)

vd <- as.data.frame(cbind(seq(1,7,1),
                          var.decomp$sentsmooth[,14],
                          var.decomp$sentsmooth[,13],
                          var.decomp$sentsmooth[,12],
                          var.decomp$sentsmooth[,11],
                          var.decomp$sentsmooth[,10],
                          var.decomp$sentsmooth[,9],
                          var.decomp$sentsmooth[,8],
                          var.decomp$sentsmooth[,7],
                          var.decomp$sentsmooth[,6],
                          var.decomp$sentsmooth[,5],
                          var.decomp$sentsmooth[,4],
                          var.decomp$sentsmooth[,3],
                          var.decomp$sentsmooth[,2],
                          var.decomp$sentsmooth[,1]))

colnames(vd) <- c("id", "Dólar", "Res. BCRA", "Casos Arg.Rel.", 
                  "Muertes Arg. Rel.", "Temp. Min.", "Temp. Max.",
                  "Vacunas Arg.", "Muertes Arg.", "Casos Arg.", "Base Mon.",
                  "Tasa Int.", "Retweets", "Favoritos", "Sentimiento AF")

vd_panel <- melt(vd, id = c("id"))


ggplot(vd_panel, aes(fill=variable, y=value, x=id)) + 
  geom_bar(position="fill", stat="identity") + 
  theme_minimal() + 
  scale_x_discrete(limits=1:7, labels = c("1", "2", "3", "4",
                                           "5", "6", "7")) +
  scale_fill_manual("", values = c("Dólar" = colores2[8], 
                                             "Res. BCRA" = colores2[7], 
                                             "Casos Arg.Rel." = colores2[6], 
                                             "Muertes Arg. Rel." = colores2[5],
                                             "Temp. Min." = colores2[4], 
                                             "Temp. Max." = colores2[3], 
                                             "Vacunas Arg." = colores2[2], 
                                             "Muertes Arg." = colores2[1],
                                             "Casos Arg." = colores[6], 
                                             "Base Mon." = colores[5], 
                                             "Tasa Int." = colores[4], 
                                             "Retweets" = colores[3],
                                             "Favoritos" = colores[2], 
                                             "Sentimiento AF" = colores[1]))+
  labs(x = "Horizonte", 
       y = "Porcentaje explicado", 
       title = "Descomposición de la varianza",
       subtitle = "Sentimiento del presidente",
       caption = "Fuente: elaboración propia") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
  
ggsave(file="VarDecomp.eps", width=5.5, height=4, dpi=300)

# Ahora, vamos a estimar un modelo FAVAR. Comenzamos aplicando la técnica de componentes
# principales a nuestras variables explicativas.

PCA1 <- prcomp(in.sample[,3:15], scale =TRUE) 

get_eig(PCA1) # probar esto 

# Veremos los autovalores para evaluar qué cantidad de componentes utilizaremos.

PCA1$sdev^2

# Como hay 4 componentes cuyo autovalor es superior a 1, los usaremos.

library(dplyr)

PC.is <- scale(in.sample[,3:15])%*%PCA1$rotation

# Aplicamos los componentes principales a toda la base de datos (no solo el período in-sample).

PC <- scale(data[,3:15])%*%PCA1$rotation

# Ahora elegimos el orden.

favar.data <- cbind(in.sample[,2], PC.is[,1:4])

VARselect(favar.data, lag.max = 10, type = c("both"))

# Con el mismo criterio que antes, seleccionamos p=8.

favar<- VAR(favar.data, p= 8, type = "both")
summary(favar)

# Probamos autocorrelación en los residuos.

serial.test(favar) 

favar$varresult$X

# No rechazamos al 1%. Exportamos la tabla.

stargazer(var.dl$varresult$sentsmooth, favar$varresult$X,
          align = TRUE, 
          dep.var.labels = c("SentIndex", "SentIndex"),
          omit = c("sd1", 
                   "sd2", "sd3"),
          keep.stat = c("n", "ll", "rsq"),
          no.space = TRUE,
          type = "latex")


#### Pronósticos ####

#### h = 1 ####

#### Esquema fijo ####

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC <- ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.f.h1 <- ts(matrix(0, 58, 6), frequency = 365, start=c(2020,11))
colnames(pr.f.h1) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR", "FAVAR")

h <- 1

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

# ADL. Seleccionamos el orden. 


order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + muertes.arg.rel + casos.arg.rel, 
                          data = in.sample[,-1], max_order = 8)

count <- 2

for(i in 1:58){
  temp2 <-window(data1[,-1],start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "muertes.arg.rel", "casos.arg.rel")
          
  # El orden del ADL es el conseguido al aplicar el modelo al período in-sample.
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm.
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = temp2)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.f.h1[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# Realizamos los pronósticos con el VAR. Construimos la serie 
# diferenciando las variables que son I(1).

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

# Pronosticamos.

for(i in 1:58){ 
  temp2 <- window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  f5 <- VAR(temp2, p = 6, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.f.h1[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Comentario que se aplicará al resto de los pronósticos. Dado que los loops tardan mucho
# tiempo en correr, descargaremos una base de datos con los valores pronosticados. 
# Los importaremos directamente con el link. Sin embargo, de querer verificar todos los archivos
# sugerimos visitar el repositorio de Github cuya dirección se encuentra al comienzo del script.

pr.f.h1 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.f.h1.csv")
pr.f.h1 <- pr.f.h1[,-c(1:3)]
pr.f.h1<-ts(pr.f.h1,frequency = 365, start = c(2020,11))

# Graficamos los pronósticos.

autoplot(ts.union(out.of.sample[,2], pr.f.h1[,1], pr.f.h1[,2],pr.f.h1[,3],
                  pr.f.h1[,4],pr.f.h1[,5], pr.f.h1[,6]), size = 0.7) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema fijo",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.f.h1.eps", width=6.5, height=4, dpi=300)

#### Esquema recursivo ####

data1 <- ts(data, frequency = 365, start = c(2019,12))

PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.rec.h1 <- ts(matrix(0, 58, 6), frequency = 365, start=c(2020,11))
colnames(pr.rec.h1) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")

h <- 1

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

# ADL.

count <- 2
for(i in 1:58){
  temp2 <-window(data1[,-1], start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp","muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rec.h1[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:58){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")$selection[1]
  f5 <- VAR(temp2, p = a, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rec.h1[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos los pronósticos.

pr.rec.h1 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rec.h1.csv")
pr.rec.h1 <- pr.rec.h1[,-(1:3)]
pr.rec.h1 <- ts(pr.rec.h1, frequency = 365, start = c(2020,11))

# Gráficamos.

autoplot(ts.union(out.of.sample[,2], pr.rec.h1[,1], pr.rec.h1[,2],pr.rec.h1[,3],
                  pr.rec.h1[,4],pr.rec.h1[,5], pr.rec.h1[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema recursivo",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rec.h1.eps", width=6.5, height=4, dpi=300)
  

#### Esquema rolling ####

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
  
  # ARIMAX
  f2 <- Arima(temp,model=auto.arima(temp), newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h1[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rol.h1[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rol.h1[i,6] <- forecast4$forecast$temp$mean[h] 
}

#ADL. 


count <- 2

for(i in 1:58){
  temp2 <-window(data1[,-1], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rol.h1[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

h <- 1

for(i in 1:58){ 
  temp2<-window(data.diff, start = 2019.033 + (i-1)/365, end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")$selection[1]
  f5 <- VAR(temp2, p = a, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rol.h1[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.rol.h1 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rol.h1.csv")
pr.rol.h1 <- pr.rol.h1[,-(1:3)]
pr.rol.h1 <- ts(pr.rol.h1, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[,2], pr.rol.h1[,1], pr.rol.h1[,2],pr.rol.h1[,3],
                  pr.rol.h1[,4],pr.rol.h1[,5], pr.rol.h1[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema 'rolling'",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rol.h1.eps", width=6.5, height=4, dpi=300)


#### h = 2 ####

#### Esquema fijo ####

# Con esta cantidad de pasos adelante, perdemos la primera observación.

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
  
  # ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h2[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f3, n.ahead=h)
  pr.f.h2[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.f.h2[i,6] <- forecast4$forecast$temp$mean[h]
  
}

# ADL.



order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + muertes.arg.rel + casos.arg.rel, 
                          data = in.sample[,-1], max_order = 5)

count <- 2
for(i in 1:57){
  temp2 <-window(data1[,-1],start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "muertes.arg.rel", "casos.arg.rel")
  
  # El orden del ADL es el conseguido al aplicar el modelo al período in-sample 
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = temp2)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.f.h2[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:57){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  f5 <- VAR(temp2, p = 6, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.f.h2[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.f.h2 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.f.h2.csv")
pr.f.h2 <- pr.f.h2[,-(1:3)]
pr.f.h2 <- ts(pr.f.h2, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.f.h2[,1], pr.f.h2[,2],pr.f.h2[,3],
                  pr.f.h2[,4],pr.f.h2[,5], pr.f.h2[,6]), size = 0.7) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema fijo",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.f.h2.eps", width=6.5, height=4, dpi=300)


#### Esquema recursivo ####

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
  
  # ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rec.h2[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rec.h2[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rec.h2[i,6] <- forecast4$forecast$temp$mean[h]
}

# ADL.


count <- 2

for(i in 1:57){
  temp2 <-window(data1[,-1], start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp","muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp +  muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp  + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.rec.h2[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:57){ 
  temp2 <-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rec.h2[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.rec.h2 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rec.h2.csv")
pr.rec.h2 <- pr.rec.h2[,-(1:3)]
pr.rec.h2 <- ts(pr.rec.h2, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.rec.h2[,1], pr.rec.h2[,2],pr.rec.h2[,3],
                  pr.rec.h2[,4],pr.rec.h2[,5], pr.rec.h2[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema recursivo",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rec.h2.eps", width=6.5, height=4, dpi=300)


#### Esquema rolling ####

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
  
  # ARIMAX
  f2 <- Arima(temp,model=auto.arima(temp), newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h2[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rol.h2[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rol.h2[i,6] <- forecast4$forecast$temp$mean[h]
  
}

# ADL.



count <- 2

for(i in 1:57){
  temp2 <-window(data1[,-1], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp  + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.rol.h2[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:57){ 
  temp2<-window(data.diff, start = 2019.033 + (i-1)/365, end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rol.h2[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.rol.h2 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rol.h2.csv")
pr.rol.h2 <- pr.rol.h2[,-(1:3)]
pr.rol.h2 <- ts(pr.rol.h2, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.rol.h2[,1], pr.rol.h2[,2],pr.rol.h2[,3],
                  pr.rol.h2[,4],pr.rol.h2[,5], pr.rol.h2[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema 'rolling'",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rol.h2.eps", width=6.5, height=4, dpi=300)


#### h = 7 ####

#### Esquema fijo ####

# Perdemos las primeras seis observaciones.

data1 <- ts(data, frequency = 365, start = c(2019,12))
PC<-ts(PC[,1:4], frequency = 365, start = c(2019,12))

pr.f.h7 <- ts(matrix(0, 52, 6), frequency = 365, start=c(2020,11))
colnames(pr.f.h7) <- c("ARIMA", "ARIMAX", "ETS", "ADL","VAR","FAVAR")

h <- 7

for(i in 1:52){
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)

  # ARIMA 
  f1 <- Arima(temp, model=arima.1)
  forecast <- forecast(f1,h = 7)
  pr.f.h7[i,1] <- forecast$mean[7]
  
  # ARIMAX
  f2 <- Arima(temp,model=arima.1,xreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.f.h7[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp, model=ets.1, use.initial.values=TRUE)
  pre <- forecast(f3, n.ahead=h)
  pr.f.h7[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.f.h7[i,6] <- forecast4$forecast$temp$mean[h]
}

# ADL.


order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                            casosarg + muertosarg + vacunasarg + maxtemp + 
                            mintemp + muertes.arg.rel + casos.arg.rel|
                            mes01 + mes02 + mes03 +  mes04 + mes05 + mes06 +
                            mes07 + mes08 + mes09 +  mes10 + mes11, 
                          data = in.sample[,-1], max_order = 5)
count <- 2

for(i in 1:52){
  temp2 <-window(data1[,-1],start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp",
                       "muertes.arg.rel", "casos.arg.rel")
  
  # El orden del ADL es el conseguido al aplicar el modelo al período in sample 
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = temp2)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.f.h7[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

h<-7

for(i in 1:52){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  f5 <- VAR(temp2, p = 6, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.f.h7[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.f.h7 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.f.h7.csv")
pr.f.h7 <- pr.f.h7[,-(1:3)]
pr.f.h7 <- ts(pr.f.h7, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.f.h7[,1], pr.f.h7[,2], pr.f.h7[,3],
                  pr.f.h7[,4], pr.f.h7[,5], pr.f.h7[,6]), size = 0.7) +
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema fijo",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.f.h7.eps", width=6.5, height=4, dpi=300)

#### Esquema recursivo ####

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
  
  # ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rec.h7[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rec.h7[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rec.h7[i,6] <- forecast4$forecast$temp$mean[h]
  
}

# ADL.

count <- 2

for(i in 1:52){
  temp2 <-window(data1[,-1], start = c(2019,12), end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp","muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp  + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp  + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=h)
  pr.rec.h7[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:52){ 
  temp2<-window(data.diff, start = c(2019,12), end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rec.h7[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.rec.h7 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rec.h7.csv")
pr.rec.h7 <- pr.rec.h7[,-(1:3)]
pr.rec.h7 <- ts(pr.rec.h7, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.rec.h7[,1], pr.rec.h7[,2],pr.rec.h7[,3],
                  pr.rec.h7[,4],pr.rec.h7[,5], pr.rec.h7[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema recursivo",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.f.h7.eps", width=6.5, height=4, dpi=300)

#### Esquema rolling ####

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
  
  # ARIMAX
  f2 <- Arima(temp,model=f1, newxreg=temp2)
  forecast2 <- forecast(f2$fitted,h=h)
  pr.rol.h7[i,2] <- forecast2$mean[h]
  
  # ETS 
  f3 <- ets(temp)
  pre <- forecast(f3, n.ahead=h)
  pr.rol.h7[i,3] <- pre$mean[h]
  
  # FAVAR
  a<-VARselect(cbind(temp, temp3), lag.max = 13, type = "const")$selection[1]
  f4 <- VAR(cbind(temp, temp3), p = a, type= "trend")
  forecast4<-forecast(f4, h=h)
  pr.rol.h7[i,6] <- forecast4$forecast$temp$mean[h]
}

# ADL.

count <- 2

for(i in 1:52){
  temp2 <-window(data1[,-1], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  colnames(temp2) <- c("sentsmooth","twfav", "twret", "reservasbcra", "tasaint", "basemon", "tcdolar", "casosarg",          
                       "muertosarg", "vacunasarg", "maxtemp", "mintemp", "muertes.arg.rel", "casos.arg.rel")
  
  # Actualizamos el orden del ADL
  
  order.adl.dl <- auto_ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                              casosarg + muertosarg + vacunasarg + maxtemp + 
                              mintemp  + muertes.arg.rel + casos.arg.rel, 
                            data = temp2, max_order = 5)
  
  adl.dl <- ardl(sentsmooth ~ twfav + twret + reservasbcra + tasaint + basemon + tcdolar + 
                   casosarg + muertosarg + vacunasarg + maxtemp + 
                   mintemp  + muertes.arg.rel + casos.arg.rel, 
                 data = temp2, order = as.vector(order.adl.dl$best_order))
  
  # Estimamos el ADL pero con la función dynlm
  
  adl<-adl.dl$full_formula
  adl.1 <- dynlm(adl, data = adl.dl$data)
  forecast2 <- predict(adl.1$fitted.values,h=1)
  pr.rol.h7[i,4] <- forecast2$mean[h]
  print(count) 
  count = count + 1
}

# VAR.

reservas.est <- diff(data1[,5])

dolar.est <- diff(data1[,8])

data.diff<-cbind(data1[-1,-1], reservas.est, dolar.est)

for(i in 1:52){ 
  temp2<-window(data.diff, start = 2019.033 + (i-1)/365, end = 2020.195 + (i-1)/365)
  a<-VARselect(data.diff, lag.max = 13, type = "const")
  b<-a$selection
  c<-b[1]
  f5 <- VAR(temp2, p = c, type = "trend")
  forecast1<- forecast(f5, h=h)
  pr.rol.h7[i,5] <- forecast1$forecast$data1..1...1..sentsmooth$mean[h]
}

# Importamos.

pr.rol.h7 <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rol.h7.csv")
pr.rol.h7 <- pr.rol.h7[,-(1:3)]
pr.rol.h7 <- ts(pr.rol.h7, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.rol.h7[,1], pr.rol.h7[,2],pr.rol.h7[,3],
                  pr.rol.h7[,4],pr.rol.h7[,5], pr.rol.h7[,6]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","ADL","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[4], colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos con esquema 'rolling'",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rol.h7.eps", width=6.5, height=4, dpi=300)

#### Pronósticos con Bagging ####

# En esta sección haremos los pronósticos usando la técnica de bagging.
# Extraeremos aleatoriamente 100 muestras bootstrap.
# No haremos pronósticos con el modelo ADL dada la restriccion computacional.

# Comenzamos extrayendo las muestras bootstrap.

library(quantmod)

# Plantamos la semilla para poder replicar resultados.

set.seed(444)

# Aquí extraemos las muestras de las variables de nuestra base.

for (i in 2:15) {
  a <- bld.mbb.bootstrap(data[,i], 100) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("data.bag.",i,sep = ""),ts(a, frequency = 365, start = c(2019,12)))
}

# Aquí extraemos las muestras de los componentes principales.

for (i in 1:4) {
  b <- bld.mbb.bootstrap(PC[,i], 100) %>% as.data.frame() %>% ts(start=c(2019,12), frequency=365)
  assign(paste("PC.bag.",i,sep = ""),ts(b, frequency = 365, start = c(2019,12)))
}

# Ahora continuamos con los pronósticos.

#### Esquema fijo ####

#### h = 1 ####

pr.f.h1.b <- matrix(nrow=58,ncol=5, NA)

h <- 1

for(i in 1:58){
  pr.arima <- matrix(nrow=1,ncol=100,NA)
  pr.arimax <- matrix(nrow=1,ncol=100,NA)
  pr.ets <- matrix(nrow=1,ncol=100,NA)
  
  for (j in 1:100) {
  temp <-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
  data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                           data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                           data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                           data.bag.15[,j])
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
  
  print(j)
  } 
  pr.f.h1.b[i,1] <- mean(pr.arima)
  pr.f.h1.b[i,2] <- mean(pr.arimax)
  pr.f.h1.b[i,3] <- mean(pr.ets)
  print(i)
}


# Realizamos los pronósticos con el VAR.

for(i in 1:58){
  pr.var <- matrix(nrow=1,ncol=100,NA)
  for (j in 1:100) {
  data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), start = c(2019,12), end = 2020.195 + (i-1)/365)
  var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.195 + (i-1)/365)
  f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = 6, type = "trend")
  pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
  print(j)
  }
  pr.f.h1.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}





# Realizamos los pronósticos para el FAVAR.

for(i in 1:58){
    pr.favar <- matrix(nrow=1,ncol=100,NA)
  for (j in 1:100){
    temp <-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3[,1]), p = 6, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    print(j)
  } 
  
  pr.f.h1.b[i,4] <- mean(pr.favar,na.rm = TRUE)
  print(i)
}


write.csv(pr.f.h1.b,"pr.f.h1.b.csv" , row.names = FALSE)

# Importamos los pronósticos ya hechos.

pr.f.h1.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.f.h1.b.csv")
pr.f.h1.b <- ts(pr.f.h1.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[1:58,2], pr.f.h1.b[,1], pr.f.h1.b[,2], pr.f.h1.b[,3], pr.f.h1.b[,4], pr.f.h1.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema fijo",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.f.h1.b.eps", width=6.5, height=4, dpi=300)


#### h = 2 ####

pr.f.h2.b <- matrix(nrow=57,ncol=5, NA)

h <- 2

for(i in 1:57){
  pr.arima <- matrix(nrow=1,ncol=100,NA)
  pr.arimax <- matrix(nrow=1,ncol=100,NA)
  pr.ets <- matrix(nrow=1,ncol=100,NA)
  pr.favar <-  matrix(nrow=1,ncol=100,NA)
  
  for (j in 1:100) {
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
    f4 <- VAR(cbind(temp, temp3), p = 8, type= "trend")
    forecast4<-forecast(f4, h=h)
    pr.favar[1,j] <- forecast4$forecast$temp$mean[h]
    print(j)
  } 
  pr.f.h2.b[i,1] <- mean(pr.arima)
  pr.f.h2.b[i,2] <- mean(pr.arimax)
  pr.f.h2.b[i,3] <- mean(pr.ets)
  pr.f.h2.b[i,4] <- mean(pr.favar)
  print(i)
}

# VAR.

for(i in 1:57){
  pr.var <- matrix(nrow=1,ncol=100,NA)
  for (j in 1:100){
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

write.csv(pr.f.h2.b, "pr.f.h2.b.csv", row.names = FALSE)

# Importamos.

pr.f.h2.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.f.h2.b.csv")
pr.f.h2.b <- ts(pr.f.h2.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.f.h2.b[,1], pr.f.h2.b[,2], pr.f.h2.b[,3],
                  pr.f.h2.b[,4], pr.f.h2.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema fijo",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.f.h2.b.eps", width=6.5, height=4, dpi=300)


#### h = 7 ####

pr.f.h7.b <- matrix(nrow=52,ncol=5, NA)

h <- 7

for(i in 1:52){
  pr.arima <- matrix(nrow=1,ncol=100,NA)
  pr.arimax <- matrix(nrow=1,ncol=100,NA)
  pr.ets <- matrix(nrow=1,ncol=100,NA)
  pr.favar <- matrix(nrow=1,ncol=100,NA)
  
  for (j in 1:100){
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
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model=ets.1, use.initial.values=TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = 8, type= "trend")
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

# VAR

for(i in 1:52){
  pr.var <- matrix(nrow=1,ncol=100,NA)
  for (j in 1:100) {
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

write.csv(pr.f.h7.b, "pr.f.h7.b.csv", row.names = FALSE)

# Importamos.

pr.f.h7.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.f.h7.b.csv")
pr.f.h7.b <- ts(pr.f.h7.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.f.h7.b[,1], pr.f.h7.b[,2], pr.f.h7.b[,3],
                  pr.f.h7.b[,4], pr.f.h7.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema fijo",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.f.h7.b.eps", width=6.5, height=4, dpi=300)


#### Esquema recursivo ####

#### h = 1 ####

pr.rec.h1.b <- matrix(nrow=58,ncol=5, NA)

h <- 1

for(i in 1:58){
  pr.arima <- matrix(nrow=1,ncol=100,NA)
  pr.arimax <- matrix(nrow=1,ncol=100,NA)
  pr.ets <- matrix(nrow=1,ncol=100,NA)
  pr.favar <- matrix(nrow=1,ncol=100,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
    temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    arima.1 <- auto.arima(temp)

    
    # ETS 
    ets.1 <- ets(temp)
    
    # FAVAR
    favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]


  
  for (j in 1:100) {
    temp <-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <- window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],
                          PC.bag.4[,j]), start = c(2019,12), end = 2020.195 + (i-1)/365)
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
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

# VAR.

for(i in 1:58){
  pr.var <- matrix(nrow=1,ncol=100,NA)
  
  # Estimamos el modelo que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  
  for (j in 1:100){
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), start = c(2019,12), end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.195 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p =var.1, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rec.h1.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}



# Importamos.

pr.rec.h1.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rec.h1.b.csv")
pr.rec.h1.b <- ts(pr.rec.h1.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[1:58, 2], pr.rec.h1.b[,1], pr.rec.h1.b[,2], pr.rec.h1.b[,3], pr.rec.h1[,4], pr.rec.h1[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema recursivo",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rec.h1.b.eps", width=6.5, height=4, dpi=300)

#### h = 2 ####

pr.rec.h2.b <- matrix(nrow=57,ncol=5, NA)

h <- 2

for(i in 1:57){
  pr.arima <- matrix(nrow=1,ncol=100,NA)
  pr.arimax <- matrix(nrow=1,ncol=100,NA)
  pr.ets <- matrix(nrow=1,ncol=100,NA)
  pr.favar <-  matrix(nrow=1,ncol=100,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:100){
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j],
                             data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                             data.bag.13[,j],data.bag.14[,j],data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    #ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    #ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
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

# VAR.

for(i in 1:57){
  pr.var <- matrix(nrow=1,ncol=100,NA)
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)

  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:100) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j], 
                                    data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],
                                    data.bag.14[,j],data.bag.15[,j]), start = c(2019,12), end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.195 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j]<- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rec.h2.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}

# Importamos.

pr.rec.h2.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rec.h2.b.csv")
pr.rec.h2.b <- ts(pr.rec.h2.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.rec.h2.b[,1], pr.rec.h2.b[,2], pr.rec.h2.b[,3], pr.rec.h2.b[,4], pr.rec.h2.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema recursivo",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.f.h2.b.eps", width=6.5, height=4, dpi=300)



#### h = 7 ####

pr.rec.h7.b <- matrix(nrow=52,ncol=5, NA)

h <- 7

for(i in 1:52){
  pr.arima <- matrix(nrow=1,ncol=100,NA)
  pr.arimax <- matrix(nrow=1,ncol=100,NA)
  pr.ets <- matrix(nrow=1,ncol=100,NA)
  pr.favar <- matrix(nrow=1,ncol=100,NA)
  
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:100) {
    temp<-window(data.bag.2[,j], start = c(2019,12), end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],data.bag.7[,j],
                             data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                             data.bag.13[,j],data.bag.14[,j],data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = c(2019,12), end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]), 
                    start = c(2019,12), end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
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

# VAR.

for(i in 1:52){
  pr.var <- matrix(nrow=1,ncol=100,NA)
  temp<-window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = c(2019,12), end = 2020.195 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  for (j in 1:100) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],
                                    data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                                    data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = c(2019,12), end = 2020.195 + (i-1)/365)
    var.bag.ex.diff<- window(cbind(diff(data.bag.4[,j]), diff(data.bag.8[,j])), start = c(2019,12), end = 2020.195 + (i-1)/365)
    f5 <- VAR(cbind(data.var.bag.ex[-1,], var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j]<- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rec.h7.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}

# Importamos.

pr.rec.h7.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rec.h7.b.csv")
pr.rec.h7.b <- ts(pr.rec.h7.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.rec.h7.b[,1], pr.rec.h7.b[,2], pr.rec.h7.b[,3], pr.rec.h7.b[,4], pr.rec.h7.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema recursivo",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rec.h7.b.eps", width=6.5, height=4, dpi=300)


#### Esquema rolling ####

#### h = 1 ####

pr.rol.h1.b <- matrix(nrow=58,ncol=5, NA)

h <- 1

for(i in 1:58){
  pr.arima <- matrix(nrow=1,ncol=100,NA)
  pr.arimax <- matrix(nrow=1,ncol=100,NA)
  pr.ets <- matrix(nrow=1,ncol=100,NA)
  pr.favar <- matrix(nrow=1,ncol=100,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  
  for (j in 1:100) {
    temp <-window(data.bag.2[,j], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <- window(data.var.bag.ex, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]),
                    start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model = arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
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

# VAR.

for(i in 1:58){
  pr.var <- matrix(nrow=1,ncol=100,NA)
  
  # Estimamos el modelos que luego aplicaremos a las series bootstrap
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:100) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],
                                    data.bag.6[,j],data.bag.7[,j], data.bag.9[,j],
                                    data.bag.10[,j],data.bag.11[,j],data.bag.12[,j],
                                    data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    var.bag.ex.diff.1<- window(cbind(data.bag.4[,j], data.bag.8[,j]), start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    var.bag.ex.diff <- diff(var.bag.ex.diff.1)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rol.h1.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}

# Importamos.

pr.rol.h1.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rol.h1.b.csv")
pr.rol.h1.b <- ts(pr.rol.h1.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[,2], pr.rol.h1.b[,1], pr.rol.h1.b[,2], 
                  pr.rol.h1.b[,3], pr.rol.h1[,4], pr.rol.h1[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema 'rolling'",
       subtitle = "Un paso adelante (h=1)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rol.h1.b.eps", width=6.5, height=4, dpi=300)


#### h = 2 ####

pr.rol.h2.b <- matrix(nrow=57,ncol=5, NA)

h <- 2

for(i in 1:57){
  pr.arima <- matrix(nrow=1,ncol=100,NA)
  pr.arimax <- matrix(nrow=1,ncol=100,NA)
  pr.ets <- matrix(nrow=1,ncol=100,NA)
  pr.favar <-  matrix(nrow=1,ncol=100,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  
  for (j in 1:100) {

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
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    #ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    #ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
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

# VAR.

for(i in 1:57){
  pr.var <- matrix(nrow=1,ncol=100,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:100){
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],
                                    data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                                    data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    var.bag.ex.diff.1<- window(cbind(data.bag.4[,j], data.bag.8[,j]), start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    var.bag.ex.diff <- diff(var.bag.ex.diff.1)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rol.h2.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}

# Importamos.

pr.rol.h2.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rol.h2.b.csv")
pr.rol.h2.b <- ts(pr.rol.h2.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[2:58,2], pr.rol.h2.b[,1], pr.rol.h2.b[,2],
                  pr.rol.h2.b[,3], pr.rol.h2.b[,4], pr.rol.h2.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema 'rolling'",
       subtitle = "Dos pasos adelante (h=2)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rol.h2.b.eps", width=6.5, height=4, dpi=300)


#### h = 7 ####

pr.rol.h7.b <- matrix(nrow=52,ncol=5, NA)

h<- 7

for(i in 1:52){
  pr.arima <- matrix(nrow=1,ncol=100,NA)
  pr.arimax <- matrix(nrow=1,ncol=100,NA)
  pr.ets <- matrix(nrow=1,ncol=100,NA)
  pr.favar <- matrix(nrow=1,ncol=100,NA)
  
  # Estimamos los modelos que luego aplicaremos a las muestras bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp2 <-window(data1[,3:15], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  
  # ARIMA 
  arima.1 <- auto.arima(temp)
  
  
  # ETS 
  ets.1 <- ets(temp)
  
  # FAVAR
  favar.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:100) {
    temp<-window(data.bag.2[,j], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    data.var.bag.ex <- cbind(data.bag.3[,j],data.bag.4[,j],data.bag.5[,j],data.bag.6[,j],
                             data.bag.7[,j],data.bag.8[,j],data.bag.9[,j],data.bag.10[,j],
                             data.bag.11[,j],data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],
                             data.bag.15[,j])
    temp2 <-window(data.var.bag.ex, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    temp3 <- window(cbind(PC.bag.1[,j],PC.bag.2[,j],PC.bag.3[,j],PC.bag.4[,j]), start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    
    # ARIMA 
    f1 <- Arima(temp, model = arima.1)
    forecast <- forecast(f1,h = h)
    pr.arima[1,j] <- forecast$mean[h]
    
    # ARIMAX
    f2 <- Arima(temp,model=arima.1,newxreg=temp2)
    forecast2 <- forecast(f2$fitted,h=h)
    pr.arimax[1,j] <- forecast2$mean[h]
    
    # ETS 
    f1 <- ets(temp, model = ets.1, use.initial.values = TRUE)
    pre <- forecast(f1, n.ahead=h)
    pr.ets[1,j] <- pre$mean[h]
    
    # FAVAR
    f4 <- VAR(cbind(temp, temp3), p = favar.1, type= "trend")
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

# VAR.

for(i in 1:52){
  pr.var <- matrix(nrow=1,ncol=100,NA)
  
  # Estimamos el modelo que luego aplicaremos a las series bootstrap
  
  temp<-window(data1[,2], start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  temp3 <- window(PC, start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
  
  # VAR
  var.1 <-VARselect(cbind(temp, temp3), lag.max = 13, type = "trend")$selection[1]
  
  for (j in 1:100) {
    data.var.bag.ex <- window(cbind(data.bag.2[,j],data.bag.3[,j],data.bag.5[,j],data.bag.6[,j],
                                    data.bag.7[,j], data.bag.9[,j],data.bag.10[,j],data.bag.11[,j],
                                    data.bag.12[,j],data.bag.13[,j],data.bag.14[,j],data.bag.15[,j]), 
                              start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    var.bag.ex.diff.1<- window(cbind(data.bag.4[,j], data.bag.8[,j]), start = 2019.030 + (i-1)/365, end = 2020.195 + (i-1)/365)
    var.bag.ex.diff <- diff(var.bag.ex.diff.1)
    f5 <- VAR(cbind(data.var.bag.ex[-1,],var.bag.ex.diff), p = var.1, type = "trend")
    pr.var[1,j] <- forecast(f5)$forecast$data.var.bag.ex..1....data.bag.2...j.$mean[h]
    print(j)
  }
  pr.rol.h7.b[i,5] <- mean(pr.var)
  print(paste("VAMOS:", i, "PERIODOS"))
}

# Importamos.

pr.rol.h7.b <- read.csv("https://raw.githubusercontent.com/tomas-pacheco/ExamenPronosticos/main/forecasts/pr.rol.h7.b.csv")
pr.rol.h7.b <- ts(pr.rol.h7.b, frequency = 365, start = c(2020,11))

# Graficamos.

autoplot(ts.union(out.of.sample[7:58,2], pr.rol.h7.b[,1], pr.rol.h7.b[,2], pr.rol.h7.b[,3], pr.rol.h7.b[,4], pr.rol.h7.b[,5]), size = 0.7) + 
  scale_color_manual(name = "", labels = c("Actual", "ARIMA", "ARIMAX",
                                           "ETS","VAR", "FAVAR"), 
                     values = c("#4b4b4b", colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme_minimal() +  
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "Tiempo", 
       y = "Sentimiento del presidente", 
       title = "Pronósticos 'bagged' con esquema 'rolling'",
       subtitle = "Siete pasos adelante (h=7)",
       caption = "Fuente: elaboración propia") +
  guides(colour = guide_legend(nrow = 1))

ggsave(file="pr.rol.h7.b.eps", width=6.5, height=4, dpi=300)


#### Medidas de Accuracy ####

# Hacemos matrices en las cuales vamos a guardar estas medidas de desempeño, 
# acompañadas con el test de Diebold-Mariano.

# Separamos las tablas en función de los pasos adelante de los pronósticos.

AC<-matrix(NA,34,6)
colnames(AC) <- c("Modelo", "MAPE", "MAE", "RMSE", "Estadístico","P-valor")
AC2<-matrix(NA,34,1)

# TABLA 1

# Medidas de accuracy y test de DM de los pronósticos con esquema fijo y h=1.

AC[1,1]<-"AR(1)" 
AC2[1,1]<-"1"
AC[1,2:4]<-round(accuracy(pr.bench, out.of.sample[,2])[c(2:3,5)],4)
AC[2,1]<-"ARIMA fijo" 
AC2[2,1]<-"1"
AC[2,2:4]<-round(accuracy(pr.f.h1[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[3,1]<-"ARIMAX fijo" 
AC2[3,1]<-"1"
AC[3,2:4]<-round(accuracy(pr.f.h1[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[4,1]<-"ETS fijo" 
AC2[4,1]<-"1"
AC[4,2:4]<-round(accuracy(pr.f.h1[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[5,1]<- "ADL fijo" 
AC2[5,1]<-"1"
AC[5,2:4]<-round(accuracy(pr.f.h1[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[6,1]<- "VAR fijo"
AC2[6,1]<-"1"
AC[6,2:4]<-round(accuracy(pr.f.h1[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[7,1]<- "FAVAR fijo" 
AC2[7,1]<-"1"
AC[7,2:4]<-round(accuracy(pr.f.h1[,6], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling y h=1.

AC[8,1]<-"ARIMA rolling" 
AC2[8,1]<-"1"
AC[8,2:4]<-round(accuracy(pr.rol.h1[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[9,1]<-"ARIMAX rolling" 
AC2[9,1]<-"1"
AC[9,2:4]<-round(accuracy(pr.rol.h1[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[10,1]<-"ETS rolling" 
AC2[10,1]<-"1"
AC[10,2:4]<-round(accuracy(pr.rol.h1[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[11,1]<- "ADL rolling" 
AC2[11,1]<-"1"
AC[11,2:4]<-round(accuracy(pr.rol.h1[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[12,1]<- "VAR rolling"
AC2[12,1]<-"1"
AC[12,2:4]<-round(accuracy(pr.rol.h1[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[13,1]<- "FAVAR rolling" 
AC2[13,1]<-"1"
AC[13,2:4]<-round(accuracy(pr.rol.h1[,6], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema recursivo y h=1.

AC[14,1]<-"ARIMA recursivo" 
AC2[14,1]<-"1"
AC[14,2:4]<-round(accuracy(pr.rec.h1[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[15,1]<-"ARIMAX recursivo" 
AC2[15,1]<-"1"
AC[15,2:4]<-round(accuracy(pr.rec.h1[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[16,1]<-"ETS recursivo" 
AC2[16,1]<-"1"
AC[16,2:4]<-round(accuracy(pr.rec.h1[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[17,1]<- "ADL recursivo" 
AC2[17,1]<-"1"
AC[17,2:4]<-round(accuracy(pr.rec.h1[,4], out.of.sample[,2])[c(2:3,5)],4)
AC[18,1]<- "VAR recursivo"
AC2[18,1]<-"1"
AC[18,2:4]<-round(accuracy(pr.rec.h1[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[19,1]<- "FAVAR recursivo" 
AC2[19,1]<-"1"
AC[19,2:4]<-round(accuracy(pr.rec.h1[,6], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 1 y series boots.

AC[20,1]<-"ARIMA fijo bagged" 
AC2[20,1]<-"1"
AC[20,2:4]<-round(accuracy(pr.f.h1.b[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[21,1]<-"ARIMAX fijo bagged" 
AC2[21,1]<-"1"
AC[21,2:4]<-round(accuracy(pr.f.h1.b[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[22,1]<-"ETS fijo bagged" 
AC2[22,1]<-"1"
AC[22,2:4]<-round(accuracy(pr.f.h1.b[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[23,1]<- "VAR fijo bagged" 
AC2[23,1]<-"1"
AC[23,2:4]<-round(accuracy(pr.f.h1.b[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[24,1]<- "FAVAR fijo bagged"
AC2[24,1]<-"1"
AC[24,2:4]<-round(accuracy(pr.rec.h1.b[,4], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC[25,1]<-"ARIMA recursivo bagged" 
AC2[25,1]<-"1"
AC[25,2:4]<-round(accuracy(pr.rec.h1.b[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[26,1]<-"ARIMAX recursivo bagged" 
AC2[26,1]<-"1"
AC[26,2:4]<-round(accuracy(pr.rec.h1.b[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[27,1]<-"ETS recursivo bagged" 
AC2[27,1]<-"1"
AC[27,2:4]<-round(accuracy(pr.rec.h1.b[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[28,1]<- "VAR recursivo bagged" 
AC2[28,1]<-"1"
AC[28,2:4]<-round(accuracy(pr.rec.h1.b[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[29,1]<- "FAVAR recursivo bagged"
AC2[29,1]<-"1"
AC[29,2:4]<-round(accuracy(pr.rec.h1.b[,4], out.of.sample[,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 1 y series boots.

AC[30,1]<-"ARIMA rolling bagged" 
AC2[230,1]<-"1"
AC[30,2:4]<-round(accuracy(pr.rol.h1.b[,1], out.of.sample[,2])[c(2:3,5)],4)
AC[31,1]<-"ARIMAX rolling bagged" 
AC2[31,1]<-"1"
AC[32,2:4]<-round(accuracy(pr.rol.h1.b[,2], out.of.sample[,2])[c(2:3,5)],4)
AC[32,1]<-"ETS rollingbagged" 
AC2[32,1]<-"1"
AC[32,2:4]<-round(accuracy(pr.rol.h1.b[,3], out.of.sample[,2])[c(2:3,5)],4)
AC[33,1]<- "VAR rolling bagged" 
AC2[33,1]<-"1"
AC[33,2:4]<-round(accuracy(pr.rol.h1.b[,5], out.of.sample[,2])[c(2:3,5)],4)
AC[34,1]<- "FAVAR rolling bagged"
AC2[34,1]<-"1"
AC[34,2:4]<-round(accuracy(pr.rol.h1.b[,4], out.of.sample[,2])[c(2:3,5)],4)

# TABLA 2 

AC1<-matrix(NA,34,6)
colnames(AC1) <- c("Modelo", "MAPE", "MAE", "RMSE", "Estadístico","P-valor")
AC3<-matrix(NA,34,1)


AC1[2,1]<-"ARIMA fijo" 
AC3[2,1]<-"2"
AC1[2,2:4]<-round(accuracy(pr.f.h2[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[3,1]<-"ARIMAX fijo" 
AC3[3,1]<-"2"
AC1[3,2:4]<-round(accuracy(pr.f.h2[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[4,1]<-"ETS fijo" 
AC3[4,1]<-"2"
AC1[4,2:4]<-round(accuracy(pr.f.h2[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[5,1]<- "ADL fijo" 
AC3[5,1]<-"2"
AC1[5,2:4]<-round(accuracy(pr.f.h2[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[6,1]<- "VAR fijo"
AC3[6,1]<-"2"
AC1[6,2:4]<-round(accuracy(pr.f.h2[,5], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[7,1]<- "FAVAR fijo" 
AC3[7,1]<-"2"
AC1[7,2:4]<-round(accuracy(pr.f.h2[,6], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling y h=2.

AC1[8,1]<-"ARIMA rolling" 
AC3[8,1]<-"2"
AC1[8,2:4]<-round(accuracy(pr.rol.h2[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[9,1]<-"ARIMAX rolling" 
AC3[9,1]<-"2"
AC1[9,2:4]<-round(accuracy(pr.rol.h2[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[10,1]<-"ETS rolling" 
AC3[10,1]<-"2"
AC1[10,2:4]<-round(accuracy(pr.rol.h2[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[11,1]<- "ADL rolling" 
AC3[11,1]<-"2"
AC1[11,2:4]<-round(accuracy(pr.rol.h2[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[12,1]<- "VAR rolling"
AC3[12,1]<-"2"
AC1[12,2:4]<-round(accuracy(pr.rol.h2[,5], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[13,1]<- "FAVAR rolling" 
AC3[13,1]<-"2"
AC1[13,2:4]<-round(accuracy(pr.rol.h2[,6], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema recursivo y h=2.

AC1[14,1]<-"ARIMA recursivo" 
AC3[14,1]<-"2"
AC1[14,2:4]<-round(accuracy(pr.rec.h2[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[15,1]<-"ARIMAX recursivo" 
AC3[15,1]<-"2"
AC1[15,2:4]<-round(accuracy(pr.rec.h2[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[16,1]<-"ETS recursivo" 
AC3[16,1]<-"2"
AC1[16,2:4]<-round(accuracy(pr.rec.h2[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[17,1]<- "ADL recursivo" 
AC3[17,1]<-"2"
AC1[17,2:4]<-round(accuracy(pr.rec.h2[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[18,1]<- "VAR recursivo"
AC3[18,1]<-"2"
AC1[18,2:4]<-round(accuracy(pr.rec.h2[,5], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[19,1]<- "FAVAR recursivo" 
AC3[19,1]<-"2"
AC1[19,2:4]<-round(accuracy(pr.rec.h2[,6], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots.

AC1[20,1]<-"ARIMA fijo bagged" 
AC3[20,1]<-"2"
AC1[20,2:4]<-round(accuracy(pr.f.h2.b[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[21,1]<-"ARIMAX fijo bagged" 
AC3[21,1]<-"2"
AC1[21,2:4]<-round(accuracy(pr.f.h2.b[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[22,1]<-"ETS fijo bagged" 
AC3[22,1]<-"2"
AC1[22,2:4]<-round(accuracy(pr.f.h2.b[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[23,1]<- "VAR fijo bagged" 
AC3[23,1]<-"2"
AC1[23,2:4]<-round(accuracy(pr.f.h2.b[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[24,1]<- "FAVAR fijo bagged"
AC3[24,1]<-"2"
AC1[24,2:4]<-round(accuracy(pr.rec.h2.b[,5], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC1[25,1]<-"ARIMA recursivo bagged" 
AC3[25,1]<-"2"
AC1[25,2:4]<-round(accuracy(pr.rec.h2.b[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[26,1]<-"ARIMAX recursivo bagged" 
AC3[26,1]<-"2"
AC1[26,2:4]<-round(accuracy(pr.rec.h2.b[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[27,1]<-"ETS recursivo bagged" 
AC3[27,1]<-"2"
AC1[27,2:4]<-round(accuracy(pr.rec.h2.b[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[28,1]<- "VAR recursivo bagged" 
AC3[28,1]<-"2"
AC1[28,2:4]<-round(accuracy(pr.rec.h2.b[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[29,1]<- "FAVAR recursivo bagged"
AC3[29,1]<-"2"
AC1[29,2:4]<-round(accuracy(pr.rec.h2.b[,5], out.of.sample[2:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots.

AC1[30,1]<-"ARIMA rolling bagged" 
AC3[30,1]<-"2"
AC1[30,2:4]<-round(accuracy(pr.rol.h2.b[,1], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[31,1]<-"ARIMAX rolling bagged" 
AC3[31,1]<-"2"
AC1[31,2:4]<-round(accuracy(pr.rol.h2.b[,2], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[32,1]<-"ETS rollingbagged" 
AC3[32,1]<-"2"
AC1[32,2:4]<-round(accuracy(pr.rol.h2.b[,3], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[33,1]<- "VAR rolling bagged" 
AC3[33,1]<-"2"
AC1[33,2:4]<-round(accuracy(pr.rol.h2.b[,4], out.of.sample[2:58,2])[c(2:3,5)],4)
AC1[34,1]<- "FAVAR rolling bagged"
AC3[34,1]<-"2"
AC1[34,2:4]<-round(accuracy(pr.rol.h2.b[,5], out.of.sample[2:58,2])[c(2:3,5)],4)

# TABLA 3 

AC4<-matrix(NA,34,6)
colnames(AC1) <- c("Modelo", "MAPE", "MAE", "RMSE", "Estadístico","P-valor")
AC5<-matrix(NA,34,1)

AC4[2,1]<-"ARIMA fijo" 
AC5[2,1]<-"2"
AC4[2,2:4]<-round(accuracy(pr.f.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[3,1]<-"ARIMAX fijo" 
AC5[3,1]<-"2"
AC4[3,2:4]<-round(accuracy(pr.f.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[4,1]<-"ETS fijo" 
AC5[4,1]<-"2"
AC4[4,2:4]<-round(accuracy(pr.f.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[5,1]<- "ADL fijo" 
AC5[5,1]<-"2"
AC4[5,2:4]<-round(accuracy(pr.f.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[6,1]<- "VAR fijo"
AC5[6,1]<-"2"
AC4[6,2:4]<-round(accuracy(pr.f.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[7,1]<- "FAVAR fijo" 
AC5[7,1]<-"2"
AC4[7,2:4]<-round(accuracy(pr.f.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling y h=2.

AC4[8,1]<-"ARIMA rolling" 
AC5[8,1]<-"2"
AC4[8,2:4]<-round(accuracy(pr.rol.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[9,1]<-"ARIMAX rolling" 
AC5[9,1]<-"2"
AC4[9,2:4]<-round(accuracy(pr.rol.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[10,1]<-"ETS rolling" 
AC5[10,1]<-"2"
AC4[10,2:4]<-round(accuracy(pr.rol.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[11,1]<- "ADL rolling" 
AC5[11,1]<-"2"
AC4[11,2:4]<-round(accuracy(pr.rol.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[12,1]<- "VAR rolling"
AC5[12,1]<-"2"
AC4[12,2:4]<-round(accuracy(pr.rol.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[13,1]<- "FAVAR rolling" 
AC5[13,1]<-"2"
AC4[13,2:4]<-round(accuracy(pr.rol.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema recursivo y h=2.

AC4[14,1]<-"ARIMA recursivo" 
AC5[14,1]<-"2"
AC4[14,2:4]<-round(accuracy(pr.rec.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[15,1]<-"ARIMAX recursivo" 
AC5[15,1]<-"2"
AC4[15,2:4]<-round(accuracy(pr.rec.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[16,1]<-"ETS recursivo" 
AC5[16,1]<-"2"
AC4[16,2:4]<-round(accuracy(pr.rec.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[17,1]<- "ADL recursivo" 
AC5[17,1]<-"2"
AC4[17,2:4]<-round(accuracy(pr.rec.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[18,1]<- "VAR recursivo"
AC5[18,1]<-"2"
AC4[18,2:4]<-round(accuracy(pr.rec.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[19,1]<- "FAVAR recursivo" 
AC5[19,1]<-"2"
AC4[19,2:4]<-round(accuracy(pr.rec.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots.

AC4[20,1]<-"ARIMA fijo bagged" 
AC5[20,1]<-"2"
AC4[20,2:4]<-round(accuracy(pr.f.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[21,1]<-"ARIMAX fijo bagged" 
AC5[21,1]<-"2"
AC4[21,2:4]<-round(accuracy(pr.f.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[22,1]<-"ETS fijo bagged" 
AC5[22,1]<-"2"
AC4[22,2:4]<-round(accuracy(pr.f.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[23,1]<- "VAR fijo bagged" 
AC5[23,1]<-"2"
AC4[23,2:4]<-round(accuracy(pr.f.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[24,1]<- "FAVAR fijo bagged"
AC5[24,1]<-"2"
AC4[24,2:4]<-round(accuracy(pr.rec.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC4[25,1]<-"ARIMA recursivo bagged" 
AC5[25,1]<-"2"
AC4[25,2:4]<-round(accuracy(pr.rec.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[26,1]<-"ARIMAX recursivo bagged" 
AC5[26,1]<-"2"
AC4[26,2:4]<-round(accuracy(pr.rec.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[27,1]<-"ETS recursivo bagged" 
AC5[27,1]<-"2"
AC4[27,2:4]<-round(accuracy(pr.rec.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[28,1]<- "VAR recursivo bagged" 
AC5[28,1]<-"2"
AC4[28,2:4]<-round(accuracy(pr.rec.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[29,1]<- "FAVAR recursivo bagged"
AC5[29,1]<-"2"
AC4[29,2:4]<-round(accuracy(pr.rec.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots.

AC4[30,1]<-"ARIMA rolling bagged" 
AC5[30,1]<-"2"
AC4[30,2:4]<-round(accuracy(pr.rol.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[31,1]<-"ARIMAX rolling bagged" 
AC5[31,1]<-"2"
AC4[31,2:4]<-round(accuracy(pr.rol.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[32,1]<-"ETS rolling bagged" 
AC5[32,1]<-"2"
AC4[32,2:4]<-round(accuracy(pr.rol.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[33,1]<- "VAR rolling bagged" 
AC5[33,1]<-"2"
AC4[33,2:4]<-round(accuracy(pr.rol.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[34,1]<- "FAVAR rolling bagged"
AC5[34,1]<-"2"
AC4[34,2:4]<-round(accuracy(pr.rol.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)


### Test de Diebold Mariano ###

# Agregamos a la tabla el test de Diebold Mariano. 

# Usamos como benchmark un AR(1).

# Para esto generamos los pronósticos que resultan al utilizar el modelo AR(1).

pr.bench <- ts(matrix(nrow = 58, ncol = 1, 0), frequency = 365, start=c(2020,11))

for (i in 1:58) {
  temp <- window(data1[,2], start = c(2019,12), end = 2020.195 + (i-1)/365)
  f2 <- arima(temp, order=c(1,0,0))
  forecast2 <- forecast(f2,h=1)
  pr.bench[i,1] <- forecast2$mean[1]
}

# Generamos los errores de pronósticos de todos los modelos utilizados. 

# Separamos los casos según la cantidad de pasos adelante utilizados en cada pronóstico.

### h=1 ###

# Esquema fijo.

error.bench <- out.of.sample[,2]- pr.bench

error.arima.f.h1 <- out.of.sample[,2]-pr.f.h1[,1]

error.arimax.f.h1 <- out.of.sample[,2]-pr.f.h1[,2]

error.ets.f.h1 <- out.of.sample[,2]-pr.f.h1[,3]

error.adl.f.h1 <- out.of.sample[,2]-pr.f.h1[,4]

error.var.f.h1 <- out.of.sample[,2]-pr.f.h1[,5]

error.favar.f.h1 <- out.of.sample[,2]-pr.f.h1[,6]

# Esquema recursivo.

error.arima.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,1]

error.arimax.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,2]

error.ets.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,3]

error.adl.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,4]

error.var.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,5]

error.favar.rec.h1 <- out.of.sample[,2]-pr.rec.h1[,6]

# Esquema rolling.

error.arima.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,1]

error.arimax.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,2]

error.ets.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,3]

error.adl.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,4]

error.var.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,5]

error.favar.rol.h1 <- out.of.sample[,2]-pr.rol.h1[,6]

# Bagging 

# Esquema fijo bagged.

error.arima.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,1]

error.arimax.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,2]

error.ets.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,3]

error.var.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,4]

error.favar.f.h1.b <- out.of.sample[,2]-pr.f.h1.b[,5]

# Esquema recursivo bagged.

error.arima.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,1]

error.arimax.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,2]

error.ets.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,3]

error.var.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,4]

error.favar.rec.h1.b <- out.of.sample[,2]-pr.rec.h1.b[,5]

# Esquema rolling bagged.

error.arima.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,1]

error.arimax.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,2]

error.ets.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,3]

error.var.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,4]

error.favar.rol.h1.b <- out.of.sample[,2]-pr.rol.h1.b[,5]

### h=2 

error.arima.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,1]

error.arimax.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,2]

error.ets.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,3]

error.adl.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,4]

error.var.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,5]

error.favar.f.h2 <- out.of.sample[-1,2]-pr.f.h2[,6]

# Esquema recursivo.

error.arima.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,1]

error.arimax.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,2]

error.ets.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,3]

error.adl.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,4]

error.var.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,5]

error.favar.rec.h2 <- out.of.sample[-1,2]-pr.rec.h2[,6]

# Esquema rolling. 

error.arima.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,1]

error.arimax.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,2]

error.ets.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,3]

error.adl.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,4]

error.var.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,5]

error.favar.rol.h2 <- out.of.sample[-1,2]-pr.rol.h2[,6]

# Bagging 

# Esquema fijo bagged.

error.arima.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,1]

error.arimax.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,2]

error.ets.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,3]

error.var.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,4]

error.favar.f.h2.b <- out.of.sample[-1,2]-pr.f.h2.b[,5]

# Esquema recursivo bagged.

error.arima.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,1]

error.arimax.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,2]

error.ets.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,3]

error.var.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,4]

error.favar.rec.h2.b <- out.of.sample[-1,2]-pr.rec.h2.b[,5]

# Esquema rolling.

error.arima.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,1]

error.arimax.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,2]

error.ets.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,3]

error.var.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,4]

error.favar.rol.h2.b <- out.of.sample[-1,2]-pr.rol.h2.b[,5]

### h=7

error.arima.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,1]

error.arimax.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,2]

error.ets.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,3]

error.adl.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,4]

error.var.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,5]

error.favar.f.h7 <- out.of.sample[7:58,2]-pr.f.h7[,6]

# Esquema recursivo.

error.arima.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,1]

error.arimax.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,2]

error.ets.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,3]

error.adl.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,4]

error.var.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,5]

error.favar.rec.h7 <- out.of.sample[7:58,2]-pr.rec.h7[,6]

# Esquema rolling. 

error.arima.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,1]

error.arimax.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,2]

error.ets.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,3]

error.adl.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,4]

error.var.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,5]

error.favar.rol.h7 <- out.of.sample[7:58,2]-pr.rol.h7[,6]

# Bagging. 

# Esquema fijo bagged.

error.arima.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,1]

error.arimax.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,2]

error.ets.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,3]

error.var.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,5]

error.favar.f.h7.b <- out.of.sample[7:58,2]-pr.f.h7.b[,4]

# Esquema recursivo bagged.

error.arima.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,1]

error.arimax.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,2]

error.ets.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,3]

error.var.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,5]

error.favar.rec.h7.b <- out.of.sample[7:58,2]-pr.rec.h7.b[,4]

# Esquema rolling bagged.

error.arima.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,1]

error.arimax.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,2]

error.ets.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,3]

error.var.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,5]

error.favar.rol.h7.b <- out.of.sample[7:58,2]-pr.rol.h7.b[,4]

# Funciones de pérdida.

DL.arima.f.h1 <- error.arima.f.h1^{2} - error.bench^{2}

DL.arimax.f.h1 <- error.arimax.f.h1^{2} - error.bench^{2}

DL.ets.f.h1 <- error.ets.f.h1^{2} - error.bench^{2}

DL.adl.f.h1 <- error.adl.f.h1^{2} - error.bench^{2}

DL.var.f.h1 <- error.var.f.h1^{2} - error.bench^{2}

DL.favar.f.h1 <- error.favar.f.h1^{2} - error.bench^{2}

# Esquema recursivo.

DL.arima.rec.h1 <- error.arima.rec.h1^{2} - error.bench^{2}

DL.arimax.rec.h1 <- error.arimax.rec.h1^{2} - error.bench^{2}

DL.ets.rec.h1 <- error.ets.rec.h1^{2} - error.bench^{2}

DL.adl.rec.h1 <- error.adl.rec.h1^{2} - error.bench^{2}

DL.var.rec.h1 <- error.var.rec.h1^{2} - error.bench^{2}

DL.favar.rec.h1 <- error.favar.rec.h1^{2} - error.bench^{2}

# Esquema rolling.

DL.arima.rol.h1 <- error.arima.rol.h1^{2} - error.bench^{2}

DL.arimax.rol.h1 <- error.arimax.rol.h1^{2} - error.bench^{2}

DL.ets.rol.h1 <- error.ets.rol.h1^{2} - error.bench^{2}

DL.adl.rol.h1 <- error.adl.rol.h1^{2} - error.bench^{2}

DL.var.rol.h1 <- error.var.rol.h1^{2} - error.bench^{2}

DL.favar.rol.h1 <- error.favar.rol.h1^{2} - error.bench^{2}

# Bagging  

DL.arima.f.h1.b <- error.arima.f.h1.b^{2} - error.bench^{2}

DL.arimax.f.h1.b <- error.arimax.f.h1.b^{2} - error.bench^{2}

DL.ets.f.h1.b <- error.ets.f.h1.b^{2} - error.bench^{2}

DL.var.f.h1.b <- error.var.f.h1.b^{2} - error.bench^{2}

DL.favar.f.h1.b <- error.favar.f.h1.b^{2} - error.bench^{2}

# Esquema recursivo.

DL.arima.rec.h1.b <- error.arima.rec.h1.b^{2} - error.bench^{2}

DL.arimax.rec.h1.b <- error.arimax.rec.h1.b^{2} - error.bench^{2}

DL.ets.rec.h1.b <- error.ets.rec.h1.b^{2} - error.bench^{2}

DL.var.rec.h1.b <- error.var.rec.h1.b^{2} - error.bench^{2}

DL.favar.rec.h1.b <- error.favar.rec.h1.b^{2} - error.bench^{2}

# Esquema rolling. 

DL.arima.rol.h1.b <- error.arima.rol.h1.b^{2} - error.bench^{2}

DL.arimax.rol.h1.b <- error.arimax.rol.h1.b^{2} - error.bench^{2}

DL.ets.rol.h1.b <- error.ets.rol.h1.b^{2} - error.bench^{2}

DL.var.rol.h1.b <- error.var.rol.h1.b^{2} - error.bench^{2}

DL.favar.rol.h1.b <- error.favar.rol.h1.b^{2} - error.bench^{2}

### h=2 

# Esquema fijo.

DL.arima.f.h2 <- error.arima.f.h2^{2} - error.bench[-1,]^{2}

DL.arimax.f.h2 <- error.arimax.f.h2^{2} - error.bench[-1,]^{2}

DL.ets.f.h2 <- error.ets.f.h2^{2} - error.bench[-1,]^{2}

DL.adl.f.h2 <- error.adl.f.h2^{2} - error.bench[-1,]^{2}

DL.var.f.h2 <- error.var.f.h2^{2} - error.bench[-1,]^{2}

DL.favar.f.h2 <- error.favar.f.h2^{2} - error.bench[-1,]^{2}

# Esquema recursivo.

DL.arima.rec.h2 <- error.arima.rec.h2^{2} - error.bench[-1,]^{2}

DL.arimax.rec.h2 <- error.arimax.rec.h2^{2} - error.bench[-1,]^{2}

DL.ets.rec.h2 <- error.ets.rec.h2^{2} - error.bench[-1,]^{2}

DL.adl.rec.h2 <- error.adl.rec.h2^{2} - error.bench[-1,]^{2}

DL.var.rec.h2 <- error.var.rec.h2^{2} - error.bench[-1,]^{2}

DL.favar.rec.h2 <- error.favar.rec.h2^{2} - error.bench[-1,]^{2}

# Esquema rolling. 

DL.arima.rol.h2 <- error.arima.rol.h2^{2} - error.bench[-1,]^{2}

DL.arimax.rol.h2 <- error.arimax.rol.h2^{2} - error.bench[-1,]^{2}

DL.ets.rol.h2 <- error.ets.rol.h2^{2} - error.bench[-1,]^{2}

DL.adl.rol.h2 <- error.adl.rol.h2^{2} - error.bench[-1,]^{2}

DL.var.rol.h2 <- error.var.rol.h2^{2} - error.bench[-1,]^{2}

DL.favar.rol.h2 <- error.favar.rol.h2^{2} - error.bench[-1,]^{2}

# Bagging 

# Esquema fijo.

DL.arima.f.h2.b <- error.arima.f.h2.b^{2} - error.bench[-1,]^{2}

DL.arimax.f.h2.b <- error.arimax.f.h2.b^{2} - error.bench[-1,]^{2}

DL.ets.f.h2.b <- error.ets.f.h2.b^{2} - error.bench[-1,]^{2}

DL.var.f.h2.b <- error.var.f.h2.b^{2} - error.bench[-1,]^{2}

DL.favar.f.h2.b <- error.favar.f.h2.b^{2} - error.bench[-1,]^{2}

# Esquema recursivo. 

DL.arima.rec.h2.b <- error.arima.rec.h2.b^{2} - error.bench[-1,]^{2}

DL.arimax.rec.h2.b <- error.arimax.rec.h2.b^{2} - error.bench[-1,]^{2}

DL.ets.rec.h2.b <- error.ets.rec.h2.b^{2} - error.bench[-1,]^{2}

DL.var.rec.h2.b <- error.var.rec.h2.b^{2} - error.bench[-1,]^{2}

DL.favar.rec.h2.b <- error.favar.rec.h2.b^{2} - error.bench[-1,]^{2}

# Esquema rolling. 

DL.arima.rol.h2.b <- error.arima.rol.h2.b^{2} - error.bench[-1,]^{2}

DL.arimax.rol.h2.b <- error.arimax.rol.h2.b^{2} - error.bench[-1,]^{2}

DL.ets.rol.h2.b <- error.ets.rol.h2.b^{2} - error.bench[-1,]^{2}

DL.var.rol.h2.b <- error.var.rol.h2.b^{2} - error.bench[-1,]^{2}

DL.favar.rol.h2.b <- error.favar.rol.h2.b^{2} - error.bench[-1,]^{2}

### h = 7 

# Esquema fijo.

DL.arima.f.h7 <- error.arima.f.h7^{2}  - error.bench^{2}

DL.arimax.f.h7 <- error.arimax.f.h7^{2}  - error.bench[7:58,]^{2}

DL.ets.f.h7 <- error.ets.f.h7^{2}  - error.bench[7:58,]^{2}

DL.adl.f.h7 <- error.adl.f.h7^{2}  - error.bench[7:58,]^{2}

DL.var.f.h7 <- error.var.f.h7^{2}  - error.bench[7:58,]^{2}

DL.favar.f.h7 <- error.favar.f.h7^{2}  - error.bench[7:58,]^{2}

# Esquema recursivo.

DL.arima.rec.h7 <- error.arima.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.arimax.rec.h7 <- error.arimax.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.ets.rec.h7 <- error.ets.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.adl.rec.h7 <- error.adl.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.var.rec.h7 <- error.var.rec.h7^{2}  - error.bench[7:58,]^{2}

DL.favar.rec.h7 <- error.favar.rec.h7^{2}  - error.bench[7:58,]^{2}

# Esquema rolling. 

DL.arima.rol.h7 <- error.arima.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.arimax.rol.h7 <- error.arimax.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.ets.rol.h7 <- error.ets.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.adl.rol.h7 <- error.adl.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.var.rol.h7 <- error.var.rol.h7^{2}  - error.bench[7:58,]^{2}

DL.favar.rol.h7 <- error.favar.rol.h7^{2}  - error.bench[7:58,]^{2}

# Bagging 

# Esquema fijo.

DL.arima.f.h7.b <- error.arima.f.h7.b^{2}  - error.bench[7:58,]^{2}

DL.arimax.f.h7.b <- error.arimax.f.h7.b^{2}  - error.bench[7:58,]^{2}

DL.ets.f.h7.b <- error.ets.f.h7.b^{2}  - error.bench[7:58,]^{2}

DL.var.f.h7.b <- error.var.f.h7.b^{2}  - error.bench[7:58,]^{2}

DL.favar.f.h7.b <- error.favar.f.h7.b^{2}  - error.bench[7:58,]^{2}

# Esquema recursivo. 

DL.arima.rec.h7.b <- error.arima.rec.h7.b^{2}  - error.bench[7:58,]^{2}

DL.arimax.rec.h7.b <- error.arimax.rec.h7.b^{2}  - error.bench[7:58,]^{2}

DL.ets.rec.h7.b <- error.ets.rec.h7.b^{2}  - error.bench[7:58,]^{2}

DL.var.rec.h7.b <- error.var.rec.h7.b^{2}  - error.bench[7:58,]^{2}

DL.favar.rec.h7.b <- error.favar.rec.h7.b^{2}  - error.bench[7:58,]^{2}

# Esquema rolling.

DL.arima.rol.h7.b <- error.arima.rol.h7.b^{2}  - error.bench[7:58,]^{2}

DL.arimax.rol.h7.b <- error.arimax.rol.h7.b^{2}  - error.bench[7:58,]^{2}

DL.ets.rol.h7.b <- error.ets.rol.h7.b^{2}  - error.bench[7:58,]^{2}

DL.var.rol.h7.b <- error.var.rol.h7.b^{2}  - error.bench[7:58,]^{2}

DL.favar.rol.h7.b <- error.favar.rol.h7.b^{2}  - error.bench[7:58,]^{2}

# Realizamos el test de Diebold Mariano, ajustamos por NW para cuando h>1.

#### h = 1 

# Estadístico.

AC[2,5]<-round(summary(lm(DL.arima.f.h1~1))$coefficients[3],4)
AC[3,5]<-round(summary(lm(DL.arimax.f.h1~1))$coefficients[3],4)
AC[4,5]<-round(summary(lm(DL.ets.f.h1~1))$coefficients[3],4)
AC[5,5]<-round(summary(lm(DL.adl.f.h1~1))$coefficients[3],4)
AC[6,5]<-round(summary(lm(DL.var.f.h1~1))$coefficients[3],4)
AC[7,5]<-round(summary(lm(DL.favar.f.h1~1))$coefficients[3],4)

AC[8,5]<-round(summary(lm(DL.arima.rec.h1~1))$coefficients[3],4)
AC[9,5]<-round(summary(lm(DL.arimax.rec.h1~1))$coefficients[3],4)
AC[10,5]<-round(summary(lm(DL.ets.rec.h1~1))$coefficients[3],4)
AC[11,5]<-round(summary(lm(DL.adl.rec.h1~1))$coefficients[3],4)
AC[12,5]<-round(summary(lm(DL.var.rec.h1~1))$coefficients[3],4)
AC[13,5]<-round(summary(lm(DL.favar.rec.h1~1))$coefficients[3],4)

AC[14,5]<-round(summary(lm(DL.arima.rol.h1~1))$coefficients[3],4)
AC[15,5]<-round(summary(lm(DL.arimax.rol.h1~1))$coefficients[3],4)
AC[16,5]<-round(summary(lm(DL.ets.rol.h1~1))$coefficients[3],4)
AC[17,5]<-round(summary(lm(DL.adl.rec.h1~1))$coefficients[3],4)
AC[18,5]<-round(summary(lm(DL.var.rec.h1~1))$coefficients[3],4)
AC[19,5]<-round(summary(lm(DL.favar.rec.h1~1))$coefficients[3],4)

# Bagging.

AC[20,5]<-round(summary(lm(DL.arima.rol.h1.b~1))$coefficients[3],4)
AC[21,5]<-round(summary(lm(DL.arimax.f.h1.b~1))$coefficients[3],4)
AC[22,5]<-round(summary(lm(DL.ets.f.h1.b~1))$coefficients[3],4)
AC[23,5]<-round(summary(lm(DL.var.f.h1.b~1))$coefficients[3],4)
AC[24,5]<-round(summary(lm(DL.favar.f.h1.b~1))$coefficients[3],4)

AC[25,5]<-round(summary(lm(DL.arima.rec.h1.b~1))$coefficients[3],4)
AC[26,5]<-round(summary(lm(DL.arimax.rec.h1.b~1))$coefficients[3],4)
AC[27,5]<-round(summary(lm(DL.ets.rec.h1.b~1))$coefficients[3],4)
AC[28,5]<-round(summary(lm(DL.var.rec.h1.b~1))$coefficients[3],4)
AC[29,5]<-round(summary(lm(DL.favar.rec.h1.b~1))$coefficients[3],4)

AC[30,5]<-round(summary(lm(DL.arima.rol.h1.b~1))$coefficients[3],4)
AC[31,5]<-round(summary(lm(DL.arimax.rol.h1.b~1))$coefficients[3],4)
AC[32,5]<-round(summary(lm(DL.ets.rol.h1.b~1))$coefficients[3],4)
AC[33,5]<-round(summary(lm(DL.var.rol.h1.b~1))$coefficients[3],4)
AC[34,5]<-round(summary(lm(DL.favar.rol.h1.b~1))$coefficients[3],4)

# P-value.

AC[2,6]<-round(summary(lm(DL.arima.f.h1~1))$coefficients[4],4)
AC[3,6]<-round(summary(lm(DL.arimax.f.h1~1))$coefficients[4],4)
AC[4,6]<-round(summary(lm(DL.ets.f.h1~1))$coefficients[4],4)
AC[5,6]<-round(summary(lm(DL.adl.f.h1~1))$coefficients[4],4)
AC[6,6]<-round(summary(lm(DL.var.f.h1~1))$coefficients[4],4)
AC[7,6]<-round(summary(lm(DL.favar.f.h1~1))$coefficients[4],4)

AC[8,6]<-round(summary(lm(DL.arima.rec.h1~1))$coefficients[4],4)
AC[9,6]<-round(summary(lm(DL.arimax.rec.h1~1))$coefficients[4],4)
AC[10,6]<-round(summary(lm(DL.ets.rec.h1~1))$coefficients[4],4)
AC[11,6]<-round(summary(lm(DL.adl.rec.h1~1))$coefficients[4],4)
AC[12,6]<-round(summary(lm(DL.var.rec.h1~1))$coefficients[4],4)
AC[13,6]<-round(summary(lm(DL.favar.rec.h1~1))$coefficients[4],4)

AC[14,6]<-round(summary(lm(DL.arima.rol.h1~1))$coefficients[4],4)
AC[15,6]<-round(summary(lm(DL.arimax.rol.h1~1))$coefficients[4],4)
AC[16,6]<-round(summary(lm(DL.ets.rol.h1~1))$coefficients[4],4)
AC[17,6]<-round(summary(lm(DL.adl.rec.h1~1))$coefficients[4],4)
AC[18,6]<-round(summary(lm(DL.var.rec.h1~1))$coefficients[4],4)
AC[19,6]<-round(summary(lm(DL.arima.f.h1.b~1))$coefficients[4],4)

# Bagging. 

AC[20,6]<-round(summary(lm(DL.arima.f.h1.b~1))$coefficients[4],4)
AC[21,6]<-round(summary(lm(DL.arimax.f.h1.b~1))$coefficients[4],4)
AC[22,6]<-round(summary(lm(DL.ets.f.h1.b~1))$coefficients[4],4)
AC[23,6]<-round(summary(lm(DL.var.f.h1.b~1))$coefficients[4],4)
AC[24,6]<-round(summary(lm(DL.favar.rec.h1.b~1))$coefficients[4],4)

AC[25,6]<-round(summary(lm(DL.arima.rec.h1.b~1))$coefficients[4],4)
AC[26,6]<-round(summary(lm(DL.arimax.rec.h1.b~1))$coefficients[4],4)
AC[27,6]<-round(summary(lm(DL.ets.rec.h1.b~1))$coefficients[4],4)
AC[28,6]<-round(summary(lm(DL.var.rec.h1.b~1))$coefficients[4],4)
AC[29,6]<-round(summary(lm(DL.favar.rec.h1.b~1))$coefficients[4],4)

AC[30,6]<-round(summary(lm(DL.arima.rol.h1.b~1))$coefficients[4],4)
AC[31,6]<-round(summary(lm(DL.arimax.rol.h1.b~1))$coefficients[4],4)
AC[32,6]<-round(summary(lm(DL.ets.rol.h1.b~1))$coefficients[4],4)
AC[33,6]<-round(summary(lm(DL.var.rol.h1.b~1))$coefficients[4],4)
AC[34,6]<-round(summary(lm(DL.favar.rol.h1.b~1))$coefficients[4],4)

### h = 2

# Estadístico. 

AC1[2,5]<-round(summary(lm(DL.arima.f.h2~1))$coefficients[3],4)
AC1[3,5]<-round(summary(lm(DL.arimax.f.h2~1))$coefficients[3],4)
AC1[4,5]<-round(summary(lm(DL.ets.f.h2~1))$coefficients[3],4)
AC1[5,5]<-round(summary(lm(DL.adl.f.h2~1))$coefficients[3],4)
AC1[6,5]<-round(summary(lm(DL.var.f.h2~1))$coefficients[3],4)
AC1[7,5]<-round(summary(lm(DL.favar.f.h2~1))$coefficients[3],4)

AC1[8,5]<-round(summary(lm(DL.arima.rec.h2~1))$coefficients[3],4)
AC1[9,5]<-round(summary(lm(DL.arimax.rec.h2~1))$coefficients[3],4)
AC1[10,5]<-round(summary(lm(DL.ets.rec.h2~1))$coefficients[3],4)
AC1[11,5]<-round(summary(lm(DL.adl.rec.h2~1))$coefficients[3],4)
AC1[12,5]<-round(summary(lm(DL.var.rec.h2~1))$coefficients[3],4)
AC1[13,5]<-round(summary(lm(DL.favar.rec.h2~1))$coefficients[3],4)

AC1[14,5]<-round(summary(lm(DL.arima.rol.h2~1))$coefficients[3],4)
AC1[15,5]<-round(summary(lm(DL.arimax.rol.h2~1))$coefficients[3],4)
AC1[16,5]<-round(summary(lm(DL.ets.rol.h2~1))$coefficients[3],4)
AC1[17,5]<-round(summary(lm(DL.adl.rec.h2~1))$coefficients[3],4)
AC1[18,5]<-round(summary(lm(DL.var.rec.h2~1))$coefficients[3],4)
AC1[19,5]<-round(summary(lm(DL.favar.rec.h2~1))$coefficients[3],4)

# Bagging.

AC1[20,5]<-round(summary(lm(DL.arima.rol.h2.b~1))$coefficients[3],4)
AC1[21,5]<-round(summary(lm(DL.arimax.f.h2.b~1))$coefficients[3],4)
AC1[22,5]<-round(summary(lm(DL.ets.f.h2.b~1))$coefficients[3],4)
AC1[23,5]<-round(summary(lm(DL.var.f.h2.b~1))$coefficients[3],4)
AC1[24,5]<-round(summary(lm(DL.favar.f.h2.b~1))$coefficients[3],4)

AC1[25,5]<-round(summary(lm(DL.arima.rec.h2.b~1))$coefficients[3],4)
AC1[26,5]<-round(summary(lm(DL.arimax.rec.h2.b~1))$coefficients[3],4)
AC1[27,5]<-round(summary(lm(DL.ets.rec.h2.b~1))$coefficients[3],4)
AC1[28,5]<-round(summary(lm(DL.var.rec.h2.b~1))$coefficients[3],4)
AC1[29,5]<-round(summary(lm(DL.favar.rec.h2.b~1))$coefficients[3],4)

AC1[30,5]<-round(summary(lm(DL.arima.rol.h2.b~1))$coefficients[3],4)
AC1[31,5]<-round(summary(lm(DL.arimax.rol.h2.b~1))$coefficients[3],4)
AC1[32,5]<-round(summary(lm(DL.ets.rol.h2.b~1))$coefficients[3],4)
AC1[33,5]<-round(summary(lm(DL.var.rol.h2.b~1))$coefficients[3],4)
AC1[34,5]<-round(summary(lm(DL.favar.rol.h2.b~1))$coefficients[3],4)

# P-value.

# Esquema fijo.

AC1[2,6]<-round(coeftest(lm(DL.arima.f.h2~1), vcov = NeweyWest(lm(DL.arima.f.h2~1)))[4],4)
AC1[3,6]<-round(coeftest(lm(DL.arimax.f.h2~1), vcov = NeweyWest(lm(DL.arimax.f.h2~1)))[4],4)
AC1[4,6]<-round(coeftest(lm(DL.ets.f.h2~1), vcov = NeweyWest(lm(DL.ets.f.h2~1)))[4],4)
AC1[5,6]<-round(coeftest(lm(DL.adl.f.h2~1), vcov = NeweyWest(lm(DL.arimax.f.h2~1)))[4],4)
AC1[6,6]<-round(coeftest(lm(DL.var.f.h2~1), vcov = NeweyWest(lm(DL.var.f.h2~1)))[4],4)
AC1[7,6]<-round(coeftest(lm(DL.favar.f.h2~1), vcov = NeweyWest(lm(DL.favar.f.h2~1)))[4],4)

# Esquema recursivo.

AC1[8,6]<-round(coeftest(lm(DL.arima.rec.h2~1), vcov = NeweyWest(lm(DL.arima.rec.h2~1)))[4],4)
AC1[9,6]<-round(coeftest(lm(DL.arimax.rec.h2~1), vcov = NeweyWest(lm(DL.arimax.rec.h2~1)))[4],4)
AC1[10,6]<-round(coeftest(lm(DL.ets.rec.h2~1), vcov = NeweyWest(lm(DL.ets.rec.h2~1)))[4],4)
AC1[11,6]<-round(coeftest(lm(DL.adl.rec.h2~1), vcov = NeweyWest(lm(DL.arimax.rec.h2~1)))[4],4)
AC1[12,6]<-round(coeftest(lm(DL.var.rec.h2~1), vcov = NeweyWest(lm(DL.var.rec.h2~1)))[4],4)
AC1[13,6]<-round(coeftest(lm(DL.favar.rec.h2~1), vcov = NeweyWest(lm(DL.favar.rec.h2~1)))[4],4)

# Esquema rolling. 

AC1[14,6]<-round(coeftest(lm(DL.arima.rol.h2~1), vcov = NeweyWest(lm(DL.arima.rol.h2~1)))[4],4)
AC1[15,6]<-round(coeftest(lm(DL.arimax.rol.h2~1), vcov = NeweyWest(lm(DL.arimax.rol.h2~1)))[4],4)
AC1[16,6]<-round(coeftest(lm(DL.ets.rol.h2~1), vcov = NeweyWest(lm(DL.ets.rol.h2~1)))[4],4)
AC1[17,6]<-round(coeftest(lm(DL.adl.rol.h2~1), vcov = NeweyWest(lm(DL.arimax.rol.h2~1)))[4],4)
AC1[18,6]<-round(coeftest(lm(DL.var.rol.h2~1), vcov = NeweyWest(lm(DL.var.rol.h2~1)))[4],4)
AC1[19,6]<-round(coeftest(lm(DL.favar.rol.h2~1), vcov = NeweyWest(lm(DL.favar.rol.h2~1)))[4],4)

# Bagging.

AC1[20,6]<-round(coeftest(lm(DL.arima.f.h2.b~1), vcov = NeweyWest(lm(DL.arima.f.h2.b~1)))[4],4)
AC1[21,6]<-round(coeftest(lm(DL.arimax.f.h2.b~1), vcov = NeweyWest(lm(DL.arimax.f.h2.b~1)))[4],4)
AC1[22,6]<-round(coeftest(lm(DL.ets.f.h2.b~1), vcov = NeweyWest(lm(DL.ets.f.h2.b~1)))[4],4)
AC1[23,6]<-round(coeftest(lm(DL.var.f.h2.b~1), vcov = NeweyWest(lm(DL.var.f.h2.b~1)))[4],4)
AC1[24,6]<-round(coeftest(lm(DL.favar.f.h2.b~1), vcov = NeweyWest(lm(DL.favar.f.h2.b~1)))[4],4)

AC1[25,6]<-round(coeftest(lm(DL.arima.rec.h2.b~1), vcov = NeweyWest(lm(DL.arima.rec.h2.b~1)))[4],4)
AC1[26,6]<-round(coeftest(lm(DL.arimax.rec.h2.b~1), vcov = NeweyWest(lm(DL.arimax.rec.h2.b~1)))[4],4)
AC1[27,6]<-round(coeftest(lm(DL.ets.rec.h2.b~1), vcov = NeweyWest(lm(DL.ets.rec.h2.b~1)))[4],4)
AC1[28,6]<-round(coeftest(lm(DL.var.rec.h2.b~1), vcov = NeweyWest(lm(DL.var.rec.h2.b~1)))[4],4)
AC1[29,6]<-round(coeftest(lm(DL.favar.rec.h2.b~1), vcov = NeweyWest(lm(DL.favar.rec.h2~1)))[4],4)

AC1[30,6]<-round(coeftest(lm(DL.arima.rol.h2.b~1), vcov = NeweyWest(lm(DL.arima.rol.h2.b~1)))[4],4)
AC1[31,6]<-round(coeftest(lm(DL.arimax.rol.h2.b~1), vcov = NeweyWest(lm(DL.arimax.rol.h2.b~1)))[4],4)
AC1[32,6]<-round(coeftest(lm(DL.ets.rol.h2.b~1), vcov = NeweyWest(lm(DL.ets.rol.h2.b~1)))[4],4)
AC1[33,6]<-round(coeftest(lm(DL.var.rol.h2.b~1), vcov = NeweyWest(lm(DL.var.rol.h2.b~1)))[4],4)
AC1[34,6]<-round(coeftest(lm(DL.favar.rol.h2.b~1), vcov = NeweyWest(lm(DL.favar.rol.h2.b~1)))[4],4)

### h= 7 

# Estadístico.

AC4[2,5]<-round(summary(lm(DL.arima.f.h7~1))$coefficients[3],4)
AC4[3,5]<-round(summary(lm(DL.arimax.f.h7~1))$coefficients[3],4)
AC4[4,5]<-round(summary(lm(DL.ets.f.h7~1))$coefficients[3],4)
AC4[5,5]<-round(summary(lm(DL.adl.f.h7~1))$coefficients[3],4)
AC4[6,5]<-round(summary(lm(DL.var.f.h7~1))$coefficients[3],4)
AC4[7,5]<-round(summary(lm(DL.favar.f.h7~1))$coefficients[3],4)

AC4[8,5]<-round(summary(lm(DL.arima.rec.h7~1))$coefficients[3],4)
AC4[9,5]<-round(summary(lm(DL.arimax.rec.h7~1))$coefficients[3],4)
AC4[10,5]<-round(summary(lm(DL.ets.rec.h7~1))$coefficients[3],4)
AC4[11,5]<-round(summary(lm(DL.adl.rec.h7~1))$coefficients[3],4)
AC4[12,5]<-round(summary(lm(DL.var.rec.h7~1))$coefficients[3],4)
AC4[13,5]<-round(summary(lm(DL.favar.rec.h7~1))$coefficients[3],4)

AC4[14,5]<-round(summary(lm(DL.arima.rol.h7~1))$coefficients[3],4)
AC4[15,5]<-round(summary(lm(DL.arimax.rol.h7~1))$coefficients[3],4)
AC4[16,5]<-round(summary(lm(DL.ets.rol.h7~1))$coefficients[3],4)
AC4[17,5]<-round(summary(lm(DL.adl.rec.h7~1))$coefficients[3],4)
AC4[18,5]<-round(summary(lm(DL.var.rec.h7~1))$coefficients[3],4)
AC4[19,5]<-round(summary(lm(DL.favar.rec.h7~1))$coefficients[3],4)

# Bagging.

AC4[20,5]<-round(summary(lm(DL.arima.rol.h7.b~1))$coefficients[3],4)
AC4[21,5]<-round(summary(lm(DL.arimax.f.h7.b~1))$coefficients[3],4)
AC4[22,5]<-round(summary(lm(DL.ets.f.h7.b~1))$coefficients[3],4)
AC4[23,5]<-round(summary(lm(DL.var.f.h7.b~1))$coefficients[3],4)
AC4[24,5]<-round(summary(lm(DL.favar.f.h7.b~1))$coefficients[3],4)

AC4[25,5]<-round(summary(lm(DL.arima.rec.h7.b~1))$coefficients[3],4)
AC4[26,5]<-round(summary(lm(DL.arimax.rec.h7.b~1))$coefficients[3],4)
AC4[27,5]<-round(summary(lm(DL.ets.rec.h7.b~1))$coefficients[3],4)
AC4[28,5]<-round(summary(lm(DL.var.rec.h7.b~1))$coefficients[3],4)
AC4[29,5]<-round(summary(lm(DL.favar.rec.h7.b~1))$coefficients[3],4)

AC4[30,5]<-round(summary(lm(DL.arima.rol.h7.b~1))$coefficients[3],4)
AC4[31,5]<-round(summary(lm(DL.arimax.rol.h7.b~1))$coefficients[3],4)
AC4[32,5]<-round(summary(lm(DL.ets.rol.h7.b~1))$coefficients[3],4)
AC4[33,5]<-round(summary(lm(DL.var.rol.h7.b~1))$coefficients[3],4)
AC4[34,5]<-round(summary(lm(DL.favar.rol.h7.b~1))$coefficients[3],4)

# P-value. 

# Esquema fijo. 

AC4[2,6]<-round(coeftest(lm(DL.arima.f.h7~1), vcov = NeweyWest(lm(DL.arima.f.h7~1)))[4],4)
AC4[3,6]<-round(coeftest(lm(DL.arimax.f.h7~1), vcov = NeweyWest(lm(DL.arimax.f.h7~1)))[4],4)
AC4[4,6]<-round(coeftest(lm(DL.ets.f.h7~1), vcov = NeweyWest(lm(DL.ets.f.h7~1)))[4],4)
AC4[5,6]<-round(coeftest(lm(DL.adl.f.h7~1), vcov = NeweyWest(lm(DL.arimax.f.h7~1)))[4],4)
AC4[6,6]<-round(coeftest(lm(DL.var.f.h7~1), vcov = NeweyWest(lm(DL.var.f.h7~1)))[4],4)
AC4[7,6]<-round(coeftest(lm(DL.favar.f.h7~1), vcov = NeweyWest(lm(DL.favar.f.h7~1)))[4],4)

# Esquema recursivo. 

AC4[8,6]<-round(coeftest(lm(DL.arima.rec.h7~1), vcov = NeweyWest(lm(DL.arima.rec.h7~1)))[4],4)
AC4[9,6]<-round(coeftest(lm(DL.arimax.rec.h7~1), vcov = NeweyWest(lm(DL.arimax.rec.h7~1)))[4],4)
AC4[10,6]<-round(coeftest(lm(DL.ets.rec.h7~1), vcov = NeweyWest(lm(DL.ets.rec.h7~1)))[4],4)
AC4[11,6]<-round(coeftest(lm(DL.adl.rec.h7~1), vcov = NeweyWest(lm(DL.arimax.rec.h7~1)))[4],4)
AC4[12,6]<-round(coeftest(lm(DL.var.rec.h7~1), vcov = NeweyWest(lm(DL.var.rec.h7~1)))[4],4)
AC4[13,6]<-round(coeftest(lm(DL.favar.rec.h7~1), vcov = NeweyWest(lm(DL.favar.rec.h7~1)))[4],4)

# Esquema rolling. 

AC4[14,6]<-round(coeftest(lm(DL.arima.rol.h7~1), vcov = NeweyWest(lm(DL.arima.rol.h7~1)))[4],4)
AC4[15,6]<-round(coeftest(lm(DL.arimax.rol.h7~1), vcov = NeweyWest(lm(DL.arimax.rol.h7~1)))[4],4)
AC4[16,6]<-round(coeftest(lm(DL.ets.rol.h7~1), vcov = NeweyWest(lm(DL.ets.rol.h7~1)))[4],4)
AC4[17,6]<-round(coeftest(lm(DL.adl.rol.h7~1), vcov = NeweyWest(lm(DL.arimax.rol.h7~1)))[4],4)
AC4[18,6]<-round(coeftest(lm(DL.var.rol.h7~1), vcov = NeweyWest(lm(DL.var.rol.h7~1)))[4],4)
AC4[19,6]<-round(coeftest(lm(DL.favar.rol.h7~1), vcov = NeweyWest(lm(DL.favar.rol.h7~1)))[4],4)

# Bagging.

AC4[20,6]<-round(coeftest(lm(DL.arima.f.h7.b~1), vcov = NeweyWest(lm(DL.arima.f.h7.b~1)))[4],4)
AC4[21,6]<-round(coeftest(lm(DL.arimax.f.h7.b~1), vcov = NeweyWest(lm(DL.arimax.f.h7.b~1)))[4],4)
AC4[22,6]<-round(coeftest(lm(DL.ets.f.h7.b~1), vcov = NeweyWest(lm(DL.ets.f.h7.b~1)))[4],4)
AC4[23,6]<-round(coeftest(lm(DL.var.f.h7.b~1), vcov = NeweyWest(lm(DL.var.f.h7.b~1)))[4],4)
AC4[24,6]<-round(coeftest(lm(DL.favar.f.h7.b~1), vcov = NeweyWest(lm(DL.favar.f.h7.b~1)))[4],4)

AC4[25,6]<-round(coeftest(lm(DL.arima.rec.h7.b~1), vcov = NeweyWest(lm(DL.arima.rec.h7.b~1)))[4],4)
AC4[26,6]<-round(coeftest(lm(DL.arimax.rec.h7.b~1), vcov = NeweyWest(lm(DL.arimax.rec.h7.b~1)))[4],4)
AC4[27,6]<-round(coeftest(lm(DL.ets.rec.h7.b~1), vcov = NeweyWest(lm(DL.ets.rec.h7.b~1)))[4],4)
AC4[28,6]<-round(coeftest(lm(DL.var.rec.h7.b~1), vcov = NeweyWest(lm(DL.var.rec.h7.b~1)))[4],4)
AC4[29,6]<-round(coeftest(lm(DL.favar.rec.h7.b~1), vcov = NeweyWest(lm(DL.favar.rec.h7~1)))[4],4)

AC4[30,6]<-round(coeftest(lm(DL.arima.rol.h7.b~1), vcov = NeweyWest(lm(DL.arima.rol.h7.b~1)))[4],4)
AC4[31,6]<-round(coeftest(lm(DL.arimax.rol.h7.b~1), vcov = NeweyWest(lm(DL.arimax.rol.h7.b~1)))[4],4)
AC4[32,6]<-round(coeftest(lm(DL.ets.rol.h7.b~1), vcov = NeweyWest(lm(DL.ets.rol.h7.b~1)))[4],4)
AC4[33,6]<-round(coeftest(lm(DL.var.rol.h7.b~1), vcov = NeweyWest(lm(DL.var.rol.h7.b~1)))[4],4)
AC4[34,6]<-round(coeftest(lm(DL.favar.rol.h7.b~1), vcov = NeweyWest(lm(DL.favar.rol.h7.b~1)))[4],4)

# Exportamos las tablas.

# Para h=1

colnames(AC) <- c("Modelo", "RMSE", "MAE", "MAPE", "DM", "p-val")
stargazer(AC[1:nrow(AC),], type = "latex")

# Para h=2

colnames(AC1) <- c("Modelo", "RMSE", "MAE", "MAPE", "DM", "p-val")
AC1[1,1:ncol(AC1)] <- AC[1,1:ncol(AC)]
stargazer(AC1[1:nrow(AC1),], type = "latex")

# Para h=7

colnames(AC4) <- c("Modelo", "RMSE", "MAE", "MAPE", "DM", "p-val")
AC4[1,1:ncol(AC4)] <- AC[1,1:ncol(AC)]
stargazer(AC4[1:nrow(AC4),], type = "latex")


### MEDIDAS DE ACCURACY COMPARABLES.

# En esta sección ajustaremos la cantidad de observaciones de los pronósticos
# para comparar todos los modelos.

AC<-matrix(NA,34,4)
colnames(AC) <- c("Modelo", "MAPE", "MAE", "RMSE")
AC2<-matrix(NA,34,1)

# Tabla 1

# Medidas de accuracy y test de DM de los pronósticos con esquema fijo y h=1.

AC[1,1]<-"AR(1)" 
AC2[1,1]<-"1"
AC[1,2:4]<-round(accuracy(pr.bench[7:58,], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[2,1]<-"ARIMA fijo" 
AC2[2,1]<-"1"
AC[2,2:4]<-round(accuracy(pr.f.h1[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[3,1]<-"ARIMAX fijo" 
AC2[3,1]<-"1"
AC[3,2:4]<-round(accuracy(pr.f.h1[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[4,1]<-"ETS fijo" 
AC2[4,1]<-"1"
AC[4,2:4]<-round(accuracy(pr.f.h1[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[5,1]<- "ADL fijo" 
AC2[5,1]<-"1"
AC[5,2:4]<-round(accuracy(pr.f.h1[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[6,1]<- "VAR fijo"
AC2[6,1]<-"1"
AC[6,2:4]<-round(accuracy(pr.f.h1[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[7,1]<- "FAVAR fijo" 
AC2[7,1]<-"1"
AC[7,2:4]<-round(accuracy(pr.f.h1[7:58,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling y h=1.

AC[8,1]<-"ARIMA rolling" 
AC2[8,1]<-"1"
AC[8,2:4]<-round(accuracy(pr.rol.h1[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[9,1]<-"ARIMAX rolling" 
AC2[9,1]<-"1"
AC[9,2:4]<-round(accuracy(pr.rol.h1[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[10,1]<-"ETS rolling" 
AC2[10,1]<-"1"
AC[10,2:4]<-round(accuracy(pr.rol.h1[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[11,1]<- "ADL rolling" 
AC2[11,1]<-"1"
AC[11,2:4]<-round(accuracy(pr.rol.h1[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[12,1]<- "VAR rolling"
AC2[12,1]<-"1"
AC[12,2:4]<-round(accuracy(pr.rol.h1[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[13,1]<- "FAVAR rolling" 
AC2[13,1]<-"1"
AC[13,2:4]<-round(accuracy(pr.rol.h1[7:58,6], out.of.sample[7:58,2])[c(2:3,5)],4)

#  Medidas de accuracy de los pronósticos con esquema recursivo y h=1.

AC[14,1]<-"ARIMA recursivo" 
AC2[14,1]<-"1"
AC[14,2:4]<-round(accuracy(pr.rec.h1[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[15,1]<-"ARIMAX recursivo" 
AC2[15,1]<-"1"
AC[15,2:4]<-round(accuracy(pr.rec.h1[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[16,1]<-"ETS recursivo" 
AC2[16,1]<-"1"
AC[16,2:4]<-round(accuracy(pr.rec.h1[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[17,1]<- "ADL recursivo" 
AC2[17,1]<-"1"
AC[17,2:4]<-round(accuracy(pr.rec.h1[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[18,1]<- "VAR recursivo"
AC2[18,1]<-"1"
AC[18,2:4]<-round(accuracy(pr.rec.h1[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[19,1]<- "FAVAR recursivo" 
AC2[19,1]<-"1"
AC[19,2:4]<-round(accuracy(pr.rec.h1[7:58,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 1 y series boots. 

AC[20,1]<-"ARIMA fijo bagged" 
AC2[20,1]<-"1"
AC[20,2:4]<-round(accuracy(pr.f.h1.b[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[21,1]<-"ARIMAX fijo bagged" 
AC2[21,1]<-"1"
AC[21,2:4]<-round(accuracy(pr.f.h1.b[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[22,1]<-"ETS fijo bagged" 
AC2[22,1]<-"1"
AC[22,2:4]<-round(accuracy(pr.f.h1.b[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[23,1]<- "VAR fijo bagged" 
AC2[23,1]<-"1"
AC[23,2:4]<-round(accuracy(pr.f.h1.b[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[24,1]<- "FAVAR fijo bagged"
AC2[24,1]<-"1"
AC[24,2:4]<-round(accuracy(pr.rec.h1.b[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC[25,1]<-"ARIMA recursivo bagged" 
AC2[25,1]<-"1"
AC[25,2:4]<-round(accuracy(pr.rec.h1.b[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[26,1]<-"ARIMAX recursivo bagged" 
AC2[26,1]<-"1"
AC[26,2:4]<-round(accuracy(pr.rec.h1.b[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[27,1]<-"ETS recursivo bagged" 
AC2[27,1]<-"1"
AC[27,2:4]<-round(accuracy(pr.rec.h1.b[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[28,1]<- "VAR recursivo bagged" 
AC2[28,1]<-"1"
AC[28,2:4]<-round(accuracy(pr.rec.h1.b[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[29,1]<- "FAVAR recursivo bagged"
AC2[29,1]<-"1"
AC[29,2:4]<-round(accuracy(pr.rec.h1.b[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 1 y series boots.

AC[30,1]<-"ARIMA rolling bagged" 
AC2[230,1]<-"1"
AC[30,2:4]<-round(accuracy(pr.rol.h1.b[7:58,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[31,1]<-"ARIMAX rolling bagged" 
AC2[31,1]<-"1"
AC[32,2:4]<-round(accuracy(pr.rol.h1.b[7:58,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[32,1]<-"ETS rollingbagged" 
AC2[32,1]<-"1"
AC[32,2:4]<-round(accuracy(pr.rol.h1.b[7:58,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[33,1]<- "VAR rolling bagged" 
AC2[33,1]<-"1"
AC[33,2:4]<-round(accuracy(pr.rol.h1.b[7:58,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC[34,1]<- "FAVAR rolling bagged"
AC2[34,1]<-"1"
AC[34,2:4]<-round(accuracy(pr.rol.h1.b[7:58,4], out.of.sample[7:58,2])[c(2:3,5)],4)


# Tabla 2

AC1<-matrix(NA,34,4)
colnames(AC1) <- c("Modelo", "MAPE", "MAE", "RMSE")
AC3<-matrix(NA,34,1)

AC1[2,1]<-"ARIMA fijo" 
AC3[2,1]<-"2"
AC1[2,2:4]<-round(accuracy(pr.f.h2[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[3,1]<-"ARIMAX fijo" 
AC3[3,1]<-"2"
AC1[3,2:4]<-round(accuracy(pr.f.h2[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[4,1]<-"ETS fijo" 
AC3[4,1]<-"2"
AC1[4,2:4]<-round(accuracy(pr.f.h2[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[5,1]<- "ADL fijo" 
AC3[5,1]<-"2"
AC1[5,2:4]<-round(accuracy(pr.f.h2[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[6,1]<- "VAR fijo"
AC3[6,1]<-"2"
AC1[6,2:4]<-round(accuracy(pr.f.h2[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[7,1]<- "FAVAR fijo" 
AC3[7,1]<-"2"
AC1[7,2:4]<-round(accuracy(pr.f.h2[6:57,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling y h=2.

AC1[8,1]<-"ARIMA rolling" 
AC3[8,1]<-"2"
AC1[8,2:4]<-round(accuracy(pr.rol.h2[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[9,1]<-"ARIMAX rolling" 
AC3[9,1]<-"2"
AC1[9,2:4]<-round(accuracy(pr.rol.h2[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[10,1]<-"ETS rolling" 
AC3[10,1]<-"2"
AC1[10,2:4]<-round(accuracy(pr.rol.h2[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[11,1]<- "ADL rolling" 
AC3[11,1]<-"2"
AC1[11,2:4]<-round(accuracy(pr.rol.h2[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[12,1]<- "VAR rolling"
AC3[12,1]<-"2"
AC1[12,2:4]<-round(accuracy(pr.rol.h2[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[13,1]<- "FAVAR rolling" 
AC3[13,1]<-"2"
AC1[13,2:4]<-round(accuracy(pr.rol.h2[6:57,6], out.of.sample[7:58,2])[c(2:3,5)],4)

#  Medidas de accuracy de los pronósticos con esquema recursivo y h=2.

AC1[14,1]<-"ARIMA recursivo" 
AC3[14,1]<-"2"
AC1[14,2:4]<-round(accuracy(pr.rec.h2[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[15,1]<-"ARIMAX recursivo" 
AC3[15,1]<-"2"
AC1[15,2:4]<-round(accuracy(pr.rec.h2[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[16,1]<-"ETS recursivo" 
AC3[16,1]<-"2"
AC1[16,2:4]<-round(accuracy(pr.rec.h2[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[17,1]<- "ADL recursivo" 
AC3[17,1]<-"2"
AC1[17,2:4]<-round(accuracy(pr.rec.h2[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[18,1]<- "VAR recursivo"
AC3[18,1]<-"2"
AC1[18,2:4]<-round(accuracy(pr.rec.h2[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[19,1]<- "FAVAR recursivo" 
AC3[19,1]<-"2"
AC1[19,2:4]<-round(accuracy(pr.rec.h2[6:57,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots.

AC1[20,1]<-"ARIMA fijo bagged" 
AC3[20,1]<-"2"
AC1[20,2:4]<-round(accuracy(pr.f.h2.b[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[21,1]<-"ARIMAX fijo bagged" 
AC3[21,1]<-"2"
AC1[21,2:4]<-round(accuracy(pr.f.h2.b[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[22,1]<-"ETS fijo bagged" 
AC3[22,1]<-"2"
AC1[22,2:4]<-round(accuracy(pr.f.h2.b[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[23,1]<- "VAR fijo bagged" 
AC3[23,1]<-"2"
AC1[23,2:4]<-round(accuracy(pr.f.h2.b[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[24,1]<- "FAVAR fijo bagged"
AC3[24,1]<-"2"
AC1[24,2:4]<-round(accuracy(pr.rec.h2.b[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC1[25,1]<-"ARIMA recursivo bagged" 
AC3[25,1]<-"2"
AC1[25,2:4]<-round(accuracy(pr.rec.h2.b[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[26,1]<-"ARIMAX recursivo bagged" 
AC3[26,1]<-"2"
AC1[26,2:4]<-round(accuracy(pr.rec.h2.b[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[27,1]<-"ETS recursivo bagged" 
AC3[27,1]<-"2"
AC1[27,2:4]<-round(accuracy(pr.rec.h2.b[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[28,1]<- "VAR recursivo bagged" 
AC3[28,1]<-"2"
AC1[28,2:4]<-round(accuracy(pr.rec.h2.b[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[29,1]<- "FAVAR recursivo bagged"
AC3[29,1]<-"2"
AC1[29,2:4]<-round(accuracy(pr.rec.h2.b[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots.

AC1[30,1]<-"ARIMA rolling bagged" 
AC3[30,1]<-"2"
AC1[30,2:4]<-round(accuracy(pr.rol.h2.b[6:57,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[31,1]<-"ARIMAX rolling bagged" 
AC3[31,1]<-"2"
AC1[31,2:4]<-round(accuracy(pr.rol.h2.b[6:57,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[32,1]<-"ETS rollingbagged" 
AC3[32,1]<-"2"
AC1[32,2:4]<-round(accuracy(pr.rol.h2.b[6:57,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[33,1]<- "VAR rolling bagged" 
AC3[33,1]<-"2"
AC1[33,2:4]<-round(accuracy(pr.rol.h2.b[6:57,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC1[34,1]<- "FAVAR rolling bagged"
AC3[34,1]<-"2"
AC1[34,2:4]<-round(accuracy(pr.rol.h2.b[6:57,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Tabla 3.

AC4<-matrix(NA,34,4)
colnames(AC4) <- c("Modelo", "MAPE", "MAE", "RMSE")
AC5<-matrix(NA,34,1)

AC4[2,1]<-"ARIMA fijo" 
AC5[2,1]<-"2"
AC4[2,2:4]<-round(accuracy(pr.f.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[3,1]<-"ARIMAX fijo" 
AC5[3,1]<-"2"
AC4[3,2:4]<-round(accuracy(pr.f.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[4,1]<-"ETS fijo" 
AC5[4,1]<-"2"
AC4[4,2:4]<-round(accuracy(pr.f.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[5,1]<- "ADL fijo" 
AC5[5,1]<-"2"
AC4[5,2:4]<-round(accuracy(pr.f.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[6,1]<- "VAR fijo"
AC5[6,1]<-"2"
AC4[6,2:4]<-round(accuracy(pr.f.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[7,1]<- "FAVAR fijo" 
AC5[7,1]<-"2"
AC4[7,2:4]<-round(accuracy(pr.f.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling y h=2.

AC4[8,1]<-"ARIMA rolling" 
AC5[8,1]<-"2"
AC4[8,2:4]<-round(accuracy(pr.rol.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[9,1]<-"ARIMAX rolling" 
AC5[9,1]<-"2"
AC4[9,2:4]<-round(accuracy(pr.rol.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[10,1]<-"ETS rolling" 
AC5[10,1]<-"2"
AC4[10,2:4]<-round(accuracy(pr.rol.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[11,1]<- "ADL rolling" 
AC5[11,1]<-"2"
AC4[11,2:4]<-round(accuracy(pr.rol.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[12,1]<- "VAR rolling"
AC5[12,1]<-"2"
AC4[12,2:4]<-round(accuracy(pr.rol.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[13,1]<- "FAVAR rolling" 
AC5[13,1]<-"2"
AC4[13,2:4]<-round(accuracy(pr.rol.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

#  Medidas de accuracy de los pronósticos con esquema recursivo y h=2.

AC4[14,1]<-"ARIMA recursivo" 
AC5[14,1]<-"2"
AC4[14,2:4]<-round(accuracy(pr.rec.h7[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[15,1]<-"ARIMAX recursivo" 
AC5[15,1]<-"2"
AC4[15,2:4]<-round(accuracy(pr.rec.h7[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[16,1]<-"ETS recursivo" 
AC5[16,1]<-"2"
AC4[16,2:4]<-round(accuracy(pr.rec.h7[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[17,1]<- "ADL recursivo" 
AC5[17,1]<-"2"
AC4[17,2:4]<-round(accuracy(pr.rec.h7[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[18,1]<- "VAR recursivo"
AC5[18,1]<-"2"
AC4[18,2:4]<-round(accuracy(pr.rec.h7[,5], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[19,1]<- "FAVAR recursivo" 
AC5[19,1]<-"2"
AC4[19,2:4]<-round(accuracy(pr.rec.h7[,6], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema fijo, h = 2 y series boots.

AC4[20,1]<-"ARIMA fijo bagged" 
AC5[20,1]<-"2"
AC4[20,2:4]<-round(accuracy(pr.f.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[21,1]<-"ARIMAX fijo bagged" 
AC5[21,1]<-"2"
AC4[21,2:4]<-round(accuracy(pr.f.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[22,1]<-"ETS fijo bagged" 
AC5[22,1]<-"2"
AC4[22,2:4]<-round(accuracy(pr.f.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[23,1]<- "VAR fijo bagged" 
AC5[23,1]<-"2"
AC4[23,2:4]<-round(accuracy(pr.f.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[24,1]<- "FAVAR fijo bagged"
AC5[24,1]<-"2"
AC4[24,2:4]<-round(accuracy(pr.rec.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rec, h = 1 y series boots.

AC4[25,1]<-"ARIMA recursivo bagged" 
AC5[25,1]<-"2"
AC4[25,2:4]<-round(accuracy(pr.rec.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[26,1]<-"ARIMAX recursivo bagged" 
AC5[26,1]<-"2"
AC4[26,2:4]<-round(accuracy(pr.rec.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[27,1]<-"ETS recursivo bagged" 
AC5[27,1]<-"2"
AC4[27,2:4]<-round(accuracy(pr.rec.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[28,1]<- "VAR recursivo bagged" 
AC5[28,1]<-"2"
AC4[28,2:4]<-round(accuracy(pr.rec.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[29,1]<- "FAVAR recursivo bagged"
AC5[29,1]<-"2"
AC4[29,2:4]<-round(accuracy(pr.rec.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Medidas de accuracy de los pronósticos con esquema rolling, h = 2 y series boots.

AC4[30,1]<-"ARIMA rolling bagged" 
AC5[30,1]<-"2"
AC4[30,2:4]<-round(accuracy(pr.rol.h7.b[,1], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[31,1]<-"ARIMAX rolling bagged" 
AC5[31,1]<-"2"
AC4[31,2:4]<-round(accuracy(pr.rol.h7.b[,2], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[32,1]<-"ETS rollingbagged" 
AC5[32,1]<-"2"
AC4[32,2:4]<-round(accuracy(pr.rol.h7.b[,3], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[33,1]<- "VAR rolling bagged" 
AC5[33,1]<-"2"
AC4[33,2:4]<-round(accuracy(pr.rol.h7.b[,4], out.of.sample[7:58,2])[c(2:3,5)],4)
AC4[34,1]<- "FAVAR rolling bagged"
AC5[34,1]<-"2"
AC4[34,2:4]<-round(accuracy(pr.rol.h7.b[,5], out.of.sample[7:58,2])[c(2:3,5)],4)

# Exportamos.

# Para h=1
colnames(AC) <- c("Modelo", "RMSE", "MAE", "MAPE")
stargazer(AC[1:nrow(AC),], type = "latex")

# Para h=2

colnames(AC1) <- c("Modelo", "RMSE", "MAE", "MAPE")
AC1[1,1:ncol(AC1)] <- AC[1,1:ncol(AC)]
stargazer(AC1[1:nrow(AC1),], type = "latex")

# Para h = 3
colnames(AC4) <- c("Modelo", "RMSE", "MAE", "MAPE")
AC4[1,1:ncol(AC4)] <- AC[1,1:ncol(AC)]
stargazer(AC4[1:nrow(AC4),], type = "latex")

#### Test de Giacomini Rossi ####

# Para realizar este test, vamos a plantear las funciones de 
# pérdida cuadráticas. 

### h=1 

# Esquema fijo.

error.bench.sq <- error.bench^2 

error.arima.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,1])^2

error.arimax.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,2])^2

error.ets.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,3])^2

error.adl.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,4])^2

error.var.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,5])^2

error.favar.f.h1.sq <- (out.of.sample[,2]-pr.f.h1[,6])^2

# Esquema recursivo.

error.arima.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,1])^2

error.arimax.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,2])^2

error.ets.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,3])^2

error.adl.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,4])^2

error.var.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,5])^2

error.favar.rec.h1.sq <- (out.of.sample[,2]-pr.rec.h1[,6])^2

# Esquema rolling.

error.arima.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,1])^2

error.arimax.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,2])^2

error.ets.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,3])^2

error.adl.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,4])^2

error.var.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,5])^2

error.favar.rol.h1.sq <- (out.of.sample[,2]-pr.rol.h1[,6])^2

# Bagging 

# Esquema fijo bagged.

error.arima.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,1])^2

error.arimax.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,2])^2

error.ets.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,3])^2

error.var.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,4])^2

error.favar.f.h1.b.sq <- (out.of.sample[,2]-pr.f.h1.b[,5])^2

# Esquema recursivo bagged.

error.arima.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,1])^2

error.arimax.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,2])^2

error.ets.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,3])^2

error.var.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,4])^2

error.favar.rec.h1.b.sq <- (out.of.sample[,2]-pr.rec.h1.b[,5])^2

# Esquema rolling bagged.

error.arima.rol.h1.b.sq <-  (out.of.sample[,2]-pr.rol.h1.b[,1])^2

error.arimax.rol.h1.b.sq <- (out.of.sample[,2]-pr.rol.h1.b[,2])^2

error.ets.rol.h1.b.sq <- (out.of.sample[,2]-pr.rol.h1.b[,3])^2

error.var.rol.h1.b.sq <- (out.of.sample[,2]-pr.rol.h1.b[,4])^2

error.favar.rol.h1.b.sq <- (out.of.sample[,2]-pr.rol.h1.b[,5])^2

### h=2 

error.arima.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,1])^{2}

error.arimax.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,2])^{2}

error.ets.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,3])^{2}

error.adl.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,4])^{2}

error.var.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,5])^{2}

error.favar.f.h2.sq <- (out.of.sample[2:58,2]-pr.f.h2[,6])^{2}

# Esquema recursivo.

error.arima.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,1])^{2}

error.arimax.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,2])^{2}

error.ets.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,3])^{2}

error.adl.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,4])^{2}

error.var.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,5])^{2}

error.favar.rec.h2.sq <- (out.of.sample[2:58,2]-pr.rec.h2[,6])^{2}

# Esquema rolling.

error.arima.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,1])^{2}

error.arimax.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,2])^{2}

error.ets.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,3])^{2}

error.adl.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,4])^{2}

error.var.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,5])^{2}

error.favar.rol.h2.sq <- (out.of.sample[2:58,2]-pr.rol.h2[,6])^{2}

# Bagging. 

# Esquema fijo bagged.

error.arima.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,1])^{2}

error.arimax.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,2])^{2}

error.ets.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,3])^{2}

error.var.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,4])^{2}

error.favar.f.h2.b.sq <- (out.of.sample[2:58,2]-pr.f.h2.b[,5])^{2}

# Esquema recursivo bagged.

error.arima.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,1])^{2}

error.arimax.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,2])^{2}

error.ets.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,3])^{2}

error.var.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,4])^{2}

error.favar.rec.h2.b.sq <- (out.of.sample[2:58,2]-pr.rec.h2.b[,5])^{2}

# Esquema rolling.

error.arima.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,1])^{2}

error.arimax.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,2])^{2}

error.ets.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,3])^{2}

error.var.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,4])^{2}

error.favar.rol.h2.b.sq <- (out.of.sample[2:58,2]-pr.rol.h2.b[,5])^{2}

### h=7

error.arima.f.h7.sq <- (out.of.sample[7:58,2]-pr.f.h7[,1])^2

error.arimax.f.h7.sq <- (out.of.sample[7:58,2]-pr.f.h7[,2])^2

error.ets.f.h7.sq<- (out.of.sample[7:58,2]-pr.f.h7[,3])^2

error.adl.f.h7.sq <- (out.of.sample[7:58,2]-pr.f.h7[,4])^2

error.var.f.h7.sq <-  (out.of.sample[7:58,2]-pr.f.h7[,5])^2

error.favar.f.h7.sq <- (out.of.sample[7:58,2]-pr.f.h7[,6])^2

# Esquema recursivo.

error.arima.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,1])^2

error.arimax.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,2])^2

error.ets.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,3])^2

error.adl.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,4])^2

error.var.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,5])^2

error.favar.rec.h7.sq <- (out.of.sample[7:58,2]-pr.rec.h7[,6])^2

# Esquema rolling. 

error.arima.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,1])^2

error.arimax.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,2])^2

error.ets.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,3])^2

error.adl.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,4])^2

error.var.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,5])^2

error.favar.rol.h7.sq <- (out.of.sample[7:58,2]-pr.rol.h7[,6])^2

# Bagging 

# Esquema fijo bagged.

error.arima.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,1])^2

error.arimax.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,2])^2

error.ets.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,3])^2

error.var.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,5])^2

error.favar.f.h7.b.sq <- (out.of.sample[7:58,2]-pr.f.h7.b[,4])^2

# Esquema recursivo bagged.

error.arima.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,1])^2

error.arimax.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,2])^2

error.ets.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,3])^2

error.var.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,5])^2

error.favar.rec.h7.b.sq <- (out.of.sample[7:58,2]-pr.rec.h7.b[,4])^2

# Esquema rolling bagged.

error.arima.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,1])^2

error.arimax.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,2])^2

error.ets.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,3])^2

error.var.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,5])^2

error.favar.rol.h7.b.sq <- (out.of.sample[7:58,2]-pr.rol.h7.b[,4])^2

# Ahora graficamos.

library(murphydiagram)
library(dplyr)

# Pronósticos h=1 fijo.

gr1 <- fluctuation_test(error.arima.f.h1.sq, error.bench.sq, mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h1.sq, error.bench.sq, mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h1.sq, error.bench.sq, mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.f.h1.sq, error.bench.sq, mu = 0.5)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.f.h1.sq, error.bench.sq, mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h1.sq, error.bench.sq, mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                   "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
    xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo, un paso hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.f.h1.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=2 fijo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.f.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.f.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo, dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.f.h2.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=7 fijo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.f.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.f.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo, siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.f.h7.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=1 recursivo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h1.sq, error.bench.sq, mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h1.sq, error.bench.sq, mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h1.sq, error.bench.sq, mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rec.h1.sq, error.bench.sq, mu = 0.5)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rec.h1.sq, error.bench.sq, mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h1.sq, error.bench.sq, mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

colores <- c("#00ABC5","#cfb0b4" ,"#ff3c84","#FF7F32", "#edf71c", "#941cf7")

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo, un paso hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rec.h1.eps", width=6.5, height=4, dpi=300)


# Pronósticos h=2 recursivo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rec.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rec.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo, dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rec.h2.eps", width=6.5, height=4, dpi=300)


# Pronósticos h=7 recursivo.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rec.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rec.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo, siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rec.h7.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=1 rolling.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h1.sq, error.bench.sq, mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h1.sq, error.bench.sq, mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h1.sq, error.bench.sq, mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rol.h1.sq, error.bench.sq, mu = 0.5)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rol.h1.sq, error.bench.sq, mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h1.sq, error.bench.sq, mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'rolling', un paso hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rol.h1.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=2 rolling.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rol.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rol.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h2.sq, error.bench.sq[2:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'rolling', dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rol.h2.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=7 rolling.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr4 <- fluctuation_test(error.adl.rol.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr4 <- as.data.frame(gr4$df)

gr5 <- fluctuation_test(error.var.rol.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h7.sq, error.bench.sq[7:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr4, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'rolling;, siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rol.h7.eps", width=6.5, height=4, dpi=300)


# Pronosticos h=1 fijo bagged.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.f.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[5],
                                colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo 'bagged', un paso hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.f.h1.b.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=2 fijo bagged.

rm(gr1, gr2, gr3, gr4, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr4, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h2.b.sq, error.bench.sq[1:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h2.b.sq, error.bench.sq[1:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.f.h2.b.sq, error.bench.sq[1:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h2.b.sq, error.bench.sq[1:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo 'bagged', dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.f.h2.b.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=7 fijo bagged.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.f.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.f.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.f.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.f.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.f.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position ="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema fijo 'bagged', siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.f.h7.b.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=1 recursivo bagged.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rec.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo 'bagged', un paso hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rec.h1.b.eps", width=6.5, height=4, dpi=300)


# Pronósticos h=2 recursivo.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(28,57) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo 'bagged', dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rec.h2.b.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=7 recursivo.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rec.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rec.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rec.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rec.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", 
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema recursivo 'bagged', siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rec.h7.b.eps", width=6.5, height=4, dpi=300)


# Pronósticos h=1 rolling.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band1 <- gr1$CV[1]
band2 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rol.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h1.b.sq, error.bench.sq, mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3", "dmstat4",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band1), size = 1, color = "black") +
  geom_hline(aes(yintercept = band2), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'bagged rolling', un paso hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rol.h1.b.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=2 rolling.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band3 <- gr1$CV[1]
band4 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h2.b.sq, error.bench.sq[2:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,60) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band3), size = 1, color = "black") +
  geom_hline(aes(yintercept = band4), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'bagged rolling', dos pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rol.h2.b.eps", width=6.5, height=4, dpi=300)

# Pronósticos h=7 rolling.

rm(gr1, gr2, gr3, gr5, gr6, dm.1, 
   values.gr1, values.gr2, values.gr3, values.gr5,
   values.gr6)

gr1 <- fluctuation_test(error.arima.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr1 <- as.data.frame(gr1$df)
band5 <- gr1$CV[1]
band6 <- gr1$CV[2]

gr2 <- fluctuation_test(error.arimax.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr2 <- as.data.frame(gr2$df)

gr3 <- fluctuation_test(error.ets.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr3 <- as.data.frame(gr3$df)

gr5 <- fluctuation_test(error.var.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr5 <- as.data.frame(gr5$df)

gr6 <- fluctuation_test(error.favar.rol.h7.b.sq, error.bench.sq[7:58], mu = 0.5)
values.gr6 <- as.data.frame(gr6$df)

dm.1 <- full_join(values.gr1, values.gr2, by = "time")
dm.1 <- full_join(dm.1, values.gr3, by = "time")
dm.1 <- full_join(dm.1, values.gr5, by = "time")
dm.1 <- full_join(dm.1, values.gr6, by = "time")
colnames(dm.1) <- c("time", "dmstat1", "dmstat2", "dmstat3",
                    "dmstat5", "dmstat6")

dm.1 <- melt(dm.1, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,52) +
  ylim(-4,6) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","ADL","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3], colores[4],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Pronósticos: esquema 'bagged rolling', siete pasos hacia adelante",
       caption = "Fuente: elaboración propia")

ggsave(file="gr.rol.h7.b.eps", width=6.5, height=4, dpi=300)


#### GR Comparando ####

# A continuación, haremos el test de Giacomini-Rossi comparando a cada modelo con 
# su versión bagged. Graficamos por esquema-ventana.

# Fijos h = 1.

gr1c <- fluctuation_test(error.arima.f.h1.sq, error.arima.f.h1.b.sq , mu = 0.5)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.f.h1.sq, error.arimax.f.h1.b.sq, mu = 0.5)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.f.h1.sq, error.ets.f.h1.b.sq, mu = 0.5)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.f.h1.sq, error.var.f.h1.b.sq, mu = 0.5)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.f.h1.sq, error.favar.f.h1.b.sq, mu = 0.5)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                    "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema fijo un paso adelante",
       caption = "Nota: valores negativos reflejan un mejor desempeño de la versión bagging. \
       Fuente: elaboración propia")

# Fijos h = 2.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c, 
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.f.h2.sq, error.arima.f.h2.b.sq , mu = 0.5)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.f.h2.sq, error.arimax.f.h2.b.sq, mu = 0.5)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.f.h2.sq, error.ets.f.h2.b.sq, mu = 0.5)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.f.h2.sq, error.var.f.h2.b.sq, mu = 0.5)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.f.h2.sq, error.favar.f.h2.b.sq, mu = 0.5)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,58) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema fijo dos pasos adelante",
       caption = "Nota: valores negativos reflejan un mejor desempeño de la versión bagging. \
       Fuente: elaboración propia")

# Fijo con h=7.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.f.h7.sq, error.arima.f.h7.b.sq , mu = 0.5)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.f.h7.sq, error.arimax.f.h7.b.sq, mu = 0.5)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.f.h7.sq, error.ets.f.h7.b.sq, mu = 0.5)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.f.h7.sq, error.var.f.h7.b.sq, mu = 0.5)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.f.h7.sq, error.favar.f.h7.b.sq, mu = 0.5)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,52) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema fijo siente pasos adelante",
       caption = "Nota: valores negativos reflejan un mejor desempeño de la versión bagging. \
       Fuente: elaboración propia")

# Rolling h = 1.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c, 
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rol.h1.sq, error.arima.rol.h1.b.sq , mu = 0.5)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rol.h1.sq, error.arimax.rol.h1.b.sq, mu = 0.5)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rol.h1.sq, error.ets.rol.h1.b.sq, mu = 0.5)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rol.h1.sq, error.var.rol.h1.b.sq, mu = 0.5)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rol.h1.sq, error.favar.rol.h1.b.sq, mu = 0.5)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,57) +
  ylim(-5,4) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema fijo un paso adelante",
       caption = "Nota: valores negativos reflejan un mejor desempeño de la versión bagging. \
       Fuente: elaboración propia")

# Rolling h = 2.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.f.h2.sq, error.arima.f.h2.b.sq , mu = 0.5)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.f.h2.sq, error.arimax.f.h2.b.sq, mu = 0.5)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.f.h2.sq, error.ets.f.h2.b.sq, mu = 0.5)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.f.h2.sq, error.var.f.h2.b.sq, mu = 0.5)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.f.h2.sq, error.favar.f.h2.b.sq, mu = 0.5)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(27,57) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema 'rolling' dos pasos adelante",
       caption = "Nota: valores negativos reflejan un mejor desempeño de la versión bagging. \
       Fuente: elaboración propia")

# Rolling con h=7.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rol.h7.sq, error.arima.rol.h7.b.sq , mu = 0.5)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rol.h7.sq, error.arimax.rol.h7.b.sq, mu = 0.5)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rol.h7.sq, error.ets.rol.h7.b.sq, mu = 0.5)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rol.h7.sq, error.var.rol.h7.b.sq, mu = 0.5)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rol.h7.sq, error.favar.rol.h7.b.sq, mu = 0.5)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,52) +
  ylim(-5,4) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema 'rolling' siete pasos adelante",
       caption = "Nota: valores negativos reflejan un mejor desempeño de la versión bagging. \
       Fuente: elaboración propia")

# Recursivo h = 1.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c, 
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rec.h1.sq, error.arima.rec.h1.b.sq , mu = 0.5)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rec.h1.sq, error.arimax.rec.h1.b.sq, mu = 0.5)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rec.h1.sq, error.ets.rec.h1.b.sq, mu = 0.5)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rec.h1.sq, error.var.rec.h1.b.sq, mu = 0.5)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rol.h1.sq, error.favar.rec.h1.b.sq, mu = 0.5)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(29,57) +
  ylim(-5,4) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema recursivo un paso adelante",
       caption = "Nota: valores negativos reflejan un mejor desempeño de la versión bagging. \
       Fuente: elaboración propia")

# Recursivo h = 2.

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rec.h2.sq, error.arima.rec.h2.b.sq , mu = 0.5)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rec.h2.sq, error.arimax.rec.h2.b.sq, mu = 0.5)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rec.h2.sq, error.ets.rec.h2.b.sq, mu = 0.5)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rec.h2.sq, error.var.rec.h2.b.sq, mu = 0.5)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rec.h2.sq, error.favar.rec.h2.b.sq, mu = 0.5)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(27,57) +
  ylim(-5,5) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema recursivo dos pasos adelante",
       caption = "Nota: valores negativos reflejan un mejor desempeño de la versión bagging. \
       Fuente: elaboración propia")


# Recursivo con h=7

rm(gr1c, gr2c, gr3c, gr4c, gr5c, dm.1c, 
   values.gr1c, values.gr2c, values.gr3c,
   values.gr4c, values.gr5c)

gr1c <- fluctuation_test(error.arima.rec.h7.sq, error.arima.rec.h7.b.sq , mu = 0.5)
values.gr1c <- as.data.frame(gr1c$df)
band5 <- gr1c$CV[1]
band6 <- gr1c$CV[2]

gr2c <- fluctuation_test(error.arimax.rec.h7.sq, error.arimax.rec.h7.b.sq, mu = 0.5)
values.gr2c <- as.data.frame(gr2c$df)

gr3c <- fluctuation_test(error.ets.rec.h7.sq, error.ets.rec.h7.b.sq, mu = 0.5)
values.gr3c <- as.data.frame(gr3c$df)

gr4c <- fluctuation_test(error.var.rec.h7.sq, error.var.rec.h7.b.sq, mu = 0.5)
values.gr4c <- as.data.frame(gr4c$df)

gr5c <- fluctuation_test(error.favar.rec.h7.sq, error.favar.rec.h7.b.sq, mu = 0.5)
values.gr5c <- as.data.frame(gr5c$df)

dm.1c <- full_join(values.gr1c, values.gr2c, by = "time")
dm.1c <- full_join(dm.1c, values.gr3c, by = "time")
dm.1c <- full_join(dm.1c, values.gr4c, by = "time")
dm.1c <- full_join(dm.1c, values.gr5c, by = "time")
colnames(dm.1c) <- c("time", "dmstat1c", "dmstat2c", "dmstat3c", "dmstat4c",
                     "dmstat5c")

dm.1c <- melt(dm.1c, id=c("time"))

ggplot(aes(x = time , y = value, group = variable, color = variable),
       data = dm.1c) +
  theme_bw() + 
  geom_line(size = 1) +
  xlim(26,52) +
  ylim(-5,4) + 
  geom_hline(aes(yintercept = 0), size = 1, color = "black",
             linetype = "dashed") + 
  geom_hline(aes(yintercept = band5), size = 1, color = "black") +
  geom_hline(aes(yintercept = band6), size = 1, color = "black") + 
  xlab("Tiempo") + 
  ylab("Estadístico Diebold-Mariano") +
  scale_color_manual(name = "", labels = c("ARIMA", "ARIMAX","ETS","VAR", "FAVAR"), 
                     values = c(colores[1], colores[2],colores[3],
                                colores[5], colores[6]))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(title = "Fluctuation Test",
       subtitle = "Comparación pronósticos vs. 'bagging': esquema recursivo siete pasos adelante",
       caption = "Nota: valores negativos reflejan un mejor desempeño de la versión bagging. \
       Fuente: elaboración propia")


rm(list = ls())
