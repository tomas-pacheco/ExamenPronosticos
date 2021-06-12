## Importamos algunas de las librer?as que vamos a usar.

library(xts)
library(ggplot2)
library(devtools)
library(ggfortify)
library(dplyr)

## Seteamos el directorio. Prueba.

dir <- "G:\\Mi unidad\\UdeSA\\Pron?sticos\\Final\\Data\\dep"
setwd("C:/Users/Usuario/Desktop/2021/Pron?siticos")
dir <- ""

setwd(dir)

# Definimos la paleta de colores.

colores <- c("#00ABC5")

# Abrimos la base de datos.

data <- read.csv("Data_Final_PF.csv")
data <- data[,-2]
data <- na.omit(data)
rownames(data) <- data$time

# Ahora generamos un objeto de series de tiempo para cada una de las variables.

for (i in colnames(data)[-(1:2)]){
    assign(i, xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d")) )
}


# Grafico de nuestra serie de interes.

autoplot(sentsmooth, ts.colour = colores[1]) + 
  ggtitle("Evoluci?n sentimental de Alberto Fern?ndez") + 
  xlab("Tiempo") + 
  ylab("Sentimiento") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


#Creamos una función que devuelve el estadístico con su significatividad 

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

results <- matrix(nrow = 23,ncol = 4, NA)
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

test.est[1,3] <- stars(test.M1.1@teststat, test.M1.1@cval)

#Descargamos la tabla 

stargazer(results , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

results1 <- matrix(nrow = 23,ncol = 3, NA)
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

#Descargamos la tabla 

stargazer(results1 , type = "latex", dep.var.labels.include = FALSE,
          notes = "Nota: *** significativo al 1%, ** significativo al 5%, * significativo al 10%")

results2 <- matrix(nrow = 23,ncol = 3, NA)
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


#Definimos la ventana in-sample y out-of-sample

in.sample <- data[1:424,]

out.of.sample <- data[425:483,]


