## Importamos algunas de las librerías que vamos a usar.

library(xts)
library(ggplot2)
library(devtools)
library(ggfortify)
library(dplyr)

## Seteamos el directorio.

dir <- "G:\\Mi unidad\\UdeSA\\Pronósticos\\Final\\Data\\dep"
dir <- ""
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


# Gráfico de nuestra serie de interés.

autoplot(sentsmooth, ts.colour = colores[1]) + 
  ggtitle("Evolución sentimental de Alberto Fernández") + 
  xlab("Tiempo") + 
  ylab("Sentimiento") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


# Unit root tests. Esto es solo con Dickey-Fuller al 1%.

library(urca)
results <- matrix(nrow = 23,ncol = 7, NA)
r = 1
for (i in colnames(data)[-(1:2)]){
  results[r,1] <- i
  series <- xts(data[[i]], order.by=as.Date(rownames(data),"%Y-%m-%d"))
  series <- na.omit(series)
  test <- ur.df(series, type = c("none"))
  results[r,2] <- test@cval[1]
  results[r,3] <- round(test@teststat[1],2)
  test <- ur.df(series, type = c("trend"))
  results[r,4] <- test@cval[1]
  results[r,5] <- round(test@teststat[1],2)
  test <- ur.df(series, type = c("drift"))
  results[r,6] <- test@cval[1]
  results[r,7] <- round(test@teststat[1],2)
  r = r+1
}
colnames(results) <- c("Variable", "None", "Stat", "Trend", "Stat", "Drift", "Stat")

# La única variable que no es estacionaria es la de las reservas del BCRA.




