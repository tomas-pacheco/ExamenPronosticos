# Event study. Loop trucho.

dir <- "G:\\Mi unidad\\UdeSA\\Pronósticos\\Final\\Data\\dep"
#dir <- "C:\\Users\\Abi\\Downloads"

setwd(dir)

options(scipen=999)

library(ggplot2)

# Definimos la paleta de colores.

colores <- c("#00ABC5", "#f7941c", "#edf71c", "#ff3c84")

# Abrimos la base de datos

data <- read.csv("Data_Final_PF2.csv")
data <- data[,c(1,2,4,28)]

data <- na.omit(data)

# Generamos los retornos, tanto de nuestra serie como la del 'mercado'.

data$retpres <- log(data$sentsmooth/lag(data$sentsmooth, 1))
data$retmarket <- log(data$sent_trends/lag(data$sent_trends, 1))

# Volvemos a borrar los missings.

data <- na.omit(data)

# Generamos la variable de tiempo.

data$t <- seq(1, nrow(data),1)

# Primer evento: declaracion de la cuarentena, el 19 de marzo de 2020.

result <- matrix(ncol = 6, nrow = 448, NA)

r <- 1
for (i in seq(30, 477)){
  result[r,1] <- i
  data$ev_cuarentena <- i
  data$ev_cuarentena_time <- data$t - data$ev_cuarentena
  result[r,2] <- mean(data$retpres[data$ev_cuarentena_time<0]) #anterior
  result[r,3] <-  mean(data$retpres[data$ev_cuarentena_time>0]) #posterior
  modelo_anuncio <- lm(retpres ~ retmarket , data=subset(data,ev_cuarentena_time<0))  
  result[r,4] <-  modelo_anuncio[["coefficients"]][2]
  AR_anuncio <- data$retpres[data$ev_cuarentena_time>=0]-predict(modelo_anuncio, newdata=subset(data,ev_cuarentena_time >=0))
  t.test(AR_anuncio, df=length(AR_anuncio)-1, alternative = "two.sided")
  AR_anuncio_cuatro <- AR_anuncio[1:4]
  a <- t.test(AR_anuncio_cuatro, df=length(AR_anuncio_cuatro)-1, alternative = "two.sided")
  result[r,5] <- a[["p.value"]]
  b <- wilcox.test(AR_anuncio_cuatro, alternative = "two.sided")
  result[r,6] <- b[["p.value"]]
  r <- r + 1
  }
colnames(result) <- c("evento", "mean_pre", "mean_pos", "coef_reg", "pval_ttest", "pval_wilcox")

results2 <- subset(result, result[,5] <= 0.05)
