# Event study

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

data$retpres <- log(data$sentsmooth/lag(data$sentsmooth))
data$retmarket <- log(data$sent_trends/lag(data$sent_trends))

# Volvemos a borrar los missings.

data <- na.omit(data)

# Generamos la variable de tiempo.

data$t <- seq(1, nrow(data),1)

# Primer evento: declaracion de la cuarentena, el 19 de marzo de 2020.

data$ev_cuarentena <- 60
data$ev_cuarentena_time <- data$t - data$ev_cuarentena

# Segundo evento. Expropiación (o intento de) de Vicentín. 21 de junio 2020

data$ev_vicentin <- 153
data$ev_vicentin_time <- data$t - data$ev_vicentin

# Tercer evento. Velatorio Maradona: 26 de noviembre de 2020.

data$ev_maradona <- 304
data$ev_maradona_time <- data$t - data$ev_maradona

# Cuarto evento. Llegada del primer lota de vacunas al país. 
# 24 de diciembre de 2020.

data$ev_vacunas <- 329
data$ev_vacunas_time <- data$t - data$ev_vacunas


# Serie de tiempo a nuestras variables, para graficar.

data$retpres <- ts(data$retpres, frequency =)

plot.ts(data$retpres)

data$retmarket <- ts(data$retmarket, frequency =)

plot.ts(data$retmarket)

# Calculamos el retorno medio, antes y después. Para cada evento:

# Anuncio:

mean(data$retpres[data$ev_cuarentena_time<0]) #anterior
mean(data$retpres[data$ev_cuarentena_time>0]) #posterior

# Vicentin

mean(data$retpres[data$ev_vicentin_time<0]) #anterior
mean(data$retpres[data$ev_vicentin_time>0]) #posterior

# Maradona

mean(data$retpres[data$ev_maradona_time<0]) #anterior
mean(data$retpres[data$ev_maradona_time>0]) #posterior

# Vacunas

mean(data$retpres[data$ev_vacunas_time<0]) #anterior
mean(data$retpres[data$ev_vacunas_time>0]) #posterior


# Estimación del modelo de mercado para cada evento (durante la ventana de estimación)

modelo_anuncio <- lm(retpres ~ retmarket , data=subset(data,ev_cuarentena_time<0))  
summary(modelo_anuncio)

modelo_vicentin <- lm(retpres ~ retmarket , data=subset(data,ev_vicentin_time<0))  
summary(modelo_vicentin)

modelo_maradona <- lm(retpres ~ retmarket , data=subset(data,ev_maradona_time<0))  
summary(modelo_maradona)

modelo_vacunas <- lm(retpres ~ retmarket , data=subset(data,ev_vacunas_time<0))  
summary(modelo_maradona)

# Ahora los retornos anormales

AR_anuncio <- data$retpres[data$ev_cuarentena_time>=0]-predict(modelo_anuncio, newdata=subset(data,ev_cuarentena_time >=0))
AR_vicentin <- data$retpres[data$ev_vicentin_time>=0]-predict(modelo_vicentin, newdata=subset(data,ev_vicentin_time >=0))
AR_maradona <- data$retpres[data$ev_maradona_time>=0]-predict(modelo_maradona, newdata=subset(data,ev_maradona_time >=0))
AR_vacunas <- data$retpres[data$ev_vacunas_time>=0]-predict(modelo_vacunas, newdata=subset(data,ev_vacunas_time >=0))


# Testeamos si cada AR medio es igual a 0:

t.test(AR_anuncio, df=length(AR_anuncio)-1, alternative = "two.sided")
t.test(AR_vicentin, df=length(AR_vicentin)-1, alternative = "two.sided")
t.test(AR_maradona, df=length(AR_maradona)-1, alternative = "two.sided")
t.test(AR_vacunas, df=length(AR_vacunas)-1, alternative = "two.sided")


# Calculamos los CARS

CAR_anuncio <- sum(AR_anuncio); CAR_anuncio
CAR_vicentin <- sum(AR_vicentin); CAR_vicentin
CAR_maradona <- sum(AR_maradona); CAR_maradona
CAR_vacunas <- sum(AR_vacunas); CAR_vacunas

# Gráfico

CAR_anuncio_tiempo <- cumsum(AR_anuncio)
plot.ts(CAR_anuncio_tiempo, xlab = "Periodo post-evento", ylab = "Suma acumulada", col="red")
abline(h=0)

# Nos quedamos con cuatro los ARs y CARs posteriores al anuncio.

AR_anuncio_cuatro <- AR_anuncio[1:4]
qqnorm(AR_anuncio_cuatro)
qqline(AR_anuncio_cuatro)

AR_vicentin_cuatro <- AR_vicentin[1:4]
qqnorm(AR_vicentin_cuatro)
qqline(AR_vicentin_cuatro)

AR_maradona_cuatro <- AR_maradona[1:4]
qqnorm(AR_maradona_cuatro)
qqline(AR_maradona_cuatro)

AR_vacunas_cuatro <- AR_vacunas[1:4]
qqnorm(AR_vacunas_cuatro)
qqline(AR_vacunas_cuatro)


# Test paramétrico:

t.test(AR_anuncio_cuatro, df=length(AR_anuncio_cuatro)-1, alternative = "two.sided")
t.test(AR_vicentin_cuatro, df=length(AR_vicentin_cuatro)-1, alternative = "two.sided")
t.test(AR_maradona_cuatro, df=length(AR_maradona_cuatro)-1, alternative = "two.sided")
t.test(AR_vacunas_cuatro, df=length(AR_vacunas_cuatro)-1, alternative = "two.sided")

# No paramétrico

wilcox.test(AR_anuncio_cuatro, alternative = "two.sided")
wilcox.test(AR_vicentin_cuatro, alternative = "two.sided")
wilcox.test(AR_maradona_cuatro, alternative = "two.sided")
wilcox.test(AR_vacunas_cuatro, alternative = "two.sided")

# Vamos a hacer un analisis de potencia.

# Para ello, nos construiremos una función. Esta exporta un data frame con los
# retornos anormales ficticios y su potencia asociada.

power_analysis <- function(event, bound){
  # Definimos los valores del retorno anormal hipotético
  sim_left_assumed <- seq(from = -bound, to = 0, by = 0.005)  
  sim_right_assumed <- seq(from = 0.005, to = bound, by = 0.005)  
  sim_assumed <- seq(from = -bound, to = bound, by = 0.005) 
  
  event_left <- 0+qt(0.025,df=length(event)-1)*sd(event)/sqrt(length(event))
  event_right <- 0+qt(0.975,df=length(event)-1)*sd(event)/sqrt(length(event))
  
  t_left_event <-(event_left - sim_left_assumed)/(sd(event)/sqrt(length(event)))
  t_right_event <-(event_right - sim_right_assumed)/(sd(event)/sqrt(length(event)))
  
  power_left <- pt(t_left_event,df=length(event)-1, lower.tail = TRUE)
  power_right <- 1-pt(t_right_event,df=length(event)-1)
  power <- append(power_left,power_right)
  export <- data.frame(sim_assumed, power)
  return(export)
}

# El input de la función son los retornos anormales para cada evento y las cotas
# de los retornos.

# Llamamos a la función para cada uno de los eventos y luego graficamos.

power_anuncio <- power_analysis(AR_anuncio, 0.25)

p1 <- ggplot(aes(x = sim_assumed, y = power ), data = power_anuncio) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[1]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia (AR del sentimiento del presidente)",
          subtitle = "Anuncio de cuarentena - 19/03/20") + 
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))
  
power_vicentin <- power_analysis(AR_vicentin, 0.25)

p2 <- ggplot(aes(x = sim_assumed, y = power ), data = power_vicentin) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[2]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 0
  ggtitle(label = "Curva de potencia (AR del sentimiento del presidente)",
          subtitle = "(Intento de) Expropiación de Vicentin - 21/06/2020 ") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

power_maradona <- power_analysis(AR_maradona, 0.25)

p3 <- ggplot(aes(x = sim_assumed, y = power ), data = power_maradona) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[3]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia (AR del sentimiento del presidente)",
          subtitle = "Velorio de Maradona - 26/11/2020 ") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

power_vacunas <- power_analysis(AR_vacunas, 0.25)

p4 <- ggplot(aes(x = sim_assumed, y = power ), data = power_vacunas) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[4]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia (AR del sentimiento del presidente)",
          subtitle = "Llegada del primer lote de vacunas - 24/12/2020 ") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2,
             top = "Evolución de variables de interés")

