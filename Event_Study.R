# Event study

dir <- "G:\\Mi unidad\\UdeSA\\Pronósticos\\Final\\Data\\dep"
dir <- "C:\\Users\\Abi\\Downloads"

setwd(dir)

options(scipen=999)

library(ggplot2)
library(dplyr)

# Definimos la paleta de colores.

colores <- c("#00ABC5", "#f7941c", "#edf71c", "#ff3c84")

# Abrimos la base de datos para esta etapa del trabajo.

data <- read.csv("Data_Final_PF2.csv")
data <- data[,c(1,2,4,28)]
data <- na.omit(data)
rownames(data) <- data$time

# Comenzamos graficando la serie de la evolución del sentimiento
# del presidente con lo que sería el sentimiento de Twitter.

# Le damos formato temporal a ambas series.

library(ggplot2)
library(ggfortify)
library(lubridate)

ggplot(aes(x = ymd(time), y = sentsmooth), data = data) + 
  theme_minimal() + 
  geom_line(aes(x = ymd(time), y = sentsmooth), size = 0.70, col = colores[1]) +
  geom_line(aes(x = ymd(time), y = sent_trends), size = 0.70, col = colores[4]) +
  labs(x = "Tiempo", 
       y = "Sentimiento", 
       title = "Sentimiento de Alberto Fernández y Twitter Argentina",
       caption = "Fuente: elaboración propia") +
  scale_color_manual(name = "", labels = c("AF", "Twitter"), 
                     values = c(colores[1], colores[4])) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = ymd("2020-07-05"), y = 0.35, label = "Alberto Fernández", 
           color = colores[1])+
  annotate("text", x = ymd("2020-07-05"), y = 0.39, label = "Twitter Argentina", 
           color = colores[4])

ggsave(file="Sentimiento_AF_TwARG.eps", width=6.5, height=4, dpi=300)

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

# Segundo evento. Expropiación (o intento de) de Vicentin. 21 de junio 2020

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

ba <- matrix(NA, nrow = 4, ncol = 3)
colnames(ba) <- c("Evento", "Antes", "Después")

# Anuncio:

ba[1,1] <- "Anuncio cuarentena"
ba[1,2] <- round(mean(data$retpres[data$ev_cuarentena_time<0]),4) #anterior
ba[1,3] <- round(mean(data$retpres[data$ev_cuarentena_time>0]),4) #posterior

# Vicentin

ba[2,1] <- "Vicentin"
ba[2,2] <- round(mean(data$retpres[data$ev_vicentin_time<0]),4) #anterior
ba[2,3] <- round(mean(data$retpres[data$ev_vicentin_time>0]),4) #posterior

# Maradona

ba[3,1] <- "Fallecimiento Maradona"
ba[3,2] <- round(mean(data$retpres[data$ev_maradona_time<0]),4) #anterior
ba[3,3] <- round(mean(data$retpres[data$ev_maradona_time>0]),4) #posterior

# Vacunas

ba[4,1] <- "Primer lote de vacunas"
ba[4,2] <- round(mean(data$retpres[data$ev_vacunas_time<0]),4) #anterior
ba[4,3] <- round(mean(data$retpres[data$ev_vacunas_time>0]),4) #posterior

# Exportamos.

stargazer(ba, type = "text")

# Estimación del modelo de mercado para cada evento (durante la ventana de estimación).

modelo_anuncio <- lm(retpres ~ retmarket , data=subset(data,ev_cuarentena_time<0))  
summary(modelo_anuncio)

modelo_vicentin <- lm(retpres ~ retmarket , data=subset(data,ev_vicentin_time<0))  
summary(modelo_vicentin)

modelo_maradona <- lm(retpres ~ retmarket , data=subset(data,ev_maradona_time<0))  
summary(modelo_maradona)

modelo_vacunas <- lm(retpres ~ retmarket , data=subset(data,ev_vacunas_time<0))  
summary(modelo_maradona)

stargazer(modelo_anuncio, modelo_vicentin, modelo_maradona, modelo_vacunas,
          align = TRUE, 
          keep.stat = c("n", 'rsq'),
          no.space = TRUE,
          add.lines=list(c("Evento", "Cuarentena", "Vicentin", "Maradona", "Vacunas")),
          type = "text")

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

# Vamos a ver para que cantidad de observaciones rechazamos. Sensibility.
# Para esto, hacemos una función.

# Calculamos los CARS

CAR_anuncio <- sum(AR_anuncio); CAR_anuncio
CAR_vicentin <- sum(AR_vicentin); CAR_vicentin
CAR_maradona <- sum(AR_maradona); CAR_maradona
CAR_vacunas <- sum(AR_vacunas); CAR_vacunas

# Gráfico.

CAR_anuncio_tiempo <- cumsum(AR_anuncio)
plot.ts(CAR_anuncio_tiempo, xlab = "Periodo post-evento", ylab = "Suma acumulada", col="red")
abline(h=0)

# Nos quedamos con siete los ARs y CARs posteriores al anuncio.

AR_anuncio_siete <- AR_anuncio[1:7]
qqnorm(AR_anuncio_siete)
qqline(AR_anuncio_siete)
mean(AR_anuncio_siete)

AR_vicentin_siete <- AR_vicentin[1:7]
qqnorm(AR_vicentin_siete)
qqline(AR_vicentin_siete)
mean(AR_vicentin_siete)

AR_maradona_siete <- AR_maradona[1:7]
qqnorm(AR_maradona_siete)
qqline(AR_maradona_siete)
mean(AR_maradona_siete)

AR_vacunas_siete <- AR_vacunas[1:7]
qqnorm(AR_vacunas_siete)
qqline(AR_vacunas_siete)
mean(AR_vacunas_siete)

# Test paramétrico.

test.p.anuncio <- t.test(AR_anuncio_siete, df=length(AR_anuncio_siete)-1,    alternative = "two.sided")
test.p.vicentin <- t.test(AR_vicentin_siete, df=length(AR_vicentin_siete)-1, alternative = "two.sided")
test.p.maradona <- t.test(AR_maradona_siete, df=length(AR_maradona_siete)-1, alternative = "two.sided")
test.p.vacunas <- t.test( AR_vacunas_siete, df=length( AR_vacunas_siete)-1,  alternative = "two.sided")

# No paramétrico.

test.np.anuncio  <- wilcox.test(AR_anuncio_siete, alternative =  "two.sided")
test.np.vicentin <- wilcox.test(AR_vicentin_siete, alternative = "two.sided")
test.np.maradona <- wilcox.test(AR_maradona_siete, alternative = "two.sided")
test.np.vacunas  <- wilcox.test(AR_vacunas_siete, alternative =  "two.sided")

# Ponemos estos resultados en una tabla.

pvtest <- matrix(NA, nrow = 4, ncol = 3)

pvtest[1,1] <- "Anuncio cuarentena"
pvtest[1,2] <- round(test.p.anuncio$p.value,4)
pvtest[1,3] <- round(test.np.anuncio$p.value,4)

pvtest[2,1] <- "Vicentin"
pvtest[2,2] <- round(test.p.vicentin$p.value,4)
pvtest[2,3] <- round(test.np.vicentin$p.value,4)

pvtest[3,1] <- "Fallecimiento Maradona"
pvtest[3,2] <- round(test.p.maradona$p.value,4)
pvtest[3,3] <- round(test.np.maradona$p.value,4)

pvtest[4,1] <- "Llegada de vacunas"
pvtest[4,2] <- round(test.p.vacunas$p.value,4)
pvtest[4,3] <- round(test.np.vacunas$p.value,4)

# Analisis de sensibilidad.

sensibility <- function(list){
  pv <- matrix(nrow = length(list), ncol = 3)
  row <- 1
  for (i in seq(4, length(list))){
    temp <- list[1:i]
    test <- t.test(temp, df = length(temp) - 1, alternative = "two.sided")
    test2 <- wilcox.test(temp)
    pv[row, 1] <- i
    pv[row, 2] <- test[["p.value"]]
    pv[row, 3] <- test2[["p.value"]]
    row <- row + 1
  }
  pv <- as.data.frame(pv)
  colnames(pv) <- c("obsahead", "test.ttest", "test.wilcox")
  pv <- na.omit(pv)
  return(pv)
}

# Aplicamos una función para cada caso y graficamos. 

sensibility_anuncio <- sensibility(AR_anuncio)
sensibility_anuncio <- melt(sensibility_anuncio, id=c("obsahead"))

ggplot(aes(x = obsahead, y = value, group = variable, color = variable),
       data = sensibility_anuncio) + 
  geom_line(size = 0.7) + 
  scale_color_manual(name = "", labels = c("t-test", "Wilcoxon"), 
                     values = c(colores[1], colores[4]))+
  theme_bw() + 
  xlab("Observaciones adelante") + 
  ylab("p-valor") + 
  ylim(0,0.30) + 
  labs(title = "Análisis de sensibilidad - Anuncio cuarentena",
       caption = "Fuente: elaboración propia") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") + 
  geom_hline(yintercept=0.1, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.05, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.01, linetype="dashed", color = colores[5]) + 
  scale_x_continuous(n.breaks = 20) + 
  annotate("text", x = 400, y = 0.03, label = "p<0.01")+
  annotate("text", x = 400, y = 0.07, label = "p<0.05")+
  annotate("text", x = 400, y = 0.12, label = "p<0.10") 




sensibility_vicentin <- sensibility(AR_vicentin)
sensibility_vicentin <- melt(sensibility_vicentin, id=c("obsahead"))

ggplot(aes(x = obsahead, y = value, group = variable, color = variable),
       data = sensibility_vicentin) + 
  geom_line(size = 0.7) + 
  scale_color_manual(name = "", labels = c("t-test", "Wilcoxon"), 
                     values = c(colores[1], colores[4]))+
  theme_bw() + 
  xlab("Observaciones adelante") + 
  ylab("p-valor") + 
  ylim(0,1) + 
  labs(title = "Análisis de sensibilidad - Vicentin",
       caption = "Fuente: elaboración propia") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") + 
  geom_hline(yintercept=0.1, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.05, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.01, linetype="dashed", color = colores[5]) + 
  scale_x_continuous(n.breaks = 20) + 
  annotate("text", x = 400, y = 0.03, label = "p<0.01")+
  annotate("text", x = 400, y = 0.07, label = "p<0.05")+
  annotate("text", x = 400, y = 0.12, label = "p<0.10") 





sensibility_maradona <- sensibility(AR_maradona)
sensibility_maradona <- melt(sensibility_maradona, id=c("obsahead"))

ggplot(aes(x = obsahead, y = value, group = variable, color = variable),
       data = sensibility_maradona) + 
  geom_line(size = 0.7) + 
  scale_color_manual(name = "", labels = c("t-test", "Wilcoxon"), 
                     values = c(colores[1], colores[4]))+
  theme_bw() + 
  xlab("Observaciones adelante") + 
  ylab("p-valor") + 
  ylim(0,1) + 
  xlim(0,200) +
  labs(title = "Análisis de sensibilidad - Fallecimiento Maradona",
       caption = "Fuente: elaboración propia") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") + 
  geom_hline(yintercept=0.1, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.05, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.01, linetype="dashed", color = colores[5]) + 
  scale_x_continuous(n.breaks = 20) + 
  annotate("text", x = 200, y = 0.03, label = "p<0.01")+
  annotate("text", x = 200, y = 0.07, label = "p<0.05")+
  annotate("text", x = 200, y = 0.12, label = "p<0.10") 


sensibility_vacunas<- sensibility(AR_vacunas)
sensibility_vacunas <- melt(sensibility_vacunas, id=c("obsahead"))

ggplot(aes(x = obsahead, y = value, group = variable, color = variable),
       data = sensibility_vacunas) + 
  geom_line(size = 0.7) + 
  scale_color_manual(name = "", labels = c("t-test", "Wilcoxon"), 
                     values = c(colores[1], colores[4]))+
  theme_bw() + 
  xlab("Observaciones adelante") + 
  ylab("p-valor") + 
  ylim(0,1) + 
  xlim(0,170) +
  labs(title = "Análisis de sensibilidad - Llegada de vacunas",
       caption = "Fuente: elaboración propia") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") + 
  geom_hline(yintercept=0.1, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.05, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.01, linetype="dashed", color = colores[5]) + 
  scale_x_continuous(n.breaks = 20) + 
  annotate("text", x = 150, y = 0.03, label = "p<0.01")+
  annotate("text", x = 150, y = 0.07, label = "p<0.05")+
  annotate("text", x = 150, y = 0.12, label = "p<0.10") 


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
  ggtitle(label = "Curva de potencia",
          subtitle = "Anuncio de cuarentena - 19/03/20") + 
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))
  
power_vicentin <- power_analysis(AR_vicentin, 0.25)

p2 <- ggplot(aes(x = sim_assumed, y = power ), data = power_vicentin) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[2]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia",
          subtitle = "Expropiación de Vicentin - 21/06/2020 ") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

power_maradona <- power_analysis(AR_maradona, 0.25)

p3 <- ggplot(aes(x = sim_assumed, y = power ), data = power_maradona) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[3]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia",
          subtitle = "Velorio de Maradona - 26/11/2020 ") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

power_vacunas <- power_analysis(AR_vacunas, 0.25)

p4 <- ggplot(aes(x = sim_assumed, y = power ), data = power_vacunas) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[4]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia",
          subtitle = "Llegada de de vacunas - 24/12/2020 ") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)


