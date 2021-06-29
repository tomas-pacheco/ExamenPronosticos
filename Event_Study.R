# Event study

dir <- "G:\\Mi unidad\\UdeSA\\Pronósticos\\Final\\Data\\dep"
dir <- "C:\\Users\\Abi\\Downloads"

setwd(dir)

options(scipen=999)

library(ggplot2)
library(dplyr)

# Definimos la paleta de colores.

colores <- c("#00ABC5","#edf71c",  "#ff3c84","#FF7F32", "#cfb0b4", "#941cf7")

# Abrimos la base de datos para esta etapa del trabajo.

data <- read.csv("Data_Final_PF2.csv")
data <- data[,c(1,2,4,28)]
data <- na.omit(data)

setwd("C:\\Users\\Tomas\\Desktop\\Pronosticos_Final\\Script\\Graficos")


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

# Segundo evento. Expropiación (o intento de) de Vicentin. 11 de junio 2020

data$ev_vicentin <- 143
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

stargazer(ba, type = "latex")

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
          type = "latex")

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

pvtest <- matrix(NA, nrow = 4, ncol = 4)

pvtest[1,1] <- "Anuncio cuarentena"
pvtest[1,3] <- round(test.p.anuncio$p.value,4)
pvtest[1,4] <- round(test.np.anuncio$p.value,4)
pvtest[1,2] <- round(mean(AR_anuncio_siete),4)

pvtest[2,1] <- "Vicentin"
pvtest[2,3] <- round(test.p.vicentin$p.value,4)
pvtest[2,4] <- round(test.np.vicentin$p.value,4)
pvtest[2,2] <- round(mean(AR_vicentin_siete),4)


pvtest[3,1] <- "Fallecimiento Maradona"
pvtest[3,3] <- round(test.p.maradona$p.value,4)
pvtest[3,4] <- round(test.np.maradona$p.value,4)
pvtest[3,2] <- round(mean(AR_maradona_siete),4)


pvtest[4,1] <- "Llegada de vacunas"
pvtest[4,3] <- round(test.p.vacunas$p.value,4)
pvtest[4,4] <- round(test.np.vacunas$p.value,4)
pvtest[4,2] <- round(mean(AR_vacunas_siete),4)

colnames(pvtest) <- c("Evento", "Media", "p-val ttest", "p-val Wilcoxon")
stargazer(pvtest, type = "latex")


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
  geom_hline(yintercept=0.1, linetype="dashed", color = colores[3])+ 
  geom_hline(yintercept=0.05, linetype="dashed", color = colores[3])+ 
  geom_hline(yintercept=0.01, linetype="dashed", color = colores[3]) + 
  scale_x_continuous(n.breaks = 20) + 
  annotate("text", x = 400, y = 0.03, label = "p<0.01")+
  annotate("text", x = 400, y = 0.07, label = "p<0.05")+
  annotate("text", x = 400, y = 0.12, label = "p<0.10") 

ggsave(file="sensitivity_anuncio.eps", width=6.5, height=4, dpi=300)


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
  labs(title = "Análisis de sensibilidad - Expropiación de Vicentin",
       caption = "Fuente: elaboración propia") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") + 
  geom_hline(yintercept=0.1, linetype="dashed", color = colores[3])+ 
  geom_hline(yintercept=0.05, linetype="dashed", color = colores[3])+ 
  geom_hline(yintercept=0.01, linetype="dashed", color = colores[3]) + 
  scale_x_continuous(n.breaks = 20) + 
  annotate("text", x = 400, y = 0.03, label = "p<0.01")+
  annotate("text", x = 400, y = 0.07, label = "p<0.05")+
  annotate("text", x = 400, y = 0.12, label = "p<0.10") 

ggsave(file="sensitivity_vicentin.eps", width=6.5, height=4, dpi=300)


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
  labs(title = "Análisis de sensibilidad - Funeral de Maradona",
       caption = "Fuente: elaboración propia") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") + 
  geom_hline(yintercept=0.1, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.05, linetype="dashed", color = colores[5])+ 
  geom_hline(yintercept=0.01, linetype="dashed", color = colores[5]) + 
  scale_x_continuous(n.breaks = 20) + 
  annotate("text", x = 180, y = 0.03, label = "p<0.01")+
  annotate("text", x = 180, y = 0.07, label = "p<0.05")+
  annotate("text", x = 180, y = 0.12, label = "p<0.10") 

ggsave(file="sensitivity_maradona.eps", width=6.5, height=4, dpi=300)


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

ggsave(file="sensitivity_vacunas.eps", width=6.5, height=4, dpi=300)


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

power_anuncio <- power_analysis(AR_anuncio_siete, 2)

p1 <- ggplot(aes(x = sim_assumed, y = power ), data = power_anuncio) + 
  geom_segment(aes(x = mean(AR_anuncio_siete), y = 0, 
                   xend = mean(AR_anuncio_siete), yend = 0.405, colour = "segment"),
               size = 1, col = colores[3]) +
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[1]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia",
          subtitle = "Cuarentena - 19/03/20") + 
  theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none" )

  
power_vicentin <- power_analysis(AR_vicentin_siete, 1.5)

p2 <- ggplot(aes(x = sim_assumed, y = power ), data = power_vicentin) + 
  geom_segment(aes(x = mean(AR_vicentin_siete), y = 0, 
                   xend = mean(AR_vicentin_siete), yend = 0.035, colour = "segment"),
               size = 1, col = colores[1]) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[2]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia",
          subtitle = "Vicentin - 11/06/2020 ") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none" ) 


power_maradona <- power_analysis(AR_maradona_siete, 1)

p3 <- ggplot(aes(x = sim_assumed, y = power ), data = power_maradona) + 
  geom_segment(aes(x = mean(AR_maradona_siete), y = 0, 
                   xend = mean(AR_maradona_siete), yend = 0.03, colour = "segment"),
               size = 1, col = colores[4]) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[3]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia",
          subtitle = "Funeral Maradona - 26/11/2020 ") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none" )



power_vacunas <- power_analysis(AR_vacunas_siete, 1)

p4 <- ggplot(aes(x = sim_assumed, y = power ), data = power_vacunas) + 
  geom_segment(aes(x = mean(AR_vacunas_siete), y = 0, 
                   xend = mean(AR_vacunas_siete), yend = 0.125, colour = "segment"),
               size = 1, col = colores[2]) + 
  geom_line(aes(x = sim_assumed , y = power), size = 1, col = colores[4]) + 
  theme_minimal() + 
  ylab("Potencia") + 
  xlab("Retorno anormal hipotético") + 
  ggtitle(label = "Curva de potencia",
          subtitle = "Llegada vacunas - 24/12/2020 ") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none" ) 

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)


# Finalmente vamos a hacer un control de robustez, chequeando si para el primer evento
# hubo algún efecto en el aumento de los retornos del sentimiento del presidente.

data$ev_cuarentena_rob <- 60 - 7
data$ev_cuarentena_time_rob <- data$t - data$ev_cuarentena_rob 

# Calculamos medias antes y despues

round(mean(data$retpres[data$ev_cuarentena_time_rob<0]),4) #anterior
round(mean(data$retpres[data$ev_cuarentena_time_rob>0]),4) #posterior

# Estimamoos

modelo_anuncio_rob <- lm(retpres ~ retmarket , data=subset(data,ev_cuarentena_time_rob<0))  
summary(modelo_anuncio_rob)

# Calculamos ARs.

AR_anuncio_rob <- data$retpres[data$ev_cuarentena_time_rob>=0]-predict(modelo_anuncio, newdata=subset(data,ev_cuarentena_time_rob >=0))

# Testeamos si cada AR medio es igual a 0:

t.test(AR_anuncio_rob, df=length(AR_anuncio_rob)-1, alternative = "two.sided")

# Rechazamos.

# Ahora solo lo hacemos para los siete dias siguientes

AR_anuncio_siete_rob <- AR_anuncio_rob[1:7]
qqnorm(AR_anuncio_siete)
qqline(AR_anuncio_siete)

# Testeamos paramétricamanete.

test.p.anuncio.rob <- t.test(AR_anuncio_siete_rob, df=length(AR_anuncio_siete_rob)-1, 
                         alternative = "two.sided")

# No paramétrico.

test.np.anuncio.rob  <- wilcox.test(AR_anuncio_siete_rob, alternative =  "two.sided")

# No rechazamos ningún test, validando la hipótesis de que fue el día del anuncio.

# Hacemos el mismo ejercicio pero probando con fecha de evento desde una semana antes del evento hasta el mismo día

rob.test <- matrix(NA, nrow = 10, ncol = 4)
colnames(rob.test) <- c("Fecha de evento", "Media ARs", "p-val t-test", "p-val Wilcoxon")
for (i in 0:7){
  data$ev_cuarentena_rob_loop <- 60 - i 
  data$ev_cuarentena_time_rob_loop <- data$t - data$ev_cuarentena_rob_loop 
  modelo_anuncio_rob_loop <- lm(retpres ~ retmarket , data=subset(data,ev_cuarentena_time_rob_loop<0))  
  AR_anuncio_rob <- data$retpres[data$ev_cuarentena_time_rob_loop>=0]-predict(modelo_anuncio_rob_loop,
                                                                              newdata=subset(data,ev_cuarentena_time_rob_loop >=0))
  AR_anuncio_siete_rob_loop <- AR_anuncio_rob[1:7]
  test.p.anuncio.rob.loop <- t.test(AR_anuncio_siete_rob_loop, 
                                    df=length(AR_anuncio_siete_rob_loop)-1, 
                                    alternative = "two.sided")
  test.np.anuncio.rob.loop  <- wilcox.test(AR_anuncio_siete_rob_loop, 
                                           alternative =  "two.sided")
  rob.test[i + 1,2] <- round(mean(AR_anuncio_siete_rob_loop),4)
  rob.test[i + 1,3] <- round(test.p.anuncio.rob.loop$p.value,4)
  rob.test[i + 1,4] <- round(test.np.anuncio.rob.loop$p.value,4)
}

rob.test[1,1] <- "19/03/2019"
rob.test[2,1] <- "18/03/2019"
rob.test[3,1] <- "17/03/2019"
rob.test[4,1] <- "16/03/2019"
rob.test[5,1] <- "15/03/2019"
rob.test[6,1] <- "14/03/2019"
rob.test[7,1] <- "13/03/2019"
rob.test[8,1] <- "12/03/2019"

stargazer(rob.test, type = "latex")

# Vemos que en ningún momento rechazando, haciendo aún más fuerte nuestra hipótesis.

# Graficamos los retornos anormales luego del evento.

AR_anuncio_plot <- data.frame(
  AR_anuncio, 
  seq(1, length(AR_anuncio),1),
  c(AR_anuncio[1:7], rep(NA, length(AR_anuncio)-7)),
  c(seq(1, 7,1),  rep(NA, length(AR_anuncio)-7))
)

colnames(AR_anuncio_plot) <- c("ars", "days", "ars2", "days2")

ggplot(aes(x = days, y = ars), data = AR_anuncio_plot) + 
  theme_bw() + 
  geom_point(size= 1.5) + 
  geom_line(aes(x = days2, y = ars2), color = colores[1]) + 
  geom_hline(yintercept = 0, color = colores[3], size = 1) + 
  labs(title = "Retornos anormales luego del evento", 
       subtitle = "Anuncio de cuarentena", 
       caption = "Fuente: elaboración propia", 
       x = "Días post evento",
       y = "Retorno anormal") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(file="abnormal_post.eps", width=6.5, height=4, dpi=300)

