# Trabajo Series de Tiempo
# Serie: Precio de acciones del grupo Argos
# Características: 
# - Mediciones diarias (días laborales lun-vie)
# - Precio de la acción medido en pesos 

# Grupo 2
# Script elaborado por Sebastian Gil, Cesar Prieto, Gabriel Peña


# Librerías y directorio ----
library(forecast)
library(MASS)
library(tidyverse)
library(lubridate)
library(timetk)
library(zoo)
library(tsibble)
library(dplyr)
library(feasts)
library(fable)
library(tsibble)
library(astsa)
library(nonlinearTseries)
library(tseriesChaos)
library(readxl)
library(readr)

# Importanción y reconocimiento de la base ----
data <- read_delim("Datos/G_ARGOS.csv", delim = ";", escape_double = FALSE, 
                   col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                   trim_ws = TRUE)

colnames(data)
colnames(data) <- c("Fecha","Ultimo","Apertura","Maximo","Minimo","Vol","% var")
head(data)

summary(data)

attach(data)
par(mfrow = c(2,2))
plot(x = Fecha , y = Apertura ,type = "l", main = 'Serie de tiempo variable OPEN')
plot(x = Fecha , y = Ultimo , type = "l", main = 'Serie de tiempo variable CLOSE')
plot(x = Fecha , y = Maximo , type = "l", main = 'Serie de tiempo variable HIGH')
plot(x = Fecha , y = Minimo , type = "l", main = 'Serie de tiempo variable LOW')
par(mfrow = c(1,1))

################################################################################
############ Análisis descriptivo y exploratorio de la serie ###################
Serie <- data[,c(1,2)]
head(Serie)
summary(Serie)

Apertura <- ts(Serie[,2], start = c(2014,11,07), frequency = 7)
plot(Apertura, type = "l", main = 'Serie de tiempo OPEN' )

### Estabilizacion de la varianza
lambda_optimo <- forecast::BoxCox.lambda(Apertura, method = "loglik", lower = -2, upper = 2) 
print(lambda_optimo)

# Aplicar Box-Cox solo a la columna 'Último'
Serie$BoxCox <- BoxCox(Apertura, lambda = 1.7)

# Graficar de la comparacion de las series
par(mfrow = c(1,2))
plot(x = Fecha, y = Apertura, type = "l", main = 'Serie de tiempo OPEN' )
plot(x = Fecha, y = Serie$BoxCox, type = "l", main = 'Serie de tiempo OPEN-BoxCox')
par(mfrow = c(1,1))

# Coincide con el anterior valor de la varianza
MASS::boxcox(lm(Apertura ~1), seq(0, 4.5, length = 50))
a <- MASS::boxcox(lm(Apertura ~1), seq(0, 4.5, length = 50))

a$x[which.max(a$y)]
plot(a, lambda = a$x[which.max(a$y)])
abline(v = a$x[which.max(a$y)], col= "red")

logApertura <- log(Apertura)

par(mfrow = c(1,2))
plot(x = Fecha, y = logApertura, type = "l", main = 'Serie de tiempo Log-Apertura' )
plot(x = Fecha, y = Apertura, type = "l", main = 'Serie de tiempo Apertura')
par(mfrow = c(1,1))

## Estimación de la tendencia -----
fit_Apertura <- lm (Apertura ~ time(Apertura), na.action = NULL)
summary(fit_Apertura)

# modelo en escala log
fit_logApertura <- lm (logApertura ~ time(logApertura), na.action = NULL)
summary(fit_Apertura)

# Regresión paramétrica no hay tendencia lineal

par(mfrow = c(1, 2))
# Primer panel: Gráfico de la serie original
plot(Serie$Fecha, Apertura, type = "l", ylab = "Valor en escala original")
lines(Serie$Fecha, predict(fit_Apertura), col = "red")


# Segundo panel: Gráfico de la serie en escala logarítmica
plot(Serie$Fecha, logApertura, type = "l", ylab = "Valor en escala log")
lines(Serie$Fecha, predict(fit_logApertura), col = "red")
par(mfrow = c(1, 1))


# Eliminamos la tendencia con la predicción de la recta
# se hace con la diferencia de la tend log, y el mod ajustdo

Apertura.sin.tend <- Apertura- predict(fit_Apertura)

# serie sin tendencia en escala log
logApertura.sin.tend <- logApertura- predict(fit_logApertura)

plot(x= Fecha, y = Apertura.sin.tend, type = "l", main = "Serie Log sin tendencia")
acf(Apertura, lag.max = length(Apertura))
pacf(Apertura, lag.max = length(Apertura))

plot(x = Fecha, y = Apertura.sin.tend, type = "l", main = "Apertura sin tendencia")
acf(Apertura.sin.tend, lag.max = length(Apertura.sin.tend)) 
pacf(Apertura.sin.tend, lag.max = length(Apertura.sin.tend)) 


plot(x= Fecha, y = logApertura.sin.tend, main = "Serie Log sin tendencia")
acf(logApertura, lag.max = length(logApertura))
pacf(logApertura, lag.max = length(logApertura))
acf(logApertura.sin.tend, lag.max = length(logApertura.sin.tend)) 
pacf(logApertura.sin.tend, lag.max = length(logApertura.sin.tend)) 


## Promedio Móvil -----


descomposicion_serie <- decompose(Apertura)
plot(descomposicion_serie)

descomposicion_lserie <- decompose(logApertura)
plot(descomposicion_lserie)

## Tendencia de STL -----

indice_serie <- as.Date(as.yearmon(tk_index(Apertura)))
indice_serie1 <- yearmonth(as.yearmon(tk_index(Apertura)))

indice_logserie <- as.Date(as.yearmon(tk_index(logApertura)))
indice_logserie1 <- yearmonth(as.yearmon(tk_index(logApertura)))

#Forma alternativa de extraer el indice
df_serie <- data.frame(Fecha = indice_serie, 
                       serie = as.matrix(Apertura))
str(df_serie)
tibble_serie <- tibble(df_serie)
tsibble_serie <- as_tsibble(df_serie)

# escala log
df_logserie <- data.frame(Fecha = indice_logserie, 
                          logserie = as.matrix(logApertura))
str(df_logserie)
tibble_logserie <- tibble(df_logserie)
tsibble_logserie <- as_tsibble(df_logserie)

# Revisar si hay registros duplicados
duplicates(tibble_serie, index = Fecha) # No hay duplicados
duplicates(tibble_logserie, index = Fecha) # No hay duplicados

# Primera aproximación al ajuste STL 
tsibble_serie %>%
  timetk::plot_time_series(Serie$Fecha, Apertura,
                           .interactive = TRUE,
                           .plotly_slider = TRUE)
# escala log
tsibble_serie %>%
  timetk::plot_time_series(Serie$Fecha, logApertura,
                           .interactive = TRUE,
                           .plotly_slider = TRUE)
# Ajuste STL 
tibble_serie %>%  mutate(
  serie_ajust =smooth_vec(Apertura, span = 0.75, degree =2)
)

# escala log
tibble_logserie %>%  mutate(
  Logserie_ajust =smooth_vec(logApertura, span = 0.75, degree =2)
)

# Ajuste STL moviendo los parámetros
tibble_serie %>% mutate(
  serie_ajus = smooth_vec(Apertura, span = 0.9, degree = 2)) %>% 
  ggplot(aes(Serie$Fecha, Apertura)) + 
  geom_line()+
  geom_line(aes(y = serie_ajus), color = "red")

# escala log
tibble_logserie %>% mutate(
  Logserie_ajus = smooth_vec(logApertura, span = 0.9, degree = 2)) %>% 
  ggplot(aes(Serie$Fecha, logApertura)) + 
  geom_line()+
  geom_line(aes(y = Logserie_ajus), color = "red")

### STL trend y estacionalidad -------------------------------------



tsibble_serie <- as_tsibble(Apertura)
str(tsibble_serie)


tsibble_lserie <- as_tsibble(logApertura)
str(tsibble_serie)


### AQUI NOS DIMOS CUENTA QUE HABIAN FECHAS FALTANTES EN LA SERIE TEMPORAL, 
### TALVEZ DEBIDO A QUE ES UNA SERIE SOBRE ACCIONES DE UNA EMPRESA Y LOS FINES DE 
### SEMANA NO SE OBSERVAN DATOS

# Convierte los huecos implícitos en valores faltantes explícitos
# Imputar valores faltantes con el último valor observado

tsibble_serie <- tsibble_serie %>% group_by_key() %>% 
  fill_gaps() %>% tidyr::fill(value, .direction = "down" )

tsibble_lserie <- tsibble_lserie %>% group_by_key() %>% 
  fill_gaps() %>% tidyr::fill(value, .direction = "down" )

# Verificar si aún quedan valores faltantes
sum(is.na(tsibble_serie))
sum(is.na(tsibble_lserie))


tsibble_serie %>%
  model(
    STL(value ~ trend() + 
          season(window = "periodic"),
        robust = TRUE)) %>% 
  components() %>% 
  autoplot()  

# escala log

tsibble_lserie %>% 
  model(
    STL(value ~ trend() + 
          season(window = "periodic"),
        robust = TRUE)) %>% 
  components() %>% 
  autoplot()

## Diferencia Ordinaria -------------------------------------------
# Usando diferencia ordinaria
tsibble_serie|>mutate(
  diff_serie = tsibble::difference(value, lag = 1, 
                                   differences = 1))|>
  autoplot(.vars = diff_serie) + 
  labs(subtitle = "Cambio del Costo")

# escala log
tsibble_lserie|>mutate(
  diff_lserie = tsibble::difference(value, lag = 1, 
                                    differences = 1))|>
  autoplot(.vars = diff_lserie) + 
  labs(subtitle = "Cambios en escala logarítmicade del Costo")

#--------------------

tsibble_serie <- tsibble_serie|>mutate(
  diff_serie = tsibble::difference(value, lag = 1,
                                   difference = 1))
# escala log
tsibble_lserie <- tsibble_lserie|>mutate(
  diff_lserie = tsibble::difference(value, lag = 1,
                                    difference = 1))

# Diferenciando con base en el objeto ts
dserie <- diff(Apertura)
plot(dserie)

# escala log
dlserie <- diff(logApertura)
plot(dlserie)


## Relaciones no-lineales dispersión ---------------------------------

par(mar = c(3,2,3,2))
astsa::lag1.plot(dserie, 12, corr = T)

#escala log
par(mar = c(3,2,3,2))
astsa::lag1.plot(dlserie, 12, corr = T)

### ACF ---------------------------------------------------------------

acf(dserie, 48, main = "Serie diferenciada de costos")
pacf(dserie, 48)

# escla log
acf(dlserie, 48, main = "Serie diferenciada y con logaritmo de costos")
pacf(dlserie, 48)

## Índice AMI -------------------------------------------------------
# Indice de información mutua
par(mar = c(3,2,3,2))
astsa::lag1.plot(Apertura, 12, corr = F)
nonlinearTseries::mutualInformation(Apertura, lag.max = 100,
                                    n.partitions = 50, 
                                    units = "Bits",
                                    do.plot = TRUE)
# escala log
par(mar = c(3,2,3,2))
astsa::lag1.plot(logApertura, 12, corr = F)
nonlinearTseries::mutualInformation(logApertura, lag.max = 100,
                                    n.partitions = 50, 
                                    units = "Bits",
                                    do.plot = TRUE)

## Explorando la estacionalidad subseries -------------------------

monthplot(dserie)
tsibble_serie %>% na.omit()|>gg_subseries(diff_serie, period = 12)
#ggseasonplot(dserie)   REVISAR A FONDO

# escala log
monthplot(dlserie)
tsibble_lserie %>% na.omit()|>gg_subseries(diff_lserie, period = 12)
#ggseasonplot(dlserie)  REVISAR A FONDO



## Gráfico de cajas --------------------------REVISAR OBJETO 
# basado en el objeto tibble

## NO EJECUTAR LO SIGUIENTE
tibble_sserie %>% na.omit() %>% 
  plot_seasonal_diagnostics(
    .date_var = Fecha,
    .value = diff_sserie, 
    .feature_set = c("month.lbl"), 
    .geom = "boxplot"
  )

library(ggplot2)
ggplot(tibble_sserie %>%na.omit()|>
         mutate(
           Mes = str_c("Mes ", as.character(lubridate::month(Fecha)))
         ), aes(x = diff_sserie)) +
  geom_density(aes(fill = Mes)) +
  ggtitle("LosPass - Estimación de la densidad vía Kernel por mes") +
  facet_grid(rows = vars(as.factor(Mes)))

# escala log
tibble_logserie %>% na.omit() %>% 
  plot_seasonal_diagnostics(
    .date_var = Fecha,
    .value = diff_logserie, 
    .feature_set = c("month.lbl"), 
    .geom = "boxplot"
  )

library(ggplot2)
ggplot(tibble_logserie %>%na.omit()|>
         mutate(
           Mes = str_c("Mes ", as.character(lubridate::month(Fecha)))
         ), aes(x = diff_logserie)) +
  geom_density(aes(fill = Mes)) +
  ggtitle("LosPass - Estimación de la densidad vía Kernel por mes") +
  facet_grid(rows = vars(as.factor(Mes)))


## Periodograma -----------------------------------------------------
# YA PUEDES EJECUTAR DE NUEVO
spectrum(as.numeric(dserie),log='no')

PeriodgramadAperturas=spectrum(as.numeric(dserie),log='no')
ubicacionlogAper=which.max(PeriodgramadAperturas$spec)
sprintf("El valor de la frecuencia donde se máximiza el periodograma para la serie es: %s",PeriodgramadAperturas$freq[ubicacionlogAper])

sprintf("El periodo correspondiente es aproximadamente: %s",1/PeriodgramadAperturas$freq[ubicacionlogAper])


spectrum(as.numeric(dlserie),log='no')

PeriodgramadlAperturas=spectrum(as.numeric(dlserie),log='no')
ubicacionloglAper=which.max(PeriodgramadlAperturas$spec)
sprintf("El valor de la frecuencia donde se máximiza el periodograma para la serie es: %s",PeriodgramadlAperturas$freq[ubicacionloglAper])

sprintf("El periodo correspondiente es aproximadamente: %s",1/PeriodgramadlAperturas$freq[ubicacionloglAper])


## Ajuste de la estocionalidad con componentes de Fourier y Dummy ----

#Variables Dummy y Armónicos
forecast::seasonaldummy(Apertura)
Armonicos = TSA::harmonic(Apertura, m = 1)

# Armóicos
forecast::fourier(Apertura, K = 1)
tiempo = 1 
j = 1
sin ( 2 * pi *tiempo* j/12)
cos ( 2 * pi * tiempo * j /12)

# Gráfica de los armónicos
harmonics = fourier(Apertura, K = 2)
harmonics
par (mar = c(1,4,1,1), mfrow = c(6,2))

for (i in 1:ncol(harmonics)){
  plot(harmonics[,i], 
       type = 'l', xlab = "Time", ylab = colnames(harmonics)[i])
} 

par(mar = rep(4,4), mfrow = c(1,1))

diff_tsibble <- tsibble_serie|>
  mutate(logdiff_air = difference(log(value)))|>
  select(logdiff_air) 

# Explore diferentes valores de K
# Estimar los coeficientes 
Modelo_serie_diff <- diff_tsibble|>
  model('Fourier1Airdiff' = ARIMA(
    logdiff_air~fourier(K=2)+ # coeficientes de fourier de orden 2 
      pdq(0,0,0) + PDQ(0,0,0))) # esto es como un error


real_ajustado1<-diff_tsibble%>%
  left_join(fitted(Modelo_serie_diff,by=index))%>%
  select(-.model) 

real_ajustado1 %>%
  autoplot() +
  geom_line(data=real_ajustado1,aes(y=logdiff_air,colour="real"))+
  geom_line(data=real_ajustado1,aes(y=.fitted,colour="ajustado"))+
  scale_color_manual(name = "real/ajustado", values = c("real" = "black", "ajustado" = "red"))

# Ajuste Dummy

Modelo_serie_diff_Dummy<-diff_tsibble|>model(
  DummyAirdiff=ARIMA(logdiff_air~season()+pdq(0, 0, 0) + PDQ(0, 0, 0))
  
)

Modelo_serie_diff_Dummy<-diff_tsibble%>%
  left_join(fitted(Modelo_serie_diff,by=index))%>%
  select(-.model) 

Modelo_serie_diff_Dummy %>%
  autoplot() +
  geom_line(data=Modelo_serie_diff_Dummy,aes(y=logdiff_air,colour="real"))+
  geom_line(data=Modelo_serie_diff_Dummy,aes(y=.fitted,colour="ajustado"))+
  scale_color_manual(name = "real/ajustado", values = c("real" = "black", "ajustado" = "red"))
# Varios modelos la mismo tiempo


# fable nos deja ver el ajuste con diferentes modelos
ajuste_final_models<-diff_tsibble%>%model(
  Fourier1Airdiff=ARIMA(logdiff_air~fourier(K=1)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  Fourier2Airdiff=ARIMA(logdiff_air~fourier(K=2)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  Fourier3Airdiff=ARIMA(logdiff_air~fourier(K=3)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  DummyAirdiff=ARIMA(logdiff_air~season()+pdq(0, 0, 0) + PDQ(0, 0, 0))
)

glance(ajuste_final_models)

ajuste_final_models %>%
  select(Fourier1Airdiff)%>%coef()

Modelo_serie_diff_models<-diff_tsibble %>%
  left_join(fitted(ajuste_final_models)|>group_by(.model)%>%
              pivot_wider(names_from = .model, values_from = .fitted))

Modelo_serie_diff_models %>%
  autoplot() +
  geom_line(data=Modelo_serie_diff_models,
            aes(y=logdiff_air,colour="real"))+
  geom_line(data=Modelo_serie_diff_models,
            aes(y=Fourier1Airdiff,colour="ajustadoFourier1"))+
  geom_line(data=Modelo_serie_diff_models,
            aes(y=Fourier2Airdiff,colour="ajustadoFourier2"))+ 
  geom_line(data=Modelo_serie_diff_models,
            aes(y=Fourier3Airdiff,colour="ajustadoFourier3"))+
  geom_line(data=Modelo_serie_diff_models,
            aes(y=DummyAirdiff,colour="ajustadoDummy")) +
  scale_color_manual(name = "real/ajustado", 
                     values = c("real" = "black", "ajustadoFourier1" = "red",
                                "ajustadoFourier2" = "blue","ajustadoFourier3"="green",
                                "ajustadoDummy"="yellow"))

