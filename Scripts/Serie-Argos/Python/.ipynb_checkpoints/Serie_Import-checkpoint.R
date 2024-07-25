# Trabajo Series de Tiempo
# Serie: Importaciones Colombianas via maritimas
# Características: 
# - Mediciones Mensuales (Meses)
# - Precio de la carga al momento de ser transportada via maritima

# Grupo 2
# Script elaborado por Cesar Prieto, Gabriel Peña y Sebastian Gil


# Librerías y directorio --------------------------------------------------
library(readxl)
library(readr)


# Importanción y reconocimiento de la base ----
importaciones <- read.csv("Datos/Importaciones.csv")[1:120,]
names(importaciones)

# Definiciones
# VACIP: Valor CIF pesoso de la mercancia. Valor de las mercancías
#       que incluye el flete hasta el lugar de destino
# VACID: Valor CIF en dólares de la mercancia
# VAFODO: Valor FOB dólares de la mercancía. Valor de la mercancia
#       en el momento que se carga a bordo del medio de transporte 
#       marítimo.
# FLETE: costo a pagar por el desplazamiento de una carga en un medio
#       de transporte.
# IMP1: impuesto a las ventas.
# PBK: peso bruto en kilos.
# PNK: peso neto en kilos.
sum(is.na(importaciones[,3]))
a <- c()
for ( i in 3:9){
  a <- c(a, sum(is.na(importaciones[,i])))
}

vacip <- ts(importaciones[,2], start = c(2012, 01), frequency =12)
vafodo <- ts(importaciones[,3], start = c(2012, 01), frequency =12)
flete <- ts(importaciones[,4], start = c(2012, 01), frequency =12)
imp1 <- ts(importaciones[,5], start = c(2012, 01), frequency =12)
vacid <- ts(importaciones[,6], start = c(2012, 01), frequency =12)
pbk <- ts(importaciones[,7], start = c(2012, 01), frequency =12)
pnk <- ts(importaciones[,8], start = c(2012, 01), frequency =12)

# Primera exploración de variables
plot(vacip)# si
plot(vafodo) # si
plot(flete) # si 
plot(imp1) # no
plot(vacid) # no 
plot(pbk) # si
plot(pnk) # si


# Librerias ---------------------------------------------------------------

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
library(astsa)
library(nonlinearTseries)
library(tseriesChaos)
library(fabletools)
library(TSA)


library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(TSA)



# Descriptivo -------------------------------------------------------------
# vafodo
# flete
# pbk
# pnk
serie <- vafodo/1000000000
plot(serie)

## Estabilización de la varianza -----

# Serie estacionaria ???
# Esta función devuelve el valor de lambda
forecast::BoxCox.lambda(serie, method = "loglik", 
                        lower = -2, # limitie menor para el lambda 
                        upper = 2) # limite superior para el lambda
plot(BoxCox(serie, lambda = -0.1))
# NO parece haber mucha diferencia entre ambos graficos
# plot(BoxCox(serie, lambda = 0.75)) y plot(serie)
# podría decirse que la varianza es la misma


# Coincide con el anterior valor de la varianza
MASS::boxcox(lm(serie ~ 1), seq(-2, 7, length = 50))
a <- MASS::boxcox(lm(serie ~ 1), seq(-2, 7, length = 50))

a$x[which.max(a$y)]
abline(v = a$x[which.max(a$y)], col= "red")

<<<<<<< HEAD
# Graficar la serie original
plot(serie, type = "l", col = "black", lwd = 2, main = "Gráfica con Box-Cox")
# Graficar BoxCox con lambda = 0.75
plot(BoxCox(serie, lambda = BC.f), col = "black", lwd = 2,
     ylim = c(2, 3), main = "Gráfica con Box-Cox", ylab = "")
lines(BoxCox(serie, lambda = BC.m), col = "blue", lwd = 2)
=======

plot(serie)
plot(BoxCox(serie, lambda = -0.1))
plot(BoxCox(serie, lambda = a$x[which.max(a$y)]))

# Graficar la serie original
plot(serie, type = "l", col = "black", lwd = 2, main = "Gráfica con Box-Cox")
# Graficar BoxCox con lambda = 0.75
plot(BoxCox(serie, lambda = -0.1), col = "red", lwd = 2,
     ylim = c(2.4, 6.3), main = "Gráfica con Box-Cox", ylab = "")
# Graficar BoxCox con lambda = 0.4545
lines(BoxCox(serie, lambda = a$x[which.max(a$y)]),
      col = "green", lwd = 2, xlab = "")


# Agregar leyenda
legend("topright", legend = c("Lambda = -0.1", "Lambda = 0.0909"), 
       col = c("red", "green"), lwd = 2)
>>>>>>> 4beb33b8eaa9a52e1a1ce2832ca447d170a0f11e

min(serie)
serie
lserie <- log(serie)
plot(lserie)
plot(serie)

# Definimos la serie con lambda 0.4545
serie <- BoxCox(serie, lambda = 0.4545)
plot(serie)

## Estimación de la tendencia -----
fit_serie <- lm (serie ~ time(serie), na.action = NULL)
summary(fit_serie)

# modelo en escala log
fit_lserie <- lm (lserie ~ time(serie), na.action = NULL)
summary(fit_lserie)

# Justificar por qué no le quitamos la tendencia
# Regresión paramétrica no hay tendencia lineal

plot(serie)
plot(serie, ylab = "Valor")
abline(fit_serie, col = "red") 

# modelo en escala log
plot(lserie, ylab = "Valor/Costo en escala logarítmica")
# añadimos la recta de ajuste
abline(fit_lserie, col = "red") 

# Eliminamos la tendencia con la predicción de la recta
# se hace con la diferencia de la tend log, y el mod ajustdo

serie.sin.tend <- serie- predict(fit_serie)
# serie sin tendencia en escala log
lserie.sin.tend <- lserie- predict(fit_lserie)

plot(serie, main = "Serie sin tendencia", col = "black", lwd = 1.7,
     ylim =c(-10,20))
lines(serie.sin.tend, col = "red", lwd = 1.7)

# Agregar leyenda
legend("topright", legend = c("Lambda = 0.4545", "Sin tendencia" ), 
       col = c("black", "red"), lwd = 2)

acf(serie, lag.max = length(lserie))

# nos da -indicios- de estacionalidad
acf(serie.sin.tend, lag.max = length(serie.sin.tend)) 


plot(lserie.sin.tend, main = "Serie Log sin tendencia")
acf(lserie, lag.max = length(lserie))
# nos da -indicios- de estacionalidad
acf(lserie.sin.tend, lag.max = length(lserie.sin.tend)) 


## Promedio Móvil -----

descomposicion_serie <- decompose(serie)
plot(descomposicion_serie)

descomposicion_serie.sin.tend <- decompose(serie.sin.tend)
plot(descomposicion_serie.sin.tend)

descomposicion_lserie <- decompose(lserie)
plot(descomposicion_lserie)

## Tendencia de STL -----
# Ajuste no paramétrico
indice_serie <- as.Date(as.yearmon(tk_index(serie)))
indice_serie1 <- yearmonth(as.yearmon(tk_index(serie)))
plot(indice_serie1)

indice_logserie <- as.Date(as.yearmon(tk_index(lserie)))
indice_logserie1 <- yearmonth(as.yearmon(tk_index(lserie)))

# Forma alternativa de extraer el indice
df_serie <- data.frame(Fecha = indice_serie, 
                          sserie = as.matrix(serie))
str(df_serie)
tibble_serie <- tibble(df_serie)
tsibble_serie <- as_tsibble(df_serie)

# escala log
df_logserie <- data.frame(Fecha = indice_logserie, 
                              logserie = as.matrix(lserie))
str(df_logserie)
tibble_logserie <- tibble(df_logserie)
tsibble_logserie <- as_tsibble(df_logserie)

# Revisar si hay registros duplicados
# duplicates(tibble_logserie, key = Null, index = Fecha)

# Primera aproximación al ajuste STL 
tsibble_serie %>%
  timetk::plot_time_series(Fecha, sserie,
                           .interactive = TRUE,
                           .plotly_slider = TRUE)
# escala log
tsibble_serie %>%
  timetk::plot_time_series(Fecha, logserie,
                           .interactive = TRUE,
                           .plotly_slider = TRUE)

# Ajuste STL 
tibble_serie %>%  mutate(
  serie_ajust =smooth_vec(sserie,
                          span = 0.14,
                          degree =2)
)

# escala log
tibble_logserie %>%  mutate(
  Logserie_ajust =smooth_vec(logserie, span = 0.75, degree =2)
)

# Ajuste STL moviendo los parámetros
tibble_serie %>% mutate(
  serie_ajus = smooth_vec(sserie, 
                          span = 0.15, # mas bajo mejor ajuste
                          degree = 2)) %>% 
  ggplot(aes(Fecha, sserie)) + 
  geom_line()+
  geom_line(aes(y = serie_ajus), color = "red")

# escala log
tibble_logserie %>% mutate(
  Logserie_ajus = smooth_vec(logserie, span = 0.9, degree = 2)) %>% 
  ggplot(aes(Fecha, logserie)) + 
  geom_line()+
  geom_line(aes(y = Logserie_ajus), color = "red")

### STL trend y estacionalidad -------------------------------------

tsibble_serie <- as_tsibble(serie)
str(tsibble_serie)
# escala log
tsibble_lserie <- as_tsibble(lserie)
str(tsibble_lserie)

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

tsibble_serie <- tsibble_serie|>mutate(
  diff_serie = tsibble::difference(value, lag = 1,
                                    difference = 1))
# escala log
tsibble_lserie <- tsibble_lserie|>mutate(
  diff_lserie = tsibble::difference(value, lag = 1,
                                      difference = 1))

# Diferenciando basado en el objeto tibble
tibble_serie %>% 
  mutate(diff_sserie = sserie - lag(sserie)) %>% 
  plot_time_series(Fecha, diff_sserie)

tibble_serie %>% 
  mutate(diff_sserie = sserie - lag(sserie)) %>% 
  plot_time_series(Fecha, diff_sserie)

tibble_sserie <- tibble_serie %>% 
  mutate(diff_sserie = sserie - lag(sserie))

# escala log
tibble_logserie %>% 
  mutate(diff_Logserie = logserie - lag(logserie)) %>% 
  plot_time_series(Fecha, diff_Logserie)

tibble_logserie %>% 
  mutate(diff_Logserie = logserie - lag(logserie)) %>% 
  plot_time_series(Fecha, diff_Logserie)

tibble_logserie <- tibble_logserie %>% 
  mutate(diff_logserie = logserie - lag(logserie))

# Diferenciando con base en el objeto ts
dserie <- diff(serie)
plot(dserie)

# escala log
dlserie <- diff(lserie)
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
astsa::lag1.plot(serie, 12, corr = F)
nonlinearTseries::mutualInformation(serie, lag.max = 100,
                                    n.partitions = 50, 
                                    units = "Bits",
                                    do.plot = TRUE)
# escala log
par(mar = c(3,2,3,2))
astsa::lag1.plot(lserie, 12, corr = F)
nonlinearTseries::mutualInformation(lserie, lag.max = 100,
                                    n.partitions = 50, 
                                    units = "Bits",
                                    do.plot = TRUE)
## Explorando la estacionalidad subseries -------------------------

monthplot(dserie)
tsibble_serie %>% na.omit()|>gg_subseries(diff_serie, period = 12)

tibble_sserie %>% na.omit()|>
  mutate(
    Mes = str_c("", as.character(lubridate::month(Fecha, label = TRUE)))
  ) %>% 
  plot_time_series(
    .date_var = Fecha, 
    .value = diff_sserie, 
    .facet_vars = Mes, 
    .facet_ncol = 4, 
    .color_var = Mes,
    .facet_scale = "fixed", 
    .interactive = FALSE,
    .legend_show = FALSE, 
    .smooth = FALSE
  )
library(forecast)
ggseasonplot(dserie)  

# escala log
monthplot(dlserie)
tsibble_lserie %>% na.omit()|>gg_subseries(diff_lserie, period = 12)

tibble_logserie %>% na.omit()|>
  mutate(
    Mes = str_c("", as.character(lubridate::month(Fecha, label = TRUE)))
  ) %>% 
  plot_time_series(
    .date_var = Fecha, 
    .value = diff_logserie, 
    .facet_vars = Mes, 
    .facet_ncol = 4, 
    .color_var = Mes,
    .facet_scale = "fixed", 
    .interactive = FALSE,
    .legend_show = FALSE, 
    .smooth = FALSE
  )
library(forecast)
ggseasonplot(dlserie)  

## Gráfico de cajas --------------------------------------------------
# basado en el objeto tibble

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

spectrum(as.numeric(dserie), log = "no") #periodograma de la serie sin tendencia
spectrum(as.numeric(dserie)) # al hacerlo en escala log los 

# Ubicación del valor que hace al periodograma más grande
ubicacionlogserie <- which.max(Periodgramadlserie$spec)
sprintf("El valor de la frecuencia donde se máximiza el periodograma para le series es: %s", 
        PeriodgramadlAirPass$frq[ubicacionlogAir])

sprintf("El periodo correspondiente es aproximadamente: %s", 
        1/PeriodgramadlAirPass$frq[ubicacionlogAir])


# escala log
spectrum(as.numeric(dlserie), log = "no") #periodograma de la serie sin tendencia
spectrum(as.numeric(dlserie)) # al hacerlo en escala log los 

# Ubicación del valor que hace al periodograma más grande
ubicacionlogserie <- which.max(PeriodgramadlAirPass$spec)
sprintf("El valor de la frecuencia donde se máximiza el periodograma para le series es: %s", 
        PeriodgramadlAirPass$frq[ubicacionlogAir])

sprintf("El periodo correspondiente es aproximadamente: %s", 
        1/PeriodgramadlAirPass$frq[ubicacionlogAir])


## Ajuste de la estocionalidad con componentes de Fourier y Dummy ----
# linea de prueba
tsibble_lserie <- as_tsibble(serie)
tsibble_serie <- as_tsibble(serie)

#Variables Dummy y Armónicos
forecast::seasonaldummy(serie)
Armonicos = TSA::harmonic(serie, m = 1)

# Armóicos
forecast::fourier(serie, K = 1)
tiempo = 1 
j = 1
sin ( 2 * pi *tiempo* j/12)
cos ( 2 * pi * tiempo * j /12)

# Gráfica de los armónicos
harmonics = fourier(serie, K = 6)
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















