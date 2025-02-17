# Trabajo Series de Tiempo
# Serie: Precio de acciones del grupo Argos
# Características: 
# - Mediciones diarias (días laborales lun-vie)
# - Precio de la acción medido en pesos 

# Autor: Cesar Prieto

# Librerías y directorio ----
library(easypackages)

libraries(c("zoo", "TSA", "MASS", "readr", "dplyr", "fable", "astsa", "readxl", "feasts", 
           "timetk", "plotly", "tibble", "tsibble", "forecast", "tidyverse", "lubridate", 
           "modeldata", "fabletools", "tseriesChaos", "nonlinearTseries", "rsample", "modeltime", "parsnip"))


# IMPORTACION Y RECONOCIMIENTO DE LA BASE ----

G_ARGOS <- read_delim("Datos/G_ARGOS.csv", delim = ";", escape_double = FALSE,
                      col_types = cols(Fecha = col_date(format = "%d/%m/%Y")), 
                      trim_ws = TRUE)

str(G_ARGOS)

## COMPLETAR DATOS FALTANTES MEDIANTE LA FUNCION zoo::na.locf

FC <- data.frame(Fecha = seq(min(G_ARGOS$Fecha), max(G_ARGOS$Fecha), by = "1 day"))

G_ARGOS <- merge(FC, G_ARGOS, by = "Fecha", all.x = TRUE)

G_ARGOS$Último <- na.locf(G_ARGOS$Último)
G_ARGOS$Apertura <- na.locf(G_ARGOS$Apertura)
G_ARGOS$Máximo <- na.locf(G_ARGOS$Máximo)
G_ARGOS$Mínimo <- na.locf(G_ARGOS$Mínimo)

colnames(G_ARGOS) <- c("Fecha","Ultimo","Apertura","Maximo","Minimo")
head(G_ARGOS)

summary(G_ARGOS)

attach(G_ARGOS)
par(mfrow = c(2,2))
plot(x = Fecha , y = Apertura ,type = "l", main = 'Serie de tiempo variable Apertura')
plot(x = Fecha , y = Ultimo , type = "l", main = 'Serie de tiempo variable Ultimo')
plot(x = Fecha , y = Maximo , type = "l", main = 'Serie de tiempo variable Maximo')
plot(x = Fecha , y = Minimo , type = "l", main = 'Serie de tiempo variable Minimo')
par(mfrow = c(1,1))


# SERIE PARA LA APERTURA DIARIA DEL ACTIVO EN LA BOLSA ---- 


Apertura <- ts(data = G_ARGOS$Apertura, start = c(2010,4),frequency = 365)
class(Apertura)
str(Apertura)
head(Apertura)

plot(Apertura, main="PRECIO DIARIO DE LA ACCION DEL\nGRUPO ARGOS EN LA APERTURA",
     ylab="Miles de pesos", xlab="Año")

# ESTABILIZACION DE LA VARIANZA ----

Lambda1 <- forecast::BoxCox.lambda(Apertura, method = "loglik")
BoxCox1 <- BoxCox(Apertura, lambda = 0.25) 

plot(BoxCox1, main="TRANSFORMACION BOXCOX PRECIO DIARIO DE\nLA ACCION DEL GRUPO ARGOS EN LA APERTURA",
     ylab="Trans BoxCox1", xlab="Año")


MASS::boxcox(lm(Apertura ~ 1),seq(-0.5, 0.5, length = 1000))
BoxCox2 <- BoxCox(Apertura, lambda = 0.17)

plot(BoxCox2, main="TRANSFORMACION BOXCOX PRECIO DIARIO DE\nLA ACCION DEL GRUPO ARGOS EN LA APERTURA",
     ylab="Trans BoxCox2", xlab="Año")

# ESTIMACION Y ELIMINACION DE LA TENDENCIA ---- 

fit_Aper <- lm(BoxCox2~time(BoxCox2), na.action=NULL)
summary(fit_Aper)

plot.ts(BoxCox2, main = "Tendencia del modelo",
        ylab = 'Trans BoxCox2', xlab = 'Años')
abline(fit_Aper, col = "red")

Aper.sin.tend <- BoxCox2 - predict(fit_Aper)
plot(Aper.sin.tend, main='SERIE SIN TENDENCIA',xlab='Año',ylab='Trans BoxCox2')

acf(Aper.sin.tend, lag.max = 100)

### LOESS PARA BOXCOX ----

df_Aper <- data.frame(Fecha = G_ARGOS$Fecha, BoxCox2 = as.matrix(BoxCox2))
str(df_Aper)

tibble_Aper <- as_tibble(df_Aper)

tibble_Aper%>%
  timetk::plot_time_series(Fecha, BoxCox2,.interactive = TRUE,.plotly_slider = TRUE)

tibble_Aper%>%
  mutate( Mod_BoxCox2 = smooth_vec(BoxCox2,span = 0.75, degree = 2) )

Plot1 <- tibble_Aper %>%
  mutate(Mod_BoxCox2 = smooth_vec(BoxCox2, span = 0.25, degree = 2))%>%
  ggplot(aes(Fecha, BoxCox2)) +
  geom_line() +
  geom_line(aes(y = Mod_BoxCox2), color = "darkblue") +
  labs(title = "Estimacion de LOESS de la tendencia", x = "AÑOS", y = "Trans BoxCox2")

tibble_Aper <- tibble_Aper %>%
  mutate(Mod1_BoxCox2 = smooth_vec(BoxCox2, span = 0.25, degree = 2))

Aper.sin.LOESS <- BoxCox2 - as.numeric(tibble_Aper$Mod1_BoxCox2)

plot.ts(Aper.sin.LOESS, main='Serie sin tendencia LOESS',xlab='Año',
        ylab='Trans BoxCox2')

acf(Aper.sin.LOESS, lag.max = 365)

### LOESS PARA DATOS EN ESCALA ORIGINAL ----

df_Apertura <- data.frame(Fecha = G_ARGOS$Fecha, Apertura = as.matrix(Apertura))
str(df_Apertura)

tibble_Apertura <- as_tibble(df_Apertura)

tibble_Apertura%>%
  timetk::plot_time_series(Fecha, Apertura,.interactive = TRUE,.plotly_slider = TRUE)

tibble_Apertura%>%
  mutate( Mod_Apertura = smooth_vec(Apertura,span = 0.75, degree = 2))

Plot2 <- tibble_Apertura %>%
  mutate(Mod_Apertura = smooth_vec(Apertura, span = 0.25, degree = 2))%>%
  ggplot(aes(Fecha, Apertura)) +
  geom_line() +
  geom_line(aes(y = Mod_Apertura), color = "darkblue") +
  labs(title = "Estimacion de LOESS de la tendencia", x = "AÑOS", y = "Apertura")

tibble_Apertura <- tibble_Apertura %>%
  mutate(Mod1_Apertura = smooth_vec(Apertura, span = 0.25, degree = 2))

Apertura.sin.LOESS <- Apertura - as.numeric(tibble_Apertura$Mod1_Apertura)

plot.ts(Apertura.sin.LOESS, main='Serie sin tendencia LOESS',xlab='Año',
        ylab='Apertura')

acf(Apertura.sin.LOESS, lag.max = 365)

# DESCOMPOSICION STL ----

tsibble_Aper <- as_tsibble(df_Aper)

tsibble_Aper %>%
  model(
    STL(BoxCox2 ~ trend() +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


Mod_STL <- tsibble_Aper %>%
  model(STL(BoxCox2 ~ trend() + season(window = "periodic"), robust = TRUE))

# ESTIMACION DE LA TENDENCIA POR STL ----

Comp <- components(Mod_STL)
Sin.Tend.STL <- BoxCox2 - Comp$trend

plot(Sin.Tend.STL, main='Serie sin tendencia STL', xlab="Año", ylab="Trans BoxCox")

acf(Sin.Tend.STL, lag.max = 100)

# ESTIMACION DE LA TENDENCIA USANDO SPLINES ----

Spl_BoxCox <- smooth.spline(x = time(BoxCox2), y = BoxCox2, spar = 0.90)

Sin.Tend.Spl <- BoxCox2 - Spl_BoxCox$y

plot(BoxCox2, main='ESTIMACION POR SPLINES DE LA TENDENCIA', ylab="BoxCox2", xlab="AÑO")
lines(x = Spl_BoxCox$x, y=Spl_BoxCox$y, col = 'red')

plot(Sin.Tend.Spl, main='Serie sin tendencia SPLINES',xlab='Año',ylab="BoxCox")

acf(Sin.Tend.Spl, lag.max = 100)

# ESTIMACION DE LA TENDENCIA USANDO REGRESION KERNEL ----

Ker_BoxCox <- ksmooth(x = time(BoxCox2), y = BoxCox2, kernel = "normal", bandwidth = 0.3)

plot(BoxCox2)
lines(x = Ker_BoxCox$x, Ker_BoxCox$y, col = "red")

Sin.Tend.Ker <- BoxCox2 - Ker_BoxCox$y
plot(Sin.Tend.Ker, main="Serie sin tendencia KERNEL", xlab="AÑO", ylab="BoxCoX")

acf(Sin.Tend.Ker, lag.max = 100)

# Dickey-Fuller PARA DETERMINAR SI LA SERIE ES ESTACIONARIA ----

ar(BoxCox2) #El coeficiente para el primer rezago indica una fuerte correlacion
tseries::adf.test(BoxCox2, alternative = "stationary", k = 12) # La prueba indica que la serie no es estacionaria 

Diff_BoxCox <- diff(BoxCox2,lag = 1) #Serie Diferenciada
ar(Diff_BoxCox) 
# Los coeficientes negativos y pequeños en este modelo podrían indicar una menor 
# dependencia de los valores pasados en los valores actuales, lo que es deseable
# para una serie estacionaria.

tseries::adf.test(Diff_BoxCox)# La prueba indica que la serie diferenciada si es estacionaria

plot(Diff_BoxCox)

acf(Diff_BoxCox, lag.max = 100)

# RELACIONES NO LINEALES PARA LA SERIE EN TRANFORMADA Y DIFERENCIADA ----

par(mar = c(3,2,3,2))
astsa::lag1.plot(Diff_BoxCox, 12, corr = T)



par(mar = c(3,2,3,2))
astsa::lag1.plot(Sin.Tend.Ker, 12, corr = T)


#INDICE DE INFORMACION MUTUA (AMI) PARA LA SERIE EN TRANSFORMADA Y DIFERENCIADA ----

par(mar = c(3,2,3,2))
astsa::lag1.plot(Diff_BoxCox, 12, corr = F)
nonlinearTseries::mutualInformation(Diff_BoxCox, lag.max = 100,
                                    n.partitions = 50, 
                                    units = "Bits",
                                    do.plot = TRUE)

par(mar = c(3,2,3,2))
astsa::lag1.plot(Sin.Tend.Ker, 12, corr = F)
nonlinearTseries::mutualInformation(Sin.Tend.Ker, lag.max = 100,
                                    n.partitions = 50, 
                                    units = "Bits",
                                    do.plot = TRUE)

# MAPAS DE CALOR

TSstudio::ts_heatmap(Sin.Tend.Ker, padding = FALSE  ,
                     title = "Mapa de calor - Apertura Dif Argos en bolsa dias año")


# DETECCION DE ESTACIONALIDAD DE LA SERIE TRANSF Y DIFF  ----

acf(Diff_BoxCox,lag.max = 365, main='Serie diferenciada')
pacf(Diff_BoxCox,lag.max = 365, main='Serie diferenciada')
Periodo.Diff_BoxCox <- spectrum(BoxCox2, main = "Periodograma serie diferenciada",
                                xlim = c(0,10), log = "no", )
abline(v = Periodo.Diff_BoxCox$freq[match(max(Periodo.Diff_BoxCox$spec),
                                          Periodo.Diff_BoxCox$spec)], col='red')

periodograma <- Periodo.Diff_BoxCox
max(Periodo.Diff_BoxCox$spec)
periodograma$freq[match(max(periodograma$spec),periodograma$spec)]
periodo=1/periodograma$freq[match(max(periodograma$spec),periodograma$spec)]
periodo # El periodo estimado es de aproximadamente 3.424658 días

# DETECCION DE ESTACIONALIDAD DE LA SERIE SIN TEND POR KERNEL  ----

acf(Sin.Tend.Ker,lag.max = 365, main='Serie sin tendecia Kernel')
pacf(Sin.Tend.Ker,lag.max = 365, main='Serie sin tendecia Kernel')
Periodo.Sin.Tend.Ker <- spectrum(Sin.Tend.Ker, main = "Periodograma Serie sin Tend Kernel", 
                                 xlim = c(0,20), log = "no")
abline(v = Periodo.Sin.Tend.Ker$freq[match(max(Periodo.Sin.Tend.Ker$spec),
                                           Periodo.Sin.Tend.Ker$spec)], col='red')

periodograma <- Periodo.Sin.Tend.Ker
max(Periodo.Sin.Tend.Ker$spec)
periodograma$freq[match(max(periodograma$spec),periodograma$spec)]
periodo=1/periodograma$freq[match(max(periodograma$spec),periodograma$spec)]
periodo

# DIAGNOSTICO DE LA SERIE POR DÍAS DE LA SEMANA ----

Tb_BoxCox<-as_tsibble(Diff_BoxCox,index=tibble(fecha))
colnames(Tb_BoxCox)<-c("Fecha","Apertura")

# Definir columna de día y el mes como factor (abreviado)
Tb_BoxCox$dia <- wday(Tb_BoxCox$Fecha, label = TRUE, abbr = TRUE, week_start = 1)
Tb_BoxCox$mes <- factor(month.abb[month(Tb_BoxCox$Fecha)], levels = month.abb)

Tb_BoxCox %>%
  mutate(diff_ND = Apertura - lag(Apertura)) %>%
  ggplot(aes(x = dia, y = diff_ND)) +
  geom_boxplot() +
  labs(title = "Distribución de diferencias díarias", x = "Día", y = "Diferencia respecto al valor anterior")

# DIAGNOSTICO DE LA SERIE POR MESES DEL AÑO ----

Tb_BoxCox <- Tb_BoxCox %>%
  mutate(mes = factor(month.abb[month(Fecha)], levels = month.abb))

Tb_BoxCox %>%
  mutate(diff_ND = Apertura - lag(Apertura)) %>%
  ggplot(aes(x = mes, y = diff_ND)) +
  geom_boxplot() +
  labs(title = "Distribución de diferencias mensuales", x = "Mes", y = "Diferencia respecto al valor anterior")

# MODELAMIENTO DE LA ESTACIONALIDAD DE LA SERIE ----


TsbApertura <- as_tsibble(G_ARGOS[,c(1,3)], index = Fecha)
forecast::seasonaldummy(Apertura)
Armonicos=TSA::harmonic(Apertura, m = 1)

### Armónicos
forecast::fourier(Apertura,K=1)
tiempo=1
j=1
sin(2*pi*tiempo*j/12)
cos(2*pi*tiempo*j/12)


###Gráfica de los armónicos
harmonics = fourier(Apertura, K = 6)
harmonics
par(mar = c(1,4,1,1), mfrow = c(6,2))
for(i in 1:ncol(harmonics)){
  plot(harmonics[,i], type = 'l', xlab = "Time", ylab = colnames(harmonics)[i])
}
par(mar = rep(4, 4), mfrow=c(1,1))

diff_TsbApertura <- TsbApertura |> 
  mutate(logdiff_apertura = difference(log(Apertura))) |> 
  select(Fecha, logdiff_apertura)

Modelo_serie_diff<-diff_TsbApertura|>model(
  `Fourier1Airdiff`=ARIMA(logdiff_apertura~fourier(K=2)+pdq(0, 0, 0) + PDQ(0, 0, 0))
)

real_ajustado1<-diff_TsbApertura%>%left_join(fitted(Modelo_serie_diff,by=index))%>%select(-.model) 

real_ajustado1 %>%
  autoplot() +
  geom_line(data=real_ajustado1,aes(y=logdiff_apertura,colour="real"))+
  geom_line(data=real_ajustado1,aes(y=.fitted,colour="ajustado"))+
  scale_color_manual(name = "real/ajustado", values = c("real" = "black", "ajustado" = "red"))

#####Ajuste Dummy

Modelo_serie_diff_Dummy <- diff_TsbApertura|>model(
  `DummyAirdiff`=ARIMA(logdiff_apertura~season()+pdq(0, 0, 0) + PDQ(0, 0, 0))
)

Modelo_serie_diff_Dummy <- diff_TsbApertura %>% left_join(fitted(Modelo_serie_diff,by=index)) %>%
  select(-.model) 

Modelo_serie_diff_Dummy %>%
  autoplot() +
  geom_line(data=Modelo_serie_diff_Dummy,aes(y=logdiff_apertura,colour="real"))+
  geom_line(data=Modelo_serie_diff_Dummy,aes(y=.fitted,colour="ajustado"))+
  scale_color_manual(name = "real/ajustado", values = c("real" = "black", "ajustado" = "red"))

#### Varios modelos la mismo tiempo

ajuste_final_models<-diff_TsbApertura%>%model(
  `Fourier1Airdiff`=ARIMA(logdiff_apertura~fourier(K=1)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  `Fourier2Airdiff`=ARIMA(logdiff_apertura~fourier(K=2)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  `Fourier3Airdiff`=ARIMA(logdiff_apertura~fourier(K=3)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  `DummyAirdiff`=ARIMA(logdiff_apertura~season()+pdq(0, 0, 0) + PDQ(0, 0, 0))
)

glance(ajuste_final_models)

ajuste_final_models %>%
  select(Fourier1Airdiff)%>%coef()

Modelo_serie_diff_models <- diff_TsbApertura %>%
  left_join(fitted(ajuste_final_models)|>group_by(.model)%>%
              pivot_wider(names_from = .model, values_from = .fitted))

ajuste_final_models <- diff_TsbApertura %>% model(
  `Fourier1Airdiff` = ARIMA(logdiff_apertura ~ fourier(K = 1) + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  `Fourier2Airdiff` = ARIMA(logdiff_apertura ~ fourier(K = 2) + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  `Fourier3Airdiff` = ARIMA(logdiff_apertura ~ fourier(K = 3) + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  `DummyAirdiff` = ARIMA(logdiff_apertura ~ season() + pdq(1, 0, 0) + PDQ(0, 0, 0))
)

# Unir los valores ajustados
Modelo_serie_diff_models <- diff_TsbApertura %>%
  left_join(
    fitted(ajuste_final_models) %>% 
      group_by(.model) %>%
      pivot_wider(names_from = .model, values_from = .fitted),
    by = "Fecha"
  )


# Convertir a un formato largo para facilitar el uso de facet_wrap
Modelo_serie_diff_long <- Modelo_serie_diff_models %>%
  pivot_longer(cols = starts_with("Fourier") | starts_with("Dummy"),
               names_to = "Modelo",
               values_to = "Ajuste")

# Crear gráficas separadas usando facet_wrap
Modelo_serie_diff_long %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = logdiff_apertura, colour = "Real"), color = "black") +
  geom_line(aes(y = Ajuste, colour = Modelo)) +
  scale_color_manual(values = c( "red", "blue", "green", "yellow")) +
  facet_wrap(~ Modelo, ncol = 2) +
  labs(title = "MODELAMIENTO DE LA ESTACIONALIDAD DE LA SERIE",
       y = "Diferencia logarítmica", color = "Series") +
  theme_minimal()


# SUAVIZAMIENTO EXPONENCIAL ----

P <- expand.grid('beta' = list(F, NULL),
                 'gamma' = list(F, NULL),
                 'seasonal' = list('additive', 'multiplicative'))

# Definir las fechas de inicio y fin
fecha_inicio <- as.Date("2010-01-05")
fecha_fin <- as.Date("2019-12-31")

# Crear un vector de fechas
fechas <- seq(fecha_inicio, fecha_fin, by = "day")

# Asumiendo que BoxCox2 es tu vector de datos transformados
# Si no lo tienes, reemplaza BoxCox2 con tus datos reales
BoxCox <- ts(Diff_BoxCox, start = c(2010, 4), frequency = 365)

# Calcular el punto de división para el 70%
n <- length(BoxCox)
punto_division <- floor(n * 0.7)

# Crear los conjuntos de entrenamiento y prueba
BoxCox_train <- window(BoxCox, end = time(BoxCox)[punto_division])
BoxCox_test <- window(BoxCox, start = time(BoxCox)[punto_division + 1])

# Ajustar las fechas de inicio para train y test
fecha_inicio_test <- fechas[punto_division + 1]

# Crear las series de tiempo finales
BoxCox_train <- ts(BoxCox_train, start = c(year(fecha_inicio), yday(fecha_inicio)), frequency = 365)
BoxCox_test <- ts(BoxCox_test, start = c(year(fecha_inicio_test), yday(fecha_inicio_test)), frequency = 365)


FE_Apertura = data.frame(matrix(ncol = 6, nrow = nrow(P)))

colnames(FE_Apertura) = c('alpha', 'beta', 'gamma', 'seasonal','MSE', 'MSE test')

cross_validate_hw <- function(ts_data, params, h = 365, k_folds = 10) {
  n <- length(ts_data)
  fold_size <- floor(n / k_folds)
  
  errors <- numeric(k_folds)
  
  for (k in 1:k_folds) {
    end_train <- n - (k_folds - k) * fold_size
    ts_train <- window(ts_data, end = time(ts_data)[end_train])
    ts_test <- window(ts_data, start = time(ts_data)[end_train + 1])
    
    # Agregar mensajes de depuración para verificar los valores
    print(paste("Fold:", k, "Train end:", time(ts_data)[end_train], "Test start:", time(ts_data)[end_train + 1]))
    
    # Verificar que las ventanas no sean NULL o NA
    if (is.null(ts_train) || is.null(ts_test)) {
      stop("Error: Train or test set is NULL")
    }
    
    # Ajustar el modelo Holt-Winters
    model <- HoltWinters(ts_train,
                         alpha = params$alpha,
                         beta = params$beta,
                         gamma = params$gamma,
                         seasonal = params$seasonal)
    
    # Predicciones
    predictions <- predict(model, h = h)
    
    # Calcular el MSE para este fold
    errors[k] <- mean((ts_test - predictions)^2)
  }
  
  return(mean(errors))
}

# Ajustar un modelo Holt-Winters optimizando los parámetros alpha, beta, gamma
optimize_hw <- function(ts_data, h = 365) {
  model <- HoltWinters(ts_data, 
                       alpha = NULL, 
                       beta = NULL, 
                       gamma = NULL,
                       seasonal = "additive")
  
  # Predecir para el conjunto de prueba
  predictions <- predict(model, h = h)
  
  # Calcular el MSE
  return(list(model = model, mse = mean((BoxCox_test - predictions)^2)))
}

# Ejecutar la optimización
best_model <- optimize_hw(BoxCox_train)

# Visualizar el resultado
plot(fitted(best_model$model))

# Definir márgenes para la gráfica
par(mar = c(5, 4, 4, 5))

# Graficar la serie temporal original
plot.ts(BoxCox, col = 'gray21',
        main = 'Precio de la acción del grupo Argos en la apertura',
        ylab = 'Precio',
        xlab = 'Tiempo')

# Graficar los valores ajustados del mejor modelo (fitted)
lines(best_model$model$fitted[, 'xhat'], col = 'red', lwd = 1)  # Valores ajustados

# Graficar las predicciones del conjunto de prueba
predicted_values <- predict(best_model$model, length(BoxCox_test))
predicted_values <- ts(predicted_values, start = start(BoxCox_test), frequency = frequency(BoxCox_test))

lines(predicted_values, col = 'orange', lwd = 1)  # Predicciones

# --------------


ModExp2 = stats::HoltWinters(BoxCox_train,
                             beta = FALSE,
                             seasonal = 'multiplicative')
plot(fitted(ModExp2))

# Graficar resultados
par(mar = c(5, 4, 4, 5))
plot.ts(BoxCox, col = 'gray21',
        main = 'Precio de la acción del grupo Argos en la apertura',
        ylab = 'Precio',
        xlab = 'Tiempo')

# Graficar valores ajustados
# ModExp$fitted['xhat'] ya es una serie de tiempo, podemos utilizarla directamente
lines(ModExp2$fitted[, 'xhat'], col = 'red')

# Graficar predicciones
# Asegurarse de que los valores predichos sean una serie temporal con el mismo inicio y frecuencia
predicted_values <- predict(ModExp2, length(BoxCox_test))
predicted_values <- ts(predicted_values, start = start(BoxCox_test), frequency = frequency(BoxCox_test))
lines(predicted_values, col = 'orange')

# Leyenda
legend('topright', legend = c('Datos originales', 'Valores ajustados', 'Test'), 
       col = c('gray21', 'red', 'orange'), lty = 1, bty = 'n', lwd = c(1, 2, 2))
