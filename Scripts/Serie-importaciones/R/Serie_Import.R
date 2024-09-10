# Trabajo Series de Tiempo
# Serie: Importaciones Colombianas via maritimas
# Características: 
# - Mediciones Mensuales (Meses)
# - Precio de la carga al momento de ser transportada via maritima

# Autor: Cesar Prieto



# Librerías y directorio ----
library(easypackages)

libraries(c("zoo", "TSA", "MASS", "readr", "dplyr", "fable", "astsa", "readxl", "feasts", 
            "timetk", "plotly", "tibble", "tsibble", "forecast", "tidyverse", "lubridate", 
            "modeldata", "fabletools", "tseriesChaos", "nonlinearTseries", "rsample", "modeltime", "parsnip"))


# Importanción y reconocimiento de la base ----
importaciones <- read.csv("Datos/Importaciones.csv")
names(importaciones)

h <- importaciones[,3]
h <- h/1000000000

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

par(mfrow = c(2,2))
plot(vafodo ,type = "l", main = 'Serie de tiempo variable VAFODO')
plot(flete , type = "l", main = 'Serie de tiempo variable FLETE')
plot(pbk , type = "l", main = 'Serie de tiempo variable PBK')
plot(pnk , type = "l", main = 'Serie de tiempo variable PNK')
par(mfrow = c(1,1))


# Análisis de la serie VAFODO ----

VAFODO <- ts(data = importaciones[,3]/1000000000, start = c(2012, 01), frequency = 12 )

class(VAFODO)
str(VAFODO)
head(VAFODO)

plot.ts(VAFODO, main="PRECIO DEL CARGAMENTO AL MOMENTO \n DE SER TRANSPORTADO VIA MARITIMA",
        ylab="MM DOLARES", xlab="FECHA")
plot(VAFODO, main="PRECIO DEL CARGAMENTO AL MOMENTO \n DE SER TRANSPORTADO VIA MARITIMA",
     ylab="MM DOLARES", xlab="FECHA")


# ESTABILIZACION DE LA VARIANZA ----

MASS::boxcox(lm(VAFODO ~ 1),seq(-1, 1, length = 1000))
BC <- MASS::boxcox(lm(VAFODO ~ 1),seq(-1, 1, length = 1000))
max(BC$x[BC$y == max(BC$y)])
BoxCox <- BoxCox(VAFODO, lambda = 0.4074074)

plot(BoxCox, main="TRANSFORMACION BOXCOX PRECIO DIARIO DE\nLA ACCION DEL GRUPO ARGOS EN LA APERTURA",
     ylab="Trans BoxCox2", xlab="Año")

forecast::BoxCox.lambda(BoxCox, method = "loglik")


# ESTIMACION Y ELIMINACION DE LA TENDENCIA ---- 

fit_vfd <- lm(BoxCox~time(BoxCox), na.action=NULL)
summary(fit_vfd)

plot.ts(BoxCox, main = "Tendencia del modelo",
        ylab = 'Trans BoxCox', xlab = 'Años')
abline(fit_vfd, col = "red")

Vfd.sin.tend <- BoxCox - predict(fit_vfd)
plot(Vfd.sin.tend, main='SERIE SIN TENDENCIA',xlab='Año',ylab='Trans BoxCox')

acf(Vfd.sin.tend, lag.max = 100)

### LOESS PARA BOXCOX ----

df_vdf <- data.frame(Fecha = as.Date(importaciones$fecha_completa), BoxCox = as.matrix(BoxCox))
str(df_vdf)

tibble_Vdf <- as_tibble(df_vdf)

tibble_Vdf%>%
  timetk::plot_time_series(Fecha, BoxCox,.interactive = TRUE,.plotly_slider = TRUE)

tibble_Vdf%>%
  mutate( Mod_BoxCox = smooth_vec(BoxCox,span = 0.75, degree = 2) )

Plot1 <- tibble_Vdf %>%
  mutate(Mod_BoxCox = smooth_vec(BoxCox, span = 0.25, degree = 2))%>%
  ggplot(aes(Fecha, BoxCox)) +
  geom_line() +
  geom_line(aes(y = Mod_BoxCox), color = "darkblue") +
  labs(title = "Estimacion de LOESS de la tendencia", x = "AÑOS", y = "Trans BoxCox2")

tibble_Vdf <- tibble_Vdf %>%
  mutate(Mod1_BoxCox = smooth_vec(BoxCox, span = 0.25, degree = 2))

Vdf.sin.LOESS <- BoxCox - as.numeric(tibble_Vdf$Mod1_BoxCox)

plot.ts(Vdf.sin.LOESS, main='Serie sin tendencia LOESS',xlab='Año',
        ylab='Trans BoxCox2')

acf(Vdf.sin.LOESS, lag.max = 100)

# DESCOMPOSICION STL ----

tsibble_Vdf <- as_tsibble(df_vdf)

tsibble_Vdf <- tsibble_Vdf %>%
  mutate(Fecha = yearmonth(Fecha)) %>%
  as_tsibble(index = Fecha)


# Verifica que ahora es mensual
print(tsibble_Vdf)

tsibble_Vdf %>% 
  is_regular()

tsibble_Vdf %>% 
  has_gaps()

tsibble_Vdf %>% 
  count_gaps()

tsibble_Vdf %>%
  model(
    STL(BoxCox ~ trend() +
          season(period= 12, window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


Mod_STL <- tsibble_Vdf %>%
  model(STL(BoxCox ~ trend() + season(period= 12, window = "periodic"), robust = TRUE))

# ESTIMACION DE LA TENDENCIA POR STL ----

Comp <- components(Mod_STL)
Sin.Tend.STL <- BoxCox - Comp$trend

plot(Sin.Tend.STL, main='Serie sin tendencia STL', xlab="Año", ylab="Trans BoxCox")

acf(Sin.Tend.STL, lag.max = 100)

# ESTIMACION DE LA TENDENCIA USANDO SPLINES ----

Spl_BoxCox <- smooth.spline(x = time(BoxCox), y = BoxCox, spar = 0.90)

Sin.Tend.Spl <- BoxCox - Spl_BoxCox$y

plot(BoxCox, main='ESTIMACION POR SPLINES DE LA TENDENCIA', ylab="BoxCox2", xlab="AÑO")
lines(x = Spl_BoxCox$x, y=Spl_BoxCox$y, col = 'red')

plot(Sin.Tend.Spl, main='Serie sin tendencia SPLINES',xlab='Año',ylab="BoxCox")

acf(Sin.Tend.Spl, lag.max = 100)

# ESTIMACION DE LA TENDENCIA USANDO REGRESION KERNEL ----

Ker_BoxCox <- ksmooth(x = time(BoxCox), y = BoxCox, kernel = "normal", bandwidth = 0.3)

plot(BoxCox)
lines(x = Ker_BoxCox$x, Ker_BoxCox$y, col = "red")

Sin.Tend.Ker <- BoxCox - Ker_BoxCox$y
plot(Sin.Tend.Ker, main="Serie sin tendencia KERNEL", xlab="AÑO", ylab="BoxCoX")

acf(Sin.Tend.Ker, lag.max = 100)

# Dickey-Fuller PARA DETERMINAR SI LA SERIE ES ESTACIONARIA ----

ar(BoxCox) #El coeficiente para el primer rezago indica una fuerte correlacion
tseries::adf.test(BoxCox, alternative = "stationary", k = 12) # La prueba indica que la serie no es estacionaria 

Diff_BoxCox <- diff(BoxCox,lag = 1) #Serie Diferenciada
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
Periodo.Diff_BoxCox <- spectrum(BoxCox, main = "Periodograma serie diferenciada",
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
                                 xlim = c(0,6), log = "no")
abline(v = Periodo.Sin.Tend.Ker$freq[match(max(Periodo.Sin.Tend.Ker$spec),
                                           Periodo.Sin.Tend.Ker$spec)], col='red')

periodograma <- Periodo.Sin.Tend.Ker
max(Periodo.Sin.Tend.Ker$spec)
periodograma$freq[match(max(periodograma$spec),periodograma$spec)]
periodo=1/periodograma$freq[match(max(periodograma$spec),periodograma$spec)]
periodo

# DIAGNOSTICO DE LA SERIE POR MESES DEL AÑO ----

Tb_BoxCox<-as_tsibble(Diff_BoxCox,index=tibble(fecha))
colnames(Tb_BoxCox)<-c("Fecha","Apertura")

Tb_BoxCox <- Tb_BoxCox %>%
  mutate(mes = factor(month.abb[month(Fecha)], levels = month.abb))

Tb_BoxCox %>%
  mutate(diff_ND = Apertura - lag(Apertura)) %>%
  ggplot(aes(x = mes, y = diff_ND)) +
  geom_boxplot() +
  labs(title = "Distribución de diferencias mensuales", x = "Mes", y = "Diferencia respecto al valor anterior")

# MODELAMIENTO DE LA ESTACIONALIDAD DE LA SERIE ----
importaciones$fecha_completa <- as.Date(importaciones$fecha_completa)

# Convertir a tsibble asegurando que es una serie mensual
TsbVdf <- as_tsibble(importaciones[,c(13,3)], index = fecha_completa) %>%
  mutate(Month = yearmonth(fecha_completa)) # Definir la frecuencia mensual explícitamente

# Actualizar el índice de la serie a "Month"
TsbVdf <- TsbVdf %>% 
  as_tsibble(index = Month)

# Llenar huecos en la serie temporal (si hay)
TsbVdf <- TsbVdf %>% fill_gaps()

# Crear armónicos y Fourier para la serie VAFODO
forecast::seasonaldummy(VAFODO) # Dummies estacionales
Armonicos = TSA::harmonic(VAFODO, m = 1) # Armónicos

# Fourier con K=1
harmonics = forecast::fourier(VAFODO, K = 6)

# Graficar los armónicos
par(mar = c(1,4,1,1), mfrow = c(6,2))
for(i in 1:ncol(harmonics)) {
  plot(harmonics[,i], type = 'l', xlab = "Time", ylab = colnames(harmonics)[i])
}
par(mar = rep(4, 4), mfrow=c(1,1))

#Continuar con la diferenciación logarítmica
diff_TsbVdf <- TsbVdf |> 
  mutate(logdiff_Vdf = difference(log(VAFODO))) |> 
  select(Month, logdiff_Vdf)

# Ajustar el modelo ARIMA con Fourier
Modelo_serie_diff <- diff_TsbVdf |> model(
  Fourier1Airdiff = ARIMA(logdiff_Vdf ~ fourier(K = 2) + pdq(0, 0, 0) + PDQ(0, 0, 0))
)

# Obtener valores ajustados
real_ajustado1 <- diff_TsbVdf %>%
  left_join(fitted(Modelo_serie_diff, by = index)) %>%
  select(-.model)

# Graficar los valores reales y ajustados
real_ajustado1 %>%
  autoplot() +
  geom_line(aes(y = logdiff_Vdf, colour = "real")) +
  geom_line(aes(y = .fitted, colour = "ajustado")) +
  scale_color_manual(name = "real/ajustado", values = c("real" = "black", "ajustado" = "red"))

#####Ajuste Dummy

Modelo_serie_diff_Dummy <- diff_TsbVdf|>model(
  `DummyAirdiff`=ARIMA(logdiff_Vdf~season()+pdq(0, 0, 0) + PDQ(0, 0, 0))
)

Modelo_serie_diff_Dummy <- diff_TsbVdf %>% left_join(fitted(Modelo_serie_diff,by=index)) %>%
  select(-.model) 

Modelo_serie_diff_Dummy %>%
  autoplot() +
  geom_line(data=Modelo_serie_diff_Dummy,aes(y=logdiff_Vdf,colour="real"))+
  geom_line(data=Modelo_serie_diff_Dummy,aes(y=.fitted,colour="ajustado"))+
  scale_color_manual(name = "real/ajustado", values = c("real" = "black", "ajustado" = "red"))

#### Varios modelos la mismo tiempo

ajuste_final_models<-diff_TsbVdf%>%model(
  `Fourier1Airdiff`=ARIMA(logdiff_Vdf~fourier(K=1)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  `Fourier2Airdiff`=ARIMA(logdiff_Vdf~fourier(K=2)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  `Fourier3Airdiff`=ARIMA(logdiff_Vdf~fourier(K=3)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  `DummyAirdiff`=ARIMA(logdiff_Vdf~season()+pdq(0, 0, 0) + PDQ(0, 0, 0))
)

glance(ajuste_final_models)

ajuste_final_models %>%
  select(Fourier1Airdiff)%>%coef()

Modelo_serie_diff_models <- diff_TsbVdf %>%
  left_join(fitted(ajuste_final_models)|>group_by(.model)%>%
              pivot_wider(names_from = .model, values_from = .fitted))

ajuste_final_models <- diff_TsbVdf %>% model(
  `Fourier1Airdiff` = ARIMA(logdiff_Vdf ~ fourier(K = 1) + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  `Fourier2Airdiff` = ARIMA(logdiff_Vdf ~ fourier(K = 2) + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  `Fourier3Airdiff` = ARIMA(logdiff_Vdf ~ fourier(K = 3) + pdq(1, 0, 0) + PDQ(0, 0, 0)),
  `DummyAirdiff` = ARIMA(logdiff_Vdf ~ season() + pdq(1, 0, 0) + PDQ(0, 0, 0))
)

# Unir los valores ajustados
Modelo_serie_diff_models <- diff_TsbVdf %>%
  left_join(
    fitted(ajuste_final_models) %>% 
      group_by(.model) %>%
      pivot_wider(names_from = .model, values_from = .fitted),
    by = "Month"
  )


# Convertir a un formato largo para facilitar el uso de facet_wrap
Modelo_serie_diff_long <- Modelo_serie_diff_models %>%
  pivot_longer(cols = starts_with("Fourier") | starts_with("Dummy"),
               names_to = "Modelo",
               values_to = "Ajuste")

# Crear gráficas separadas usando facet_wrap
Modelo_serie_diff_long %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = logdiff_Vdf, colour = "Real"), color = "black") +
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

# Definir las fechas de inicio y fin correctamente usando as.Date
fecha_inicio <- as.Date("2012-01-01")
fecha_fin <- as.Date("2023-12-01")

# Crear un vector de fechas mensuales desde fecha_inicio hasta fecha_fin
fechas <- seq(from = fecha_inicio, to = fecha_fin, by = 'month')

# Asumiendo que BoxCox2 es tu vector de datos transformados
# Si no lo tienes, reemplaza BoxCox2 con tus datos reales
BoxCox <- ts(Diff_BoxCox, start = c(2012, 1), frequency = 12)

# Calcular el punto de división para el 70%
n <- length(BoxCox)
punto_division <- floor(n * 0.7)

# Crear los conjuntos de entrenamiento y prueba
BoxCox_train <- window(BoxCox, end = time(BoxCox)[punto_division])
BoxCox_test <- window(BoxCox, start = time(BoxCox)[punto_division + 1])

# Ajustar las fechas de inicio para train y test
fecha_inicio_test <- fechas[punto_division + 1]

# Crear las series de tiempo finales
BoxCox_train <- ts(BoxCox_train, start = c(year(fecha_inicio)), frequency = 12)
BoxCox_test <- ts(BoxCox_test, start = c(year(fecha_inicio_test)), frequency = 12)


FE_Apertura = data.frame(matrix(ncol = 6, nrow = nrow(P)))

colnames(FE_Apertura) = c('alpha', 'beta', 'gamma', 'seasonal','MSE', 'MSE test')

cross_validate_hw <- function(ts_data, params, h = 43, k_folds = 10) {
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
optimize_hw <- function(ts_data, h = 43) {
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
                             seasonal = 'additive')
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

