# Trabajo Series de Tiempo
# Serie: Precio de acciones del grupo Argos
# Características: 
# - Mediciones diarias (días laborales lun-vie)
# - Precio de la acción medido en pesos 

# Grupo 2
# Script elaborado por Cesar Prieto, Gabriel Peña, Sebastian Gil


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

Ker_BoxCox <- ksmooth(x = time(BoxCox2), y = BoxCox2, kernel = "normal", bandwidth = 0.1)

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

# ESTIMACION DE LA COMPONENTE ESTACIONAL PARA LA SERIE ORIGINAL ----

TsbApertura <- as_tsibble(G_ARGOS[,c(1,3)], index = Fecha)

Mod.Est.Apertura <-  TsbApertura %>% model(
  'Fourier (1 Componentes)' = ARIMA(Apertura ~ fourier(K = 1) + pdq(0, 0, 0) + PDQ(0, 0, 2)),
  'Fourier (2 Componentes)' = ARIMA(Apertura ~ fourier(K = 2.5) + pdq(0, 0, 0) + PDQ(0, 0, 3)),
  'Dummy' = ARIMA(Apertura ~ season() + pdq(0, 0, 0) + PDQ(0, 0, 1))
)

Mod.Ajd.Est.Apertura <- TsbApertura%>%
  left_join(fitted(Mod.Est.Apertura)|>group_by(.model)%>%
              pivot_wider(names_from = .model, values_from = .fitted))

# Obtener sigma^2, AIC y BIC de los modelos en Mod.Est.Apertura
model_results <- Mod.Est.Apertura %>%
  glance() %>%
  select(.model, sigma2, AIC, BIC)

# Agregar información adicional sobre los modelos
model_results <- model_results %>%
  mutate(Modelo = case_when(
    .model == 'Fourier (1 Componentes)' ~ 'Fourier (1 Componentes)',
    .model == 'Fourier (2 Componentes)' ~ 'Fourier (2 Componentes)',
    .model == 'Dummy' ~ 'Dummy'
  )) %>%
  select(Modelo, sigma2, AIC, BIC)

# Mostrar la tabla con los resultados
print(model_results)

Mod.Ajd.Est.Apertura = as.data.frame(Mod.Ajd.Est.Apertura)

# Establecer el diseño de las gráficas
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))  # 3 filas, 1 columna, márgenes ajustados

# Graficar cada serie de datos ajustada por separado
for (i in 3:5) {  # Columnas 3 a 5 corresponden a 'Fourier (2 Componentes)', 'Fourier (3 Componentes)', 'Dummy'
  # Nombre de la columna actual
  col_name <- colnames(Mod.Ajd.Est.Apertura)[i]
  
  # Graficar los datos originales vs. valores ajustados para la columna actual
  plot(
    x = Mod.Ajd.Est.Apertura$Fecha,
    y = Mod.Ajd.Est.Apertura[, i],
    type = 'l',
    col = 'red',  # Color para la serie ajustada
    lwd = 1.2,  # Grosor de la línea
    ylim = c(min(Mod.Ajd.Est.Apertura[, c(3:5)], na.rm = TRUE), max(Mod.Ajd.Est.Apertura[, c(3:5)], na.rm = TRUE)),  # Establecer límites del eje y
    main = paste("Datos Originales vs. Valores Ajustados:", col_name),
    xlab = "Fecha",
    ylab = "Valor"
  )
  
  # Agregar la línea de los datos originales ('value') en negro
  lines(
    x = Mod.Ajd.Est.Apertura$Fecha,
    y = Mod.Ajd.Est.Apertura$Apertura,
    type = 'l',
    col = 'black',  # Color negro para los datos originales
    lwd = 0.7  # Grosor de la línea
  )
}

par(mfrow = c(1,1))

# PRONOSTICO BASADO EN DESCOMPOSICION ----

fit <- stl(Sin.Tend.Ker, s.window = 7, s.degree = 1, t.degree = 1, robust = TRUE)

# Obtener la componente ajustada estacionalmente
seasadj_fit <- seasadj(fit)

# Realizar la predicción naive
naive_forecast <- naive(seasadj_fit, h = 30) # Aumenta el horizonte de predicción

# Visualizar la predicción con autoplot y ggplot2
autoplot(naive_forecast) +
  ylab("Nuevo índices ordenados.") +
  ggtitle("Pronóstico Naive de la componente ajustada estacionalmente") +
  theme_minimal() + # Usar un tema minimalista
  theme(
    plot.title = element_text(hjust = 0.5), # Centrar el título
    legend.position = "bottom", # Colocar la leyenda en la parte inferior
    legend.title = element_blank() # Quitar el título de la leyenda
  ) +
  scale_color_manual(values = c("blue", "red")) + # Personalizar colores
  scale_fill_manual(values = c("lightblue", "pink")) # Personalizar colores de bandas


fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

# SUAVIZAMIENTO EXPONENCIAL ----

STK <- as.matrix(Sin.Tend.Ker)
STK <- as.matrix(cbind(as.Date(G_ARGOS[,1]), STK))
STK <- as.data.frame(STK)
names <-  c("Fecha", "Apertura")
colnames(STK) <- names
STK$Fecha <- as.Date(STK$Fecha) 
ST_Kernel <- as_tsibble(STK[,c(1,2)], index = Fecha)

HWAP <- HoltWinters(Sin.Tend.Ker, seasonal = "additive")

plot(HWAP)

ajustados <- fitted(HWAP)
plot(ajustados)

summary(HWAP)

predictionHWAP=forecast::forecast(HWAP, h=30, level =0.95, lambda = 0)
predictionHWAP
plot(predictionHWAP, xlim = c(2018,2020.0822))

ajustepass <- ST_Kernel %>%
  model(ETS(Apertura~ error("A")+trend("A")+season("A")))

pronostico=ajustepass%>%
  fabletools::forecast(h=60)
pronostico

pronostico %>% autoplot(ST_Kernel) +
  geom_line(aes(y=.fitted),col="#D55E00",data=augment(ajustepass)) + 
  labs(y=" ",title="Pronósticos ajustados") + 
  guides(colour="none")

modelos <- ST_Kernel %>%
  model(ets=ETS(Apertura ~ error("A")+trend("A")+season("A")),
        stl=decomposition_model(STL(Apertura ~ trend(window = 13) +
                                      season(window = "periodic"),
                                    robust = TRUE),NAIVE(season_adjust)))
modelos 

# Realizar la predicción
predicciones <- modelos %>% fabletools::forecast(h = 30)

# Visualizar el pronóstico desde 2018
autoplot(predicciones, ST_Kernel) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c("2018-01-01", NA))) +
  ylab("Nuevo índices ordenados.") +
  ggtitle("Pronóstico desde 2018") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# MODELTIME PARA ETS 

Aper_tbl <- as_tibble(ST_Kernel)
Aper_tbl$index = as.Date(ST_Kernel$Fecha)
Aper_tbl <- Aper_tbl %>% mutate(Apertura2 = exp(Apertura))

Aper_tbl <- Aper_tbl[,c(1,2,4)]

Aper_tbl


splits_Aper_tbl = timetk::time_series_split(Aper_tbl, date_var = Fecha, 
                                            assess = 1000 ,cumulative = TRUE)

splits_Aper_tbl %>% tk_time_series_cv_plan()%>%
  plot_time_series_cv_plan(Fecha, Apertura2)

ets_Apertura <- modeltime::exp_smoothing(
  error="additive",
  trend="additive",
  season="additive"
) %>%
  set_engine("ets")%>%
  fit(Apertura2 ~ Fecha, data = training(splits_Aper_tbl))

# Modeltime ----

modeltime_table(ets_Apertura) %>%
  modeltime_calibrate(testing(splits_Aper_tbl))%>%
  modeltime_forecast(
    new_data = testing(splits_Aper_tbl),
    actual_data = Aper_tbl
  )%>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")

pronostico_ets<-modeltime_table(ets_Apertura) %>%
  modeltime_calibrate(testing(splits_Aper_tbl))%>%
  modeltime_forecast(
    new_data = testing(splits_Aper_tbl),
    actual_data = Aper_tbl
  )

pronostico_ets 

## Modeltime

model_tbl<-modeltime_table(ets_Apertura)

## Calibración 

calibration_tbl<-model_tbl%>%
  modeltime_calibrate(testing(splits_Aper_tbl))

## Prueba de pronóstico

calibration_tbl%>%
  modeltime_forecast(
    new_data = testing(splits_Aper_tbl),
    actual_data = Aper_tbl
  ) 

## Residuales 

residuales_ajuste <- model_tbl %>%
  modeltime_calibrate(new_data=training(splits_Aper_tbl))%>%
  modeltime_residuals()

residuales_ajuste %>% plot_modeltime_residuals(
  .type="timeplot",
  .interactive = TRUE)

### Precisión de los pronósticos

calibration_tbl%>%
  modeltime_accuracy()

## Re-ajuste y predicción de valores futuros

re_ajuste <- calibration_tbl %>%
  modeltime_refit(data = Aper_tbl)

re_ajuste%>%
  modeltime_forecast(h = 365,
                     actual_data = Aper_tbl
  )%>%
  plot_modeltime_forecast()

## Especificando algunos valores del modelo 

ets_Apertura_fixed<-modeltime::exp_smoothing(
  error="additive",
  trend="additive",
  season="additive",
  smooth_level=0.5,
  smooth_trend=0.02,
  smooth_seasonal=0.01
)%>%
  set_engine("ets")%>%
  fit(BoxCox2 ~ Fecha, data = training(splits_Aper_tbl))

# modeltime

modeltime_table(ets_Apertura_fixed) %>%
  modeltime_calibrate(testing(splits_Aper_tbl))%>%
  modeltime_forecast(
    new_data = testing(splits_Aper_tbl),
    actual_data = Aper_tbl
  )%>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue")

pronostico_ets 


## Modeltime

model_tbl_fixed <- modeltime_table(ets_Apertura_fixed)

## Calibración 

calibration_tbl_fixed <- model_tbl_fixed %>%
  modeltime_calibrate(testing(splits_Aper_tbl))

## Prueba de pronóstico

calibration_tbl_fixed %>%
  modeltime_forecast(
    new_data = testing(splits_Aper_tbl),
    actual_data = Aper_tbl
  ) 

### Precisión de los pronósticos

calibration_tbl_fixed%>%
  modeltime_accuracy()

# EVALUACION DE PRONOSTICOS ----

# Definir h
h <- 30

# Definir las longitudes de la serie y la porción de entrenamiento
lserie <- length(BoxCox2)
ntrain <- trunc(lserie * 0.85)

# Crear conjuntos de entrenamiento y prueba
train <- window(BoxCox2, end = time(BoxCox2)[ntrain])
test <- window(BoxCox2, start = time(BoxCox2)[ntrain] + 1/365)

# Inicializar variables
ntest <- length(test)
fchstepahe <- matrix(0, nrow = ntest, ncol = h)

verval <- matrix(NA, nrow = ntest, ncol = h)
for (j in 1:h) {
  verval[1:(ntest - j + 1), j] <- test[j:ntest]
}
colnames(verval) <- paste0("h", 1:h)


# Ajuste del modelo Holt-Winters
HWAP_train <- stats::HoltWinters(train, seasonal = "additive")

# Extracción de parámetros del modelo
alpha <- HWAP_train$alpha
beta <- HWAP_train$beta
gamma <- HWAP_train$gamma

# Refinamiento y predicción
for (i in 1:ntest) {
  x <- window(BoxCox2, end = time(BoxCox2)[ntrain] + (i - 1) / 365)
  refit <- stats::HoltWinters(x, seasonal = "additive", alpha = alpha, 
                              beta = beta, gamma = gamma)
  fchstepahe[i, ] <- as.numeric(forecast::forecast(refit, h = h)$mean)
}

# Cálculo de errores

errores_pred <- fchstepahe - verval
ECM <- apply(errores_pred^2, MARGIN = 2, mean, na.rm = TRUE)
RECM <- sqrt(ECM)
RECM


# ROLLING ----

library(greybox)


# Definir y ajustar el modelo Holt-Winters
HWAP_train <- stats::HoltWinters(train, seasonal = "additive")

# Definir 'ourCallETS' y 'ourValueETS'
ourCallETS <- "forecast::forecast(stats::HoltWinters(train, alpha=HWAP_train$alpha, beta=HWAP_train$beta, gamma=HWAP_train$gamma), h=h, level=95)"
ourValueETS <- c("mean", "lower", "upper")

# Llamada a 'ro' para generar predicciones
origins <- 30
Valoresretornados1 <- ro(BoxCox2, h = h, origins = origins, call = ourCallETS, value = ourValueETS, ci = FALSE, co = FALSE)

# Verificar dimensiones
holdout_dim <- dim(Valoresretornados1$holdout)
mean_dim <- dim(Valoresretornados1$mean)
print(paste("Dimensiones de holdout: ", toString(holdout_dim)))
print(paste("Dimensiones de mean: ", toString(mean_dim)))

# Calcular el error si las dimensiones coinciden
if (all(holdout_dim == mean_dim)) {
  RECM <- apply(sqrt((Valoresretornados1$holdout - Valoresretornados1$mean)^2), 1, mean, na.rm=TRUE)
  RECM
} else {
  stop("Dimensiones de 'holdout' y 'mean' no coinciden.")
}


apply(abs(Valoresretornados1$holdout - -Valoresretornados1$mean),
      1, mean, na.rm = TRUE) / mean(Valoresretornados1$actuals) ### Error medio absoluto escalado


