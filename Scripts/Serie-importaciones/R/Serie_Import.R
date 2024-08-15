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

par(mfrow = c(2,2))
plot(vafodo ,type = "l", main = 'Serie de tiempo variable VAFODO')
plot(flete , type = "l", main = 'Serie de tiempo variable FLETE')
plot(pbk , type = "l", main = 'Serie de tiempo variable PBK')
plot(pnk , type = "l", main = 'Serie de tiempo variable PNK')
par(mfrow = c(1,1))


# Análisis de la serie VAFODO ----

VAFODO <- ts(importaciones[,3]/10000000000, start = c(2012, 01), frequency =12)
class(VAFODO)
str(VAFODO)
head(VAFODO)


plot(VAFODO, main="PRECIO DEL CARGAMENTO AL MOMENTO \n DE SER TRANSPORTADO VIA MARITIMA",
     ylab="MM DOLARES", xlab="FECHA")


# ESTABILIZACION DE LA VARIANZA ----

MASS::boxcox(lm(VAFODO ~ 1),seq(-1, 1, length = 1000))
BC <- MASS::boxcox(lm(VAFODO ~ 1),seq(-1, 1, length = 1000))
max(BC$x[BC$y == max(BC$y)])
BoxCox <- BoxCox(VAFODO, lambda = -0.1891892)

plot(BoxCox, main="TRANSFORMACION BOXCOX PRECIO DIARIO DE\nLA ACCION DEL GRUPO ARGOS EN LA APERTURA",
     ylab="Trans BoxCox2", xlab="Año")

forecast::BoxCox.lambda(BoxCox, method = "loglik")


# ESTIMACION Y ELIMINACION DE LA TENDENCIA ---- 

fit_Aper <- lm(BoxCox~time(BoxCox), na.action=NULL)
summary(fit_Aper)

plot.ts(BoxCox, main = "Tendencia del modelo",
        ylab = 'Trans BoxCox', xlab = 'Años')
abline(fit_Aper, col = "red")

Aper.sin.tend <- BoxCox - predict(fit_Aper)
plot(Aper.sin.tend, main='SERIE SIN TENDENCIA',xlab='Año',ylab='Trans BoxCox')

acf(Aper.sin.tend, lag.max = 100)

### LOESS PARA BOXCOX ----

df_Aper <- data.frame(Fecha = as.Date(importaciones$fecha_completa), BoxCox = as.matrix(BoxCox))
str(df_Aper)

tibble_Aper <- as_tibble(df_Aper)

tibble_Aper%>%
  timetk::plot_time_series(Fecha, BoxCox,.interactive = TRUE,.plotly_slider = TRUE)

tibble_Aper%>%
  mutate( Mod_BoxCox = smooth_vec(BoxCox,span = 0.75, degree = 2) )

Plot1 <- tibble_Aper %>%
  mutate(Mod_BoxCox = smooth_vec(BoxCox, span = 0.25, degree = 2))%>%
  ggplot(aes(Fecha, BoxCox)) +
  geom_line() +
  geom_line(aes(y = Mod_BoxCox), color = "darkblue") +
  labs(title = "Estimacion de LOESS de la tendencia", x = "AÑOS", y = "Trans BoxCox2")

tibble_Aper <- tibble_Aper %>%
  mutate(Mod1_BoxCox = smooth_vec(BoxCox, span = 0.25, degree = 2))

Aper.sin.LOESS <- BoxCox - as.numeric(tibble_Aper$Mod1_BoxCox)

plot.ts(Aper.sin.LOESS, main='Serie sin tendencia LOESS',xlab='Año',
        ylab='Trans BoxCox2')

acf(Aper.sin.LOESS, lag.max = 100)

# DESCOMPOSICION STL ----

tsibble_Aper <- as_tsibble(df_Aper)

tsibble_Aper %>%
  model(
    STL(BoxCox ~ trend() +
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

