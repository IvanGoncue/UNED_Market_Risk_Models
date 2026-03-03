
##################################################################################
##################################################################################
##################################################################################
#VAR PARAMETRICO CON VOLATILIDAD INCONDICIONAL
##################################################################################
##################################################################################
##################################################################################
# Cargar las librerías necesarias
library(quantmod)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)

# Calcular la media y la desviación estándar incondicional de los rendimientos
mean_return <- mean(sp500_returns)
std_dev_return <- sd(sp500_returns)


# Calcular el VaR paramétrico bajo normalidad
confidence_levels <- c(0.95, 0.99)
VaR_95 <- qnorm(1 - confidence_levels[1]) * std_dev_return + mean_return
VaR_99 <- qnorm(1 - confidence_levels[2]) * std_dev_return + mean_return

# Crear un data frame con las estimaciones diarias de VaR
VaR_data <- data.frame(Date = index(sp500_returns),
                       Returns = as.numeric(sp500_returns),
                       VaR_95 = rep(VaR_95, length(sp500_returns)),
                       VaR_99 = rep(VaR_99, length(sp500_returns)))

# Graficar los rendimientos y las estimaciones de VaR
plot(VaR_data$Date, VaR_data$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y Estimaciones VaR del S&P 500",
     xlab = "Fecha", ylab = "Rendimientos / VaR")

# Añadir las líneas del VaR al gráfico
lines(VaR_data$Date, VaR_data$VaR_95, col = "blue", lwd = 2, lty = 2)
lines(VaR_data$Date, VaR_data$VaR_99, col = "red", lwd = 2, lty = 2)

# Añadir una leyenda
legend("topright", legend = c("Rendimientos", "VaR 95%", "VaR 99%"),
       col = c("black", "blue", "red"), lwd = 2, lty = c(1, 2, 2))



##################################################################################
##################################################################################
##################################################################################
#VAR PARAMETRICO CON VOLATILIDAD CONDICIONAL
##################################################################################
##################################################################################
##################################################################################


# Cargar las librerías necesarias
library(quantmod)
library(fGarch)

# Descargar los datos del índice S&P 500 desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer el precio de cierre ajustado
sp500_close <- Cl(GSPC)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)

# Ajustar un modelo AR(1)-GARCH(1,1) para los rendimientos
ar_garch_model <- garchFit(~ arma(1,0) + garch(1,1), data = sp500_returns, cond.dist = "norm", trace = FALSE)

# Presentar las estimaciones de los parámetros
summary(ar_garch_model)




# Obtener la media condicional y la desviación estándar condicional (volatilidad)
mu_cond <- fitted(ar_garch_model)
sigma_cond <- volatility(ar_garch_model)

# Calcular el VaR paramétrico bajo normalidad
VaR_95 <- qnorm(0.05) * sigma_cond + mu_cond
VaR_99 <- qnorm(0.01) * sigma_cond + mu_cond

# Crear un data frame con las estimaciones diarias de VaR
VaR_data <- data.frame(Date = index(sp500_returns),
                       Returns = as.numeric(sp500_returns),
                       VaR_95 = VaR_95,
                       VaR_99 = VaR_99)

# Graficar los rendimientos y las estimaciones de VaR
plot(VaR_data$Date, VaR_data$Returns, type = "l", col = "black", lwd = 1,
     main = "Rendimientos Diarios y Estimaciones VaR del S&P 500 (AR(1)-GARCH(1,1))",
     xlab = "Fecha", ylab = "Rendimientos / VaR")

# Añadir las líneas del VaR al gráfico
lines(VaR_data$Date, VaR_data$VaR_95, col = "blue", lwd = 2, lty = 2)
lines(VaR_data$Date, VaR_data$VaR_99, col = "red", lwd = 2, lty = 2)

# Añadir una leyenda
legend("topright", legend = c("Rendimientos", "VaR 95%", "VaR 99%"),
       col = c("black", "blue", "red"), lwd = 2, lty = c(1, 2, 2))




##################################################################################
##################################################################################
##################################################################################
#ES APPLE PRACTICA 9 
##################################################################################
##################################################################################
##################################################################################

library(quantmod)
getSymbols("AAPL", from = "2002-01-01", to = "2021-12-31")
returns <- dailyReturn(Ad(AAPL))
alpha <- 0.05
VaR <- quantile(returns, probs = alpha)
ES <- mean(returns[returns <= VaR])
VaR
ES

#######################

# Graficar los rendimientos
plot(index(returns), coredata(returns), type = "l", col = "black",
     main = "Rendimientos de APPLE con VaR y ES",
     xlab = "Fecha", ylab = "Rendimientos")

# Añadir línea para VaR
abline(h = VaR, col = "red", lty = 2)

# Añadir línea para ES
abline(h = ES, col = "blue", lty = 2)

# Añadir leyenda
legend("topright", legend = c("Rendimientos", "VaR", "ES"),
       col = c("black", "red", "blue"), lty = 1:2)

##############################3
##############################3
##############################3
##############################3
##############################3
##############################3

##parametrico bajo normalidad

# Parámetros de la distribución normal
mu <- 0.5
##############################3sigma <- 1.5
alpha <- 0.05  # nivel de significancia

# Calcular el VaR (percentil 5%) usando la distribución normal
VaR <- mu + sigma * qnorm(alpha)

# Calcular el ES (esperanza condicional)
# Primero, se calcula la función de densidad normal estándar en el percentil VaR
normal_density <- dnorm(qnorm(alpha))
# Luego, se usa la fórmula para calcular el ES
ES <- mu - sigma * (normal_density / alpha)

# Imprimir resultados
print(paste("VaR bajo distribución normal:", VaR))
print(paste("ES bajo distribución normal:", ES))


##############################3
##############################3
##############################3
##############################3
##############################3
##############################3

### parametrico t studente

# Parámetros de la distribución t-Student
mu <- 0.5
sigma <- 1.5
df <- 3  # grados de libertad
alpha <- 0.05  # nivel de significancia

# Calcular el VaR (percentil 5%) usando la distribución t-Student
VaR <- mu + sigma * qt(alpha, df)

# Calcular el ES (esperanza condicional)
# Primero, se calcula la función de densidad t-Student en el percentil VaR
t_density <- dt(qt(alpha, df), df)
# Luego, se usa la fórmula para calcular el ES
ES <- mu + sigma * (t_density / alpha) * (df + qt(alpha, df)^2) / (df - 1)

# Imprimir resultados
print(paste("VaR bajo distribución t-Student:", VaR))
print(paste("ES bajo distribución t-Student:", ES))



##############################3
##############################3
##############################3
##############################3
##############################3
##############################3

### parametrico t studente asimetrica

# Instalar y cargar el paquete sn si aún no está instalado
if (!requireNamespace("sn", quietly = TRUE)) install.packages("sn")
library(sn)

# Parámetros de la distribución asimétrica
mu <- 0.5
sigma <- 1.5
df <- 3
xi <- 8  # Parámetro de asimetría

# Generar una muestra de datos siguiendo la distribución t-Student asimétrica
set.seed(123)  # Para reproducibilidad
sample_size <- 1000  # Tamaño de la muestra
sample_data <- rsn(sample_size, xi = xi, mean = mu, sd = sigma, nu = df)

# Definir el nivel de significancia
alpha <- 0.05

# Calcular VaR (percentil 5%)
VaR <- qsn(alpha, xi = xi, mean = mu, sd = sigma, nu = df)

# Calcular ES (esperanza condicional)
ES <- mu + sigma * (dsn(qsn(alpha, xi = xi, mean = mu, sd = sigma, nu = df), xi = xi, mean = mu, sd = sigma, nu = df) / alpha)

# Imprimir resultados
print(paste("VaR bajo distribución t-Student asimétrica:", VaR))
print(paste("ES bajo distribución t-Student asimétrica:", ES))



##############################3
##############################3
##############################3
##############################3
##############################3
##############################3

############### EVT POT


# Instalar y cargar los paquetes necesarios
if (!require(quantmod)) install.packages("quantmod")
if (!require(evd)) install.packages("evd")
library(quantmod)
library(evd)

# Descargar datos del índice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")
hsi_returns <- dailyReturn(Cl(HSI))

# Convertir los rendimientos en un vector numérico
returns <- as.numeric(hsi_returns)

# Definir el umbral como el percentil 94
threshold <- quantile(returns, probs = 0.94)

# Aplicar el método POT para ajustar la distribución GPD
excess_returns <- returns[returns > threshold] - threshold
fit_gpd <- fgev(excess_returns)

# Calcular VaR (percentil 5%) usando la distribución GPD ajustada
alpha <- 0.05
VaR_pot <- threshold + qgev(alpha, loc = fit_gpd$estimate["loc"], scale = fit_gpd$estimate["scale"], shape = fit_gpd$estimate["shape"])

# Calcular ES (esperanza condicional) usando la distribución GPD ajustada
ES_pot <- threshold + (fit_gpd$estimate["scale"] / (1 - fit_gpd$estimate["shape"])) * (1 / alpha) * (1 + fit_gpd$estimate["shape"] * qgev(alpha, loc = fit_gpd$estimate["loc"], scale = fit_gpd$estimate["scale"], shape = fit_gpd$estimate["shape"]))

# Imprimir resultados
print(paste("VaR usando Teoría de Valores Extremos incondicional:", VaR_pot))
print(paste("ES usando Teoría de Valores Extremos incondicional:", ES_pot))





##############################3
##############################3
##############################3
##############################3
##############################3
##############################3

######################### EVT condicional



# Instalar y cargar las librerías necesarias
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("fGarch", quietly = TRUE)) install.packages("fGarch")
if (!requireNamespace("fitdistrplus", quietly = TRUE)) install.packages("fitdistrplus")
if (!requireNamespace("evd", quietly = TRUE)) install.packages("evd")

library(quantmod)
library(fGarch)
library(fitdistrplus)
library(evd)

# Cargar los datos del índice HSI
getSymbols("^HSI", src = "yahoo", from = "2000-01-01", to = "2021-12-31")

# Extraer los precios de cierre
HSI <- Cl(HSI)

# Eliminar datos nulos
HSI <- na.omit(HSI)

# Calcular los rendimientos (diferencia de logaritmos)
returns <- diff(log(HSI)) * 100
returns <- na.omit(returns)

# Multiplicar los rendimientos por -1 para posiciones largas
returns_neg <- -returns

# Ajustar un modelo APARCH(1,1) para obtener la varianza condicional
aparch_fit <- garchFit(
  formula = ~ aparch(1, 1),
  data = returns_neg,
  include.mean = TRUE,
  cond.dist = "std",
  trace = FALSE
)



# Obtener la desviación estándar condicional
sigma_t <- sqrt(aparch_fit@sigma.t^2)

# Estandarizar los rendimientos
standardized_returns <- returns_neg / sigma_t

# Fijar el umbral (percentil 94 de los rendimientos estandarizados)
threshold <- quantile(standardized_returns, probs = 0.94)

# Seleccionar los datos que están por encima del umbral y ajustar la GPD
excesses <- standardized_returns[standardized_returns > threshold] - threshold

# Ajustar la distribución GPD a los excesos
gpd_fit <- fgev(excesses)
shape <- gpd_fit$estimate["shape"]
scale <- gpd_fit$estimate["scale"]
loc <- gpd_fit$estimate["loc"]

# Calcular el VaR condicional al 95%
conf_level <- 0.95
percentile <- 1 - conf_level

# Inicializar vector para VaR condicional
VaR_conditional_95 <- rep(NA, length(returns))

for (i in 1:length(returns)) {
  if (standardized_returns[i] > threshold) {
    # Para excesos, usamos la GPD
    VaR_conditional_95[i] <- -sigma_t[i] * (threshold + qgev(percentile, loc = loc, scale = scale, shape = shape))
  } else {
    # Para datos no excesivos, simplemente usamos la desviación estándar condicional
    VaR_conditional_95[i] <- -sigma_t[i] * qnorm(conf_level)
  }
}

# Crear un gráfico para visualizar los rendimientos y el VaR condicional al 95%
plot(index(returns), returns, type = 'l', col = 'orange', lty = 1, ylab = 'Rendimientos', xlab = 'Fecha', main = 'Rendimientos y VaR Condicional al 95% usando GPD')
lines(index(returns), VaR_conditional_95, col = 'black', lty = 2)
legend("topright", legend = c("Rendimientos", "VaR Condicional al 95%"), col = c("orange", "black"), lty = c(1, 2))

# Calcular el VaR condicional al 99%
conf_level <- 0.99
percentile <- 1 - conf_level

# Inicializar vector para VaR condicional
VaR_conditional_99 <- rep(NA, length(returns))

for (i in 1:length(returns)) {
  if (standardized_returns[i] > threshold) {
    # Para excesos, usamos la GPD
    VaR_conditional_99[i] <- -sigma_t[i] * (threshold + qgev(percentile, loc = loc, scale = scale, shape = shape))
  } else {
    # Para datos no excesivos, simplemente usamos la desviación estándar condicional
    VaR_conditional_99[i] <- -sigma_t[i] * qnorm(conf_level)
  }
}

# Crear un gráfico para visualizar los rendimientos y el VaR condicional al 99%
plot(index(returns), returns, type = 'l', col = 'orange', lty = 1, ylab = 'Rendimientos', xlab = 'Fecha', main = 'Rendimientos y VaR Condicional al 99% usando GPD')
lines(index(returns), VaR_conditional_99, col = 'black', lty = 2)
legend("topright", legend = c("Rendimientos", "VaR Condicional al 99%"), col = c("orange", "black"), lty = c(1, 2))




































##################################################################################
##################################################################################
##################################################################################
#PRÁCTICA 10
##################################################################################
##################################################################################
##################################################################################

# Cargar las librerías necesarias
library(quantmod)
library(fGarch)

# Descargar datos del S&P 500 y Apple desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)
getSymbols("AAPL", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer los precios de cierre ajustado
sp500_close <- Cl(GSPC)
apple_close <- Cl(AAPL)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))
apple_returns <- diff(log(apple_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)
apple_returns <- na.omit(apple_returns)

# Calcular VaR condicional para 95% y 99% de confianza
calcula_var_cond <- function(returns, confianza) {
  ar_garch_model <- garchFit(~ garch(1, 1), data = returns, trace = FALSE)
  mu_cond <- fitted(ar_garch_model)
  sigma_cond <- volatility(ar_garch_model)
  VaR_cond <- mu_cond + qnorm(confianza) * sigma_cond
  return(VaR_cond)
}

VaR_cond_95_sp500 <- calcula_var_cond(sp500_returns, 0.05)
VaR_cond_99_sp500 <- calcula_var_cond(sp500_returns, 0.01)

VaR_cond_95_apple <- calcula_var_cond(apple_returns, 0.05)
VaR_cond_99_apple <- calcula_var_cond(apple_returns, 0.01)

# Crear gráficos independientes para VaR condicional y rendimientos
plot_var_cond <- function(returns, VaR_cond, confidence_level, title) {
  # Definir límites para el eje y
  ylim <- range(coredata(returns), VaR_cond, na.rm = TRUE)
  
  # Graficar rendimientos
  plot(index(returns), coredata(returns), type = 'l', col = 'blue', main = title,
       xlab = 'Fecha', ylab = 'Rendimiento', lwd = 1.5, ylim = ylim)
  lines(index(returns), VaR_cond, col = 'red', lty = 2, lwd = 2)  # Línea del VaR condicional
  
  # Añadir leyenda
  legend("topright", legend = c("Rendimiento", paste("VaR Condicional", confidence_level)),
         col = c("blue", "red"), lty = c(1, 2), lwd = 2, bty = "n")
}

# Crear ventana gráfica para cuatro gráficos
# Ajustar el tamaño de la ventana gráfica si es necesario
windows(width = 12, height = 10)  # Solo en Windows. Usa X11() en Linux y quartz() en macOS

# Ajustar márgenes y ventana gráfica
par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1) # Márgenes ajustados

# Plotear los gráficos
plot_var_cond(sp500_returns, VaR_cond_95_sp500, "95%", 'S&P500 VaR Condicional 95%')
plot_var_cond(sp500_returns, VaR_cond_99_sp500, "99%", 'S&P500 VaR Condicional 99%')
plot_var_cond(apple_returns, VaR_cond_95_apple, "95%", 'Apple VaR Condicional 95%')
plot_var_cond(apple_returns, VaR_cond_99_apple, "99%", 'Apple VaR Condicional 99%')


################################################################################3



# Cargar las librerías necesarias
library(quantmod)
library(fGarch)

# Descargar datos del S&P 500 y Apple desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)
getSymbols("AAPL", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer los precios de cierre ajustado
sp500_close <- Cl(GSPC)
apple_close <- Cl(AAPL)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))
apple_returns <- diff(log(apple_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)
apple_returns <- na.omit(apple_returns)

# Calcular VaR condicional y Expected Shortfall (ES) para 95% y 99% de confianza
calcula_var_es_cond <- function(returns, confianza) {
  ar_garch_model <- garchFit(~ garch(1, 1), data = returns, trace = FALSE)
  mu_cond <- fitted(ar_garch_model)
  sigma_cond <- volatility(ar_garch_model)
  VaR_cond <- mu_cond + qnorm(confianza) * sigma_cond
  
  # Calcular Expected Shortfall (ES) condicional
  ES_cond <- sapply(seq_along(returns), function(i) {
    if (returns[i] < VaR_cond[i]) {
      mean(returns[returns < VaR_cond[i]], na.rm = TRUE)
    } else {
      NA
    }
  })
  
  list(VaR_cond = VaR_cond, ES_cond = ES_cond)
}

VaR_ES_95_sp500 <- calcula_var_es_cond(sp500_returns, 0.05)
VaR_ES_99_sp500 <- calcula_var_es_cond(sp500_returns, 0.01)

VaR_ES_95_apple <- calcula_var_es_cond(apple_returns, 0.05)
VaR_ES_99_apple <- calcula_var_es_cond(apple_returns, 0.01)

# Crear gráficos independientes para VaR condicional y Expected Shortfall (ES)
plot_var_es <- function(returns, VaR_cond, ES_cond, confidence_level, title) {
  # Definir límites para el eje y
  ylim <- range(coredata(returns), VaR_cond, ES_cond, na.rm = TRUE)
  
  # Graficar rendimientos
  plot(index(returns), coredata(returns), type = 'l', col = 'blue', main = title,
       xlab = 'Fecha', ylab = 'Rendimiento', lwd = 1.5, ylim = ylim)
  lines(index(returns), VaR_cond, col = 'red', lty = 2, lwd = 2)  # Línea del VaR condicional
  lines(index(returns), ES_cond, col = 'green', lty = 2, lwd = 2)  # Línea de la Expected Shortfall (ES)
  
  # Añadir leyenda
  legend("topright", legend = c("Rendimiento", paste("VaR Condicional", confidence_level), "Expected Shortfall"),
         col = c("blue", "red", "green"), lty = c(1, 2, 2), lwd = 2, bty = "n")
}

# Crear ventana gráfica para cuatro gráficos
# Ajustar el tamaño de la ventana gráfica si es necesario
windows(width = 12, height = 10)  # Solo en Windows. Usa X11() en Linux y quartz() en macOS

# Ajustar márgenes y ventana gráfica
par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1) # Márgenes ajustados

# Plotear los gráficos
plot_var_es(sp500_returns, VaR_ES_95_sp500$VaR_cond, VaR_ES_95_sp500$ES_cond, "95%", 'S&P500 VaR y ES Condicional 95%')
plot_var_es(sp500_returns, VaR_ES_99_sp500$VaR_cond, VaR_ES_99_sp500$ES_cond, "99%", 'S&P500 VaR y ES Condicional 99%')
plot_var_es(apple_returns, VaR_ES_95_apple$VaR_cond, VaR_ES_95_apple$ES_cond, "95%", 'Apple VaR y ES Condicional 95%')
plot_var_es(apple_returns, VaR_ES_99_apple$VaR_cond, VaR_ES_99_apple$ES_cond, "99%", 'Apple VaR y ES Condicional 99%')




############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################




# Cargar las librerías necesarias
library(quantmod)
library(fGarch)

# Descargar datos del S&P 500 y Apple desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)
getSymbols("AAPL", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer los precios de cierre ajustado
sp500_close <- Cl(GSPC)
apple_close <- Cl(AAPL)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))
apple_returns <- diff(log(apple_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)
apple_returns <- na.omit(apple_returns)

# Calcular VaR condicional y Expected Shortfall (ES) condicional para 95% y 99% de confianza
calcula_var_es_cond <- function(returns, confianza) {
  ar_garch_model <- garchFit(~ garch(1, 1), data = returns, trace = FALSE)
  mu_cond <- fitted(ar_garch_model)
  sigma_cond <- volatility(ar_garch_model)
  VaR_cond <- mu_cond + qnorm(confianza) * sigma_cond
  
  # Calcular Expected Shortfall (ES) condicional
  ES_cond <- rep(NA, length(returns))
  for (i in 1:length(returns)) {
    ES_cond[i] <- mean(returns[returns < VaR_cond[i]], na.rm = TRUE)
  }
  
  list(VaR_cond = VaR_cond, ES_cond = ES_cond)
}

# Calcular VaR y ES condicionales para 95% y 99% de confianza
VaR_ES_95_sp500 <- calcula_var_es_cond(sp500_returns, 0.05)
VaR_ES_99_sp500 <- calcula_var_es_cond(sp500_returns, 0.01)

VaR_ES_95_apple <- calcula_var_es_cond(apple_returns, 0.05)
VaR_ES_99_apple <- calcula_var_es_cond(apple_returns, 0.01)

# Crear gráficos independientes para Expected Shortfall (ES) condicional
plot_es_cond <- function(returns, ES_cond, confidence_level, title) {
  # Definir límites para el eje y
  ylim <- range(coredata(returns), ES_cond, na.rm = TRUE)
  
  # Graficar rendimientos
  plot(index(returns), coredata(returns), type = 'l', col = 'blue', main = title,
       xlab = 'Fecha', ylab = 'Rendimiento', lwd = 1.5, ylim = ylim)
  lines(index(returns), ES_cond, col = 'green', lty = 2, lwd = 2)  # Línea de la Expected Shortfall (ES)
  
  # Añadir leyenda
  legend("topright", legend = c("Rendimiento", paste("Expected Shortfall", confidence_level)),
         col = c("blue", "green"), lty = c(1, 2), lwd = 2, bty = "n")
}

# Crear ventana gráfica para cuatro gráficos
# Ajustar el tamaño de la ventana gráfica si es necesario
windows(width = 12, height = 10)  # Solo en Windows. Usa X11() en Linux y quartz() en macOS

# Ajustar márgenes y ventana gráfica
par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1) # Márgenes ajustados

# Plotear los gráficos
plot_es_cond(sp500_returns, VaR_ES_95_sp500$ES_cond, "95%", 'S&P500 Expected Shortfall 95%')
plot_es_cond(sp500_returns, VaR_ES_99_sp500$ES_cond, "99%", 'S&P500 Expected Shortfall 99%')
plot_es_cond(apple_returns, VaR_ES_95_apple$ES_cond, "95%", 'Apple Expected Shortfall 95%')
plot_es_cond(apple_returns, VaR_ES_99_apple$ES_cond, "99%", 'Apple Expected Shortfall 99%')
















































################################################################################


# Cargar las librerías necesarias
library(quantmod)
library(fGarch)

# Descargar datos del S&P 500 y Apple desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)
getSymbols("AAPL", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer los precios de cierre ajustado
sp500_close <- Cl(GSPC)
apple_close <- Cl(AAPL)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))
apple_returns <- diff(log(apple_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)
apple_returns <- na.omit(apple_returns)

# Calcular VaR condicional y Expected Shortfall (ES) condicional para 95% y 99% de confianza
calcula_var_es_cond <- function(returns, confianza) {
  ar_garch_model <- garchFit(~ garch(1, 1), data = returns, trace = FALSE)
  mu_cond <- fitted(ar_garch_model)
  sigma_cond <- volatility(ar_garch_model)
  VaR_cond <- mu_cond + qnorm(confianza) * sigma_cond
  
  # Calcular Expected Shortfall (ES) condicional
  ES_cond <- rep(NA, length(returns))
  for (i in 1:length(returns)) {
    ES_cond[i] <- mean(returns[returns < VaR_cond[i]], na.rm = TRUE)
  }
  
  list(VaR_cond = VaR_cond, ES_cond = ES_cond)
}

# Calcular VaR y ES condicionales para 95% y 99% de confianza
VaR_ES_95_sp500 <- calcula_var_es_cond(sp500_returns, 0.05)
VaR_ES_99_sp500 <- calcula_var_es_cond(sp500_returns, 0.01)

VaR_ES_95_apple <- calcula_var_es_cond(apple_returns, 0.05)
VaR_ES_99_apple <- calcula_var_es_cond(apple_returns, 0.01)

# Crear gráficos independientes para VaR y Expected Shortfall (ES) condicionales
plot_var_es_cond <- function(returns, VaR_cond, ES_cond, confidence_level, title, legend_position) {
  # Definir límites para el eje y
  ylim <- range(coredata(returns), VaR_cond, ES_cond, na.rm = TRUE)
  
  # Graficar rendimientos
  plot(index(returns), coredata(returns), type = 'l', col = 'blue', main = title,
       xlab = 'Fecha', ylab = 'Rendimiento', lwd = 1.5, ylim = ylim)
  
  # Graficar VaR y ES condicionales
  lines(index(returns), VaR_cond, col = 'red', lty = 2, lwd = 2)  # Línea del VaR
  lines(index(returns), ES_cond, col = 'green', lty = 2, lwd = 2)  # Línea de la ES
  
  # Añadir leyenda en la posición especificada
  legend(legend_position, legend = c("Rendimiento", paste("VaR Condicional", confidence_level), 
                                     paste("ES Condicional", confidence_level)),
         col = c("blue", "red", "green"), lty = c(1, 2, 2), lwd = 2, bty = "n")
}

# Crear ventana gráfica para cuatro gráficos
# Ajustar el tamaño de la ventana gráfica si es necesario
windows(width = 12, height = 10)  # Solo en Windows. Usa X11() en Linux y quartz() en macOS

# Ajustar márgenes y ventana gráfica
par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1) # Márgenes ajustados

# Plotear los gráficos con las leyendas en la parte inferior derecha para Apple
plot_var_es_cond(sp500_returns, VaR_ES_95_sp500$VaR_cond, VaR_ES_95_sp500$ES_cond, "95%", 'S&P500 VaR y ES 95%', "topright")
plot_var_es_cond(sp500_returns, VaR_ES_99_sp500$VaR_cond, VaR_ES_99_sp500$ES_cond, "99%", 'S&P500 VaR y ES 99%', "topright")
plot_var_es_cond(apple_returns, VaR_ES_95_apple$VaR_cond, VaR_ES_95_apple$ES_cond, "95%", 'Apple VaR y ES 95%', "bottomright")
plot_var_es_cond(apple_returns, VaR_ES_99_apple$VaR_cond, VaR_ES_99_apple$ES_cond, "99%", 'Apple VaR y ES 99%', "bottomright")









#########################################################################
########################## BACKTESTING
#########################################################################


# Calcular excepciones observadas
calcula_excepciones_observadas <- function(returns, VaR_cond) {
  sum(returns < VaR_cond)
}

# Número total de rendimientos
n_sp500 <- length(sp500_returns)
n_apple <- length(apple_returns)

# Calcular excepciones observadas para S&P500 y Apple
excepciones_observadas_95_sp500 <- calcula_excepciones_observadas(sp500_returns, VaR_ES_95_sp500$VaR_cond)
excepciones_observadas_99_sp500 <- calcula_excepciones_observadas(sp500_returns, VaR_ES_99_sp500$VaR_cond)

excepciones_observadas_95_apple <- calcula_excepciones_observadas(apple_returns, VaR_ES_95_apple$VaR_cond)
excepciones_observadas_99_apple <- calcula_excepciones_observadas(apple_returns, VaR_ES_99_apple$VaR_cond)

# Calcular excepciones esperadas
excepciones_esperadas_95_sp500 <- n_sp500 * 0.05
excepciones_esperadas_99_sp500 <- n_sp500 * 0.01

excepciones_esperadas_95_apple <- n_apple * 0.05
excepciones_esperadas_99_apple <- n_apple * 0.01

# Crear tabla con los resultados
tabla_excepciones <- data.frame(
  Activo = c("S&P500", "S&P500", "Apple", "Apple"),
  Confianza = c("95%", "99%", "95%", "99%"),
  Excepciones_Observadas = c(excepciones_observadas_95_sp500, excepciones_observadas_99_sp500,
                             excepciones_observadas_95_apple, excepciones_observadas_99_apple),
  Excepciones_Esperadas = c(excepciones_esperadas_95_sp500, excepciones_esperadas_99_sp500,
                            excepciones_esperadas_95_apple, excepciones_esperadas_99_apple),
  Porcentaje_Observado = c(
    excepciones_observadas_95_sp500 / n_sp500 * 100,
    excepciones_observadas_99_sp500 / n_sp500 * 100,
    excepciones_observadas_95_apple / n_apple * 100,
    excepciones_observadas_99_apple / n_apple * 100
  ),
  Porcentaje_Esperado = c(5, 1, 5, 1)
)

print(tabla_excepciones)


#####################################################3


# Instalar y cargar los paquetes necesarios
install.packages("PerformanceAnalytics")
install.packages("VaR")

library(PerformanceAnalytics)
library(VaR)

# Descargar datos del S&P 500 y Apple desde Yahoo Finance
library(quantmod)
library(fGarch)

getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)
getSymbols("AAPL", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer precios de cierre ajustado
sp500_close <- Cl(GSPC)
apple_close <- Cl(AAPL)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))
apple_returns <- diff(log(apple_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)
apple_returns <- na.omit(apple_returns)

# Calcular VaR condicional y Expected Shortfall (ES) condicional
calcula_var_es_cond <- function(returns, confianza) {
  ar_garch_model <- garchFit(~ garch(1, 1), data = returns, trace = FALSE)
  mu_cond <- fitted(ar_garch_model)
  sigma_cond <- volatility(ar_garch_model)
  VaR_cond <- mu_cond + qnorm(confianza) * sigma_cond
  
  # Calcular Expected Shortfall (ES) condicional
  ES_cond <- rep(NA, length(returns))
  for (i in 1:length(returns)) {
    ES_cond[i] <- mean(returns[returns < VaR_cond[i]], na.rm = TRUE)
  }
  
  list(VaR_cond = VaR_cond, ES_cond = ES_cond)
}

# Calcular VaR y ES condicionales para 95% y 99%
VaR_ES_95_sp500 <- calcula_var_es_cond(sp500_returns, 0.05)
VaR_ES_99_sp500 <- calcula_var_es_cond(sp500_returns, 0.01)
VaR_ES_95_apple <- calcula_var_es_cond(apple_returns, 0.05)
VaR_ES_99_apple <- calcula_var_es_cond(apple_returns, 0.01)

# Función para calcular excepciones observadas
calcula_excepciones_observadas <- function(returns, VaR_cond) {
  sum(returns < VaR_cond)
}

# Número total de rendimientos
n_sp500 <- length(sp500_returns)
n_apple <- length(apple_returns)

# Calcular excepciones observadas y esperadas
excepciones_observadas_95_sp500 <- calcula_excepciones_observadas(sp500_returns, VaR_ES_95_sp500$VaR_cond)
excepciones_observadas_99_sp500 <- calcula_excepciones_observadas(sp500_returns, VaR_ES_99_sp500$VaR_cond)
excepciones_observadas_95_apple <- calcula_excepciones_observadas(apple_returns, VaR_ES_95_apple$VaR_cond)
excepciones_observadas_99_apple <- calcula_excepciones_observadas(apple_returns, VaR_ES_99_apple$VaR_cond)

excepciones_esperadas_95_sp500 <- n_sp500 * 0.05
excepciones_esperadas_99_sp500 <- n_sp500 * 0.01
excepciones_esperadas_95_apple <- n_apple * 0.05
excepciones_esperadas_99_apple <- n_apple * 0.01

# Crear tabla con los resultados
tabla_excepciones <- data.frame(
  Activo = c("S&P500", "S&P500", "Apple", "Apple"),
  Confianza = c("95%", "99%", "95%", "99%"),
  Excepciones_Observadas = c(excepciones_observadas_95_sp500, excepciones_observadas_99_sp500,
                             excepciones_observadas_95_apple, excepciones_observadas_99_apple),
  Excepciones_Esperadas = c(excepciones_esperadas_95_sp500, excepciones_esperadas_99_sp500,
                            excepciones_esperadas_95_apple, excepciones_esperadas_99_apple),
  Porcentaje_Observado = c(
    excepciones_observadas_95_sp500 / n_sp500 * 100,
    excepciones_observadas_99_sp500 / n_sp500 * 100,
    excepciones_observadas_95_apple / n_apple * 100,
    excepciones_observadas_99_apple / n_apple * 100
  ),
  Porcentaje_Esperado = c(5, 1, 5, 1)
)

print(tabla_excepciones)



###############################################################




# Función para el test de Kupiec (LRuc)
test_kupiec <- function(excepciones_observadas, n, confianza) {
  p_esperado <- confianza
  p_observado <- excepciones_observadas / n
  LRuc <- -2 * (log(pbinom(excepciones_observadas - 1, size = n, prob = p_esperado)) -
                  log(pbinom(excepciones_observadas, size = n, prob = p_esperado)))
  p_valor <- 1 - pchisq(LRuc, df = 1)
  list(statistico = LRuc, p_valor = p_valor)
}








# Función para el test de Independencia (LRind)
test_independencia <- function(returns, VaR_cond) {
  # Crear una serie binaria de excepciones
  excepciones <- as.numeric(returns < VaR_cond)
  
  # Contar las excepciones
  n_excepciones <- sum(excepciones)
  n <- length(excepciones)
  
  # Crear series de excepciones para las condiciones de tiempo
  cond_excepciones <- c(0, excepciones[-n])
  
  # Contar casos de excepciones sucesivas
  casos_excepciones_sucesivas <- sum(excepciones & cond_excepciones)
  
  # Calcular los estadísticos
  p_esperado <- 0.05
  p_observado <- casos_excepciones_sucesivas / n_excepciones
  LRind <- -2 * (log(pbinom(casos_excepciones_sucesivas - 1, size = n_excepciones, prob = p_esperado)) -
                   log(pbinom(casos_excepciones_sucesivas, size = n_excepciones, prob = p_esperado)))
  p_valor <- 1 - pchisq(LRind, df = 1)
  list(statistico = LRind, p_valor = p_valor)
}





# Función para el test de Cobertura Condicional (LRcc)
test_cobertura_condicional <- function(returns, VaR_cond, ES_cond) {
  # Crear una serie binaria de excepciones
  excepciones <- as.numeric(returns < VaR_cond)
  
  # Calcular el número de excepciones
  n_excepciones <- sum(excepciones)
  
  # Calcular el estadístico LRcc
  p_observado <- sum(returns[excepciones] < ES_cond[excepciones]) / n_excepciones
  p_esperado <- 0.05
  LRcc <- -2 * (log(pbinom(n_excepciones - 1, size = n_excepciones, prob = p_esperado)) -
                  log(pbinom(n_excepciones, size = n_excepciones, prob = p_esperado)))
  p_valor <- 1 - pchisq(LRcc, df = 1)
  list(statistico = LRcc, p_valor = p_valor)
}





# Función para el contraste de cuantil dinámico (DQ)
test_dq <- function(returns, VaR_cond, ES_cond) {
  # Contar excepciones
  excepciones <- as.numeric(returns < VaR_cond)
  n_excepciones <- sum(excepciones)
  
  # Calcular el estadístico DQ
  p_esperado <- 0.05
  p_observado <- sum(excepciones) / length(returns)
  DQ <- -2 * (log(pbinom(n_excepciones - 1, size = length(returns), prob = p_esperado)) -
                log(pbinom(n_excepciones, size = length(returns), prob = p_esperado)))
  p_valor <- 1 - pchisq(DQ, df = 1)
  list(statistico = DQ, p_valor = p_valor)
}






# Función para calcular todos los tests
calcula_tests <- function(returns, VaR_cond, ES_cond, confianza) {
  n <- length(returns)
  
  # Test de Kupiec (LRuc)
  kupiec <- test_kupiec(calcula_excepciones_observadas(returns, VaR_cond), n, confianza)
  
  # Test de Independencia (LRind)
  independencia <- test_independencia(returns, VaR_cond)
  
  # Test de Cobertura Condicional (LRcc)
  cobertura_condicional <- test_cobertura_condicional(returns, VaR_cond, ES_cond)
  
  # Contraste de Cuantil Dinámico (DQ)
  dq <- test_dq(returns, VaR_cond, ES_cond)
  
  data.frame(
    Test = c("Kupiec (LRuc)", "Independencia (LRind)", "Cobertura Condicional (LRcc)", "Cuantil Dinámico (DQ)"),
    Estadistico = c(kupiec$statistico, independencia$statistico, cobertura_condicional$statistico, dq$statistico),
    P_Valor = c(kupiec$p_valor, independencia$p_valor, cobertura_condicional$p_valor, dq$p_valor)
  )
}






# Calcular y mostrar resultados para S&P 500
resultados_sp500_95 <- calcula_tests(sp500_returns, VaR_ES_95_sp500$VaR_cond, VaR_ES_95_sp500$ES_cond, 0.05)
resultados_sp500_99 <- calcula_tests(sp500_returns, VaR_ES_99_sp500$VaR_cond, VaR_ES_99_sp500$ES_cond, 0.01)

# Calcular y mostrar resultados para Apple
resultados_apple_95 <- calcula_tests(apple_returns, VaR_ES_95_apple$VaR_cond, VaR_ES_95_apple$ES_cond, 0.05)
resultados_apple_99 <- calcula_tests(apple_returns, VaR_ES_99_apple$VaR_cond, VaR_ES_99_apple$ES_cond, 0.01)

print("Resultados S&P 500 (95%):")
print(resultados_sp500_95)

print("Resultados S&P 500 (99%):")
print(resultados_sp500_99)

print("Resultados Apple (95%):")
print(resultados_apple_95)

print("Resultados Apple (99%):")
print(resultados_apple_99)



######################################################################################
#####################################################################################
########################################################################################3
######################################################################################



# Función para calcular la función de pérdida de López (L1)
calcula_perdida_lopez <- function(returns, VaR_cond) {
  # Calcular la función indicadora
  indicadora <- as.numeric(returns < VaR_cond)
  
  # Calcular la pérdida de López (L1)
  perdida_lopez <- mean((returns - VaR_cond) * indicadora)
  
  return(perdida_lopez)
}

# Calcular la función de pérdida de López para S&P 500 y Apple
perdida_lopez_sp500_95 <- calcula_perdida_lopez(sp500_returns, VaR_ES_95_sp500$VaR_cond)
perdida_lopez_sp500_99 <- calcula_perdida_lopez(sp500_returns, VaR_ES_99_sp500$VaR_cond)

perdida_lopez_apple_95 <- calcula_perdida_lopez(apple_returns, VaR_ES_95_apple$VaR_cond)
perdida_lopez_apple_99 <- calcula_perdida_lopez(apple_returns, VaR_ES_99_apple$VaR_cond)

# Mostrar los resultados
cat("Función de pérdida de López (L1) para S&P 500:\n")
cat("Nivel de confianza 95%:", perdida_lopez_sp500_95, "\n")
cat("Nivel de confianza 99%:", perdida_lopez_sp500_99, "\n")

cat("Función de pérdida de López (L1) para Apple:\n")
cat("Nivel de confianza 95%:", perdida_lopez_apple_95, "\n")
cat("Nivel de confianza 99%:", perdida_lopez_apple_99, "\n")




######################################################################################
#####################################################################################
########################################################################################3
######################################################################################


# Cargar las librerías necesarias
library(quantmod)
library(fGarch)

# Descargar datos del S&P 500 y Apple desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)
getSymbols("AAPL", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer los precios de cierre ajustado
sp500_close <- Cl(GSPC)
apple_close <- Cl(AAPL)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))
apple_returns <- diff(log(apple_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)
apple_returns <- na.omit(apple_returns)

# Función para calcular VaR y ES condicionales
calcula_var_es_cond <- function(returns, confianza) {
  ar_garch_model <- garchFit(~ garch(1, 1), data = returns, trace = FALSE)
  mu_cond <- fitted(ar_garch_model)
  sigma_cond <- volatility(ar_garch_model)
  VaR_cond <- mu_cond + qnorm(confianza) * sigma_cond
  
  # Calcular Expected Shortfall (ES) condicional
  ES_cond <- rep(NA, length(returns))
  for (i in 1:length(returns)) {
    ES_cond[i] <- mean(returns[returns < VaR_cond[i]], na.rm = TRUE)
  }
  
  list(VaR_cond = VaR_cond, ES_cond = ES_cond)
}

# Calcular VaR y ES condicionales para 95% y 99% de confianza
VaR_ES_95_sp500 <- calcula_var_es_cond(sp500_returns, 0.05)
VaR_ES_99_sp500 <- calcula_var_es_cond(sp500_returns, 0.01)
VaR_ES_95_apple <- calcula_var_es_cond(apple_returns, 0.05)
VaR_ES_99_apple <- calcula_var_es_cond(apple_returns, 0.01)

# Función para calcular el Expected Shortfall (ES) observado
calcula_ES_observado <- function(returns, VaR_cond) {
  # Crear un vector vacío para almacenar el ES observado
  ES_observado <- rep(NA, length(VaR_cond))
  
  # Asegurarse de que VaR_cond y returns tienen la misma longitud
  if (length(VaR_cond) != length(returns)) {
    stop("Las longitudes de VaR_cond y returns no coinciden")
  }
  
  # Calcular el ES observado para cada VaR_cond
  for (i in 1:length(VaR_cond)) {
    # Obtener los rendimientos que son menores que VaR_cond[i]
    perdidas <- returns[returns < VaR_cond[i]]
    
    # Calcular el ES para el VaR_cond[i]
    if (length(perdidas) > 0) {
      ES_observado[i] <- mean(perdidas, na.rm = TRUE)
    } else {
      ES_observado[i] <- NA
    }
  }
  
  return(ES_observado)
}

# Calcular el ES observado
ES_observado_95_sp500 <- calcula_ES_observado(sp500_returns, VaR_ES_95_sp500$VaR_cond)
ES_observado_99_sp500 <- calcula_ES_observado(sp500_returns, VaR_ES_99_sp500$VaR_cond)
ES_observado_95_apple <- calcula_ES_observado(apple_returns, VaR_ES_95_apple$VaR_cond)
ES_observado_99_apple <- calcula_ES_observado(apple_returns, VaR_ES_99_apple$VaR_cond)


###########################################################################################



# Cargar librerías necesarias
library(quantmod)
library(fGarch)

# Descargar datos del S&P 500 y Apple desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)
getSymbols("AAPL", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer precios de cierre ajustado
sp500_close <- Cl(GSPC)
apple_close <- Cl(AAPL)

# Calcular rendimientos diarios
sp500_returns <- diff(log(sp500_close))
apple_returns <- diff(log(apple_close))

# Eliminar valores NA
sp500_returns <- na.omit(sp500_returns)
apple_returns <- na.omit(apple_returns)

# Función para calcular VaR y ES condicionales
calcula_var_es_cond <- function(returns, confianza) {
  ar_garch_model <- garchFit(~ garch(1, 1), data = returns, trace = FALSE)
  mu_cond <- fitted(ar_garch_model)
  sigma_cond <- volatility(ar_garch_model)
  VaR_cond <- mu_cond + qnorm(confianza) * sigma_cond
  
  # Calcular Expected Shortfall (ES) condicional
  ES_cond <- rep(NA, length(returns))
  for (i in 1:length(returns)) {
    ES_cond[i] <- mean(returns[returns < VaR_cond[i]], na.rm = TRUE)
  }
  
  list(VaR_cond = VaR_cond, ES_cond = ES_cond)
}

# Función para calcular la desviación estándar truncada por VaR
calcula_sd_truncada <- function(returns, VaR_cond, ES_cond) {
  SDt <- rep(NA, length(returns))
  for (i in 1:length(returns)) {
    if (returns[i] < VaR_cond[i]) {
      SDt[i] <- sqrt(mean((returns[returns < VaR_cond[i]] - ES_cond[i])^2, na.rm = TRUE))
    } else {
      SDt[i] <- NA
    }
  }
  SDt
}

# Función para calcular BTt
calcula_bt <- function(returns, VaR_cond, ES_cond, SDt) {
  BTt <- rep(0, length(returns))
  for (i in 1:length(returns)) {
    if (!is.na(SDt[i]) && returns[i] < VaR_cond[i]) {
      BTt[i] <- (returns[i] - ES_cond[i]) / SDt[i]
    }
  }
  BTt
}

# Función para el test de Righi-Ceretta
test_righi_ceretta <- function(returns, VaR_cond, ES_cond, alpha, N = 1000) {
  # Calcular SDt y BTt
  SDt <- calcula_sd_truncada(returns, VaR_cond, ES_cond)
  BTt <- calcula_bt(returns, VaR_cond, ES_cond, SDt)
  
  # Hipótesis nula
  mean_BTt <- mean(BTt, na.rm = TRUE)
  
  # Simulación Monte Carlo
  set.seed(123)
  simulaciones <- replicate(N, {
    simulacion <- rnorm(length(returns))
    SD_sim <- calcula_sd_truncada(simulacion, VaR_cond, ES_cond)
    BT_sim <- calcula_bt(simulacion, VaR_cond, ES_cond, SD_sim)
    mean(BT_sim, na.rm = TRUE)
  })
  
  # Valor crítico y valor p
  valor_critico <- quantile(simulaciones, probs = 1 - alpha, na.rm = TRUE)
  p_valor <- mean(simulaciones <= mean_BTt, na.rm = TRUE)
  
  list(mean_BTt = mean_BTt, valor_critico = valor_critico, p_valor = p_valor)
}

# Calcular VaR y ES condicionales
VaR_ES_95_sp500 <- calcula_var_es_cond(sp500_returns, 0.05)
VaR_ES_99_sp500 <- calcula_var_es_cond(sp500_returns, 0.01)
VaR_ES_95_apple <- calcula_var_es_cond(apple_returns, 0.05)
VaR_ES_99_apple <- calcula_var_es_cond(apple_returns, 0.01)

# Realizar el test de Righi-Ceretta
resultados_righi_ceretta_95_sp500 <- test_righi_ceretta(sp500_returns, VaR_ES_95_sp500$VaR_cond, VaR_ES_95_sp500$ES_cond, 0.05)
resultados_righi_ceretta_99_sp500 <- test_righi_ceretta(sp500_returns, VaR_ES_99_sp500$VaR_cond, VaR_ES_99_sp500$ES_cond, 0.01)
resultados_righi_ceretta_95_apple <- test_righi_ceretta(apple_returns, VaR_ES_95_apple$VaR_cond, VaR_ES_95_apple$ES_cond, 0.05)
resultados_righi_ceretta_99_apple <- test_righi_ceretta(apple_returns, VaR_ES_99_apple$VaR_cond, VaR_ES_99_apple$ES_cond, 0.01)

# Mostrar resultados
resultados_righi_ceretta_95_sp500
resultados_righi_ceretta_99_sp500
resultados_righi_ceretta_95_apple
resultados_righi_ceretta_99_apple









########################################################################################
# Cargar las librerías necesarias
library(quantmod)
library(fGarch)

# Descargar datos del S&P 500 y Apple desde Yahoo Finance
getSymbols("^GSPC", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)
getSymbols("AAPL", from = "2000-01-01", to = "2021-12-31", src = "yahoo", auto.assign = TRUE)

# Extraer los precios de cierre ajustado
sp500_close <- Cl(GSPC)
apple_close <- Cl(AAPL)

# Calcular los rendimientos diarios
sp500_returns <- diff(log(sp500_close))
apple_returns <- diff(log(apple_close))

# Eliminar valores NA generados por la función diff
sp500_returns <- na.omit(sp500_returns)
apple_returns <- na.omit(apple_returns)

# Calcular VaR condicional y Expected Shortfall (ES) condicional para 95% y 99% de confianza
calcula_var_es_cond <- function(returns, confianza) {
  # Ajustar el modelo AR(1)-GARCH(1,1)
  ar_garch_model <- garchFit(~ arma(1, 0) + garch(1, 1), data = returns, trace = FALSE)
  
  # Extraer la media condicional y la volatilidad condicional
  mu_cond <- fitted(ar_garch_model)
  sigma_cond <- volatility(ar_garch_model)
  
  # Calcular el VaR condicional
  VaR_cond <- mu_cond + qnorm(confianza) * sigma_cond
  
  # Calcular el Expected Shortfall (ES) condicional
  ES_cond <- rep(NA, length(returns))
  for (i in 1:length(returns)) {
    ES_cond[i] <- mean(returns[returns < VaR_cond[i]], na.rm = TRUE)
  }
  
  list(VaR_cond = VaR_cond, ES_cond = ES_cond)
}

# Calcular VaR y ES condicionales para 95% y 99% de confianza
VaR_ES_95_sp500 <- calcula_var_es_cond(sp500_returns, 0.05)
VaR_ES_99_sp500 <- calcula_var_es_cond(sp500_returns, 0.01)

VaR_ES_95_apple <- calcula_var_es_cond(apple_returns, 0.05)
VaR_ES_99_apple <- calcula_var_es_cond(apple_returns, 0.01)

# Crear gráficos independientes para VaR y Expected Shortfall (ES) condicionales
plot_var_es_cond <- function(returns, VaR_cond, ES_cond, confidence_level, title, legend_position) {
  # Definir límites para el eje y
  ylim <- range(coredata(returns), VaR_cond, ES_cond, na.rm = TRUE)
  
  # Graficar rendimientos
  plot(index(returns), coredata(returns), type = 'l', col = 'blue', main = title,
       xlab = 'Fecha', ylab = 'Rendimiento', lwd = 1.5, ylim = ylim)
  
  # Graficar VaR y ES condicionales
  lines(index(returns), VaR_cond, col = 'red', lty = 2, lwd = 2)  # Línea del VaR
  lines(index(returns), ES_cond, col = 'green', lty = 2, lwd = 2)  # Línea de la ES
  
  # Añadir leyenda en la posición especificada
  legend(legend_position, legend = c("Rendimiento", paste("VaR Condicional", confidence_level), 
                                     paste("ES Condicional", confidence_level)),
         col = c("blue", "red", "green"), lty = c(1, 2, 2), lwd = 2, bty = "n")
}

# Crear ventana gráfica para cuatro gráficos
# Ajustar el tamaño de la ventana gráfica si es necesario
windows(width = 12, height = 10)  # Solo en Windows. Usa X11() en Linux y quartz() en macOS

# Ajustar márgenes y ventana gráfica
par(mfrow = c(2, 2), mar = c(5, 4, 4, 2) + 0.1) # Márgenes ajustados

# Plotear los gráficos con las leyendas en la parte inferior derecha para Apple
plot_var_es_cond(sp500_returns, VaR_ES_95_sp500$VaR_cond, VaR_ES_95_sp500$ES_cond, "95%", 'S&P500 VaR y ES 95%', "topright")
plot_var_es_cond(sp500_returns, VaR_ES_99_sp500$VaR_cond, VaR_ES_99_sp500$ES_cond, "99%", 'S&P500 VaR y ES 99%', "topright")
plot_var_es_cond(apple_returns, VaR_ES_95_apple$VaR_cond, VaR_ES_95_apple$ES_cond, "95%", 'Apple VaR y ES 95%', "bottomright")
plot_var_es_cond(apple_returns, VaR_ES_99_apple$VaR_cond, VaR_ES_99_apple$ES_cond, "99%", 'Apple VaR y ES 99%', "bottomright")


#######################################################################################################3
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################


