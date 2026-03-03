# Librerías necesarias
library(quantmod)
library(tseries)
library(FinTS)

######################################################################################################
############################################## EJERCICIO 1 ###########################################
######################################################################################################


# Ejercicio: Descargar datos del S&P 500
# --------------------------------------
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")
SP500 <- GSPC$GSPC.Close

# Verificar los datos descargados
print(head(SP500))
print(tail(SP500))
print(sum(is.na(SP500))) # Contar valores NA
  
# Eliminar valores nulos
SP500 <- na.omit(SP500)

# Ejercicio: Calcular rendimientos
# --------------------------------
# Calcular rendimientos (diferencia de logaritmos)
yield <- diff(log(SP500))
yield <- yield * 100

# Verificar los rendimientos calculados
print(head(yield))
print(tail(yield))
print(sum(is.na(yield))) # Contar valores NA

# Eliminar valores nulos resultantes
yield <- na.omit(yield)

# Ejercicio: Calcular estadísticas descriptivas
# ---------------------------------------------
# Funciones para asimetría y curtosis
kurtosis <- function(yield) {
  m4 <- mean((yield - mean(yield))^4)
  kurt <- m4 / (sd(yield)^4) - 3
  return(kurt)
}

skewness <- function(yield) {
  m3 <- mean((yield - mean(yield))^3)
  skew <- m3 / (sd(yield)^3)
  return(skew)
}

# Estadísticos descriptivos
media <- mean(yield)
mediana <- median(yield)
desv <- sd(yield)
maximo <- max(yield)
minimo <- min(yield)
asimetria <- skewness(yield)
curtosis <- kurtosis(yield)

# Mostrar resultados
resultados <- list(
  media = media,
  mediana = mediana,
  desviacion = desv,
  maximo = maximo,
  minimo = minimo,
  asimetria = asimetria,
  curtosis = curtosis
)

print(resultados)


######################################################################################################
############################################## EJERCICIO 2 ###########################################
######################################################################################################

# Ejercicio: Representación gráfica
# ---------------------------------
# Representación gráfica de la evolución del índice SP500 y sus rendimientos
plot(SP500, main = "Evolución del índice SP500", col = "blue")
plot(yield, main = "Rendimientos del índice SP500", col = "blue")

# Ejercicio: Análisis de la función de correlación simple (ACF)
# -------------------------------------------------------------
# Calcular la función de correlación simple (acf) de los rendimientos
acf(yield, lag.max = 15)

# Ejercicio: Test de Ljung-Box para 1, 10 y 20 retardos
# -----------------------------------------------------
lags <- c(1, 10, 20)
for (lag in lags) {
  lb_test <- Box.test(yield, lag = lag, type = "Ljung-Box", fitdf = 0)
  cat(paste("Test de Ljung-Box para", lag, "retardos:\n"))
  print(lb_test)
  cat("\n")  # Espacio adicional entre resultados
}


# Ejercicio: Test de Ljung-Box para 1, 10 y 20 retardos
# -----------------------------------------------------
lags <- c(1, 10, 20)
for (lag in lags) {
  lb_test <- Box.test(yield, lag = lag, type = "Ljung-Box", fitdf = 0)
  cat(paste("Test de Ljung-Box para", lag, "retardos:\n"))
  cat("X-squared =", lb_test$statistic, ", df =", lb_test$parameter, 
      ", p-value =", format.pval(lb_test$p.value), "\n\n")
}


######################################################################################################
############################################## EJERCICIO 3 ###########################################
######################################################################################################


# Calcular rendimientos al cuadrado
yield_squared <- yield^2

# Calcular la función de correlación simple (acf) de los rendimientos al cuadrado
acf_squared <- acf(yield_squared, lag.max = 15)

# Mostrar resultados
cat("ACF de los rendimientos al cuadrado:\n")
print(acf_squared)

# Comparación con los resultados del apartado (b) del ejercicio anterior
cat("\nComparación con los resultados del ejercicio anterior (ACF de los rendimientos):\n")
acf_yield <- acf(yield, lag.max = 15, plot = FALSE)
print(acf_yield)



#Test ARCH-LM
library(tseries)
# Test ARCH a 1 lag
arch_test_1 <- ArchTest(yield, lags = 1)
print(arch_test_1)

# Test ARCH a 10 lags
arch_test_10 <- ArchTest(yield, lags = 10)
print(arch_test_10)

# Test ARCH a 20 lags
arch_test_20 <- ArchTest(yield, lags = 20)
print(arch_test_20)


######################################################################################################
############################################## EJERCICIO 4 ###########################################
######################################################################################################

# Cargar librerías necesarias
library(quantmod)
library(tseries)

# Función para calcular el test de sesgo de signo y tamaño
sesgo_signo <- function(yield) {
  # Calculamos los residuos estandarizados
  residuos <- yield
  z <- residuos / sd(residuos)
  
  # Calculamos z al cuadrado
  z_square <- z^2
  
  # Variables dummy I_negative y I_positive
  I_negative <- ifelse(residuos < 0, 1, 0)
  I_positive <- ifelse(residuos > 0, 1, 0)
  
  # Calculamos el sesgo de signo y tamaño
  negative_bias <- I_negative * residuos
  positive_bias <- I_positive * residuos
  
  # Preparamos los datos para la regresión
  data <- data.frame(z_square, I_negative, negative_bias, I_positive, positive_bias)
  
  # Eliminamos la primera fila porque I_negative e I_positive tienen un valor inicial de 0
  data <- data[-1, ]
  
  # Estimación del modelo de regresión lineal MCO
  modelo <- lm(z_square ~ I_negative + negative_bias + I_positive + positive_bias, data = data)
  
  return(modelo)
}

# Descargar datos del S&P 500
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")
SP500 <- GSPC$GSPC.Close

# Calcular rendimientos
yield <- diff(log(SP500)) * 100
yield <- na.omit(yield)

# Estimar el modelo de sesgo de signo y tamaño
modelo <- sesgo_signo(yield)

# Mostrar resultados del modelo
summary(modelo)

# Test F para la significatividad conjunta de los coeficientes
anova(modelo)







######################3


library(quantmod)

# Descargar datos del S&P 500
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")
SP500 <- GSPC$GSPC.Close

# Calcular rendimientos
returns <- diff(log(SP500)) * 100
returns <- na.omit(returns)

# Estimación de los rendimientos estandarizados
mean_return <- mean(returns)
sd_return <- sd(returns)
z <- (returns - mean_return) / sd_return

# Cuadrados de los rendimientos estandarizados
z_square <- z^2

# Crear las variables dummy
I_negative <- ifelse(lag(z, -1) < 0, 1, 0) # I(-) t-1
I_positive <- ifelse(lag(z, -1) > 0, 1, 0) # I(+) t-1

# Crear las innovaciones rezagadas
epsilon_lag <- lag(z, -1)

# Crear las interacciones
negative_bias <- I_negative * epsilon_lag
positive_bias <- I_positive * epsilon_lag

# Eliminar valores nulos resultantes de los lags
data <- na.omit(data.frame(z_square, I_negative, I_positive, negative_bias, positive_bias))

# Estimar la regresión
modelo <- lm(z_square ~ I_negative + negative_bias + I_positive + positive_bias, data = data)

# Resumen del modelo
summary(modelo)


































# Cargar la librería necesaria
library(quantmod)

# Descargar los datos del S&P 500
getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = "2021-12-31", periodicity = "daily")

# Extraer los precios de cierre
SP500 <- GSPC$GSPC.Close

# Calcular los rendimientos logarítmicos diarios
returns <- diff(log(SP500)) * 100
returns <- na.omit(returns)  # Eliminar NA


# Estimación de los rendimientos estandarizados
mean_return <- mean(returns)
sd_return <- sd(returns)
z <- (returns - mean_return) / sd_return

# Cuadrado de los rendimientos estandarizados
z_square <- z^2

# Crear las variables dummy
epsilon_lag <- lag(z, -1)
I_negative <- ifelse(epsilon_lag < 0, 1, 0) # I(-) t-1
I_positive <- ifelse(epsilon_lag > 0, 1, 0) # I(+) t-1

# Crear las interacciones
negative_bias <- I_negative * epsilon_lag
positive_bias <- I_positive * epsilon_lag

# Crear el dataframe final, eliminando los NA
data <- na.omit(data.frame(z_square, I_negative, I_positive, negative_bias, positive_bias))



# Estimar la regresión
modelo <- lm(z_square ~ I_negative + negative_bias + I_positive + positive_bias, data = data)

# Resumen del modelo
summary(modelo)



# Test F para la significatividad conjunta de los coeficientes
anova(modelo)
