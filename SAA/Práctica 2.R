#1.1 Función Logística y sus Casos de Uso en R:

#La función logística es comúnmente utilizada en estadística y machine learning para modelar procesos que tienen un comportamiento de crecimiento o decrecimiento limitado

#Casos de Uso en R:
  
  #La regresión logística en análisis de datos y clasificación binaria. Se puede implementar la función logística:

# Definir la función logística en R
logistica <- function(x) {
  return(1 / (1 + exp(-x)))
}

# Crear datos de ejemplo
set.seed(123)
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- ifelse(0.5 * x1 - 0.8 * x2 + rnorm(100) > 0, 1, 0)

# Ajustar el modelo de regresión logística
modelo_logistico <- glm(y ~ x1 + x2, family = binomial)

# Visualizar los resultados
summary(modelo_logistico)

#1.2 Representación Gráfica de la Función de Densidad:

x_vals <- seq(-6, 6, length.out = 100)
y_vals <- logistica(x_vals)

# Graficar la función logística
plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
     main = "Función Logística", xlab = "x", ylab = "f(x)")

#1.3 Generación de Números Aleatorios con la Función Logística:

#La función logística no se usa típicamente para generar números aleatorios directamente, pero puedes generar datos aleatorios siguiendo una distribución logística utilizando la función rlogis en R.

# Generar números aleatorios con distribución logística
num_aleatorios <- rlogis(1000, location = 0, scale = 1)

# Visualizar el histograma de los números aleatorios generados
hist(num_aleatorios, main = "Histograma de Números Aleatorios Logísticos",
     xlab = "Valor", ylab = "Frecuencia", col = "lightblue", border = "black")

#1.4 Cálculo de Probabilidades con la Función de Distribución Acumulada:

#La función de distribución acumulada (CDF) de la función logística se puede calcular con la función plogis en R.

prob_x_menor_0 <- plogis(0, location = 0, scale = 1)
prob_x_menor_1 <- plogis(1, location = 0, scale = 1)

cat("P(X <= 0):", prob_x_menor_0, "\n")
cat("P(X <= 1):", prob_x_menor_1, "\n")



#2.1 Función t-Student y sus Casos de Uso:

#La distribución t-Student es ampliamente utilizada en estadística para inferencia sobre medias.

#Casos de Uso:

#La distribución t-Student se utiliza en casos donde la desviación estándar de la población es desconocida, y se hace una inferencia sobre la media de la población. Algunos casos de uso comunes incluyen:
#Intervalos de Confianza para la Media: Cuando se estima el intervalo en el cual la verdadera media poblacional podría caer.
#Pruebas de Hipótesis para la Media: Para evaluar si la media de una población es significativamente diferente de un valor dado.
#Regresión Lineal: En la regresión lineal cuando se trabaja con muestras pequeñas.

#2.2 Representación Gráfica de la Función de Densidad:

nu <- 5  # Número de grados de libertad
t_vals <- seq(-4, 4, length.out = 100)
y_vals <- dt(t_vals, df = nu)

# Graficar la función t-Student
plot(t_vals, y_vals, type = "l", col = "red", lwd = 2,
     main = "Función t-Student", xlab = "t", ylab = "f(t)")

#2.3 Generación de Números Aleatorios con la Distribución t-Student:

num_aleatorios <- rt(1000, df = nu)

# Visualizar el histograma de los números aleatorios generados
hist(num_aleatorios, main = "Histograma de Números Aleatorios t-Student",
     xlab = "Valor", ylab = "Frecuencia", col = "lightcoral", border = "black")

#2.4 Cálculo de Probabilidades con la Función de Distribución Acumulada:

prob_t_menor_0 <- pt(0, df = nu)
prob_t_menor_1 <- pt(1, df = nu)

cat("P(T <= 0):", prob_t_menor_0, "\n")
cat("P(T <= 1):", prob_t_menor_1, "\n")

#2.5 Comparativa Visual entre Normal y t-Student:

grados_libertad <- c(2, 5, 15, 25)
colors <- c("blue", "green", "orange", "purple")

# Crear una ventana de gráficos
par(mfrow = c(2, 2))

# Graficar la función de densidad para diferentes grados de libertad
for (i in 1:length(grados_libertad)) {
  t_vals <- seq(-4, 4, length.out = 100)
  y_vals_t <- dt(t_vals, df = grados_libertad[i])
  y_vals_norm <- dnorm(t_vals)
  
  plot(t_vals, y_vals_t, type = "l", col = colors[i], lwd = 2,
       main = paste("t-Student vs Normal (df =", grados_libertad[i], ")"),
       xlab = "t", ylab = "f(t)")
  
  lines(t_vals, y_vals_norm, col = "red", lwd = 2, lty = 2)
  legend("topright", legend = c("t-Student", "Normal"), col = c(colors[i], "red"), lty = 1:2, cex = 0.8)
}



#3. Distribución uniforme discreta

runifd <- function(n, a, b) {
  if (!is.integer(n) || n <= 0) {
    stop("n debe ser un entero positivo.")
  }
  if (!is.integer(a) || !is.integer(b) || a >= b) {
    stop("a y b deben ser enteros, y a debe ser menor que b.")
  }
  
  sample(a:b, n, replace = TRUE)
}

dunifd <- function(x, a, b) {
  if (!is.integer(x) || !is.integer(a) || !is.integer(b) || x < a || x > b) {
    stop("x debe ser un entero en el rango [a, b].")
  }
  
  return(1 / (b - a + 1))
}

punifd <- function(q, a, b) {
  if (!is.integer(q) || !is.integer(a) || !is.integer(b) || q < a || q > b) {
    stop("q debe ser un entero en el rango [a, b].")
  }
  
  return((q - a + 1) / (b - a + 1))
}

sapply(0:10, a = 1, b = 6, FUN = dunifd)

cdf_q4 <- punifd(4, a = 1, b = 6)
print(cdf_q4)



#4.1 Función Multinomial:

#La distribución multinomial es una extensión de la distribución binomial que describe el número de éxitos en un experimento con más de dos resultados posibles, donde cada resultado tiene una probabilidad fija de ocurrir. Es especialmente útil cuando se trata de experimentos con más de dos categorías o clases.

#4.2 Generación de Números Aleatorios:

#Puedes generar números aleatorios que sigan una distribución multinomial utilizando la función rmultinom. Aquí hay un ejemplo:

n <- 1000  # Número total de ensayos
probs <- c(0.2, 0.3, 0.5)  # Probabilidades de éxito para cada categoría

# La suma de las probabilidades debe ser 1
if (sum(probs) != 1) {
  stop("Las probabilidades deben sumar 1.")
}

# Generar valores aleatorios
random_values <- rmultinom(n, size = 1, prob = probs)

# Mostrar los primeros 10 valores generados
print(random_values[, 1:10])


#4.3 Cálculo de Probabilidades con la Función de Distribución Acumulada:

#La función de distribución acumulada (CDF) para la distribución multinomial no tiene una forma cerrada simple como en algunas otras distribuciones. Sin embargo, puedes calcular las probabilidades acumulativas utilizando la función pmultinom. Aquí hay un ejemplo:

# Definir valores observados
observed_values <- c(150, 200, 300)

# Calcular la probabilidad acumulada de observar menos de 150 éxitos en la primera categoría,
# menos de 200 éxitos en la segunda categoría, y menos de 300 éxitos en la tercera categoría.
cumulative_prob <- pmultinom(observed_values - 1, size = sum(observed_values), prob = probs)

# Mostrar el resultado
print(cumulative_prob)


#5.1 Función de Distribución de Probabilidad de Poisson:

#La distribución de Poisson es una distribución de probabilidad discreta que describe el número de eventos que ocurrirán en un intervalo de tiempo fijo o en un área espacial fija, bajo la condición de que estos eventos ocurren con una tasa promedio constante y de manera independiente

#5.3 Cálculo de Probabilidades con la Función de Distribución Acumulada:

# Calcular la probabilidad acumulada P(X <= 3) para lambda = 2
k <- 3
lambda <- 2

# Usar la función ppois en R
cumulative_prob <- ppois(k, lambda)

# Mostrar el resultado
print(cumulative_prob)


