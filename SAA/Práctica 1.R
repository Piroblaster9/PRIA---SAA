# 1.1 Define el espacio muestral cuando realizamos tiradas con dos dados donde obtenemos como resultado de cada experimento aleatorio la suma de los números de cada dado.

dados <- 1:6
espacio_muestral <- expand.grid(dado1 = dados, dado2 = dados)
espacio_muestral$suma <- espacio_muestral$dado1 + espacio_muestral$dado2
print(espacio_muestral)


#1.2 Calcula las probabilidades de cada uno de los elementos del conjunto espacio muestral

probabilidad <- table(espacio_muestral$suma) / length(espacio_muestral$suma)
print(probabilidad)

#1.3 Representa gráficamente, mediante un diagrama de columnas las probabilidades anteriores (eje y),
# donde aparezcan los resultados del espacio muestral en el eje x

probabilidades <- table(espacio_muestral$suma) / length(espacio_muestral$suma)

barplot(probabilidades, names.arg = unique(espacio_muestral$suma),
        col = "skyblue", main = "Diagrama de Columnas de Probabilidades",
        xlab = "Suma de los dados", ylab = "Probabilidad")

# 1.4 ¿Los ensayos son dependientes o independientes?

# Son independientes. 

# 1.5 Calcula la probabilidad de obtener tres veces consecutivas un 7 en las tres primeras tiradas.

prob_7 <- 6 / 36  

prob_tres_7 <- prob_7^3
print(prob_tres_7)

#2.1 Realiza el siguiente experimento utilizando código R y las librerías dplyr (sample_n y operador %>%) y ggplot2: Simula el lanzamiento de un dado. 
#Cada vez que salga un 2, recibes 5 € (el tuyo y 4 de ganancia neta), cada vez que no aciertes, pierdes 1.
#Calcula la esperanza matemática teórica y mediante un experimento con 10000 simulaciones y una semilla = “777”.
# Representa gráficamente en una línea, tu ganancia acumulada (eje y) sobre el número de simulaciones (eje x) y agrega una línea a la gráfica con la esperanza matemática, para medir el grado de suerte.
#Aplica ahora la semilla 999. ¿Con qué semilla ha habido más suerte?

library(dplyr)
library(ggplot2)

set.seed(777)

n_simulaciones <- 10000

resultados <- data.frame(simulacion = 1:n_simulaciones, ganancia_acumulada = 0)

for (i in 2:n_simulaciones) {
  lanzamiento <- sample(1:6, 1)

  ganancia <- ifelse(lanzamiento == 2, 5, -1)

  resultados$ganancia_acumulada[i] <- resultados$ganancia_acumulada[i - 1] + ganancia
}

esperanza_matematica_teoria <- (1/6) * 5 + (5/6) * (-1)

ggplot(resultados, aes(x = simulacion, y = ganancia_acumulada)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = esperanza_matematica_teoria, linetype = "dashed", color = "red") +
  labs(title = "Ganancia Acumulada en Simulaciones",
       x = "Número de Simulaciones",
       y = "Ganancia Acumulada") +
  theme_minimal()

# 2.2 Realiza el experimento, apostando al 7 en la ruleta, 1.000.000 veces, con 35 € de ganancia neta cada vez que aciertes y un euro de pérdidas cada vez que falles.
# Calcula la esperanza matemática teórica y la esperanza matemática según el experimento. Representa visualmente la línea de la esperanza matemática (si en cada tirada ganas Ex).
#Utiliza como semillas 1 y 123. En base a los resultados obtenidos ¿Qué opinas sobre la varianza y la esperanza matemática de este juego?

set.seed(1)

n_simulaciones <- 1000000

resultados_ruleta <- data.frame(simulacion = 1:n_simulaciones, ganancia_acumulada = 0)

for (i in 2:n_simulaciones) {
  resultado_ruleta <- sample(c(1:36, 0, 00), 1)
  
  ganancia <- ifelse(resultado_ruleta == 7, 35, -1)
  
  resultados_ruleta$ganancia_acumulada[i] <- resultados_ruleta$ganancia_acumulada[i - 1] + ganancia
}

esperanza_matematica_teoria_ruleta <- (1/38) * 35 + (37/38) * (-1)

ggplot(resultados_ruleta, aes(x = simulacion, y = ganancia_acumulada)) +
  geom_line(color = "green") +
  geom_hline(yintercept = esperanza_matematica_teoria_ruleta, linetype = "dashed", color = "red") +
  labs(title = "Ganancia Acumulada en Simulación de Ruleta",
       x = "Número de Simulaciones",
       y = "Ganancia Acumulada") +
  theme_minimal()



#Ahora con semilla 123

set.seed(123)

resultados_ruleta_123 <- data.frame(simulacion = 1:n_simulaciones, ganancia_acumulada = 0)

for (i in 2:n_simulaciones) {
  resultado_ruleta_123 <- sample(c(1:36, 0, 00), 1)
  
  ganancia <- ifelse(resultado_ruleta_123 == 7, 35, -1)

  resultados_ruleta_123$ganancia_acumulada[i] <- resultados_ruleta_123$ganancia_acumulada[i - 1] + ganancia
}




#3 Utilizando el dataset “lqsa”, simula 1000 veces la extracción de muestras aleatorias de tamaño n = 15 de las 30 cartas. Calcula la media muestral para cada extracción y construye una matriz o data frame con las 5 variables numéricas de la baraja como columnas y en cada fila la media muestral del valor de cada extracción.
#Calcula la media y la desviación típica de las mil filas para cada variable.
#Representa gráficamente la distribución de los datos para cada variable. Utiliza geom_density() en lugar de barras. Ejemplo del experimento con la variable “locura”.


library(dplyr)

# Simulación de 1000 extracciones aleatorias de tamaño 15
set.seed(123)  # Establecer la semilla para reproducibilidad
n_simulaciones <- 1000
tamanio_muestra <- 15

# Crear un data frame para almacenar las medias muestrales
medias_muestrales <- matrix(NA, nrow = n_simulaciones, ncol = 5,
                            dimnames = list(NULL, c("variable1", "variable2", "variable3", "variable4", "variable5")))

# Realizar las simulaciones
for (i in 1:n_simulaciones) {
  muestra <- lqsa[sample(nrow(lqsa), tamanio_muestra, replace = TRUE), ]
  medias_muestrales[i, ] <- colMeans(muestra)
}

# Calcular la media y la desviación típica de las mil filas para cada variable
medias_globales <- colMeans(medias_muestrales)
desviaciones_globales <- apply(medias_muestrales, 2, sd)

# Visualizar la distribución de la variable "locura"
library(ggplot2)

ggplot(data.frame(locura = medias_muestrales[, "locura"]), aes(x = locura)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribución de Medias Muestrales de Locura",
       x = "Media Muestral",
       y = "Densidad") +
  theme_minimal()



