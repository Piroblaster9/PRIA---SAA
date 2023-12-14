




# Práctica 0:

## Cargar Datos
El código comienza cargando un archivo CSV llamado "Práctica 0 - SAA - Hoja 1.csv" en un DataFrame llamado 'df' utilizando la función `read.csv`:

```r
df <- read.csv("C:\\Users\\Piroblaster9\\Desktop\\Práctica 0 - SAA - Hoja 1.csv")
print(df)

# Visualización de Datos
# Luego, utiliza la librería ggplot2 para crear histogramas de diferentes variables del DataFrame 'df'. Se generan gráficos de histograma para las siguientes variables:

ggplot(df, aes(x=Poder)) +
  geom_histogram(bins = 30)

# Estadísticas Descriptivas
# A continuación, el código calcula diversas estadísticas descriptivas para las variables:

# Medias
media <- mean(df$Poder)
print(media)

# Medianas
mediana <- median(df$Poder)
print(mediana)

# Moda
tabla_frecuencias <- table(df$Poder)
moda <- as.numeric(names(tabla_frecuencias)[which.max(tabla_frecuencias)])
print(moda)

# Varianzas
varianza_R <- var(df$Poder)
print(varianza_R)

# Distribución de Edad y Sexo
# El código también genera gráficos de distribución para las variables 'Grupo_edad' y 'Sexo' utilizando gráficos de pastel (pie charts):

tabla_frecuencias_edad <- table(df$Grupo_edad)
pie(tabla_frecuencias_edad, main = "Distribución de Edad")

tabla_frecuencias_sexo <- table(df$Sexo)
pie(tabla_frecuencias_sexo, main = "Distribución de Sexo")

#Rango e IQR
#El rango de cada variable se calcula con la función range(), pero el cálculo del IQR (rango intercuartílico) no se realiza en el código proporcionado:

range(df$Poder)
```

## Definir espacio muestral
Este código genera el espacio muestral para el lanzamiento de dos dados. expand.grid crea todas las combinaciones posibles, y se calcula la suma de los números en cada dado.

```r
dados <- 1:6
espacio_muestral <- expand.grid(dado1 = dados, dado2 = dados)
espacio_muestral$suma <- espacio_muestral$dado1 + espacio_muestral$dado2
print(espacio_muestral)
```

## Calcular probabilidades del espacio muestral
Calcula las probabilidades de cada elemento en el espacio muestral dividiendo la frecuencia de cada suma por el total de resultados posibles.

```r
probabilidad <- table(espacio_muestral$suma) / length(espacio_muestral$suma)
print(probabilidad)

```

## Representar gráficamente las probabilidades
Crea un diagrama de columnas que representa visualmente las probabilidades de cada suma en el espacio muestral.

```r
probabilidades <- table(espacio_muestral$suma) / length(espacio_muestral$suma)

barplot(probabilidades, names.arg = unique(espacio_muestral$suma),
        col = "skyblue", main = "Diagrama de Columnas de Probabilidades",
        xlab = "Suma de los dados", ylab = "Probabilidad")

```

## Representar gráficamente las probabilidades
Crea un diagrama de columnas que representa visualmente las probabilidades de cada suma en el espacio muestral.

```r
probabilidades <- table(espacio_muestral$suma) / length(espacio_muestral$suma)

barplot(probabilidades, names.arg = unique(espacio_muestral$suma),
        col = "skyblue", main = "Diagrama de Columnas de Probabilidades",
        xlab = "Suma de los dados", ylab = "Probabilidad")

```

## Calcular probabilidades de obtener x resultado
Calcula la probabilidad de obtener tres 7 consecutivos en las tres primeras tiradas.

```r
prob_7 <- 6 / 36  

prob_tres_7 <- prob_7^3
print(prob_tres_7)

```

## Calcular esperanza matemática
Simula el lanzamiento de un dado con ganancias y pérdidas, calcula la esperanza matemática y visualiza la ganancia acumulada.

```r
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


```

## Simular apuestas y analizar resultados
Simula apuestas en la ruleta, calcula la esperanza matemática y visualiza la ganancia acumulada. Compara resultados con diferentes semillas.

```r
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

```

## Simulación de Extracciones Aleatorias en un Dataset
Simula extracciones aleatorias en el dataset "lqsa", calcula medias muestrales y visualiza la distribución con densidad.

```r
library(dplyr)

set.seed(123)

n_simulaciones <- 1000
tamanio_muestra <- 15

medias_muestrales <- matrix(NA, nrow = n_simulaciones, ncol = 5,
                            dimnames = list(NULL, c("variable1", "variable2", "variable3", "variable4", "variable5")))

for (i in 1:n_simulaciones) {
  muestra <- lqsa[sample(nrow(lqsa), tamanio_muestra, replace = TRUE), ]
  medias_muestrales[i, ] <- colMeans(muestra)
}

medias_globales <- colMeans(medias_muestrales)
desviaciones_globales <- apply(medias_muestrales, 2, sd)

library(ggplot2)

ggplot(data.frame(locura = medias_muestrales[, "locura"]), aes(x = locura)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribución de Medias Muestrales de Locura",
       x = "Media Muestral",
       y = "Densidad") +
  theme_minimal()

```
