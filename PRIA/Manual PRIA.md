




# Datos destacables de Práctica 0:

## Booleano o lógico (logical)
TRUE ó FALSE. Cuidado porque en R hace distinción de mayúsculas

- **Framgmento ejercicio 4:**

  ¿Qué ha ocurrido? Imprime el valor de la variable mi_fruta Corrige el error y ejecuta la última línea
  ```R
  mi_fruta <- mis_manzanas + mis_naranjas

  El error está en que el valor de mis_naranjas es un string en lugar de un número, por eso da error al sumarlos.

Ejercicio 7:

La función date() nos muestra la fecha actual del sistema. Averigua a qué tipo de dato básico es.

En este caso da: "Thu Oct 5 18:45:07 2023"

Ejercicio 8:

¿Cuántos tipos básicos tiene R? Enuméralos en un comentario.

# Numérico, ya sea entero o doble (numeric)
# Texto (character)
# Booleano o lógico (logical). TRUE ó FALSE. Cuidado porque en R hace distinción de mayúsculas
# Complejos (complex).

# Funciones Aprendidas en la Práctica 1

## 1. Vectores

### 1.1 `1:50` y `seq(1, 50)`:

- `1:50` crea un vector que contiene los números del 1 al 50 de manera consecutiva.
- `seq(1, 50)` hace lo mismo, pero permite un mayor control, incluyendo la opción de especificar el paso.

### 1.2 `seq(1, 50, by = 0.5)`:

- `seq` se utiliza para generar secuencias de números. En este caso, crea un vector con números desde 1 hasta 50, con un paso de 0.5.

### 1.3 `length()`:

- `length` es una función que calcula la longitud (número de elementos) de un vector.

### 1.4 `class()`:

- `class` es una función que devuelve la clase (tipo) de un objeto en R. En este caso, se utiliza para determinar la clase del resultado de `length()`.

### 1.5 `seq(from = 1, to = 22, length.out = 10)`:

- `seq` también puede utilizarse para generar secuencias de números con un número específico de elementos (`length.out`), en este caso, 10 elementos entre 1 y 22.

### 1.6 `rep(1, 4)`:

- `rep` se usa para repetir un valor o un vector varias veces. En este caso, se crea un vector con cuatro elementos iguales a 1.

### 1.7 `rep(1:5, 3)`:

- Esta función repite un vector (en este caso, del 1 al 5) un número específico de veces (3 veces), creando un vector resultante.

### 1.8 `c()`:

- `c` se usa para combinar elementos en un solo vector. Aquí se combina una selección de elementos de `v1_22_10` para crear un nuevo vector.

## 2. Matrices

### 2.1 `matrix(1:9, nrow = 3, byrow = TRUE)`:

- `matrix` se utiliza para crear matrices. Aquí se crea una matriz de 3 filas con los números del 1 al 9, organizados por filas debido al argumento `byrow = TRUE`.

### 2.2 `matrix(1:9, nrow = 3)`:

- Esta función es similar a la anterior, pero organiza los números por columnas en lugar de filas.

### 2.3, 2.4, 2.5, 2.6, 2.7 y 2.8:

- Estos ejercicios involucran operaciones con matrices y manipulación de datos en matrices. Las funciones utilizadas en estos ejercicios no son funciones específicas de R, sino operaciones matriciales y funciones genéricas para manipular matrices.

## 3. Factores

### 3.1 `factor()`:

- `factor` se utiliza para crear factores en R, que son una forma de representar variables categóricas. Convierte un vector de datos en un factor con niveles que representan las categorías.

### 3.2 `levels`:

- `levels` es un argumento de la función `factor` que permite especificar el orden de los niveles en el factor.

### 3.3 `summary()`:

- `summary` se utiliza para obtener un resumen estadístico de un objeto. Cuando se aplica a un factor, muestra la frecuencia de cada nivel.

# Material aprendido práctica 2

Esta función resuelve cualquier tipo de ecuación cuadrática, incluso aquellas con soluciones en números complejos. Utiliza la función polyroot para calcular las soluciones.

```r
ec_2grado <- function(a, b, c) {
  discriminante <- b^2 - 4 * a * c
  
  if (discriminante >= 0) {
    sol_real_1 <- (-b + sqrt(discriminante)) / (2 * a)
    sol_real_2 <- (-b - sqrt(discriminante)) / (2 * a)
    soluciones <- c(sol_real_1, sol_real_2)
  } else {
    sol_complex_1 <- (-b + sqrt(as.complex(discriminante))) / (2 * a)
    sol_complex_2 <- (-b - sqrt(as.complex(discriminante))) / (2 * a)
    soluciones <- c(sol_complex_1, sol_complex_2)
  }
  
  return(soluciones)
}

```

# Material aprendido práctica 3

## Instalación y carga de librerías

```r
install.packages("tidyverse")
library(tidyverse)
install.packages("gapminder")
library(gapminder)
```

## Exploración de datos
Se utiliza help para explorar la estructura de los datos con glimpse como alternativa a str.

```r
?gapminder
str(gapminder)

```

## Filtrado de Datos
Se filtran los datos para el año 1952 y se almacenan en gm_1952.

```r
gm_1952 <- gapminder %>% filter(year == 1952)

```
Se filtran los datos para China en el año 2002 y se almacenan en china_2002.
```r
china_2002 <- gapminder %>% filter(country == "China" & year == 2002)

```

## Ordenamiento y Localización de Datos
Se ordenan los datos por esperanza de vida en ambas direcciones y se localizan los datos del año más reciente.

```r
gapminder_asc <- gapminder %>% arrange(lifeExp)
gapminder_desc <- gapminder %>% arrange(desc(lifeExp))

ultimo_anio <- max(gapminder$year, na.rm = TRUE)
datos_anio_mas_reciente <- gapminder %>% filter(year == ultimo_anio)

```

## Creación de Columna de Población en Millones
Se crea una nueva columna popM que representa la población en millones.

```r
gapminder <- gapminder %>% mutate(popM = pop / 1e6)

```

##  Extracción de los 10 Países con Mayor Población en 2007
Se extraen los 10 países con mayor población en 2007 y su población en 1957.

```r
top_10_paises_2007 <- gapminder %>% filter(year == 2007) %>% arrange(desc(popM)) %>% head(10)
poblacion_1957_top_10 <- gapminder %>% filter(country %in% top_10_paises_2007$country, year == 1957) %>% select(country, year, popM)


```

##  Cálculo del Promedio de Esperanza de Vida por Continente
Se calcula el promedio de esperanza de vida por continente.

```r
promedio_vida_por_continente <- gapminder %>% group_by(continent) %>% summarize(Promedio_Vida = mean(lifeExp, na.rm = TRUE))

```

## Adición de Columna de Logaritmo del PIB per Cápita y Ordenamiento
Se agrega una columna de logaritmo del PIB per cápita y se ordenan los datos por esta columna.

```r
gapminder <- gapminder %>% mutate(gdpPercapLog = log(gdpPercap))
gapminder <- gapminder %>% arrange(desc(gdpPercapLog))

```

## Filtrado de Países con Esperanza de Vida Mayor a 80 años
Se filtran los países con esperanza de vida mayor a 80 años.

```r
paises_esperanza_alta <- gapminder %>% filter(lifeExp > 80)


```

## Cálculo de la Suma del PIB por País y Año

```r
suma_PIB <- gapminder %>% mutate(PIB = gdpPercap * pop) %>% group_by(country, year) %>% summarize(Suma_PIB = sum(PIB, na.rm = TRUE))

```

## Creación de Columna de Clasificación de PIB per Cápita

```r
gapminder <- gapminder %>% mutate(Clasificacion = ifelse(gdpPercap > 10000, 'Alto PIB per cápita', 'Bajo PIB per cápita'))

```

## Aplicación del Logaritmo Natural a Columnas Numéricas

```r
gapminder <- gapminder %>% mutate_if(is.numeric, function(x) if(all(x > 0)) log(x) else x)

```

## País con Mayor Población por Continente en Cada Año

```r
pais_mayor_poblacion_por_anio <- gapminder %>% group_by(continent, year) %>% filter(pop == max(pop, na.rm = TRUE)) %>% ungroup() %>% arrange(year)


```

## Resumen de Datos por Continente

```r
resumen_por_continente <- gapminder %>% group_by(continent) %>% summarize(Min_LifeExp = min(lifeExp, na.rm = TRUE), Max_LifeExp = max(lifeExp

```

## Diagrama de dispersión

```r
# Cargar bibliotecas
library(ggplot2)

# Crear datos de ejemplo
set.seed(123)
data <- data.frame(x = rnorm(100), y = rnorm(100))

# Crear diagrama de dispersión
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  labs(title = "Diagrama de Dispersión", x = "Variable X", y = "Variable Y")

```


## Escalado Logarítmico

```r
# Crear datos de ejemplo (evitar valores negativos)
data_log <- data.frame(x = abs(rnorm(100)), y = abs(rnorm(100)))

# Aplicar escalado logarítmico (log10)
data_log$log_x <- log10(data_log$x)
data_log$log_y <- log10(data_log$y)

# Crear diagrama de dispersión con escalado logarítmico
ggplot(data_log, aes(x = log_x, y = log_y)) +
  geom_point(color = "green") +
  labs(title = "Diagrama de Dispersión con Escalado Logarítmico", 
       x = "log(X)", y = "log(Y)")

```

## Diagrama de Dispersión con Mapeo de Color y Tamaño
```r
# Crear datos de ejemplo con categorías y tamaños
set.seed(456)
data_map <- data.frame(x = rnorm(100), y = rnorm(100), 
                       category = sample(c("A", "B", "C"), 100, replace = TRUE),
                       size = runif(100, min = 1, max = 3))

# Crear diagrama de dispersión con mapeo de color y tamaño
ggplot(data_map, aes(x = x, y = y, color = category, size = size)) +
  geom_point(alpha = 0.7) +
  labs(title = "Diagrama de Dispersión con Mapeo de Color y Tamaño", 
       x = "Variable X", y = "Variable Y")

```