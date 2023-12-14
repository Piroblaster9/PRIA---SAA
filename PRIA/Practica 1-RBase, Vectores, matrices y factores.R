#1. Vectores
##1.1 Genera un vector con los números del 1 al 50 con 1:50 y guárdalo en una variable v1_50. Repítelo usando seq().

v1_50 <- 1:50
  print(v1_50)

# con seq()
v1_50 <- seq(1,50)
  print(v1_50)

##1.2 Lo mismo que el anterior pero con incrementos de 0.5 {0.5,1,1.5, … 10} en la variable v1_50h (usa “by”).

v1_50h <- seq(1,50, by=0.5)
  print(v1_50h)

##1.3 Calcula el tamaño del vector anterior y guárdalo en tam_v1_50h.

  
tam_v1_50h <- length(v1_50h)
  print(tam_v1_50h)

##1.4 Describe el tipo de dato que devuelve la función length aplicada sobre tam_v1_50h

tipo_length <- tam_v1_50h
  print(class(tipo_length))
# Diferencia entre typeof y class. En una Matriz, typeof() devuelve el tipo de los elementos,
# class() el tipo o clase de objeto (vector o matriz), en ese caso, usamos class

##1.5 Genera un vector de 10 elementos comprendidos entre 1 y 22 con incrementos iguales.

v1_22_10 <- seq(from = 1, to = 22, length.out = 10)
  print(v1_22_10)

##1.6 vrep_1x4 <- Genera un vector de unos de tamaño 4 usando rep().

vrep_1x4 <- rep(1,4)
  print(vrep_1x4)

##1.7 vrep <- Genera un vector que cuente 3 veces del 1 al 5 usando rep() y sin usar seq().

vrep <- rep(1:5,3)
  print(vrep)

##1.8 Crea un vector con los elementos del 1º a 4º y de 8º a 9º de v1_22_10.

sub_v1_22_10 <- c(v1_22_10[1:4], v1_22_10[8:9])
  print(sub_v1_22_10)

#2. Matrices
##2.1 Construye una matriz de 3 filas que contenga los números del 1 al 9 ordenados por filas. 


  matrix(1:9, nrow = 3 ,byrow=3)

##2.2 Construye una matriz de 3 filas que contenga los números del 1 al 9 ordenados por columnas. 


  matrix(1:9, nrow = 3)

##2.3 Los siguientes vectores representan la recaudación en euros, el número de semanas en cartelera y la recaudación total de ciertas películas en los cines españoles durante. Fuente. PDF

the_creator <- c(702465, 1, 702465)
barbie <- c(61351, 11, 33935231)
campeonesx <- c(235394, 7, 1732961)

##Crea una matriz de 9 elemenos concatenando los 3 vectores cuyas filas correspondan a cada película y las columnas tengan el mismo significado que los respectivos vectores. Guarda la matriz en la variable cine_29_01_oct

cine_29_01_oct <- matrix(c(the_creator, barbie, campeonesx), nrow=3, byrow=TRUE)
print(cine_29_01_oct)

##2.4 Usa las funciones typeof y class, pasándole como argumento la matriz. Vuelve a usarlas pero pasándole cine_29_01_oct[1,1]

typeof(cine_29_01_oct)
class(cine_29_01_oct)

typeof(cine_29_01_oct[1,1])
class(cine_29_01_oct[1,1])

##2.5 Teniendo en cuenta los datos de los vectores del ejercicio 2.3:
  
##Crea un vector con el significado de las columnas (rec_semana, semanas_cart, rec_total). Crea un vector con los nombres de las tres películas. Usa los vectores creados y a continuación utilizalos para nombrar las columnas y las filas de la matriz. Busca las dos funciones en R que permiten poner nombre a filas y columnas. Imprime ahora la matriz cine_29_01_oct con los nombres de filas y columnas asignados.

columnas<- c("Recaudación en €","Semanas en cartelera","Recaudación Total")
filas <- c("the_creator","barbie","campeonesx")

# Nombra las filas y columnas de la matriz

rownames(cine_29_01_oct) <- filas
colnames(cine_29_01_oct) <- columnas
print(cine_29_01_oct)

##2.6 Agrega una nueva columna a la matriz cine_29_01_oct, con el % de recaudación de la semana actual sobre el total.

# Código para calcular y agregar la nueva columna

# Calcula el porcentaje de recaudación de la semana actual sobre el total
cine_29_01_oct <- cbind(cine_29_01_oct, cine_29_01_oct[, 1] / cine_29_01_oct[, 3] * 100)

# Nombra la nueva columna
colnames(cine_29_01_oct)[4] <- "% Recaudación Semana"

# Imprime la matriz actualizada
print(cine_29_01_oct)


##2.7 Localiza en la fuente de datos dos películas que estén en cartelera 2 semanas. Repite los pasos anteriores. ¿Cómo varía la recaudación de segunda semana respecto a la primera?
  
  # Código para crear la nueva matriz
cerrar_los_ojos <- c(67529, 2, 246146 )
ternura_La <- c(18246, 2, 167142)
mision_a_marte <- c(87, 2, 723)
cine_29_01_oct2 <- matrix(c(cerrar_los_ojos, ternura_La, mision_a_marte), nrow=3, byrow=TRUE)

columnas<- c("Recaudación en €","Semanas en cartelera","Recaudación Total")
filas <- c("cerrar_los_ojos","ternura_La", "mision_a_marte")

rownames(cine_29_01_oct2) <- filas
colnames(cine_29_01_oct2) <- columnas
print(cine_29_01_oct2)

  # Calculamos lo que se ha ganado en la primera semana

# Crear una nueva columna llamada "Diferencia"
cine_29_01_oct2$Diferencia <- cine_29_01_oct2[, "Recaudación Total"] - cine_29_01_oct2[, "Recaudación en €"]

# Imprimir la matriz actualizada
print(cine_29_01_oct2)


##2.8 Une las primera matriz con la segunda por filas

cine_29_01_oct_combined <- rbind(cine_29_01_oct, cine_29_01_oct2)
print(cine_29_01_oct_combined)


  #3. Factores
  ##3.1 Tenemos el vector c(“Elefante”, “Girafa”, “Asno”, “Caballo”), obtén a partir de éste el vector de variables cualitativas

animals_vector <- c("Elefante", "Girafa", "Asno", "Caballo")
factor_animals_vector <- factor(animals_vector)
print(factor_animals_vector)


##3.2 A partir del vector c(“High”, “Low”, “High”,“Low”, “Medium”), utiliza los niveles c(“Low”, “Medium”, “High”), aplica orden y guárdalo en el vector factor_temperature_vector

temperature_vector <- c("High", "Low", "High", "Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, levels = c("Low", "Medium", "High"))
print(factor_temperature_vector)


##[1] High   Low    High   Low    Medium
##Levels: Low Medium High
##3.3 Aplica la función summary() a temperature_vector y factor_temperature_vector ¿Podrías explicar los resultados?

# Veo que summary() proporciona información sobre la estructura de los objetos que estás resumiendo. 
# En el caso del vector original, muestra su longitud, clase y modo. En el caso del vector de variables cualitativas, muestra la frecuencia de cada nivel.