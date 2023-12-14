

#3.1 Sea la ecuación y  la solución a la misma, crea una función que reciba como parámetros a, b y c y devuelva 2 si tiene 2 soluciones reales.
# 1 si tiene una solución real, 0 si no tiene solución real o NA si a = 0.

ec_2grado_tipo <- function(a, b, c) {
  if (a == 0) {
    return("NA")
  }
  
  discriminante <- b^2 - 4 * a * c
  
  if (discriminante > 0) {
    return(2)  # Dos soluciones reales
  } else if (discriminante == 0) {
    return(1)  # Una solución real
  } else {
    return(0)  # No tiene soluciones reales
  }
}

# Ejemplos de uso:
resultado1 <- ec_2grado_tipo(a = 2, b = 5, c = 0)
resultado2 <- ec_2grado_tipo(a = 1, b = 2, c = 1)
resultado3 <- ec_2grado_tipo(a = 1, b = 2, c = 3)
resultado4 <- ec_2grado_tipo(a = 0, b = 2, c = 0)

print(resultado1)
print(resultado2)
print(resultado3)
print(resultado4)



#3.2 Escribe una función, que sirviéndose de la anterior, resuelva la ecuación en el campo de los números reales. 
# Considera la salida un vector de 2 elementos numéricos

ec_2grado_solr <- function(a, b, c) {
  tipo_solucion <- ec_2grado_tipo(a, b, c)
  if (tipo_solucion == "NA") {
    return("No es una ecuación de segundo grado")
  } else if (tipo_solucion == 0) {
    return("No tiene soluciones reales")
  } else {
    discriminante <- b^2 - 4 * a * c
    x1 <- (-b + sqrt(discriminante)) / (2 * a)
    x2 <- (-b - sqrt(discriminante)) / (2 * a)
    return(c(x1, x2))
  }
}

# Ejemplos de uso:
resultado1 <- ec_2grado_solr(a = 2, b = 5, c = 0)
resultado2 <- ec_2grado_solr(a = 1, b = 2, c = 1)
resultado3 <- ec_2grado_solr(a = 1, b = 2, c = 3)
resultado4 <- ec_2grado_solr(a = 0, b = 2, c = 0)

print(resultado1)
print(resultado2)
print(resultado3)
print(resultado4)



#3.3 Cuando no tiene solución real, considera utilizar los números complejos y escribe una función que resuelva cualquier tipo de ecuación, aunque tenga solución con números complejos.
# R dispone de herramientas para ello. Ayuda sqrt(as.complex(-1)) Considera la salida un vector de 2 elementos numéricos o complejos.


ec_2grado <- function(a, b, c) {
  # Calcula el discriminante
  discriminante <- b^2 - 4 * a * c
  
  # Calcula las soluciones utilizando polyroot
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

# Ejemplos de uso
solucion1 <- ec_2grado(2, 5, 0)
solucion2 <- ec_2grado(1, 2, 1)
solucion3 <- ec_2grado(1, 0, 1)
solucion4 <- ec_2grado(0, 2, 0)

print(solucion1)
print(solucion2)
print(solucion3)
print(solucion4)

