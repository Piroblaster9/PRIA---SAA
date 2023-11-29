#He puesto el código y el csv en la misma carpeta para que no haya problemas con la dirección
#Las imágenes van aparte, pero como también están dentro de la misma carpeta también debería irte bien

if (!require("ggradar")) {
  install.packages("ggradar")
}

if (!require("devtools")) {
  install.packages("devtools")
}

if (!requireNamespace("ggradar", quietly = TRUE)) {
  # El paquete del ggradar por sí solo me da error, así que he tenido que meterlo con github como puedes ver
  devtools::install_github("ricardo-bion/ggradar")
}

if (!require("readr")) {
  install.packages("readr")
}

library(ggradar)
library(readr)

datos_trump <- read.csv('TrumpCards.csv')

#Esto es la función para el radar
analisis_radar <- function(datos, personas) {
  datos_filtrados <- datos[datos$Nombre %in% personas, ]
  
  datos_radar <- datos_filtrados[, c("Poder", "Convivencia", "Liante", "Atractivo", "Locura")]
  
  datos_escalados <- as.data.frame(lapply(datos_radar, function(x) (x - min(x)) / (max(x) - min(x))))
  
  datos_escalados$Nombre <- datos_filtrados$Nombre
  
  datos_escalados <- datos_escalados[, c("Nombre", "Poder", "Convivencia", "Liante", "Atractivo", "Locura")]
  
  ggradar(datos_escalados, 
          axis.labels = c("Poder", "Convivencia", "Liante", "Atractivo", "Locura"),
          grid.min = 0, 
          grid.mid = 0.7, 
          grid.max = 1,
          grid.label.size = 6,
          legend.text.size = 10)
}

analisis_radar(datos_trump, c("Lola", "Berta", "Antonio Recio", "Amador", "Coque"))

library(dplyr)

#Aquí filtro la información
proceso_imagenes <- function(datos, personas, imagenes) {
  nombres_personajes <- personas
  rutas_imagenes <- imagenes
  
  datos_filtrados <- dplyr::filter(datos, Nombre %in% personas)
  
  datos_filtrados <- dplyr::mutate(datos_filtrados, Imagen = rutas_imagenes[match(Nombre, nombres_personajes)])
  
  return(datos_filtrados)
}

datos_filtrados <- proceso_imagenes(datos_trump, c("Lola", "Coque", "Berta", "Antonio Recio", "Amador"), c("imagenes/Lola.jpg", "imagenes/Coque.jpg", "imagenes/Berta.jpg", "imagenes/Antonio_Recio.jpg", "imagenes/Amador.jpg"))
print(datos_filtrados)

library(ggplot2)
library(ggimg)
library(dplyr)

#Aquí he tenido que poner el +1 o -1 porque si no las imágenes se solapan unas a las otras
#El problema es que se ven super estiradas, pero no he encontrado imágenes mejores de las propias cartas
ggplot(datos_filtrados) +
  geom_rect_img(aes(
    xmin = Poder - 1,
    xmax = Poder + 1,
    ymin = Convivencia - 1,
    ymax = Convivencia + 1,
    img = Imagen
  )) +
  theme_minimal()
