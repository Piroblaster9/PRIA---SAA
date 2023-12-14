# Cargar librerías necesarias
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Función para descargar archivos
download_files <- function(season, league) {
  url <- sprintf("https://www.football-data.co.uk/mmz4281/2324/SP1.csv", season, league)
  destfile <- sprintf("%04d_%s.csv", season, league)
  download.file(url, destfile = destfile, mode = "wb")
}

# Temporadas y ligas de interés
temporadas <- 2000:2022
ligas <- c("SP1", "SP2", "E0", "E1", "D1", "D2", "I1", "I2")

# Crear todas las combinaciones de temporadas y ligas
combinaciones <- expand.grid(temporadas, ligas)

# Definir un punto de parada (por ejemplo, después de descargar 5 archivos)
punto_de_parada <- 5

# Aplicar la función de descarga a cada combinación
for (i in seq_len(nrow(combinaciones))) {
  download_files(combinaciones$Var1[i], combinaciones$Var2[i])
  
  # Mostrar mensaje de progreso
  cat("Descargado:", i, "de", nrow(combinaciones), "\n")
  
  # Verificar el punto de parada
  if (i >= punto_de_parada) {
    cat("Descarga detenida en el punto de parada.\n")
    break
  }
}
