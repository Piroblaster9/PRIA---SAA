#1 Instala y carga la librería gapminder.

install.packages("tidyverse")
library(tidyverse)
install.packages("gapminder")
library(gapminder)

#1.2 Usa la función help para entender los datos. Explóralo con glimpse, como alternativa a str.
#Compara ambas salidas. ¿Cuál prefieres? Razona tu respuesta

?gapminder

str(gapminder)

# 2.1 Usa filter para filtrar por el año 1952 y guarda el resultado en la variable gm_1952.

gm_1952 <- gapminder %>%
  filter(year == 1952)
head(gm_1952)

# 2.2 Filtra por país (China) y por año (2002) y guarda el resultado en la variable china_2002

# 2.2 Filtra por país (China) y por año (2002) y guarda el resultado en la variable china_2002.
china_2002 <- gapminder %>%
  filter(country == "China" & year == 2002)
head(china_2002)

# 2.3 Ordena por lifExp en ambos sentidos. Localiza el año más reciente con datos y filtra por éste. ¿Qué países son los extremos?

# Ascendente
gapminder_asc <- gapminder %>%
  arrange(lifeExp)

# Descendente
gapminder_desc <- gapminder %>%
  arrange(desc(lifeExp))


ultimo_anio <- max(gapminder$year, na.rm = TRUE)

datos_anio_mas_reciente <- gapminder %>% 
  filter(year == ultimo_anio)

pais_con_menor_esperanza <- datos_anio_mas_reciente %>%
  filter(lifeExp == min(lifeExp))

pais_con_mayor_esperanza <- datos_anio_mas_reciente %>%
  filter(lifeExp == max(lifeExp))

print(paste("País con la esperanza de vida más baja:", pais_con_menor_esperanza$country))
print(paste("País con la esperanza de vida más alta:", pais_con_mayor_esperanza$country))


# 2.4 Crea una nueva columna popM, con la población en millones de habitantes.

gapminder <- gapminder %>%
  mutate(popM = pop / 1e6)

head(gapminder)

# 2.5 Extrae los 10 países con mayor población, expresados en millones de habitantes en el año 2007.
#Filtra por esos 10 países y extrae la población de los mismos en 1957.

top_10_paises_2007 <- gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(popM)) %>%
  head(10)

poblacion_1957_top_10 <- gapminder %>%
  filter(country %in% top_10_paises_2007$country, year == 1957) %>%
  select(country, year, popM)

print(top_10_paises_2007)
print(poblacion_1957_top_10)

# 2.6 Encuentra el promedio de esperanza de vida (lifeExp) para cada continente y resúmelo en una tabla.

promedio_vida_por_continente <- gapminder %>%
  group_by(continent) %>%
  summarize(Promedio_Vida = mean(lifeExp, na.rm = TRUE))

promedio_vida_por_continente

# 2.7 Añade una columna llamada gdpPercapLog que sea el logaritmo de gdpPercap, luego organiza los datos por esta nueva columna de forma descendente.

gapminder <- gapminder %>%
  mutate(gdpPercapLog = log(gdpPercap))

gapminder <- gapminder %>%
  arrange(desc(gdpPercapLog))

head(gapminder)

# 2.8 Filtra los países donde la esperanza de vida (lifeExp) es mayor que 80 años.
paises_esperanza_alta <- gapminder %>%
  filter(lifeExp > 80)
print(paises_esperanza_alta)

# 2.9 Calcula la suma del PIB (gdpPercap * pop) por país y año.
suma_PIB <- gapminder %>%
  mutate(PIB = gdpPercap * pop) %>%  # Calcula el PIB por fila
  group_by(country, year) %>%
  summarize(Suma_PIB = sum(PIB, na.rm = TRUE))

suma_PIB

# 2.10 Crea una nueva columna de clasificación 'Alto PIB per cápita' y 'Bajo PIB per cápita'.
gapminder <- gapminder %>%
  mutate(Clasificacion = ifelse(gdpPercap > 10000, 'Alto PIB per cápita', 'Bajo PIB per cápita'))

head(gapminder)

# 2.11 Utiliza mutate_if para aplicar el logaritmo natural a las columnas numéricas con entradas positivas.
gapminder <- gapminder %>%
  mutate_if(is.numeric, function(x) if(all(x > 0)) log(x) else x)

head(gapminder)

# 2.12 Encuentra el país con la mayor población por continente en cada año.
pais_mayor_poblacion_por_anio <- gapminder %>%
  group_by(continent, year) %>%
  filter(pop == max(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year)

pais_mayor_poblacion_por_anio


# 2.13 Obtén el mínimo, el máximo y la media de lifeExp y gdpPercap por continente.
resumen_por_continente <- gapminder %>%
  group_by(continent) %>%
  summarize(Min_LifeExp = min(lifeExp, na.rm = TRUE),
            Max_LifeExp = max(lifeExp, na.rm = TRUE),
            Media_LifeExp = mean(lifeExp, na.rm = TRUE),
            Min_GdpPercap = min(gdpPercap, na.rm = TRUE),
            Max_GdpPercap = max(gdpPercap, na.rm = TRUE),
            Media_GdpPercap = mean(gdpPercap, na.rm = TRUE))

resumen_por_continente


#3.1 Usando la variable gm_1952, crea un diagrama de dispersión donde se represente la renta per cápita en función de la población.
#Ahora la esperanza de vida en función de la población.


# Instala y carga la biblioteca gapminder si aún no está instalada/cargada

library(gapminder)

# Filtra los datos para un año específico (por ejemplo, 1952)
gm_1952 <- gapminder %>% filter(year == 1952)

# Cargar bibliotecas
library(ggplot2)
library(dplyr)

# Tu código para el diagrama de dispersión
gm_1952 %>%
  ggplot() +
  geom_point(aes(x = pop, y = gdpPercap), color = "blue", alpha = 0.7, size = 3) +
  geom_point(aes(x = pop, y = lifeExp), color = "green", alpha = 0.7, size = 3) +
  labs(title = "Diagrama de Dispersión",
       x = "Población",
       y = "Renta per cápita / Esperanza de vida") +
  scale_color_manual(values = c("blue", "green"))


#3.2 Aplica a las gráficas anteriores un escalado logarítmico (log10) en el eje x e y


gm_1952$log_pop <- log10(gm_1952$pop)
gm_1952$log_gdpPercap <- log10(gm_1952$gdpPercap)
gm_1952$log_lifeExp <- log10(gm_1952$lifeExp)


gm_1952 %>%
  ggplot() +
  geom_point(aes(x = log_pop, y = log_gdpPercap), color = "blue", alpha = 0.7, size = 3) +
  geom_point(aes(x = log_pop, y = log_lifeExp), color = "green", alpha = 0.7, size = 3) +
  labs(title = "Diagrama de Dispersión con Escalado Logarítmico",
       x = "log(Población)",
       y = "log(Renta per cápita / Esperanza de vida)") +
  scale_color_manual(values = c("blue", "green"))

#3.3 Realiza un diagrama de dispersión de la esperanza de vida en función de la población, pero mapea la variable continent por color.
#Usa escalado para el eje x

gm_1952 %>%
  ggplot(aes(x = pop, y = lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  labs(title = "Diagrama de Dispersión con Escalado Logarítmico y Mapeo de Color",
       x = "log(Población)",
       y = "Esperanza de vida")

#3.4 Repite la representación anterior pero añade el mapeo de la renta per cápita por tamaño del punto

gm_1952 %>%
  ggplot(aes(x = pop, y = lifeExp, size = gdpPercap, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  labs(title = "Diagrama de Dispersión con Escalado Logarítmico, Mapeo de Color y Tamaño",
       x = "log(Población)",
       y = "Esperanza de vida",
       size = "Renta per cápita")


#3.5 Ahora, en lugar de mapear por continente, utiliza contienente como variable “faceta” y genera 5 gráficas en un lienzo con la esperanza de vida en función de la población en 1952.

gm_1952 %>%
  ggplot(aes(x = pop, y = lifeExp, size = gdpPercap)) +
  geom_point() + 
  scale_x_log10() +
  facet_wrap(~ continent) +
  labs(title = "Diagramas de Dispersión por Continente",
       x = "log(Población)",
       y = "Esperanza de vida",
       size = "Renta per cápita")

#3.6 Representa la esperanza de vida en función del año, usando facetas (por continente). Utiliza la función de agregación apropiada.
#Prueba diagramas de dispersión, líneas ó barras.

gapminder %>%
  mutate(pop = pop/1000000) %>%
  group_by(year, continent) %>%
  summarise(lifeExp = median(lifeExp), pop = median(pop)) %>%
  ggplot(aes(x = year, y = lifeExp, size = pop)) +
  geom_point() +
  facet_wrap(~ continent) +
  labs(title = "Esperanza de Vida en Función del Año con Facetas",
       x = "Año",
       y = "Esperanza de Vida",
       size = "Población (millones)")

# 3.7 Representa la población total en función del año, usando facetas (por continente). Utiliza la función de agregación apropiada.

gapminder %>%
  mutate(pop = pop/1000000) %>%
  group_by(year, continent) %>%
  summarise(pop = sum(pop)) %>%
  ggplot(aes(x = year, y = pop)) +
  geom_line() +
  facet_wrap(~ continent, scales = "free") +
  labs(title = "Población Total en Función del Año con Facetas",
       x = "Año",
       y = "Población Total (millones)")

#3.8 Representa en una sola gráfica, sin el uso de facetas, la información de los dos ejercicios anteriores, pero utiliza colores para mapear los continentes.

gapminder %>%
  mutate(pop = pop/1000000) %>%
  group_by(year, continent) %>%
  summarise(lifeExp = median(lifeExp), pop = sum(pop)) %>%
  ggplot(aes(x = year, y = lifeExp, size = pop, color = continent)) +
  geom_point() +
  labs(title = "Información de Esperanza de Vida y Población Total",
       x = "Año",
       y = "Esperanza de Vida",
       size = "Población Total (millones)",
       color = "Continente")

#3.9 Representa la renta per cápita en 2007 mapeando los puntos del diagrama de dispersión por color según el contienente y por tamaño, según la población.

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = pop, y = gdpPercap, color = continent, size = pop)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Renta Per Cápita en 2007",
       x = "log(Población)",
       y = "log(Renta per cápita)",
       color = "Continente",
       size = "Población")

#3.10 Representa la evolución de la renta per cápita año a año entre Japón vs EEUU en una gráfica de líneas.

gapminder %>%
  filter(country %in% c("Japan", "United States")) %>%
  ggplot(aes(x = year, y = gdpPercap, color = country)) +
  geom_line() + 
  scale_x_continuous(breaks = seq(1950, 2000, by = 10)) +  # Ajusta las marcas del eje x
  scale_y_log10() +
  labs(title = "Evolución de la Renta Per Cápita: Japón vs EEUU",
       x = "Año",
       y = "log(Renta per cápita)",
       color = "País")

#3.11 Representa un diagrama de barras para comparar la renta per cápita en 2007 de europa vs EEUU+Canada.

gapminder %>%
  filter(year == 2007) %>%
  filter(country %in% c("Canada", "United States") | continent == "Europe") %>%
  mutate(continent = if_else(continent == "Europe", "Europa", "EEUU+Canada")) %>%
  mutate(PIB = gdpPercap * pop) %>%
  group_by(continent) %>%
  summarise(PIB = sum(PIB), pop = sum(pop)) %>%
  mutate(gdpPercap = PIB / pop) %>%
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_col() +
  labs(title = "Comparación de Renta Per Cápita en 2007",
       x = "Región",
       y = "Renta per cápita",
       fill = "Región")

#3.12 Dibuja un histograma con los datos de la población mundial en 2007 según gapminder.

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(pop)) +
  geom_histogram(bins = 39, color = "white", fill = "skyblue", alpha = 0.7) +
  labs(title = "Histograma de la Población Mundial en 2007",
       x = "Población",
       y = "Frecuencia")

#3.13 Modifica la gráfica usando escala logarítmica. Prueba realizar diagrama de densidad en lugar de un histograma.

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(pop)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Diagrama de Densidad de la Población Mundial en 2007",
       x = "log(Población)",
       y = "Densidad")

#3.15 Representa el histograma y diagrama de densidad de la esperanza de vida mundial por países en 2007. ¿Es una distribución simétrica?

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(lifeExp)) +
  geom_histogram(bins = 39, color = "white", fill = "skyblue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma y Diagrama de Densidad de la Esperanza de Vida en 2007",
       x = "Esperanza de Vida",
       y = "Frecuencia / Densidad")
