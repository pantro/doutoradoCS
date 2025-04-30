if (!require(leaflet)) install.packages("leaflet")
if (!require(dplyr)) install.packages("dplyr")
if (!require(sf)) install.packages("sf")
if (!require(rstudioapi)) install.packages("rstudioapi")

# Cargar el paquete
library(leaflet)
library(dplyr)
library(sf)
library(rstudioapi)

current_path <- rstudioapi::getActiveDocumentContext()$path

# Establecer el directorio de trabajo como el directorio del archivo actual
if (!is.null(current_path) && current_path != "") {
  setwd(dirname(current_path))
  print(paste("El directorio de trabajo ahora es:", getwd()))
} else {
  print("No se pudo determinar la ruta del archivo actual.")
}

# Leer las cuadras de zamacola cluster 24
data <- read.csv("./dataset/cluster24_polygons_9.19.2024.csv")

# Filtrar filas no vacías en columnas long y lat
data_clean <- data %>%
  filter(!is.na(long) & !is.na(lat))

# Agrupar por `BlockID` y crear polígonos con centroide
data_poligonos <- data_clean %>%
  group_by(BlockID) %>%
  summarise(
    long = list(long),
    lat = list(lat),
    numero_random = sample(1:12, 1)  # Genera un número aleatorio para cada polígono
  )

# Convertir cada grupo de coordenadas en un polígono y calcular su centroide
data_poligonos <- data_poligonos %>%
  rowwise() %>%
  mutate(
    poligono = list(st_polygon(list(cbind(unlist(long), unlist(lat))))),
    centroide = st_centroid(st_sfc(poligono))  # Calcular el centroide de cada polígono
  )

# Definir colores y proporciones
colores <- c("green", "yellow", "orange", "red")
proporciones <- c(0.6, 0.2, 0.15, 0.05)  # Proporciones para verde, amarillo, anaranjado, rojo

# Calcular el número de polígonos para cada color
total_poligonos <- nrow(data_poligonos)
num_colores <- floor(total_poligonos * proporciones)  # Número de polígonos por color

# Crear un vector vacío para asignar colores
data_poligonos$color <- NA

# Asignar colores según proporciones
for (i in seq_along(colores)) {
  indices <- sample(which(is.na(data_poligonos$color)), num_colores[i])
  data_poligonos$color[indices] <- colores[i]
}

# Color para los lugares de vacunación
vacunacion_color <- "purple"
# Calcular el 10% de los polígonos para vacunación
numero_vacunacion <- ceiling(total_poligonos * 0.1)

# Verificar cuántos polígonos no tienen color asignado
poligonos_disponibles <- sum(is.na(data_poligonos$color))

# Asegurarse de que no se intenta tomar una muestra más grande de lo disponible
numero_vacunacion <- min(numero_vacunacion, poligonos_disponibles)

# Seleccionar aleatoriamente el número correcto de polígonos para vacunación
if (numero_vacunacion > 0) {
  indices_vacunacion <- sample(which(is.na(data_poligonos$color)), numero_vacunacion)
  
  # Asignar el color de vacunación
  data_poligonos$color[indices_vacunacion] <- vacunacion_color
}

# Crear el mapa leaflet con polígonos y etiquetas
mapa <- leaflet() %>%
  addTiles()

# Agregar cada polígono y su número aleatorio fijo en el centro
for (i in 1:nrow(data_poligonos)) {
  mapa <- mapa %>%
    addPolygons(
      lng = unlist(data_poligonos$long[i]),
      lat = unlist(data_poligonos$lat[i]),
      color = data_poligonos$color[i],  # Color asignado
      fillColor = data_poligonos$color[i],  # Color de relleno
      fillOpacity = 0.5
    ) %>%
    addLabelOnlyMarkers(
      lng = st_coordinates(data_poligonos$centroide[i])[1],
      lat = st_coordinates(data_poligonos$centroide[i])[2],
      label = as.character(data_poligonos$numero_random[i]),
      labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)
    )
}

# Añadir leyenda
mapa <- mapa %>%
  addLegend(
    position = "bottomright",
    colors = c("green", "yellow", "orange", "red", "purple"),
    labels = c("Risco Baixo", "Risco Moderado", "Risco Alto", "Risco Muito Alto", "Vacinação"),
    title = "Legenda"
  )

# Mostrar el mapa
mapa

