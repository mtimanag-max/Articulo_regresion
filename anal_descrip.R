# ==============================================================================
# PROYECTO: Re-análisis del Rendimiento de una Bomba Solar Fotovoltaica
# ARCHIVO: anal_descrip.R
# OBJETIVO: Lectura directa desde GitHub y Análisis Descriptivo Completo
# ==============================================================================

# 1. CARGA Y/O INSTALACIÓN DE LIBRERÍAS
if (!require("httr")) install.packages("httr")
if (!require("readxl")) install.packages("readxl")
if (!require("psych")) install.packages("psych")

library(httr)
library(readxl)
library(psych)

# 2. LECTURA DIRECTA DE LA BASE DE DATOS DESDE GITHUB
url_raw <- "https://raw.githubusercontent.com/mtimanag-max/Articulo_regresion/main/datos.xlsx"
temp_file <- tempfile(fileext = ".xlsx")

# Descarga del archivo temporal
GET(url = url_raw, write_disk(temp_file, overwrite = TRUE))

# Carga de la hoja omitiendo filas de títulos del artículo
datos_raw <- read_excel(temp_file, skip = 2, col_names = FALSE)

# Asignación de nombres limpios a las columnas
colnames(datos_raw) <- c("Solar_irradiance", "Tilt_angle", "Surface_temperature", "Pumping_efficiency")

# Limpieza y conversión de tipos de datos (convertir comas decimales a puntos)
datos <- as.data.frame(lapply(datos_raw, function(x) {
  if (is.character(x)) {
    as.numeric(gsub(",", ".", x))
  } else {
    as.numeric(x)
  }
}))

# 3. INSPECCIÓN GENERAL DE LA BASE DE DATOS
cat("=========================================================\n")
cat("          ESTRUCTURA Y MUESTRA DE LA BASE DE DATOS        \n")
cat("=========================================================\n")
str(datos)
cat("\nPrimeras 6 filas:\n")
print(head(datos))

# 4. RESUMEN ESTADÍSTICO DESCRIPTIVO
cat("\n=========================================================\n")
cat("             RESUMEN ESTADÍSTICO GENERAL                \n")
cat("=========================================================\n")
print(summary(datos))

cat("\n=========================================================\n")
cat("     ESTADÍSTICOS DETALLADOS (Psych Package)            \n")
cat("=========================================================\n")
resumen_detallado <- describe(datos)[, c("n", "mean", "sd", "median", "min", "max", "skew", "kurtosis")]
print(round(resumen_detallado, 3))

# 5. MATRIZ DE CORRELACIÓN DE PEARSON
cat("\n=========================================================\n")
cat("            MATRIZ DE CORRELACIÓN DE PEARSON             \n")
cat("=========================================================\n")
matriz_cor <- cor(datos)
print(round(matriz_cor, 3))

# 6. VISUALIZACIÓN GRÁFICA DESCRIPTIVA
par(mfrow = c(2, 2))

# Histograma de la Variable Respuesta
hist(datos$Pumping_efficiency, 
     main = "Eficiencia de Bombeo (%)", 
     xlab = "Eficiencia (%)", 
     col = "lightblue", 
     border = "black")

# Radiación vs Eficiencia
plot(datos$Solar_irradiance, datos$Pumping_efficiency, 
     main = "Radiación vs Eficiencia", 
     xlab = "Radiación Solar (W/m²)", 
     ylab = "Eficiencia (%)", 
     pch = 19, col = "darkorange")

# Inclinación vs Eficiencia
plot(datos$Tilt_angle, datos$Pumping_efficiency, 
     main = "Inclinación vs Eficiencia", 
     xlab = "Inclinación (°)", 
     ylab = "Eficiencia (%)", 
     pch = 19, col = "forestgreen")

# Temperatura vs Eficiencia
plot(datos$Surface_temperature, datos$Pumping_efficiency, 
     main = "Temperatura vs Eficiencia", 
     xlab = "Temperatura (°C)", 
     ylab = "Eficiencia (%)", 
     pch = 19, col = "firebrick")

par(mfrow = c(1, 1))
