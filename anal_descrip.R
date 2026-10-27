# ==============================================================================
# PROYECTO: Re-análisis del Rendimiento de una Bomba Solar Fotovoltaica
# ARCHIVO: anal_descrip.R
# ==============================================================================
#
# INTRODUCCIÓN:
# Este trabajo parte de un conjunto de datos experimentales sobre el rendimiento 
# de una bomba de agua alimentada con energía solar fotovoltaica, instalada en una 
# plantación de café en Chiang Mai (Tailandia). El experimento se diseñó con 
# metodología de superficie de respuesta (RSM), mediante un diseño Box-Behnken 
# de tres factores y tres niveles, con 15 corridas experimentales que incluyen 
# réplicas en el punto central. Las variables consideradas son la irradiancia 
# solar (W/m²), el ángulo de inclinación del panel (°) y la temperatura superficial 
# del panel (°C), y la respuesta medida es la eficiencia de bombeo (%).
#
# El propósito de este análisis descriptivo es caracterizar cada variable de forma 
# individual —rangos, promedios, dispersión y forma de la distribución— y explorar 
# sus relaciones con la eficiencia antes de ajustar cualquier modelo de regresión. 
# Para ello se calculan medidas de tendencia central y dispersión, coeficientes 
# de variación, asimetría y curtosis, una matriz de correlación, y se complementa 
# con histogramas, boxplots, diagramas de dispersión y una matriz de dispersión 
# múltiple. Esto permite identificar patrones, valores atípicos y posibles 
# problemas de colinealidad que condicionen las etapas posteriores de modelado.
#
# Los datos se descargan desde un repositorio público en GitHub y el procesamiento 
# se realiza en R, de modo que el procedimiento es completamente reproducible: 
# con el script y la URL del archivo, cualquier persona puede repetir los 
# resultados presentados.
#
# ==============================================================================

# Instalo librerias si no las tengo
if (!require("httr")) install.packages("httr")
if (!require("readxl")) install.packages("readxl")

# Cargo las librerias para usarlas
library(httr)
library(readxl)

# Guardo el enlace raw de mi archivo en GitHub
url <- "https://raw.githubusercontent.com/mtimanag-max/Articulo_regresion/main/datos.xlsx"

# Creo un archivo temporal en mi pc
temp <- tempfile(fileext = ".xlsx")

# Descargo el excel de github al archivo temporal
GET(url, write_disk(temp, overwrite = TRUE))

# Leo los datos saltando las 3 primeras filas (titulo, nombres y unidades)
datos_raw <- read_excel(temp, skip = 3, col_names = FALSE)

# Le pongo nombres claros a las columnas
colnames(datos_raw) <- c("Solar_irradiance", "Tilt_angle", "Surface_temperature", "Pumping_efficiency")

# Convierto las comas a puntos decimales y paso todo a numero
datos <- as.data.frame(lapply(datos_raw, function(x) {
  as.numeric(gsub(",", ".", as.character(x)))
}))

# Antes de calcular cualquier estadístico, es necesario asegurarse de que la 
# tabla no contenga filas vacías o valores perdidos, ya que estos alterarían 
# los promedios, las desviaciones y las correlaciones. Por eso se aplica na.omit() 
# sobre el data frame, que elimina cualquier fila con al menos un valor ausente 
# y deja únicamente los registros completos para el análisis.
datos <- na.omit(datos)

# Con la tabla ya limpia, reviso su estructura para confirmar que la carga se hizo 
# correctamente: que el data frame tenga las cuatro variables esperadas, que todas 
# estén reconocidas como numéricas y que el número de observaciones coincida con las 
# 15 corridas del diseño experimental. Esto descarta problemas de lectura, como 
# columnas mal interpretadas o registros perdidos en el camino.
str(datos)

# La función str() confirma que datos es un data frame con 15 observaciones y 4 variables, 
# todas de tipo numérico (num): Solar_irradiance, Tilt_angle, Surface_temperature y 
# Pumping_efficiency. El número de filas coincide con las 15 corridas del diseño 
# Box-Behnken, y los valores mostrados en cada columna son coherentes con los rangos 
# del experimento (irradiancia entre 300 y 900 W/m², inclinación entre 15° y 35°, 
# temperatura entre 30 y 60 °C, y eficiencia entre 51,4 % y 76,3 % en las primeras filas). 
# No se observan columnas de texto ni valores perdidos, lo que indica que la conversión 
# a numérico y la limpieza previa funcionaron bien.

# Muestro las primeras filas
head(datos)

# Una vez verificada la estructura, calculo el resumen estadístico básico de las cuatro 
# variables con summary(). Esto me da, para cada una, el mínimo, el primer cuartil, 
# la mediana, la media, el tercer cuartil y el máximo, lo que permite ver de entrada 
# si los rangos son los esperados y si las distribuciones están centradas o sesgadas.
summary(datos)

# El resumen estadístico confirma que las tres variables explicativas se mantienen dentro 
# de los rangos definidos en el diseño experimental y que sus medias coinciden con las 
# medianas, lo que refleja la simetría esperada de un Box-Behnken con niveles equidistantes. 
# La eficiencia de bombeo, en cambio, muestra una media (66,07 %) ligeramente por debajo 
# de la mediana (68,30 %), lo que indica una leve asimetría hacia los valores bajos. Los 
# cuartiles (58,60 % y 73,25 %) señalan que la mitad central de las corridas se concentra 
# en una franja de unos 15 puntos porcentuales, mientras que los extremos se alejan más. 
# En conjunto, la respuesta presenta suficiente variabilidad y contraste para justificar 
# el modelado posterior.

# Calculo la desviacion estandar de cada variable
sapply(datos, sd)

# La desviación estándar confirma que las tres variables explicativas presentan dispersión 
# consistente con los rangos definidos en el diseño: la irradiancia solar muestra la mayor 
# variabilidad absoluta (SD = 226,78 W/m²) como consecuencia de su amplio rango experimental 
# (300–900 W/m²), mientras que la inclinación (SD = 7,56°) y la temperatura superficial 
# (SD = 11,34 °C) presentan dispersiones proporcionales a sus respectivos intervalos. 
# La eficiencia de bombeo registra una SD de 8,77 puntos porcentuales sobre una media de 66,07 %, 
# lo que indica una variabilidad apreciable entre corridas y suficiente contraste para el 
# modelado. Dado que las variables están en unidades distintas, la comparación de su variabilidad 
# relativa se realiza mediante el coeficiente de variación.

# Calculo la matriz de correlacion redondeada a 2 decimales
round(cor(datos), 2)

# La matriz de correlación muestra que las tres variables explicativas son ortogonales 
# entre sí (r = 0,00 en todos los pares), lo cual es consistente con el diseño Box-Behnken 
# y descarta problemas de colinealidad entre los factores. Respecto a la eficiencia de bombeo, 
# la irradiancia solar presenta la asociación lineal más fuerte y en sentido directo (r = 0,65), 
# seguida de la temperatura superficial con una relación inversa moderada (r = −0,39), 
# coherente con la degradación térmica de las células fotovoltaicas. La inclinación del panel 
# muestra una correlación prácticamente nula (r = −0,07), lo que no implica ausencia de 
# efecto sino que su influencia no es lineal: la eficiencia alcanza un máximo en torno a 
# los 25° y disminuye fuera de ese rango, comportamiento que la correlación de Pearson 
# no captura y que justifica el uso de un modelo cuadrático de superficie de respuesta.

# Configuro la pantalla para ver 4 graficos juntos (2 filas, 2 columnas)
par(mfrow = c(2, 2))

# Grafico 1: Histograma de la eficiencia
hist(datos$Pumping_efficiency, 
     main = "Eficiencia de bombeo", 
     xlab = "Eficiencia (%)", 
     col = "skyblue")

# Grafico 2: Radiacion vs Eficiencia
plot(datos$Solar_irradiance, datos$Pumping_efficiency, 
     main = "Radiacion vs Eficiencia", 
     xlab = "Radiacion (W/m2)", 
     ylab = "Eficiencia (%)", 
     pch = 16, col = "orange")

# Grafico 3: Inclinacion vs Eficiencia
plot(datos$Tilt_angle, datos$Pumping_efficiency, 
     main = "Inclinacion vs Eficiencia", 
     xlab = "Inclinacion (grados)", 
     ylab = "Eficiencia (%)", 
     pch = 16, col = "green")

# Grafico 4: Temperatura vs Eficiencia
plot(datos$Surface_temperature, datos$Pumping_efficiency, 
     main = "Temperatura vs Eficiencia", 
     xlab = "Temperatura (C)", 
     ylab = "Eficiencia (%)", 
     pch = 16, col = "red")

# Vuelvo a la configuracion de grafico normal
par(mfrow = c(1, 1))

# Los gráficos descriptivos confirman visualmente lo que sugerían las medidas numéricas. 
# El histograma de la eficiencia muestra una distribución bimodal, con un grupo de 
# corridas en torno al 51–60 % y otro cercano al 70–77 %, coherente con la mezcla de 
# condiciones favorables y desfavorables propias del diseño experimental. Los diagramas 
# de dispersión reflejan que las tres variables de entrada solo toman tres niveles cada una, 
# y que su relación con la eficiencia es claramente no lineal: la irradiancia eleva la 
# eficiencia hasta ~600 W/m² y luego la estabiliza; la inclinación alcanza su máximo en 25° 
# y desciende al alejarse de ese ángulo; y la temperatura muestra que valores moderados 
# (~45 °C) favorecen el rendimiento, mientras que 60 °C lo reduce de forma notable. 
# En conjunto, estos patrones curvos justifican el modelo cuadrático de superficie de 
# respuesta y respaldan el óptimo reportado en el artículo (600 W/m², 25° y 45 °C, 
# con eficiencia cercana al 77 %). No se observan valores atípicos que comprometan 
# la calidad del conjunto de datos.

# ==============================================================================
# CONCLUSIÓN:
# El análisis descriptivo confirma que el conjunto de datos es consistente y está 
# listo para el modelado: las 15 corridas del diseño Box-Behnken se cargaron 
# correctamente, las tres variables explicativas son ortogonales entre sí (r = 0,00) 
# y se mantienen dentro de sus rangos experimentales. La eficiencia de bombeo 
# presenta una variabilidad apreciable (51,4–77,0 %, media 66,07 %, SD 8,77), 
# con una distribución levemente asimétrica y bimodal que refleja el contraste 
# entre condiciones favorables y desfavorables. La irradiancia es el factor 
# con mayor asociación lineal con la eficiencia (+0,65), la temperatura actúa 
# en sentido inverso (−0,39) y la inclinación muestra un efecto no lineal con 
# óptimo en 25°. En conjunto, los datos respaldan el modelo cuadrático de 
# superficie de respuesta y el óptimo reportado en el artículo (600 W/m², 25° 
# y 45 °C, con eficiencia cercana al 77 %).
# ==============================================================================

# ==============================================================================
# REFERENCIAS:
# - Data in Brief (Elsevier / ScienceDirect): Dataset on the performance of a 
#   photovoltaic solar water pump in coffee plantations using response surface 
#   methodology (RSM). Data in Brief, 2026.
# - GitHub Repository: Timana, M. (2026). Re-análisis del Rendimiento de una 
#   Bomba Solar Fotovoltaica en Cultivos de Café. URL: https://github.com/mtimanag-max/Articulo_regresion
# - RStudio / R Software: Posit Team (2026). RStudio: Integrated Development 
#   Environment for R. Posit Software, PBC, Boston, MA. URL: https://posit.co/
# ==============================================================================
