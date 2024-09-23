options(repos = c(CRAN = "https://cran.rstudio.com/"))
# Instalar y cargar el paquete readxl si es necesario
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)
# Leer el archivo de Excel sin mostrar la salida en la presentación
base_de_datos_1 <- read_excel("musica.xlsx")
#View(base_de_datos_1)

## libreria para curtosis y asimetria
if (!requireNamespace("moments", quietly = TRUE)) {
  install.packages("moments")
}
library(moments)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}
library(gridExtra)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(dplyr)

if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}
library(knitr)


if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales")
}
library(scales) # Para formatear los números con símbolo de dólar y separador de miles

estadisticas_vivienda <- base_de_datos_1 %>%
  group_by(VIVIENDA) %>%
  summarise(
    Asimetría = skewness(SALARIO),
    Media = mean(SALARIO),
    Mediana = median(SALARIO),
    Curtosis = kurtosis(SALARIO),
    Máximo = max(SALARIO),
    Mínimo = min(SALARIO)
  )

# Crear gráficos de barras para cada tipo de vivienda con anotaciones
grafico_compuesto <- ggplot(base_de_datos_1, aes(x = SALARIO)) +
  geom_histogram(aes(y = ..density..), binwidth = 50000, fill = "grey", color = "black", alpha = 0.7) + # Renderizar barras del histograma
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mediana, color = "Mediana"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Máximo, color = "Máximo"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mínimo, color = "Mínimo"), linetype = "dashed", size = 1) +
  facet_wrap(~ VIVIENDA, ncol = 1, scales = "free_x") + # Escalas libres para permitir diferentes rangos
  labs(title = "Distribución de Salario Segmentado por Tipo de Vivienda",
       x = "Salario",
       y = "Densidad") +
  theme_minimal() +
  scale_color_manual(name = "Estadístico",
                     values = c("Media" = "red", "Mediana" = "blue", "Máximo" = "green", "Mínimo" = "purple")) +
  theme(legend.position = "right") +  # Mantener la leyenda en la derecha
  expand_limits(x = max(base_de_datos_1$SALARIO) * 1.2) +  # Expandir límites para dar espacio a las anotaciones
  # Colocar anotaciones en una posición fija al lado derecho, pero cerca del área de las barras
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.1, y = 0.9, label = paste("Media:", dollar(Media))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE) +
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.1, y = 0.8, label = paste("Mediana:", dollar(Mediana))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE) +
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.1, y = 0.7, label = paste("Máximo:", dollar(Máximo))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE) +
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.1, y = 0.6, label = paste("Mínimo:", dollar(Mínimo))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE)

# Mostrar el gráfico
print(grafico_compuesto)


estadisticas_vivienda <- base_de_datos_1 %>%
  group_by(VIVIENDA) %>%
  summarise(
    Asimetría = skewness(SALARIO),
    Media = mean(SALARIO),
    Mediana = median(SALARIO),
    Curtosis = kurtosis(SALARIO),
    Máximo = max(SALARIO),
    Mínimo = min(SALARIO)
  )
# Crear gráficos de barras para cada tipo de vivienda con anotaciones
grafico_compuesto <- ggplot(base_de_datos_1, aes(x = SALARIO)) +
  geom_histogram(aes(y = ..density..), binwidth = 100000, fill = "grey", color = "black", alpha = 0.7) + # Renderizar barras del histograma
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mediana, color = "Mediana"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Máximo, color = "Máximo"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mínimo, color = "Mínimo"), linetype = "dashed", size = 1) +
  facet_wrap(~ VIVIENDA, ncol = 1, scales = "free_x") + # Escalas libres para permitir diferentes rangos
  labs(title = "Distribución de Salario Segmentado por Tipo de Vivienda",
       x = "Salario",
       y = "Densidad") +
  theme_minimal() +
  scale_color_manual(name = "Estadístico",
                     values = c("Media" = "red", "Mediana" = "blue", "Máximo" = "green", "Mínimo" = "purple")) +
  theme(legend.position = "right") +  # Mantener la leyenda en la derecha
  expand_limits(x = max(base_de_datos_1$SALARIO) * 1.5) +  # Expandir límites para dar espacio a las anotaciones
  # Colocar anotaciones en una posición fija al lado derecho, pero cerca del área de las barras
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.1, y = 0.9, label = paste("Media:", dollar(Media))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE) +
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.1, y = 0.8, label = paste("Mediana:", dollar(Mediana))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE) +
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.1, y = 0.7, label = paste("Máximo:", dollar(Máximo))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE) +
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.1, y = 0.6, label = paste("Mínimo:", dollar(Mínimo))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE)

# Mostrar el gráfico
print(grafico_compuesto)


# Crear gráficos de barras para cada tipo de vivienda con anotaciones
grafico_compuesto <- ggplot(base_de_datos_1, aes(x = SALARIO)) +
  geom_histogram(aes(y = ..density..), binwidth = 100000, fill = "grey", color = "black", alpha = 0.7) + # Asegurar que las barras se rendericen
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mediana, color = "Mediana"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Máximo, color = "Máximo"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mínimo, color = "Mínimo"), linetype = "dashed", size = 1) +
  facet_wrap(~ VIVIENDA, ncol = 1, scales = "free_x") + # Escalas libres para cada faceta
  labs(title = "Distribución de Salario Segmentado por Tipo de Vivienda",
       x = "Salario",
       y = "Densidad") +
  theme_minimal() +
  scale_color_manual(name = "Estadístico",
                     values = c("Media" = "red", "Mediana" = "blue", "Máximo" = "green", "Mínimo" = "purple")) +
  theme(legend.position = "right") +  # Mantener la leyenda a la derecha
  expand_limits(x = max(base_de_datos_1$SALARIO) * 1.1) +  # Espacio adicional a la derecha
  # Usar geom_label para anotaciones fuera de las barras
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.95, y = 0.15, label = paste("Media:", dollar(Media))),
             color = "grey20", fill = "white", hjust = 0, size = 3) +
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.95, y = 0.1, label = paste("Mediana:", dollar(Mediana))),
             color = "grey20", fill = "white", hjust = 0, size = 3) +
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.95, y = 0.05, label = paste("Máximo:", dollar(Máximo))),
             color = "grey20", fill = "white", hjust = 0, size = 3) +
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.95, y = 0, label = paste("Mínimo:", dollar(Mínimo))),
             color = "grey20", fill = "white", hjust = 0, size = 3)

# Mostrar el gráfico
print(grafico_compuesto)

estadisticas_vivienda <- base_de_datos_1 %>%
  group_by(VIVIENDA) %>%
  summarise(
    Asimetría = skewness(SALARIO),
    Media = mean(SALARIO),
    Mediana = median(SALARIO),
    Curtosis = kurtosis(SALARIO),
    Máximo = max(SALARIO),
    Mínimo = min(SALARIO)
  )
# Crear gráficos de barras para cada tipo de vivienda con anotaciones
grafico_compuesto <-  ggplot(base_de_datos_1, aes(x = SALARIO)) +
  geom_histogram(aes(y = ..density..), binwidth = 100000, fill = "grey", color = "black", alpha = 0.7) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mediana, color = "Mediana"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Máximo, color = "Máximo"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mínimo, color = "Mínimo"), linetype = "dashed", size = 1) +
  facet_wrap(~ VIVIENDA, ncol = 1) +
  labs(title = "Distribución de Salario Segmentado por Tipo de Vivienda",
       x = "Salario",
       y = "Densidad") +
  theme_minimal() +
  scale_color_manual(name = "Estadístico",
                     values = c("Media" = "red", "Mediana" = "blue", "Máximo" = "green", "Mínimo" = "purple")) +
  theme(legend.position = "right") +  # Mantener la leyenda a la derecha
  expand_limits(x = max(base_de_datos_1$SALARIO) * 1.1) +  # Espacio adicional a la derecha
  # Usar geom_label para anotaciones fuera de las barras
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.95, y = 0.15, label = paste("Media:", dollar(Media))),
             color = "grey20", fill = "white", hjust = 0, size = 3) +
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.95, y = 0.1, label = paste("Mediana:", dollar(Mediana))),
             color = "grey20", fill = "white", hjust = 0, size = 3) +
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.95, y = 0.05, label = paste("Máximo:", dollar(Máximo))),
             color = "grey20", fill = "white", hjust = 0, size = 3) +
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.95, y = 0, label = paste("Mínimo:", dollar(Mínimo))),
             color = "grey20", fill = "white", hjust = 0, size = 3)
print(grafico_compuesto)


grafico_compuesto <- ggplot(base_de_datos_1, aes(x = SALARIO)) +
  stat_bin(aes(y = ..count..), binwidth = 100000, fill = "grey", color = "black", alpha = 0.7, position = "identity") + # Usar stat_bin para contar ocurrencias
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mediana, color = "Mediana"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Máximo, color = "Máximo"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mínimo, color = "Mínimo"), linetype = "dashed", size = 1) +
  facet_wrap(~ VIVIENDA, ncol = 1, scales = "free_x") + # Escalas libres para cada faceta
  labs(title = "Distribución de Salario Segmentado por Tipo de Vivienda",
       x = "Salario",
       y = "Frecuencia") +
  theme_minimal() +
  scale_color_manual(name = "Estadístico",
                     values = c("Media" = "red", "Mediana" = "blue", "Máximo" = "green", "Mínimo" = "purple")) +
  theme(legend.position = "right") +  # Mantener la leyenda a la derecha
  expand_limits(x = max(base_de_datos_1$SALARIO) * 1.2) +  # Espacio adicional a la derecha
  # Usar geom_text para anotaciones fuera del área de las barras
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.9, y = 10, label = paste("Media:", dollar(Media))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE) +
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.9, y = 8, label = paste("Mediana:", dollar(Mediana))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE) +
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.9, y = 6, label = paste("Máximo:", dollar(Máximo))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE) +
  geom_text(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 0.9, y = 4, label = paste("Mínimo:", dollar(Mínimo))),
            color = "grey30", hjust = 0, size = 3, check_overlap = TRUE)
print(grafico_compuesto)


estadisticas_vivienda <- base_de_datos_1 %>%
  group_by(VIVIENDA) %>%
  summarise(
    Asimetría = skewness(SALARIO),
    Media = mean(SALARIO),
    Mediana = median(SALARIO),
    Curtosis = kurtosis(SALARIO),
    Maximo = max(SALARIO),
    Minimo = min(SALARIO)
  )
# Crear gráficos de barras para cada tipo de vivienda con anotaciones

grafico_compuesto <- ggplot(base_de_datos_1, aes(x = SALARIO)) +
  stat_bin(aes(y = ..count..), binwidth = 100000, fill = "grey", color = "black", alpha = 0.7, position = "identity") + # Usar stat_bin para contar ocurrencias
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Mediana, color = "Mediana"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Maximo, color = "Máximo"), linetype = "dashed", size = 1) +
  geom_vline(data = estadisticas_vivienda, aes(xintercept = Minimo, color = "Mínimo"), linetype = "dashed", size = 1) +
  facet_wrap(~ VIVIENDA, ncol = 1, scales = "free_x") + # Escalas libres para cada faceta
  labs(title = "Distribución de Salario Segmentado por Tipo de Vivienda",
       x = "Salario",
       y = "Frecuencia") +
  theme_minimal() +
  scale_color_manual(name = "Estadístico",
                     values = c("Media" = "red", "Mediana" = "blue", "Máximo" = "green", "Mínimo" = "purple")) +
  theme(legend.position = "right") +  # Mantener la leyenda a la derecha
  expand_limits(x = max(base_de_datos_1$SALARIO) * 1.3) +
  scale_x_continuous(labels = comma) + # Formatear el eje x para evitar notación científica y mostrar separadores de miles
  # Espacio adicional a la derecha
  # Usar geom_text para anotaciones fuera del área de las barras
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.05, y = 11, label = paste("Media:", dollar(Media))),
             color = "grey10", fill = "white", hjust = 0, size = 3) +
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.05, y = 8.5, label = paste("Mediana:", dollar(Mediana))),
             color = "grey10", fill = "white", hjust = 0, size = 3) +
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.05, y = 6, label = paste("Máximo:", dollar(Maximo))),
             color = "grey10", fill = "white", hjust = 0, size = 3) +
  geom_label(data = estadisticas_vivienda, aes(x = max(base_de_datos_1$SALARIO) * 1.05, y = 3.5, label = paste("Media:", dollar(Minimo))),
             color = "grey10", fill = "white", hjust = 0, size = 3) 
print(grafico_compuesto)