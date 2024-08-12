## Instalar y usar readxl
install.packages("readxl")
library(readxl)
## libreria para curtosis y asimetria
install.packages("moments")
library(moments)

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")  
library(dplyr)


#Cargar Dataset
base_de_datos_1 <- read_excel("base de datos 1.xlsx")
View(base_de_datos_1)

## Asimetría:
# La medida de asimetría permite identificar si los datos se distribuyen
# de forma uniforme alrededor del punto central (media aritmética).
# La asimetría presenta tres tipos diferentes:
# - Curva de asimetría negativa
# - Curva simétrica
# - Curva de asimetría positiva

## Curtosis:
# La medida de curtosis determina el grado de concentración que presentan los 
# valores en la región central de la distribución. A través del coeficiente 
# de curtosis, se identifica la concentración de los valores en el eje central.
# La curtosis presenta tres tipos diferentes:
# Leptocúrtica (Curtosis > 0) (alta concentración de valores en el eje central)
# Mesocúrtica (Curtosis = 0) (concentración normal de valores en el eje central)
# Platicúrtica (Curtosis < 0) (baja concentración de valores en el eje central)

#Sin Segmentacion
# Calcular la asimetría de la variable "Hijos"
asimetria_hijos <- skewness(base_de_datos_1$Hijos)
curtosis_hijos <- kurtosis(base_de_datos_1$Hijos)
media_hijos <- mean(base_de_datos_1$Hijos)
mediana_hijos <- median(base_de_datos_1$Hijos)

ggplot(base_de_datos_1, aes(x = Hijos)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 1, 
                 fill = "grey", 
                 color = "black", 
                 alpha = 0.7) +
  geom_vline(aes(xintercept = media_hijos, color = "Media"), 
             linetype = "dashed", 
             size = 1) +
  geom_vline(aes(xintercept = mediana_hijos, color = "Mediana"), 
             linetype = "dashed", 
             size = 1) +
  scale_color_manual(name = "Estadístico", 
                     values = c(Media = "red", Mediana = "blue")) +
  labs(title = "Distribución de Hijos (con Media y Mediana)", 
       x = "Número de Hijos", 
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "right") +
  annotate("text", x = Inf, y = Inf, label = paste0("Asimetría: ", round(asimetria_hijos, 2)), 
           hjust = 1.1, vjust = 2, size = 4, color = "black") +
  annotate("text", x = Inf, y = Inf, label = paste0("Media: ", round(media_hijos, 2)), 
           hjust = 1.1, vjust = 4, size = 4, color = "red") +
  annotate("text", x = Inf, y = Inf, label = paste0("Mediana: ", round(mediana_hijos, 2)), 
           hjust = 1.1, vjust = 6, size = 4, color = "blue") +
  annotate("text", x = Inf, y = Inf, label = paste0("Curtosis: ", round(curtosis_hijos, 2)), 
           hjust = 1.1, vjust = 8, size = 4, color = "purple") 



# Calcular la asimetría de la variable "Escolaridad"
asimetria_escolaridad <- skewness(base_de_datos_1$`Escolaridad (años)`)
curtosis_escolaridad <- kurtosis(base_de_datos_1$`Escolaridad (años)`)
media_escolaridad <- mean(base_de_datos_1$`Escolaridad (años)`)
mediana_escolaridad <- median(base_de_datos_1$`Escolaridad (años)`)

ggplot(base_de_datos_1, aes(x = `Escolaridad (años)`)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 1, 
                 fill = "grey", 
                 color = "black", 
                 alpha = 0.7) +
  geom_vline(aes(xintercept = media_escolaridad, color = "Media"), 
             linetype = "dashed", 
             size = 1) +
  geom_vline(aes(xintercept = mediana_escolaridad, color = "Mediana"), 
             linetype = "dashed", 
             size = 1) +
  scale_color_manual(name = "Estadístico", 
                     values = c(Media = "red", Mediana = "blue")) +
  labs(title = "Distribución de Escolaridad (con Media y Mediana)", 
       x = "Número de Años de Escolaridad", 
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "right") +
  annotate("text", x = Inf, y = Inf, label = paste0("Asimetría: ", round(asimetria_escolaridad, 2)), 
           hjust = 1.1, vjust = 2, size = 4, color = "black") +
  annotate("text", x = Inf, y = Inf, label = paste0("Media: ", round(media_escolaridad, 2)), 
           hjust = 1.1, vjust = 4, size = 4, color = "red") +
  annotate("text", x = Inf, y = Inf, label = paste0("Mediana: ", round(mediana_escolaridad, 2)), 
           hjust = 1.1, vjust = 6, size = 4, color = "blue") +
  annotate("text", x = Inf, y = Inf, label = paste0("Curtosis: ", round(curtosis_escolaridad, 2)), 
           hjust = 1.1, vjust = 8, size = 4, color = "purple")

# Segmentacion por Genero
# Calcular la asimetría de la variable "Hijos" por Genero
asimetria_por_genero_hijos <- base_de_datos_1 %>%
  group_by(Genero) %>%
  summarise(
    Asimetría = skewness(Hijos),
    Media = mean(Hijos),
    Mediana = median(Hijos),
    Curtosis = kurtosis(Hijos)
  )

print(asimetria_por_genero_hijos)

ggplot(base_de_datos_1, aes(x = Hijos)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 1, 
                 fill = "grey", 
                 color = "black", 
                 alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Hijos, na.rm = TRUE), color = "Media"), 
             linetype = "dashed", 
             size = 1) +
  geom_vline(aes(xintercept = median(Hijos, na.rm = TRUE), color = "Mediana"), 
             linetype = "dashed", 
             size = 1) +
  facet_wrap(~Genero) +
  scale_color_manual(name = "Estadístico", 
                     values = c(Media = "red", Mediana = "blue")) +
  labs(title = "Distribución de Hijos por Género (con Media y Mediana)", 
       x = "Número de Hijos", 
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_text(data = asimetria_por_genero_hijos, 
            aes(x = Inf, y = Inf, 
                label = paste0("Asimetría: ", round(Asimetría, 2), "\n",
                               "Media: ", round(Media, 2), "\n",
                               "Mediana: ", round(Mediana, 2), "\n",
                               "Curtosis: ", round(Curtosis, 2)
                               )),
            hjust = 1.1, vjust = 2, size = 4, color = "black", 
            inherit.aes = FALSE)

# Segmentacion por Estado Civil
# Calcular la asimetría de la variable "Hijos" por Estado Civil
asimetria_por_estado_hijos <- base_de_datos_1 %>%
  group_by(`Estado Civil`) %>%
  summarise(
    Asimetría = skewness(Hijos),
    Media = mean(Hijos),
    Mediana = median(Hijos),
    Curtosis = kurtosis(Hijos)
  )

print(asimetria_por_estado_hijos)

ggplot(base_de_datos_1, aes(x = Hijos)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 1, 
                 fill = "grey", 
                 color = "black", 
                 alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Hijos, na.rm = TRUE), color = "Media"), 
             linetype = "dashed", 
             size = 1) +
  geom_vline(aes(xintercept = median(Hijos, na.rm = TRUE), color = "Mediana"), 
             linetype = "dashed", 
             size = 1) +
  facet_wrap(~`Estado Civil`) +
  scale_color_manual(name = "Estadístico", 
                     values = c(Media = "red", Mediana = "blue")) +
  labs(title = "Distribución de Hijos por Estado Civil (con Media y Mediana)", 
       x = "Número de Hijos", 
       y = "Densidad") +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_text(data = asimetria_por_estado_hijos, 
            aes(x = Inf, y = Inf, 
                label = paste0("Asimetría: ", round(Asimetría, 2), "\n",
                               "Media: ", round(Media, 2), "\n",
                               "Mediana: ", round(Mediana, 2), "\n",
                               "Curtosis: ", round(Curtosis, 2))),
            hjust = 1.1, vjust = 2, size = 4, color = "black", 
            inherit.aes = FALSE)

#. Desviación Estándar:
# La desviación estándar mide la dispersión de un conjunto de datos 
# con respecto a su media. Cuanto mayor sea la desviación estándar, 
# mayor será la dispersión de los datos.
# Cálculo: Utiliza la función sd().


#Varianza:
# Significado: La varianza es el cuadrado de la desviación estándar. 
# Mide la dispersión de los datos respecto a la media, 
# pero en unidades cuadráticas.
# Cálculo: Utiliza la función var().

# Coeficiente de Variación (CV):
# Significado: El coeficiente de variación es la relación entre 
# la desviación estándar y la media. Se expresa como un porcentaje y 
# permite comparar la variabilidad relativa entre diferentes conjuntos de datos.
# Cálculo: Se calcula como CV = (sd(x) / mean(x)) * 100.
# Calcular Desviación Estándar, Varianza y Coeficiente de Variación por Género
estadisticas_por_genero_hijos <- base_de_datos_1 %>%
  group_by(Genero) %>%
  summarise(
    Desviacion_Estandar = sd(Hijos, na.rm = TRUE),
    Varianza = var(Hijos, na.rm = TRUE),
    Coeficiente_Variacion = (sd(Hijos, na.rm = TRUE) / mean(Hijos, na.rm = TRUE)) * 100
  )

print(estadisticas_por_genero_hijos)


# Calcular Desviación Estándar, Varianza y Coeficiente de Variación por Estado Civil
estadisticas_por_estado_hijos <- base_de_datos_1 %>%
  group_by(`Estado Civil`) %>%
  summarise(
    Desviacion_Estandar = sd(Hijos, na.rm = TRUE),
    Varianza = var(Hijos, na.rm = TRUE),
    Coeficiente_Variacion = (sd(Hijos, na.rm = TRUE) / mean(Hijos, na.rm = TRUE)) * 100
  )

print(estadisticas_por_estado_hijos)


# Calcular Desviación Estándar, Varianza y Coeficiente de Variación por Género y Estado Civil
estadisticas_por_genero_estado_hijos <- base_de_datos_1 %>%
  group_by(Genero, `Estado Civil`) %>%
  summarise(
    Desviacion_Estandar = sd(Hijos, na.rm = TRUE),
    Varianza = var(Hijos, na.rm = TRUE),
    Coeficiente_Variacion = (sd(Hijos, na.rm = TRUE) / mean(Hijos, na.rm = TRUE)) * 100
  )

print(estadisticas_por_genero_estado_hijos)


# boxplot hijos segmentado por genero
boxplot(Hijos~Genero, base_de_datos_1, horizontal = T, col=c("red", "blue") )
# boxplot hijos segmentado por estado civil
boxplot(Hijos~`Estado Civil`, base_de_datos_1, horizontal = T, col=c("red", "blue", "green") )
# Escolaridad segmentado por genero
boxplot(`Escolaridad (años)`~Genero, base_de_datos_1, horizontal = T, col=c("red", "blue") )
# Escolaridad hijos segmentado por estado civil
boxplot(`Escolaridad (años)`~`Estado Civil`, base_de_datos_1, horizontal = T, col=c("red", "blue", "green") )