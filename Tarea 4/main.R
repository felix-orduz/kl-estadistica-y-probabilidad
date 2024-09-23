# Instalar los paquetes necesarios (solo la primera vez)
install.packages("car")
install.packages("readxl")
install.packages("nortest")
install.packages("rstatix")
install.packages("stats19")

# Cargar las librerías necesarias
library(readxl)
library(car)
library(nortest)
library(rstatix)
library(stats19)

install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")   # Opcional, para manipulaciones adicionales
install.packages("purrr")   # Opcional, para funciones de mapeo

# Cargar los paquetes
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
# Cargar los datos desde el archivo Excel
datos <- read_excel("base de datos 1.xlsx")
datos2 <- read_excel("datos_biologicos.xlsx")

print(datos2)


# Aplicar el test de Shapiro-Wilk Lugar
resultados_shapiro <- datos2 %>%
  group_by(Lugar) %>%
  summarise(
    shapiro_crecimiento_stat = shapiro.test(Crecimiento)$statistic,
    shapiro_crecimiento_p = shapiro.test(Crecimiento)$p.value,
    shapiro_flores_stat = shapiro.test(Numero_de_flores)$statistic,
    shapiro_flores_p = shapiro.test(Numero_de_flores)$p.value,
    shapiro_resistencia_stat = shapiro.test(Resistencia_a_enfermedades)$statistic,
    shapiro_resistencia_p = shapiro.test(Resistencia_a_enfermedades)$p.value
  )

# Mostrar los resultados
print(resultados_shapiro)

# Aplicar el test de Shapiro-Wilk Especie
resultados_shapiro2 <- datos2 %>%
  group_by(Especie) %>%
  summarise(
    shapiro_crecimiento_stat = shapiro.test(Crecimiento)$statistic,
    shapiro_crecimiento_p = shapiro.test(Crecimiento)$p.value,
    shapiro_flores_stat = shapiro.test(Numero_de_flores)$statistic,
    shapiro_flores_p = shapiro.test(Numero_de_flores)$p.value,
    shapiro_resistencia_stat = shapiro.test(Resistencia_a_enfermedades)$statistic,
    shapiro_resistencia_p = shapiro.test(Resistencia_a_enfermedades)$p.value
  )

# Mostrar los resultados
print(resultados_shapiro2)




resultados_shapiro <- datos %>%
  group_by(Genero) %>%
  summarise(
    shapiro_statistic = shapiro.test(`Escolaridad (años)`)$statistic,
    shapiro_p_value = shapiro.test(`Escolaridad (años)`)$p.value
  )# Mostrar los resultados
print(resultados_shapiro)



resultados_shapiro <- datos %>%
  group_by(Genero) %>%
  summarise(
    shapiro_statistic = shapiro.test(``)$statistic,
    shapiro_p_value = shapiro.test(`Escolaridad (años)`)$p.value
  )# Mostrar los resultados
print(resultados_shapiro)
# Verifica los datos para asegurarte de que se han cargado correctamente
View(datos)
# Prueba de Normalidad (Shapiro-Wilk): Esta prueba evalúa si la variable 
# Escolaridad (años) sigue una distribución normal. Si el p-valor es mayor que 
# 0.05, puedes asumir que los datos tienen una distribución normal. Esto es 
#importante para aplicar otras pruebas estadísticas que asumen normalidad.

# Ejemplo: Si tienes un grupo de estudiantes y quieres saber si sus años de 
# escolaridad se distribuyen normalmente, realizarías esta prueba.

# Prueba de Homogeneidad de Varianzas (Levene): Esta prueba verifica si las 
# varianzas entre los grupos (en este caso, Género) son homogéneas. Si el 
# p-valor es mayor que 0.05, puedes asumir que las varianzas son similares en 
# ambos grupos.

# Ejemplo: Imagina que quieres comparar el nivel de escolaridad de hombres y 
#mujeres y necesitas asegurarte de que la variabilidad en ambos grupos es 
# similar. Esto es importante para aplicar pruebas como el ANOVA.

# Prueba de normalidad (Shapiro-Wilk) en la variable 'Escolaridad (años)'
resultado_normalidad <- shapiro.test(datos$`Escolaridad (años)`)

# Muestra los resultados de la prueba de normalidad
cat("Prueba de Normalidad (Shapiro-Wilk):\n")
print(resultado_normalidad)

# Si el p-valor es mayor a 0.05, los datos son normales
if (resultado_normalidad$p.value > 0.05) {
  cat("Los datos siguen una distribución normal.\n")
} else {
  cat("Los datos no siguen una distribución normal.\n")
}

# Prueba de homogeneidad de varianzas (Levene)
resultado_levene <- leveneTest(`Escolaridad (años)` ~ Genero, data = datos)

# Muestra los resultados de la prueba de Levene
cat("Prueba de Homogeneidad de Varianzas (Levene):\n")
print(resultado_levene)

# Si el p-valor es mayor a 0.05, las varianzas son homogéneas
if (resultado_levene$`Pr(>F)`[1] > 0.05) {
  cat("Las varianzas entre los grupos son homogéneas.\n")
} else {
  cat("Las varianzas entre los grupos no son homogéneas.\n")
}

# Prueba de normalidad Kolmogorov-Smirnov
ks_test <- ks.test(datos$`Escolaridad (años)`, "pnorm", mean(datos$`Escolaridad (años)`), sd(datos$`Escolaridad (años)`))
print(ks_test)


# Prueba de Anderson-Darling
ad_test <- ad.test(datos$`Escolaridad (años)`)
print(ad_test)

#Pruebas de homogeneidad de varianzas:
## Prueba de Bartlett
bartlett_test <- bartlett.test(`Escolaridad (años)` ~ Genero, data = datos)
print(bartlett_test)

# Prueba de Fligner-Killeen
fligner_test <- fligner.test(`Escolaridad (años)` ~ Genero, data = datos)
print(fligner_test)

# Prueba de Brown-Forsythe (variante de Levene)
brown_forsythe_test <- leveneTest(`Escolaridad (años)` ~ Genero, data = datos, center = median)
print(brown_forsythe_test)