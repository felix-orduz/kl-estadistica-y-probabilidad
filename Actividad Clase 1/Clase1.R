# Instalar y Cargar Librerias
install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)

# Crear tabla
individuos_tabla <- data.frame(
  Nombre = c("Ana García", "Carlos Martínez", "María Rodríguez", "Luis Fernández", "Sofía Gómez", 
             "Jorge Sánchez", "Lucía Pérez", "Miguel Ramírez", "Elena Torres", "Pedro Vargas"),
  Color = c("Rojo", "Azul", "Verde", "Amarillo", "Rosa", "Negro", "Blanco", "Gris", "Morado", "Naranja"),
  Lugar = c("Bogotá", "Medellín", "Cali", "Bogotá", "Bogotá", "Cartagena", "Bogotá", "Bogotá", "Bucaramanga", "Bogotá"),
  NumHermanos = c(2, 1, 3, 0, 4, 2, 1, 3, 2, 1),
  NumMaterias = c(5, 6, 4, 7, 5, 6, 3, 4, 5, 6),
  ComidaFavorita = c("Pizza", "Hamburguesa", "Sushi", "Pasta", "Tacos", "Pizza", "Pollo Frito", "Pasta", "Hamburguesa", "Cubios")
)

# Ver contenido de la tabla
View(individuos_tabla)

# Gráfico simple de la cantidad de hermanos
ggplot(individuos_tabla, aes(x = Nombre, y = NumHermanos )) + geom_bar(stat= "identity")

# Gráfico personalizado de cantidad de hermanos
individuos_tabla$Indice <- 1:nrow(individuos_tabla)
individuos_tabla$NombreIndice <- paste0(individuos_tabla$Nombre, " (", individuos_tabla$Indice, ")")

# Gráfico de barras para la columna Color
ggplot(individuos_tabla, aes(x = factor(Indice), y = NumHermanos, fill = NombreIndice)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(labels = individuos_tabla$Indice) + 
  labs(title = "Número de Hermanos por Individuo", x = "Índice", y = "Número de Hermanos") + 
  theme(legend.position = "right") + 
  guides(fill = guide_legend(title = "Nombre"))


# Gráfico de barras para la columna Color
ggplot(individuos_tabla, aes(x=Color, fill= Color)) + 
  geom_bar() + 
  labs(title="Distribución de Colores Favoritos", x="Color Favorito", y="Frecuencia")


#Gráfico de barras para la columna comida
ggplot(data, aes(x=ComidaFavorita, fill = ComidaFavorita)) + 
  geom_bar() + 
  labs(title="Distribución de Comidas Favoritas", x="Comida Favorita", y="Frecuencia")


# Gráfico de barras para la columna NumMaterias
ggplot(data, aes(x=factor(NumMaterias), fill = NumMaterias)) + 
  geom_bar() + 
  labs(title="Distribución del Número de Materias", x="Número de Materias", y="Frecuencia")
