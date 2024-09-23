db1 = table(base5ago$`Estado Civil`, base5ago$Genero)
# Sin varible
# barplot(table(base5ago$`Estado Civil`, base5ago$Genero))

barplot(db1, col=c("red", "blue", "green"), main="Género vs Estado Civil")

barplot(db1, col=c("red", "blue", "green"), main="Género vs Estado Civil", xlab="Género", ylab="Frecuencia")

barplot(db1, col=c("red", "blue", "green"), main="Género vs Estado Civil", xlab="Género", ylab="Frecuencia", ylim = c(0,200))

table_Hijos_genero = table(base5ago$Hijos, base5ago$Genero)
print(table_Hijos_genero)

barplot(table_Hijos_genero)

table_genero = table(base5ago$Genero)
pie(table_genero)

#Cuanti
#Promedio de hijos
mean(base5ago$Hijos)
# desviacion Estandar
sd(base5ago$Hijos)

#Filteo (Promedio de los hijos de los nombres)
mean(base5ago$Hijos[base5ago$Genero=="Hombre"])
mean(base5ago$Hijos[base5ago$Genero=="Mujer"])

#Promerio de escolaridad por estado civil
mean(base5ago$`Escolaridad (años)`[base5ago$`Estado Civil`=="Soltero"])
mean(base5ago$`Escolaridad (años)`[base5ago$`Estado Civil`=="Casado"])
mean(base5ago$`Escolaridad (años)`[base5ago$`Estado Civil`=="Separado"])

## Instalar y usar dplyr
install.packages("dplyr")
library(dplyr)


install.packages("stats19")
library("stats") # no requiere instalacion porque es un paquete base
library("stats19")

#primero la variable cuantitativa, y luego la de segmentacion que es la cualitativa
aggregate(Hijos~Genero, base5ago, mean)
aggregate(Hijos~Genero, base5ago, min)          
aggregate(Hijos~Genero, base5ago, max)
aggregate(Hijos~Genero, base5ago, sum)
aggregate(Hijos~Genero, base5ago, sd)
aggregate(Hijos~Genero, base5ago, summary)

boxplot(base5ago$Hijos[base5ago$Genero=="Hombre"])
boxplot(Hijos~Genero, base5ago, horizontal = T)
boxplot(Hijos~Genero, base5ago, horizontal = T, col=c("red", "blue") )
quantile(base5ago$Hijos)
quantile(base5ago$Hijos[base5ago$Genero=="Hombre"])
hist(base5ago$Hijos[base5ago$Genero=="Hombre"])
plot(density(base5ago$Hijos))
plot(density(base5ago$Hijos[base5ago$Genero=="Hombre"]), main = "Curva Densidad Hijos de los Hombres")
plot(density(base5ago$Hijos[base5ago$Genero=="Mujer"]), main = "Curva Densidad Hijos de las Mujeres")

# completar los box segmentados faltantes.(escoariada x genero) (hijos x estado civil) (escolaridad x estado civil)
# curtosis solitas, y segmentados
#Proxima Clase, Desviacion Estandar, Varianza, CV (Coeficiente de Variacion) esto segmentado (Genero y Estado Civil
#Curtosis y asmietria (segmentado)
#Sabemos sacar media, minimo, maximo sacar lo anterior puros y segmentado, concepto de lo anterior.
# Como sacar en R los datos anteriores y que significa.ejemplo si da 0.8 la Desviacion estandar 

# EN escrit
