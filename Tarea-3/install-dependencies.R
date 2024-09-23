
## Establecer un mirror de CRAN y luego instalar y cargar readxl
options(repos = c(CRAN = "https://cran.rstudio.com/"))

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