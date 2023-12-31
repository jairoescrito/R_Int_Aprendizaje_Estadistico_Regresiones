# Ejercicio: entrenar y validar un modelo de regresi�n lineal simple
# con una proporci�n de 70%, con el conjunto de datos de Hitters

install.packages("ISLR2")
library(ISLR2)

Dataset <- Hitters
Dataset <- na.omit(Dataset)

# 0 ) Reconocer la variable de respuesta: Salary
# 1) Verificar cu�l es la variable m�s influyente sobrte la 
# variable de respuesta

library(GGally)
ggpairs(Dataset)

Dataset <- Dataset[,-c(14,15,20)]
MC <- cor(Dataset)
library(corrplot)
corrplot(cor(Dataset), type = "lower"
         , method = "number",
         number.font = 0.5,
         number.cex = 0.25)

