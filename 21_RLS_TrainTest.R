library(ISLR)
library(MASS)
library(ggplot2)

Data_Boston <- Boston
# Variable de respuesta medv (precio medio de las casas)

lm_Boston<-lm(data=Data_Boston, formula = medv~lstat)
lm_Boston

# dataset de entrenamiento y un dataset de validación

set.seed(1)
prop <- 0.7
Indices_Train <- sample(1:nrow(Data_Boston),size=round(nrow(Data_Boston)*prop,0),replace=FALSE) 
Train_Boston <- Data_Boston[Indices_Train,]
Test_Boston <- Data_Boston[-Indices_Train,]
lm_Train <- lm(data=Train_Boston, formula=medv~lstat)
lm_Train

plot(Train_Boston$lstat,Train_Boston$medv)
abline(lm_Train, lwd = 1, col="red")

P_Boston <- NULL 
n <- 1
while (n <= nrow(Test_Boston)){
  P_Boston[n] <- Test_Boston$lstat[n]*(-1.003) + 35.445
  n <- n + 1
}
P_Boston <- as.data.frame(P_Boston)

RMSE <- sqrt((sum(Test_Boston$medv-P_Boston)^2)/nrow(Test_Boston))
# MSE <- (sum(Test_Boston$medv-P_Boston)^2)
Errores<-RMSE

set.seed(100)
prop <- 0.8
Indices_Train_2 <- sample(1:nrow(Data_Boston),size=round(nrow(Data_Boston)*prop,0),replace=FALSE) 
Train_Boston_2 <- Data_Boston[Indices_Train_2,]
Test_Boston_2 <- Data_Boston[-Indices_Train_2,]
lm_Train_2 <- lm(data=Train_Boston_2, formula=medv~lstat)
lm_Train_2

P_Boston_2 <- NULL 
n <- 1
while (n <= nrow(Test_Boston_2)){
  P_Boston_2[n] <- Test_Boston_2$lstat[n]*(-0.9716) + 34.4901
  n <- n + 1
}
P_Boston_2 <- as.data.frame(P_Boston_2)
RMSE_2 <- sqrt((sum(Test_Boston_2$medv-P_Boston_2)^2)/nrow(Test_Boston_2))
RMSE_2
# MSE <- (sum(Test_Boston$medv-P_Boston)^2)
Errores<-c(RMSE,RMSE_2)
Errores

# Ejercicio: entrenar y validar un modelo de regresión simple con una proporción del
# 70% para Train, con los datos del conjunto Hitters

install.packages("ISLR2")
library(ISLR2)
Dataset <- Hitters
Dataset<-na.omit(Dataset)




