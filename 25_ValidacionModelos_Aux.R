library(ISLR) # Librería con datasets y herramientas del libro An Intruduction to Statistical Learning with R
library(MASS)
library(ggplot2)
library(plotly)
Data<-Hitters
Data<-na.omit(Data)

# Validación cruzada tipo K-Fold
K<-5
set.seed(3)
folds<-sample(1:K,nrow(Data),replace=TRUE)
Matriz_Errores<-matrix(NA,K,19,dimnames=list(paste(1:K),paste(1:19)))
for (i in 1:K){
  lm_kfold<-regsubets(Salary~.,data=Data[folds=!i],nvmax=19)
  for (j in 1:19){
    coeficientes<-coef(lm_kfold,id=j)
    Test<-model.matrix(Salary~.,Data[folds==i,])
    Prediccion<-Test[,names(coeficientes)]%*%coeficientes
    Matriz_Errores[i,j]<-mean((Data$Salary[fold==i]-Prediccion)**2)
  }
}

Ejemplo<-model.matrix(Salary~.,Data[folds==1,])

