library(ISLR)
library(MASS)
library(ggplot2)
library(GGally)

Data_Boston<-Boston

### Creación de datasets train y test
set.seed(100)
prop <- 0.8
Indices_Train_2 <- sample(1:nrow(Data_Boston),size=round(nrow(Data_Boston)*prop,0),replace=FALSE) 
Train_Boston_2 <- Data_Boston[Indices_Train_2,]
Test_Boston_2 <- Data_Boston[-Indices_Train_2,]

# Regresión lineal simple
lms <- lm(data=Train_Boston_2, formula=medv~lstat)
P_Boston <- NULL 
lms$coefficients[2]
n <- 1
while (n <= nrow(Test_Boston_2)){
  P_Boston[n] <- Test_Boston_2$lstat[n]*(lms$coefficients[2]) + lms$coefficients[1]
  n <- n + 1
}
P_Boston <- as.data.frame(P_Boston)
RMSE <- sqrt((sum(Test_Boston_2$medv-P_Boston)^2)/nrow(Test_Boston_2))
RMSE
# caluclo de errores para RLS
Errores<-c(RMSE)
Errores

### Regresión lineal múltiple
# Todas las variables
lmm <- lm(medv~., data=Train_Boston_2)
Prediccion <- as.data.frame(predict(lmm,Test_Boston_2[,-14]))
Prediccion$medv_real <- Test_Boston_2$medv
names(Prediccion) <- c("medv_pronostico","medv_real")
RMSE_2 <- sqrt((sum((Prediccion$medv_real - Prediccion$medv_pronostico)^2))/(nrow(Prediccion)))
Errores<-c(Errores,RMSE_2)
Errores <- as.data.frame(Errores)
Errores$Regresion <- c("Lineal","Multiple_Todas")

# Las dos variables más influyentes
ggpairs(Data_Boston)
lmm_2 <-lm(medv~lstat+rm, data=Train_Boston_2)

## Regresión con reducción de dimensionalidad
library(leaps)
lmmrd_1 <- regsubsets(medv~., 
                    data = Train_Boston_2,
                    method = "exhaustive",
                    nvmax = 13)
resumen_lmmrd_1 <- summary(lmmrd_1) 
resumen_lmmrd_1
resumen_lmmrd_1$rss

Inters <- coef(lmmrd_1,11)[1]
Coefi <- coef(lmmrd_1,11)[-1]
Nombres<-names(Coefi)
Nombres
Valores_Test<-Test_Boston_2[,Nombres]
Prediccion_lmmrd <- Inters + as.matrix(Valores_Test) %*% as.matrix(Coefi)
Prediccion_lmmrd
Resultados_lmmrd <- as.data.frame(cbind(Test_Boston_2$medv,Prediccion_lmmrd))
names(Resultados_lmmrd) <- c("Real","Prediccion_lmmrd") 
RMSE_lmmrd <- sqrt((sum(Resultados_lmmrd[,1]-Resultados_lmmrd[,2])^2)/nrow(Resultados_lmmrd))
RMSE_lmmrd
Errores <- rbind(Errores,c(RMSE_lmmrd,"Reducción dimensionalidad"))

for (columna in Test_Boston_2){
  for (dato in columna) {
    print(dato)
  }
}

for (i in 1:length(Test_Boston_2)){
  for (j in 1:length(Test_Boston_2[,i])){
    print(Test_Boston_2[j,i])
  }
}



