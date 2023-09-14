library(ISLR) # Librería con datasets y herramientas del libro An Intruduction to Statistical Learning with R
library(MASS)
library(leaps) # Reducción de dimensionalidad
Data_Hitters_1<-Hitters
Data_Hitters_1<-na.omit(Data_Hitters_1)
Data_Hitters_2<-Data_Hitters_1[,-c(14,15,20)]

lmm_1<-regsubsets(Salary~.,
                  data=Data_Hitters_1,
                  method="exhaustive",
                  nvmax=19)
Resumen_lmm1<-summary(lmm_1)
names(Resumen_lmm1)
Resumen_lmm1$rss
Resumen_lmm1$adjr2
which.min(Resumen_lmm1$rss)
which.max(Resumen_lmm1$adjr2)
Resumen_lmm1$which

set.seed(1)
Prop<-0.8
Indices<-sample(1:nrow(Data_Hitters_1),size=(round(nrow(Data_Hitters_1)))*Prop,replace=FALSE)
Train<-Data_Hitters_1[Indices,]
Test<-Data_Hitters_1[-Indices,]

lmm_1<-regsubsets(Salary~.,
                  data=Train,
                  method="exhaustive",
                  nvmax=19)
Resumen_lmm1<-summary(lmm_1)
names(Resumen_lmm1)
Resumen_lmm1$rss
Resumen_lmm1$adjr2
which.min(Resumen_lmm1$rss)
which.max(Resumen_lmm1$adjr2)
plot(Resumen_lmm1$rss, xlab="numero de variables", ylab="RSS", 
     main="Metodo mixto", type="l")
plot(Resumen_lmm1$adjr2, xlab="numero de variables", ylab="R²Adj", 
     main="Metodo mixto", type="l")

# Predicción de resultados con los modelos de regresión

maxi<-which.max(Resumen_lmm1$adjr2) # Guardar el valor de la cantidad de variables del
# modelo con el máximo AdjR2
Intersec<-coef(lmm_1,maxi)[1] # Guardar el valor de intersecante en el eje
Coeficientes<-coef(lmm_1,maxi)[1:maxi+1] # Guardo el dato de los parámetros Bn del modelo
nombres<-names(Coeficientes) # Nombre de las variables a tener en cuenta en el modelo
# nombres[8]<-"Division"
Valores<-Test[,nombres] # del dataset de evaluación se genera un subset con las variables
# seleccionadas en el modelo
# Valores$Division<-as.character(Valores$Division) # Ajuste de variables categóricas
# Valores$Division[Valores$Division=="W"]<-1 # Ajuste de variables categóricas
# Valores$Division[Valores$Division=="E"]<-0 # Ajuste de variables categóricas
ValoresN<-matrix(data = NA, nrow = nrow(Valores), 
                 ncol = ncol(Valores))
for (i in 1:ncol(ValoresN)){
  ValoresN[,i]<-as.numeric(Valores[,i])
}
Prediccion_1<-Intersec+ValoresN%*%as.matrix(Coeficientes)
Respuestas<-cbind(Prediccion_1,Test$Salary)

MSE<-mean((Respuestas[,2]-Respuestas[,1])^2)
RMSE<-sqrt(MSE)          
RMSE


