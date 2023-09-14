##########################################
# 2.5 Validación de Modelos de Regresión #
##########################################

###################################
# 2.5.1 Validación Cruzada Simple #
###################################

set.seed(1) # "Definir una semilla", permite que los resultados en muestreos aleatorios sean simpre los mismos
# Esto garantiza que la replicabilidad del ejercicio que se este realizando de siempre los mismos resultados 
Indices_Train<-sample(1:nrow(Data_Hitters),size=round(nrow(Data_Hitters)*0.7,0),replace = FALSE)
Train_Hitters<-Data_Hitters[Indices_Train,]
Test_Hitters<-model.matrix(Salary~.,Data_Hitters[-Indices_Train,]) # la Función crea una matriz solo con 
# las variables a usar en el modelo, excluyendo la variable de respuesta e incluyendo una columna de 
# "unos" para la intresección  (útil para cuando se quieren hacer predicciones).
lmfit_reg<-regsubsets(Salary~.,data = Train_Hitters,nvmax=19) # Como se había hecho en ejemplos anteriores, se
# se "calcula" los modelos con reducción de variables, pero esta vez no se 
errores<-rep(NA,19) #replicar un valor una cantidad determinada de veces (seplicar 16 veces "NA")
for (i in 1:19){
  coeficientes<-coef(lmfit_reg,id=i)
  prediccion<-Test_Hitters[,names(coeficientes)]%*%coeficientes
  errores[i]<-mean((Data_Hitters$Salary[-Indices_Train]-prediccion)^2)
}
errores # MSE calculado para el subconjunto de datos validación test
which.min(errores) # Cuál es la ubicación del MSE mínimo, esto es, la cantidad de variables del modelo
summary(lmfit_reg)$adjr2 # Para conocer el R² ajustado del modelo seleccionado 
coef(lmfit_reg,which.min(errores)) # Coeficientes del modelo seleccionado
# De acuerdo a la validación cruzada simple, el mejor modelo es aquel con 12 variables, con un MSE
# (Media de Errores Cuadrados) de 127598.4, el cual es el menor entre los 19 modelos y un R² ajustado
# de 0.5318 el cual está entre los más altos de los 19 modelos.

########################################
# 2.5.2 Validación Cruzada Tipo K-Fold #
########################################

# Es una estensión de la validación anterior, solo que está vez se tendran k-1 subconjuntos de datos
# de entrenamiento y 1 dato de validación, y en total se harán k procesos de validación.
k<-5
set.seed(3)
folds<-sample(1:k,nrow(Data_Hitters),replace=TRUE)
Matriz_errores<-matrix(NA,k,19,dimnames = list(paste(1:k),paste(1:19)))
for (i in 1:k){
  lmfit_reg_kfold<-regsubsets(Salary~.,data=Data_Hitters[folds!=i,],nvmax=19)
  for (j in 1:19){
    coeficientes<-coef(lmfit_reg_kfold,id=j)
    Test<-model.matrix(Salary~.,Data_Hitters[folds==i,])
    prediccion<-Test[,names(coeficientes)]%*%coeficientes
    Matriz_errores[i,j]<-mean((Data_Hitters$Salary[folds==i]-prediccion)^2)
  }
}
Media_Errores<-apply(Matriz_errores,2,mean) # La función apply sirve para aplicar una función específica
# A columnas o filas de un dataset, en este caso, se calcula la media de los datos por columna
# Cada modelo se entrenó y se validó 5 veces, cada entrenamiento y validación generan un valor MSE,
# esto es, el promedio de los errores cuadrados, para cada modelo se tienen 5 datos que se promedian
# para obtener el MSE para cada modelo (por número de variables)
Media_Errores
par(mfrow=c(1,1))
Media_Errores<-as.data.frame(cbind(1:19,Media_Errores))
names(Media_Errores)<-c("Num_Variables","MSE")
Num_vars<-which.min(Media_Errores[,2])
Error_min<-min(Media_Errores[,2])
gg_Media_Errores<- ggplot(Media_Errores,aes(Num_Variables,MSE))+ # Gráfico en ggplot
  geom_line() +
  geom_point() +
  geom_point(aes(x=Num_vars,y=Error_min),colour="red")
gg_Media_Errores
ggplotly(gg_Media_Errores) # Convirtiendo a gráfico interactivo
# El modelo para el conjunto total de datos se vuelve a calcular haciendo uso del dataset completo, la cantidad de 
# variables a seleccionar es entonces conforme al resultado anterior

# ¿Cuál sería el resultado del modelo para Data_Hitters con un k=10?
# Ejercicio: identificar el mejor modelo para Data_Boston con un k=10
