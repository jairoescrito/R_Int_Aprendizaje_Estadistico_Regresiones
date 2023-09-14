###################################################
# 2.6. Regresiones Ridge y LASSO (Regularización) #
###################################################

library(glmnet)
# Son métodos que parten de las regresiones lineales que ya conocemos (por mínimos cuadrados) a las que se les
# incluye un ajuste. Las regresiones Ridge no aplican elimineación de estimadores mientras que LASSO si lo hace
# Estas regresiones son aplicadas especialmente cuando se han detectado problemas de colinealidad.
# Funcionan con un parámetro lambda el cual se selecciona teniendo en cuenta el mínimo error obtenido
# (lambda definitivo).
# Es pertinente tener en cuenmta que en estas técnicas se hace una estandarización de variables para
# Reducir los rangos de variación de cada variable, es decir, la estandarización ajusta las 
# variables a un rango que no dependa de las escalas de las variables y así , las escalas no afecten
# los valores de los coeficientes del modelo.

x<-model.matrix(medv~.,Data_Boston)[,-1] # La función model.matrix también convierte variables cualitativas en variables
# Dummy (un número por nivel). El paquete glmnet solo funciona con variables cuantitativas
y<-Data_Boston$medv 
Lambdas<-10^seq(10,-2,length=100) # seq es una función para generar una secuencia de números con una cantidad definida
# entre un punto de inicio y uno de finalización
Lambdas
set.seed(1)
Indices_Train<-sample(1:nrow(x),nrow(x)*0.7)
x_train<-x[Indices_Train,]
y_train<-y[Indices_Train]
x_test<-x[-Indices_Train,]
y_test<-y[-Indices_Train]

#########################
# 2.6.1 Regresión Ridge #
#########################

reg_ridge_Boston<-glmnet(x,y,alpha = 0,lambda = Lambdas)
plot(reg_ridge_Boston)
# Aquí se generan 100 modelos, cada modelo tiene un coeficiente, en total se tienen 13 coeficientes (las 13 variables
# independientes y el punto de intersección) por cada valor de lambda (100 en total)
dim(coef(reg_ridge_Boston)) # Para corroborar las dimensiones de la matriz de coeficientes
coef(reg_ridge_Boston)[,50]
predict(reg_ridge_Boston,s=1e10,type="coefficients")[1:14,] # Otra forma de obtener los coeficientes de la regresión
# para un valor lambda (s) específico.
# Para conocer el mejor valor de Lambda, esto es, el que minimiza el error, en este caso el MSE, el paquete glmnet
# tiene una función para hacer la validación cruzada, sólo es necesario darle como entrada el dataset de entrenamiento
# "train"
set.seed(4)
cv_reg_ridge_Boston<-cv.glmnet(x_train,y_train,alpha=0,nfolds=5,lambda=Lambdas)
plot(cv_reg_ridge_Boston) 
Best_Lambda_ridge<-cv_reg_ridge_Boston$lambda.min
Best_Lambda_ridge
Reg_ridge_Boston_Predict<-predict(reg_ridge_Boston,s=Best_Lambda_ridge,newx = x_test)
mean((Reg_ridge_Boston_Predict-y_test)^2)
# Una vez conocido el valor lambda optimo, se recalcula la regresión ridge con el total de datos y se 
# generan los coeficientes con dicho valor lambda
Reg_ridge_Boston_Final<-glmnet(x,y,alpha = 0)
ridge_coef<-predict(Reg_ridge_Boston_Final,type="coefficients",s=Best_Lambda_ridge)[1:14,]
ridge_coef
# Como ya se había mencionado, la regresión ridge no elimina ninguna de las dimensiones en el modelo

#########################
# 2.6.2 Regresión Lasso #
#########################

# El procedimiento para esta regresión es el mismo, solo que se usa el parámetro alpha=1 en la función glmnet
reg_lasso_Boston<-glmnet(x,y,alpha = 1,lambda = Lambdas)
plot(reg_lasso_Boston)
# Para la validación cruzada
set.seed(4)
cv_reg_lasso_Boston<-cv.glmnet(x_train,y_train,alpha=1,nfolds=5,lambda=Lambdas)
plot(cv_reg_lasso_Boston)
Best_Lambda_lasso<-cv_reg_lasso_Boston$lambda.min
Best_Lambda_lasso
Reg_lasso_Boston_Predict<-predict(reg_lasso_Boston,s=Best_Lambda_lasso,newx = x_test)
mean((Reg_lasso_Boston_Predict-y_test)^2)
Reg_lasso_Boston_Final<-glmnet(x,y,alpha = 1)
lasso_coef<-predict(Reg_lasso_Boston_Final,type="coefficients",s=Best_Lambda_lasso)[1:14,]
lasso_coef
lasso_coef[lasso_coef!=0]