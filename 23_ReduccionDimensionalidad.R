##################################################################
# 2.3 Regresión Lineal Múltiple con Reducción de Dimensionalidad #
##################################################################

# Hacer una reducción de variables (o dimensiones), es posible a través de metodos de reducción exahustiva, 
# hacia adelante, o hacia atrás (entre otros) esto según los algoritmos más comunes
# Se requiere hacer uso del paquete "leaps" para hacer uso de las funciones de reducción 
# install.packages("leaps"), solo se ejecuta cuando se va a usar por primera vez
library(leaps) #herramientas para reducir el número de variables de un dataset para una regresión de acuerdo a la importancia de
# cada una frente a la variable de respuesta
?regsubsets

###########################
# 2.3.1 Método Exhaustivo #
###########################

lmm_reg_e<-regsubsets(Salary~.,data=Data_Hitters, method = "exhaustive", nvmax=16) #nvmax es el número de variables que el modelo
# tendrá en cuenta, por defecto usará 8, es decir, solo mostrará resultados de modelos usando máximo 8 variables
lmm_reg_e_summary<-summary(lmm_reg_e)
lmm_reg_e_summary
names(lmm_reg_e_summary)
lmm_reg_e_summary$rss # Detalles de los valores de la Suma de Residuos al Cuadrado RSS
lmm_reg_e_summary$adjr2  # Detalles de los valores de R² para los modelos de regresión
par(mfrow=c(1,2))
plot(lmm_reg_e_summary$rss, xlab="# de variables",ylab="RSS",main = "Método Exhaustivo",type="l")
plot(lmm_reg_e_summary$adjr2, xlab="# de variables",ylab="R² Adj",main = "Método Exhaustivo",type="p")
maxi_e<-which.max(lmm_reg_e_summary$adjr2) # Que cantidad de variables tiene el modelo con el mejor valor de R² 
points(maxi_e,lmm_reg_e_summary$adjr2[maxi_e],col="red",cex=2,pch=20) #Agrego un punto rojo para señalar R² en la gráfica del mejor modelo
plot(lmm_reg_e,scale="adjr2") # Una manera visual de ver "el mejor" modelo
coef(lmm_reg_e,maxi_e)# parámetros del modelo

##################################
# 2.3.2 Reducción Hacia Adelante #
##################################

lmm_reg_f<-regsubsets(Salary~.,data=Data_Hitters, method = "forward", nvmax=16)
lmm_reg_f_summary<-summary(lmm_reg_f)
lmm_reg_f_summary
plot(lmm_reg_f_summary$rss, xlab="# de variables",ylab="RSS",main="Método Hacia Adelante",type="l")
plot(lmm_reg_f_summary$adjr2, xlab="# de variables",ylab="R² Adj",main="Método Hacia Adelante",type="p")
maxi_f<-which.max(lmm_reg_f_summary$adjr2)
points(maxi_f,lmm_reg_f_summary$adjr2[maxi_f],col="red",cex=2,pch=20)
coef(lmm_reg_f,maxi_f)

###############################
# 2.3.3 Reducción Hacia Atrás #
###############################

lmm_reg_b<-regsubsets(Salary~.,data=Data_Hitters, method = "backward", nvmax=16)
lmm_reg_b_summary<-summary(lmm_reg_b)
lmm_reg_b_summary
plot(lmm_reg_b_summary$rss, xlab="# de variables",ylab="RSS",main="Método Hacia Atrás",type="l")
plot(lmm_reg_b_summary$adjr2, xlab="# de variables",ylab="R² Adj",main="Método Hacia Atrás",type="p")
maxi_b<-which.max(lmm_reg_b_summary$adjr2)
points(maxi_b,lmm_reg_b_summary$adjr2[maxi_b],col="red",cex=2,pch=20)
coef(lmm_reg_b,maxi_b)

#Comparando los resultados de R² Ajustado
lmm_reg_b_summary$adjr2[maxi_b]
lmm_reg_f_summary$adjr2[maxi_f]
lmm_reg_e_summary$adjr2[maxi_e]
# Para los modelo obtenidos con la función "regsubsets" no existe una función para para las predicciones, no obstante, no es
# complejo generar las predicciones. Para el caso del modelo obtenido con el método "exhaustivo"
coefi_e_interc<-coef(lmm_reg_e,maxi_e)[1] # Extraer el valor de la interseción en el modelo lineal
coefi_e_var<-coef(lmm_reg_e,maxi_e)[1:maxi_e+1] # Extraer los coeficientes de las variables
Valores<-Data_Hitters[1,names(coefi_e_var)] # Valores para predicción 
predicc<-coefi_e_interc+as.matrix(Valores)%*%as.matrix(coefi_e_var) # Formula para predicción 
#¿cuál sería el resultado si se hace el análisis sin excluir las variables cualitativas (categóricas) del dataset "Hitters"?
