########################################################
# 2.4 Regresión lineal Con Interacción Entre Variables #
########################################################

lmm_Boston_e<-regsubsets(medv~.,data=Data_Boston, method = "exhaustive", nvmax=13)
lmm_Boston_e_summary<-summary(lmm_Boston_e)
lmm_Boston_e_summary
round(lmm_Boston_e_summary$adjr2,2)
maxi_Boston<-which.max(round(lmm_Boston_e_summary$adjr2,2))
maxi_Boston # El modelo con el mejor resultado usa las 11 variables
coef(lmm_Boston_e,maxi_Boston) # Para el ejercicio se van a excluir las variables categóricas
# Modelo con interacción entre variables
lmmit_Boston <-lm(medv~crim*chas*nox*rm*dis*rad*ptratio*black*lstat,data = Data_Boston) # Modelo de interacción con 9 variables
summary(lmmit_Boston)
#¿cómo puedo hacer una análisis gráfico de un modelo que no podría fácilmente graficar debido a la cantidad de dimensiones?
par(mfrow=c(2,2))
plot(lmmit_Boston) # Resumen de los resultados del modelo en términos de "residuos"
par(mfrow=c(1,1))
boxplot(Data_Boston$medv,fitted(lmmit_Boston)) # Gráfico de dispersión de los datos originales y los del modelo para la variable
# de respuesta
par(mfrow=c(1,2))
hist(Data_Boston$medv) # El histograma me permite ver la distribución de los datos de la variable de respuesta
hist(fitted(lmmit_Boston)) # Los datos del modelo deberían acercarse mucho a los originales dado el valor del R² ajustado
