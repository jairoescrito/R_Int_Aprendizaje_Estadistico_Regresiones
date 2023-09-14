#################################
# 2.2 Regresión Lineal Múltiple #
#################################

pairs(Data_Hitters)
dim(Data_Hitters) # Proporciona el dato del número de observaciones y variables que tiene el dataset
# Es recomendable verificar que no tenga datos nulos en el dataset a trabajar "NA". Existen varios métodos para tratar
# los datos nulos, en el caso de los datos nulos en la variable de respuesta lo más recomendable es eliminar la
# observación, hacer un ajuste en estos valores puede afectar los modelos de datos
# En el caso de las variables independientes, existen varias opciones para dar tratamiento a estos NA: media, mediana, moda
# etc, esto dependerá de la naturaleza de los datos, para la variable independiente eliminar las observaciones puede
# convertirse en una perdida importante de información.
sum(is.na(Data_Hitters$Salary))
Data_Hitters<-na.omit(Data_Hitters) #Elimina todas las observaciones que tengan al menos un NA en alguna de las 20 variables
sum(is.na(Data_Hitters$Salary))
dim(Data_Hitters)
Data_Hitters<-Data_Hitters[,-c(14,15,20)]
dim(Data_Hitters)
pairs(Data_Hitters)
# Regresión lineal múltiple (RLM) incluyendo las 16 variables independientes
lmm_full<-lm(Salary~.,data=Data_Hitters)
lmm_full
summary(lmm_full)
plot(lmm_full)
# La RLM presenta un F-estadístico de 17.19, del cual se podría concluir que se rechaza la hipótesis nula, esto es,
# al menos uno de los parámetros beta de la regresión es diferente de cero, no obstante el R² de la regresión es 
# apenas del 0.52, esto es, en promedio solo se explica la mitad de la varianza de los datos, no se tiene un modelo
# adecuado el cual permita explicar el comportamiento en terminos de las 16 variables independientes y un posible
# uso del mismo para la predicción de salario de beisbolistas podría incluir un nivel de error considerable
# Adicionalmente, los resultados permiten identificar variables "relevantes" o "incluyentes" en la variable de 
# respuesta, esto teniendo en cuenta los p-valores: AtBat,Hits, Walks y PutOuts
# RLM teniendo limitando las variables
lmm_red<-lm(Salary~AtBat+Hits+Walks+PutOuts,data=Data_Hitters)
lmm_red
summary(lmm_red)
plot(lmm_red)
# El modelo reducido presenta un F-estadístico mayor al anterior modelo, no obstante, el resultado del R² es mucho
# menor. 


###OJO!! FALTA COMPLETAR EL TEA CON ANÁLISIS DE COLINEALIDAD CON CORRELACIÓN Y VIF###

