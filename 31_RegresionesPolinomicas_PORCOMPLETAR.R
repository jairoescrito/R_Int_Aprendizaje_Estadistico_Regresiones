################################
## 3. Regresiones No Lineales ##
################################

###############################
# 3.1 Regresiones Polinómicas #
###############################

#############################################
# 3.1.1 Regresiones Polinómicas Función lm ##
#############################################

lmmnl_1_Boston <-lm(medv~lstat+I(lstat^2),data = Data_Boston) # modelo cuadrático de una variable 
summary(lmmnl_1_Boston)
par(mfrow=c(2,2))
plot(lmmnl_1_Boston)
par(mfrow=c(1,2))
plot(Data_Boston$lstat,Data_Boston$medv) 
lines(Data_Boston$lstat,fitted(lmmnl_1_Boston),type="p",col="red") # adición de dispersión de los datos del modelo de regresión
boxplot(Data_Boston$medv,fitted(lmmnl_1_Boston)) # se puede visualizar, a manera de comparación, la dispersión de los datos
# que toma la variable de respuesta (originales vs modelo)
hist(Data_Boston$medv) # Otra forma es observando el histograma de los datos, su distribución, se compara la original frente a 
# a los datos del modelo
hist(fitted(lmmnl_1_Boston))
# Podría generar un bucle que me permita evaluar los modelos cuadráticos (de una variable) usando todas las variables y
# determinar cuál es el mejor
n<-ncol(Data_Boston)-1
R2adj<-data.frame(matrix(ncol = 1, nrow = n))
Modelos<-vector("list", length = n)
for (i in 1:n) {
  X<-Data_Boston[,i]
  lmmnl_X_Boston <-lm(medv~X+I(X^2),data = Data_Boston)
  Modelos[[i]]<-lmmnl_X_Boston
  R2adj[i,1]<-summary(lmmnl_X_Boston)$adj.r.squared  
}
names(Data_Boston[which.max(R2adj[,1])]) # para conocer cuál es el nombre de la variable con el mejor R² ajustado
lmmnl_Boston<-Modelos[[which.max(R2adj[,1])]] # Extraer el modelo 
summary(lmmnl_Boston) # Datos del modelo
par(mfrow=c(2,2))
plot(lmmnl_Boston) # Gráficos del comportamiento del modelo
par(mfrow=c(1,2))
plot(Data_Boston[,which.max(R2adj[,1])],Data_Boston$medv) # Gráficos del modelo
lines(Data_Boston[,which.max(R2adj[,1])],fitted(lmmnl_Boston),type="p",col="red") 
boxplot(Data_Boston$medv,fitted(lmmnl_Boston))
hist(Data_Boston$medv)
hist(fitted(lmmnl_1_Boston))

###############################################
# 3.1.2 Regresiones Polinómicas Función poly ##
###############################################