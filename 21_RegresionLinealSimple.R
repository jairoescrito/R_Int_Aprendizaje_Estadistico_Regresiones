##############################
## 2.##Regresiones Lineales ##
##############################

library(ISLR) # Librería con datasets y herramientas del libro An Intruduction to Statistical Learning with R
library(MASS) # Librería con datasets y herramientas del libro Modern Applied Statistics with S
library(ggplot2)
###############################
# 2.1 Regresión lineal simple #
###############################

Data_Boston<-Boston
#attach(Boston) # Para usar las variables del dataset directamente sin 
# cargar el dataset a una variable tipo dataframe
?Boston # Información del dataset. Contiene información el valor medio de una casa
# para 506 vecindarios en Boston de acuerdo a unos parámetros
# característicos. Vamos a usar la variable mdev como 
# variable de respuesta que tiene datos en miles de dólares
summary(Data_Boston)
boxplot(Data_Boston$crim)
pairs(Data_Boston) # Permite graficar "pares" de dispersión entre las variables
# del dataset. Nos interesa observar las relaciones que tiene la variable
# de respuesta frente a las demás variables. Esto lo podemos hacer graficando 
# 1 a una cada dispersión (13 graficas) lo cual sería muy largo
# no obstante, el resultado, al ser tantas variables, resulta dificil su observación
# Excluimos del dataset las variables de las columnas 4,9 y 10
# Datos de covarianza y correlación
corr<-round(cor(Data_Boston),3)
cova<-round(cov(Data_Boston),3)
pairs(Data_Boston[,-c(4,9,10)])
# Podemos seguir "excluyendo" variables que visualmente dejan ver que no tienen relación
# lineal con la variable de respuesta
corr_lstat<-round(cor(Data_Boston[,c(13,14)]),3)
cova_lstat<-round(cov(Data_Boston[,c(13,14)]),3)
summary(Data_Boston$medv)
boxplot(Data_Boston$medv)
boxplot_medv<-ggplot(Data_Boston,aes(x="",y=medv))+
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  stat_summary(fun=median, geom="point", shape=20, size=5, color="green", fill="green")
boxplot_medv
# Podemos elaborar un gráfico visualmente más agradable con ggpairs
# install.packages("GGally")
library(ggplot2)
library(GGally)
ggpairs(Data_Boston, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none",columns = 1:14)
ggpairs(Data_Boston, lower = list(continuous = "smooth"),
        columns = 1:14)
# Esta opción nos genera información adicional: correlación entre variables y 
# una gráfica de distribución de los datos. Esta información nos permite decidir
# con qué variables nos "quedamos", en este caso las de las columnas 3,5,6,10,11,13
ggpairs(Data_Boston,
        columns = c(3,5,6,10,11,13,14),
        title = "Precio de las casas en Boston")
# Mayor correlación con la variable lstat: estatus socioeconómicos
# Generamos la regresión lineal
lm.fit= lm(medv~lstat,data=Data_Boston)
lm.fit
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
# puedo generar intervalos de confianza para la predicción de ciertos valores, es decir,
# el valor que puede tomar la variable de respuesta para un valor dado de la variable dependiente
# podría estar en este intervalo
predict(lm.fit,data.frame(lstat=c(20)),interval="confidence")
# La predicción es de 15.55 miles de dolares. Para un intervalo de confianza 95%, 
# la predicción está entre 14.77 y 16.33 miles de dólares.
plot(Data_Boston$lstat,Data_Boston$medv)
abline(lm.fit, lwd = 1, col = "red")
P_lm<-predict(lm.fit,lstat=Data_Boston$lstat)
plot(x=Data_Boston$lstat,y=P_lm)
# La gráfica en ggplot me permite observar dicho intervalo de confianza
ggfig<- ggplot(Data_Boston,aes(lstat,medv))+
  geom_point() +
  geom_smooth(method="lm")
ggfig
# Gráficos interactivos
#install.packages("plotly")
library(plotly)
#Grafico en dispersión los datos de las dos variables
figura<-plot_ly(data=Data_Boston, x=~lstat, y=~medv, type='scatter', mode = "markers")
figura
# Creo un gráfico que incluye la regresión lineal
Data_Boston%>%
  plot_ly(x=~lstat)%>%
  add_markers(y=~medv)%>%
  add_lines(x=~lstat,y=~fitted(lm.fit))
# La más fácil para pasar a plotly
ggplotly(ggfig)
# Evaluación de la precisión del modelo de regresión lineal
summary(lm.fit)
# 1) RSE (Error Estándar Residual): 6.216, el valor medio de cada casa que proporciona el modelo se desvia del modelo "real", en promedio, 6.216 dólares aprox
# Si la media y mediana de los datos está alrededor de 22 mil dólares, un error de 6 mil es muy alto por lo que el modelo no parece
# conveniente (si se observa desde este parámetro de evaluación), en otros términos, el porcentaje de error es del 27%
# 2) R²: 0.54, el modelo muestra que la variable lstat solo puede explicar el 54% de la variabilidad de medv 
# La conclusión es que la relación entre las dos variables no es del todo lineal, ya gráficamente era evidente
par(mfrow=c(1,1))
plot(predict(lm.fit),residuals(lm.fit))
# Otra opción graficando en modo "auto" la regresión
ggfig_auto<- ggplot(Data_Boston,aes(lstat,medv))+
  geom_point() +
  geom_smooth(method=NULL)
ggfig_auto
# El resultado de mejor ajuste se encuentra con el método loess (regresión local, método no paramétrico)
# Para obtener el modelo de regresión lineal 
# Ejercicio: analizar por regresión lineal simple el dataset "Hitters" (de ISLR) usando como variable de respuesta, esto es Y,
# la variable denominada "Salary"
?Hitters
Data_Hitters<-Hitters
boxplot(Data_Hitters$Salary)
summary(Data_Hitters)

#Ejercicio de regresión haciendo uso de dataset de entrenamiento y validación#

plot(Data_Boston$lstat,Data_Boston$medv)
abline(lm.fit, lwd = 1, col = "red")
P_lm<-predict(lm.fit,lstat=Data_Boston$lstat)
plot(x=Data_Boston$lstat,y=P_lm)



# Modelos de regresión con el dataset completo 
lm_Boston<-lm(data=Data_Boston, formula = medv~lstat)
lm_Boston
summary(lm_Boston)
lm_Boston_2<-lm(data=Data_Boston, formula = medv~rm)
lm_Boston_2
summary(lm_Boston_2)

# Creación de los dataset Train (Entrenamiento) y Test (Prueba) 
set.seed(1)
Prop<-0.7
Indices_Train<-sample(1:nrow(Data_Boston),size=(round(nrow(Data_Boston)*Prop,0)),replace=FALSE)
Train_Boston<-Data_Boston[Indices_Train,]
Test_Boston<-Data_Boston[-Indices_Train,]

# Para lstat (porcentaje de personas de bajo estrato en el barrio donde se ubica la casa) Train-Test
lm_lstat_Train<-lm(medv~lstat,data=Train_Boston)
summary(lm_lstat_Train)
Predict_lstat_Test<-predict(lm_lstat_Train,Test_Boston)
RSE<-sqrt((sum((Test_Boston$medv-Predict_lstat_Test)^2))/(nrow(Test_Boston)-2))
MSE<-mean((Test_Boston$medv-Predict_lstat_Test)^2)
RMSE<-sqrt(MSE)

# Para rm (promedio del número de habitaciones por vivienda) Train-Test
lm_rm_Train<-lm(medv~rm,data=Train_Boston)
summary(lm_rm_Train)
Predict_rm_Test<-predict(lm_rm_Train,Test_Boston)
RSE<-sqrt((sum((Test_Boston$medv-Predict_rm_Test)^2))/(nrow(Test_Boston)-2))
MSE<-mean((Test_Boston$medv-Predict_rm_Test)^2)
RMSE<-sqrt(MSE)

