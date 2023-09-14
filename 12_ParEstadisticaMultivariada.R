#############################################
# 1.2 Parámetros de Estadística Multivariada #
#############################################

# Cargue de librerías #

library(MASS) # Librería con datasets y herramientas del libro 
# Modern Applied Statistics with S
library(ISLR) # Librería con datasets y herramientas del libro An Intruduction
# to Statistical Learning with R
library(ggplot2) # Librería para elaboración de gráficas con mejoramiento 
# visual respecto a las de R-Base
library(GGally) # Librería que contiene la función para elaborar pares de 
# gráficos de dispersión usando las herramientas visuales de ggplot2
library(corrplot) # Librería para la exploración visual de la matriz de 
# correlaciones de un conjunto de datos

# Cargue de conjuntos de datos#

# Tabla 1.1 #

ci<-c(125,86,119,113,101,143,132,106,121,109,88,116,102,75,90,
      109,104,110,96,101,95,117,115,138,85)
peso<-c(2536,2505,2652,2573,2382,2443,2617,2556,2489,2415,2434,
        2491,2345,2350,2536,2577,2464,2571,2550,2437,2472,2580,
        2436,2200,2851)
edad<-c(28,31,32,20,30,30,27,36,34,29,27,24,26,23,24,22,35,24,
        24,23,36,21,39,41,17)

tabla1.1<-data.frame(peso,edad,ci) 
tabla1.1
summary(tabla1.1)
# Gráficos de dispersión
pairs(tabla1.1)
# Diagrama de caja (boxplot)
boxplot(tabla1.1)
boxplot(scale(tabla1.1)) # scale estandariza los datos, facilita el comparativo de
#dispersión de datos pues elimina la escala de valores en cada variable (algunas
# variables están en decenas, centenas o miles)

# Boston #

?Boston # Información del dataset que hace parte del paquete MASS.
# Este dataset ontiene información del valor medio de una casa
# para 506 vecindarios en Boston, este incluye parámetros
# que proporcionan información e las características de cada casa.
# Vamos a usar la variable mdev como variable de respuesta que tiene datos 
# en miles de dólares
Data_Boston<-Boston
head(Data_Boston)
# attach(Boston) # Para usar las variables del dataset directamente sin 
# cargar el dataset a una variable tipo dataframe
summary(Data_Boston) # Resumen de los datos detallados por variable
boxplot(Data_Boston)
boxplot(scale(Data_Boston))
# Nota: es necesario ampliar el pane de plots para que RStudio realice la gráfica
pairs(Data_Boston)
ggpairs_Boston<-ggpairs(Data_Boston,
        columns = 1:14,
        title = "Precio de las casas en Boston") # Mejora visualmente los gráficos de dispersión
ggpairs_Boston
boxplot_medv<-ggplot(Data_Boston,aes(x="",y=medv))+
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  stat_summary(fun=sd, geom="point", shape=20, size=5, color="blue", fill="blue")
boxplot_medv

# Hitters #

?Hitters # Información del dataset que hace parte de ISLR. Este dataset contiene 
# 322 observaciones de la MLB de las temporadas 86-87, son datos de algunos jugadores
# sus características o resultados de juego en la temporada 86, su carrera 
# y su salario en 1987
Data_Hitters<-Hitters
head(Data_Hitters)
summary(Data_Hitters)
# Resumen de los datos detallados por variable
boxplot(Data_Hitters)
boxplot(scale(Data_Hitters))
# No todos las variables son numéricas, adicionalmente hay valores faltantes
sum(is.na(Data_Hitters$Salary))
Data_Hitters<-na.omit(Data_Hitters) #Elimina todas las observaciones que tengan 
# al menos un NA en alguna de las 20 variables
sum(is.na(Data_Hitters$Salary))
dim(Data_Hitters) # Genera el tamaño del dataset filas y columnas
Data_Hitters<-Data_Hitters[,-c(14,15,20)] #Excluyo las variables no numéricas
boxplot(scale(Data_Hitters))
# Nota: es necesario ampliar el pane de plots para que RStudio realice la gráfica
pairs(Data_Hitters)
ggpairs_Hitters<-ggpairs(Data_Hitters,
                        columns = 1:17,
                        title = "Salario Jugadores MLB 86-87") # Mejora 
# visualmente los gráficos de dispersión
ggpairs_Hitters
boxplot_Salary<-ggplot(Data_Hitters,aes(x="",y=Salary))+
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  stat_summary(fun=sd, geom="point", shape=20, size=5, color="blue", fill="blue")
boxplot_Salary

###########################################
# 1.2.1 Vector de Medias (Valor Esperado) #
###########################################
# Para un conjunto de datos con n observaciones y p variables se calcula un
# vector de medias de tamaño p.
# Para cada variable se calcula el promedio de las n observaciones

VMedias_tabla1.1<-apply(tabla1.1, 2,mean)
VMedias_tabla1.1

VMedias_Boston<-round(apply(Data_Boston, 2,mean),2)
VMedias_Boston

VMedias_Hitters<-round(apply(Data_Hitters, 2,mean),2)
VMedias_Hitters

##############################
# 1.2.2 Matriz de Covarianza #
##############################

# Para un conjunto de datos es una matriz que tiene (en la diagonal)
# los valores de varianza de cada variable o dimensión del dataset
# y los valores de las covarianza entre pares de variables del dataset
# Las covarianzas se calculan como la sumatoria del producto entre coordenadas 
# del punto dividido en la cantidad de datos un observaciones
# la covarianza tienen "dirección" (positiva o negativa).
# La matriz de covarianza tiene un tamaño pxp donde p es el número de variables
# del conjunto de datos

# Matriz de  covarianzas para tabla1.1
# (entradas redondeadas a 3 cifras)
round(cov(tabla1.1),3)
# varianza total
sum(diag(cov(tabla1.1)))
# varianza generalizada
det(cov(tabla1.1))

# Matriz de covarianzas para Data_Boston
# (entradas redondeadas a 3 cifras)
round(cov(Data_Boston),3)
# varianza total
sum(diag(cov(Data_Boston)))
# varianza generalizada
det(cov(Data_Boston))

# Matriz de covarianzas para Data_Hitters
# (entradas redondeadas a 3 cifras)
round(cov(Data_Hitters),3)
# varianza total
sum(diag(cov(Data_Hitters)))
# varianza generalizada
det(cov(Data_Hitters))

#################################
# 1.2.3 Matriz de correlaciones #
#################################

M_Corr_tabla1.1<-round(cor(tabla1.1),3)
M_Corr_Boston<-round(cor(Data_Boston),3)
M_Corr_Hitters<-round(cor(Data_Hitters),3)

#######################################
# 1.3 Evaluación de Multicolinealidad #
#######################################

# Cuando existe dependencia entre las variables independientes (esto es, no son del todo independientes) se habla de colinealidad
# La colinealidad afecta la estimación de los coeficientes del modelo y de esta manera aumenta el sesgo (bias) del modelo 
# debido a problemas de precisión en la estimación de los coeficientes. Para calcular los coeficientes se usan
# los determinantes de las matrices de variables independientes son cercanos a cero (esto hace que los resultados
# de estimación pierdan precisión) 
# Se pueden usar dos opciones para evaluar la colinealidad
# 1) Matriz de correlaciones y 2) Factor de Inflación de Varizana (VIF)
# La primera opción de análisis se hace sobre los datos
# La segunda opción se hace sobre el modelo

###########################
# Matriz de correlaciones #
###########################

# Este análisis es posible hacerlo antes de generar un modelo e regresión lineal
# Las correlaciones superiores a 0.8 podrían ser observadas como colinealidad

corrplot(M_Corr_tabla1.1)
corrplot(M_Corr_tabla1.1, type="lower", tl.col="black", tl.srt=45, method="shade")

corrplot(M_Corr_Boston, type="upper", tl.col="black", tl.srt=45, method="square")
corrplot(M_Corr_Boston, type="upper", tl.col="black", tl.srt=45, method="number")

corrplot(M_Corr_Hitters, type="lower", tl.col="black", tl.srt=45, method="color")
corrplot(M_Corr_Hitters, type="lower", tl.col="black", tl.srt=45, 
         method="pie")

# Función para construir la Matriz de P-Valores de correlación
# Se buscar conocer si los resultados de correlación de la muestra se pueden
# inferir a la población completa. La hipótesis nula es R=0 (o muy próximo)
# y la alternativa es que R<>0.
cor.mtest <- function(M, ...) {
  mat <- as.matrix(M)
  n <- ncol(M)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(M[, i], M[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(M)
  p.mat
}
PMat<-cor.mtest(Data_Hitters)
# Definiendo el nivel de significancia, generalmente 0.05, la gráfica me muestra
# que resultados de correlación no puedo tomar como diferentes de cero, esto es, 
# me dice si puedo o no rechazar la hipótesis nula
# Esta opción me marca con una X lo que debo asumir como "no correlacionado" 
corrplot(M_Corr_Hitters, type="lower", tl.col="black", tl.srt=45, 
         method="pie", p.mat=PMat,sig.level = 0.01, diag=FALSE)

# Esta opción me deja en blanco lo que debo asumir como "no correlacionado"
# incluyo los datos del coeficiente de correlación
corrplot(M_Corr_Hitters, type="lower", tl.col="black", tl.srt=45, 
         method="color", addCoef.col = "black", number.cex = 0.5, p.mat=PMat
         ,sig.level = 0.05, insig = "blank", diag=FALSE)
# En Hitters existe correlación entre las variables independientes, esto es, 
# algunos predictores están en función de otros. 

#########################################
# Factor de Inflación de Varizana (VIF) #
#########################################

# Este método se trabajará cuando se genere el modelo de regresión multiple ya que
# se requiere el modelo generado para hacer el análisis
