library(ISLR) # Librería con datasets y herramientas del libro An Intruduction to Statistical Learning with R
library(MASS)
library(leaps) # Reduccióin de dimensionalidad}
library(ggplot2)
library(GGally)
ggpairs_Boston<-ggpairs(Data_Boston,
                        columns = 1:14,
                        title = "Precio de las casas en Boston") # Mejora visualmente los gráficos de dispersión
ggpairs_Boston

lmint<-lm(medv~lstat*rm, data=Data_Boston)
summary(lmint)
# Y=Bo+B1X1+B2X2+B3X1X2 - X3=X1X2
# Ho = B<>0
# Ha = B=0
plot(lmint)
boxplot(Data_Boston$medv,fitted(lmint))

lmint2<-lm(medv~crim*chas*nox*rm*dis*rad*ptratio*black*lstat, data=Data_Boston)
summary(lmint2)

lmint3<-lm(medv~lstat*rm*ptratio, data=Data_Boston)
summary(lmint3)