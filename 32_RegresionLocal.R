#######################
# 3.2 Regresión Local #
#######################

loess_medv<-loess(medv~lstat,data=Data_Boston,span = 1,degree = 1)
loess_medv
# Con el modelo es posible hacer predicciones
P_loess<-predict(loess_medv,lstat=Data_Boston$lstat)
plot(x=Data_Boston$lstat,y=P_loess)
data<-as.data.frame(cbind(Data_Boston$lstat,P_loess))
ggfig_loess<- ggplot(data,aes(V1,P_loess)) +
  geom_point()
ggfig_loess
