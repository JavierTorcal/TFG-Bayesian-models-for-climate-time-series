#Este script recoge el código con el cual se generan los gráficos correspondientes al Capítulo 4 de resultados del TFG:
#Modelos bayesianos para series temporales climáticas.
#Se usan los conjuntos de datos Tmax.data (Soria), Tmax.dataZ (Zaragoza) y Tmax.dataB (Bilbao).
#Dichas series se pueden descargar de la base de datos ECA&D (https://www.ecad.eu/).

#Cargar los datos
setwd("C:/Users/Javier/Desktop/TFG")
Tmax.data<-read.table('Datos-Soria.txt',header=TRUE,dec='.',sep=',')
Tmax.dataZ<-read.table('Datos-Zaragoza.txt',header=TRUE,dec='.',sep=',')
Tmax.dataB<-read.table('Datos-Bilbao.txt',header=TRUE,dec='.',sep=',')

#Adecuarlos
Tmax.data<-fun_depurar_datos(Tmax.data)
Tmax.dataZ<-fun_depurar_datos(Tmax.dataZ)
Tmax.dataB<-fun_depurar_datos(Tmax.dataB)


#Gráfico con librería "ggplot2"
library("ggplot2")


###Tendencia lineal


#Gráfico Tendencia lineal 
plot_tendencia_lineal<-function(datos,ciudad){
aux.x<-tapply(datos$YEAR,datos$YEAR,mean)
aux.tmax<-tapply(datos$TX,datos$YEAR,mean)

ggdf<-NULL
ggdf$X<-aux.x
ggdf$Y<-aux.tmax
ggdf<-as.data.frame(ggdf)

ggplot(data=ggdf,aes(y=Y,x=X)) + geom_point()+stat_smooth(method = loess)+ xlab("Año")+ylab("T máx media (ºC)")+
  ggtitle(ciudad)+ theme(plot.title = element_text(hjust = 0.5),
                          axis.text.x = element_text(face="bold",color="black",size=10),
                          axis.text.y = element_text(face="bold",color="black",size=10),
                          axis.title=element_text(size=10,face="bold")) }

#Soria
plot_tendencia_lineal(Tmax.data,"Soria")
#Zaragoza
plot_tendencia_lineal(Tmax.dataZ,"Zaragoza")
#Bilbao
plot_tendencia_lineal(Tmax.dataZ,"Bilbao")


#Gráfico Tendencia lineal conjunto
aux.x<-tapply(Tmax.data$YEAR,Tmax.data$YEAR,mean)
aux.tmax<-tapply(Tmax.data$TX,Tmax.data$YEAR,mean)
aux.tmaxB<-tapply(Tmax.dataB$TX,Tmax.dataB$YEAR,mean)
aux.tmaxZ<-tapply(Tmax.dataZ$TX,Tmax.dataZ$YEAR,mean)
ggdf<-NULL
ggdf$X<-aux.x
ggdf$Y<-aux.tmax
ggdf$YB<-aux.tmaxB
ggdf$YZ<-aux.tmaxZ
ggdf<-as.data.frame(ggdf)


ggplot(data=ggdf) + geom_point(aes(y=Y,x=X),colour="black",alpha = 1/5) +
  stat_smooth(aes(y=Y,x=X),method = loess, colour="black")+ xlab("Año")+ylab("T máx media (ºC)")+
  stat_smooth(aes(y=YB,x=X),method = loess,colour="blue")+ geom_point(aes(y=YB,x=X),colour="blue",alpha = 1/5)+
  stat_smooth(aes(y=YZ,x=X),method = loess, colour="red")+ geom_point(aes(y=YZ,x=X),colour="red",alpha = 1/5)+
  ylim(16,22.5)+theme(plot.title = element_text(hjust = 0.5),
                      axis.text.x = element_text(face="bold",color="black",size=10),
                      axis.text.y = element_text(face="bold",color="black",size=10),
                      axis.title=element_text(size=10,face="bold") )


###Estacionalidad
##Justificación del uso de dos armónicos para la estacionalidad.

library(zoo)


plot_estacionalidad<-function(datos,ciudad){
require(zoo)
y.roll<-rollapply(datos$TX ,FUN=mean, width=30, align='center', fill=NA, partial=T, na.rm=TRUE)
model1 <- lm(y.roll ~ YEAR + sin1t + cos1t, data=datos)
model1.predict<-predict(model1,newdata=datos)
model2 <- lm(y.roll ~ YEAR + sin1t + cos1t + sin2t + cos2t, data=datos)
model2.predict<-predict(model2,newdata=datos)

plot(rowMeans(matrix(y.roll, nrow = 365),na.rm=TRUE), font=2, font.lab=2,
     xlab = "Día del año", ylab = "T.Máx", ylim = c(min(y.roll,na.rm=T)-1,max(y.roll,na.rm=T)+1), 
     col = "gray", type = "l",main=ciudad)
for (i in 1:70) {
  lines(x = 1:365, y = matrix(y.roll, nrow = 365)[, i], type = "l", col = "gray")
}

lines(x = 1:365, y = rowMeans(matrix(y.roll, nrow = 365),na.rm=TRUE), col = "black",lwd=3)

lines(rowMeans(matrix(model1.predict,nrow = 365),na.rm=TRUE), col = "red",lwd=3)
lines(rowMeans(matrix(model2.predict,nrow = 365),na.rm=TRUE), col = "green",lwd=3)
}

#Soria
plot_estacionalidad(Tmax.data,"Soria")
#Zaragoza
plot_estacionalidad(Tmax.dataZ,"Zaragoza")
#Bilbao
plot_estacionalidad(Tmax.dataB,"Bilbao")


#Gráfico ACF
##Correlación serial
plot_acf<-function(datos,ciudad){
  y<-datos$TX
  model2 <- lm(y ~ YEAR + sin1t + cos1t + sin2t + cos2t, data=datos)
  bacf <- acf(residuals(model2),na.action = na.pass,4,plot=F)
  bacfdf <- with(bacf, data.frame(lag, acf))
  ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0),lwd=2, color = 'blue')+
    geom_hline(aes(yintercept = 0.05), linetype = 2,lwd=2, color = 'red')+
    ggtitle(ciudad)+ ylab("ACF")+
    theme(plot.title = element_text(face="bold",hjust = 0.5))+
    theme(axis.text.x = element_text(face="bold",color="black",size=12))+
    theme(axis.text.y = element_text(face="bold",color="black",size=12))+
    theme(axis.title=element_text(size=14,face="bold"))  }

                                      
    


#Soria
plot_acf(Tmax.data,"Soria")
#Zaragoza
plot_acf(Tmax.dataZ,"Zaragoza")
#Bilbao
plot_acf(Tmax.dataB,"Bilbao")



plot_acf2<-function(datos,ciudad){
  y<-datos$TX
  model3 <- lm(y ~ YEAR + sin1t + cos1t + sin2t + cos2t + Ylag, data=datos)
  bacf <- acf(residuals(model3),na.action = na.pass,4,plot=F)
  bacfdf <- with(bacf, data.frame(lag, acf))
  ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0),lwd=2, color = 'blue')+
    geom_hline(aes(yintercept = 0.05), linetype = 2,lwd=2, color = 'red')+
    ggtitle(ciudad)+ ylab("ACF")+
    theme(plot.title = element_text(face="bold",hjust = 0.5))+
    theme(axis.text.x = element_text(face="bold",color="black",size=12))+
    theme(axis.text.y = element_text(face="bold",color="black",size=12))+
    theme(axis.title=element_text(size=14,face="bold"))  }

#Soria
plot_acf2(Tmax.data,"Soria")
#Zaragoza
plot_acf2(Tmax.dataZ,"Zaragoza")
#Bilbao
plot_acf2(Tmax.dataB,"Bilbao")




#MODEL7 (final)
###Modelo con interacción (autocorrelación-estacionalidad)

#Soria

model7<-lm(TX ~ YEAR*(sin1t + cos1t + sin2t + cos2t) + Ylag*(sin1t + cos1t + sin2t + cos2t),data=Tmax.data) 
summary(model7)
step(model7)

model7.SO <-lm(TX ~ YEAR + Ylag*(sin1t+cos1t) + sin2t + cos2t, data=Tmax.data)

aux.coef<-coefficients(model7.SO)


#Zaragoza 
y<-Tmax.dataZ$TX
model7<-lm(y ~ YEAR*(sin1t + cos1t + sin2t + cos2t) + Ylag*(sin1t + cos1t + sin2t + cos2t),data=Tmax.dataZ) 
step(model7)

model7.Z <-lm(TX ~ YEAR+ YEAR:(sin1t+cos1t)+ Ylag*(sin1t+cos1t+ sin2t + cos2t),data=Tmax.dataZ )

aux.coefZ<-coefficients(model7.Z)



#Bilbao
y<-Tmax.dataB$TX
model7<-lm(y ~ YEAR*(sin1t + cos1t + sin2t + cos2t) + Ylag*(sin1t + cos1t + sin2t + cos2t),data=Tmax.dataB) 
step(model7)

model7.B<-lm(TX ~ YEAR+ YEAR:(sin1t+cos1t)+ Ylag*(sin1t+cos1t+ sin2t + cos2t),data=Tmax.dataB)

aux.coefB<-coefficients(model7.B)


library(ggplot2)
library(latex2exp)

t<-1:25550
sin1t <- sin(2 * pi * t / 365)
cos1t <- cos(2 * pi * t / 365)
sin2t <- sin(4 * pi * t / 365)
cos2t <- cos(4 * pi * t / 365)

#Gráfico Conjunto
ggdf<-data.frame(Tmax.data$YEAR_DAY,aux.coef[3]+aux.coef[8]*sin1t+aux.coef[9]*cos1t,
                 aux.coefZ[3]+aux.coefZ[10]*sin1t+aux.coefZ[11]*cos1t+aux.coefZ[12]*sin2t+aux.coefZ[13]*cos2t,
                 aux.coefB[3]+aux.coefB[10]*sin1t+aux.coefB[11]*cos1t+aux.coefB[12]*sin2t+aux.coefB[13]*cos2t)
names(ggdf)<-c("Dia","Interaccion.Soria","Interaccion.Zaragoza","Interaccion.Bilbao")
ggplot(ggdf) + 
  geom_point(aes(Dia,Interaccion.Soria),color="black",size=1)+
  geom_point(aes(Dia,Interaccion.Zaragoza),color="red",size=1)+
  geom_point(aes(Dia,Interaccion.Bilbao),color="blue",size=1)+
  ggtitle(TeX(r'($Y_{t-1}$ según la estacionalidad)'))+ylab("Coeficiente")+xlab("Día del año")+
  ylim(c(0.44,0.75)) +theme(plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(face="bold",color="black",size=10),
  axis.text.y = element_text(face="bold",color="black",size=10),
  axis.title=element_text(size=10,face="bold")) 





#Soria
ggplot(ggdf,aes(x=Dia,y=Interaccion.Soria)) + 
  geom_point(color="blue",size=1)+ylab("Coeficiente")+xlab("Día del año")+
  ggtitle(TeX(r'($Y_{t-1}$ según la estacionalidad)'))+
  theme(plot.title = element_text(hjust = 0.5),
         axis.text.x = element_text(face="bold",color="black",size=10),
         axis.text.y = element_text(face="bold",color="black",size=10),
         axis.title=element_text(size=10,face="bold")) 

#Zaragoza
ggplot(ggdf,aes(x=Dia,y=Interaccion.Zaragoza)) + 
  geom_point(color="blue",size=1)+ylab("Coeficiente")+xlab("Día del año")+
  ggtitle(TeX(r'($Y_{t-1}$ según la estacionalidad)'))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold")) 
 

#Bilbao
ggplot(ggdf,aes(x=Dia,y=Interaccion.Bilbao)) + 
  geom_point(color="blue",size=1)+ylab("Coeficiente")+xlab("Día del año")+
  ggtitle(TeX(r'($Y_{t-1}$ según la estacionalidad)'))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold")) 


#Interaccion Tendencia lineal-Estacionalidad
plot_inter_Year<-function(coef){
  nd<-data.frame(Tmax.data$YEAR_DAY,coef[2]+coef[8]*sin1t+coef[9]*cos1t)
  names(nd)<-c("Dia","Interaccion")
  ggplot(nd,aes(Dia,Interaccion)) + 
    geom_point(color="blue",size=1)+ylab("ºC / año")+xlab("Día del año")+
    ggtitle("Tendencia lineal según la estacionalidad")+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold",color="black",size=10),
          axis.text.y = element_text(face="bold",color="black",size=10),
          axis.title=element_text(size=10,face="bold")) }

#Zaragoza
plot_inter_Year(aux.coefZ)

#Bilbao
plot_inter_Year(aux.coefB)


ggdf2<-data.frame(Tmax.data$YEAR_DAY,aux.coefZ[2]+aux.coefZ[8]*sin1t+aux.coefZ[9]*cos1t,aux.coefB[2]+aux.coefB[8]*sin1t+aux.coefB[9]*cos1t)
names(ggdf2)<-c("Dia","Interaccion.Zaragoza","Interaccion.Bilbao")
ggplot(ggdf2) + ylab("ºC / año")+xlab("Día del año")+
  geom_point(aes(Dia,Interaccion.Zaragoza),color="red")+
  geom_point(aes(Dia,Interaccion.Bilbao),color="blue")+
  ggtitle("Tendencia lineal según la estacionalidad")+
  theme(plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(face="bold",color="black",size=10),
  axis.text.y = element_text(face="bold",color="black",size=10),
  axis.title=element_text(size=12))+ ylim(c(0,0.25))+
  scale_y_continuous(breaks=seq(0,0.25,0.005))



###Modelar la Varianza

#Soria
model7.SO <-lm(TX ~ YEAR + Ylag*(sin1t+cos1t) + sin2t + cos2t, data=Tmax.data)
model7.predict<-predict(model7.SO,newdata=Tmax.data)
resid7<-Tmax.data$TX-model7.predict
epsilon_cuadrado<-resid7^2

#Zaragoza
model7.Z <-lm(TX ~ YEAR+ YEAR:(sin1t+cos1t)+ Ylag*(sin1t+cos1t+ sin2t + cos2t),data=Tmax.dataZ )
model7.predictZ<-predict(model7.Z,newdata=Tmax.dataZ)
resid7Z<-Tmax.dataZ$TX-model7.predictZ
epsilon_cuadradoZ<-resid7Z^2

#Bilbao
model7.B<-lm(TX ~ YEAR+ YEAR:(sin1t+cos1t)+ Ylag*(sin1t+cos1t+ sin2t + cos2t),data=Tmax.dataB)
model7.predictB<-predict(model7.B,newdata=Tmax.dataB)
resid7B<-Tmax.dataB$TX-model7.predictB
epsilon_cuadradoB<-resid7B^2


library(zoo)

epsilon.roll<-rollapply(epsilon_cuadrado ,FUN=mean, width=15, align='center', fill=NA, partial=T, na.rm=TRUE)
epsilon.rollZ<-rollapply(epsilon_cuadradoZ ,FUN=mean, width=15, align='center', fill=NA, partial=T, na.rm=TRUE)
epsilon.rollB<-rollapply(epsilon_cuadradoB ,FUN=mean, width=15, align='center', fill=NA, partial=T, na.rm=TRUE)

epsilon.dia<-rowMeans(matrix(epsilon.roll,nrow=365),na.rm=T)
epsilon.diaZ<-rowMeans(matrix(epsilon.rollZ,nrow=365),na.rm=T)
epsilon.diaB<-rowMeans(matrix(epsilon.rollB,nrow=365),na.rm=T)



ggdf3<-data.frame(Tmax.data$YEAR_DAY,epsilon.dia,epsilon.diaZ,epsilon.diaB)

names(ggdf3)<-c("Día","Varianza","VarianzaZ","VarianzaB")

ggplot(ggdf3) + geom_point(color="black",aes(Día,Varianza))+
  geom_line(aes(Día,Varianza))+ geom_point(color="red",aes(Día,VarianzaZ))+
  geom_line(aes(Día,VarianzaZ))+ geom_point(color="blue",aes(Día,VarianzaB))+
  geom_line(aes(Día,VarianzaB))+ggtitle("Varianza según estacionalidad")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )


#Soria

ggplot(ggdf3) + geom_point(color="blue",aes(Día,Varianza))+
  geom_line(aes(Día,Varianza))+ylab("Varianza")+
  ggtitle("Varianza según estacionalidad")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )

#Zaragoza

ggplot(ggdf3) + geom_point(color="blue",aes(Día,VarianzaZ))+
  geom_line(aes(Día,VarianzaZ))+ ylab("Varianza")+
  ggtitle("Varianza según estacionalidad")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )

#Bilbao

ggplot(ggdf3) + geom_point(color="blue",aes(Día,VarianzaB))+
  geom_line(aes(Día,VarianzaB))+ ylab("Varianza")+
  ggtitle("Varianza según estacionalidad")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )


epsilon.sinroll<-rowMeans(matrix(epsilon_cuadrado,nrow=365),na.rm=T)
epsilon.sinrollZ<-rowMeans(matrix(epsilon_cuadradoZ,nrow=365),na.rm=T)
epsilon.sinrollB<-rowMeans(matrix(epsilon_cuadradoB,nrow=365),na.rm=T)
ggdf4<-data.frame(Tmax.data$YEAR_DAY,epsilon.sinroll,epsilon.sinrollZ,epsilon.sinrollB)
ggdf4$lag<-lag(c(NA,epsilon.sinroll[-length(epsilon.sinroll)]),k=1)
ggdf4$lagZ<-lag(c(NA,epsilon.sinrollZ[-length(epsilon.sinrollZ)]),k=1)
ggdf4$lagB<-lag(c(NA,epsilon.sinrollB[-length(epsilon.sinrollB)]),k=1)
names(ggdf4)<-c("Día","Varianza","VarianzaZ","VarianzaB","lag","lagZ","lagB")

#Varianza respecto lag
ggplot(ggdf4) + geom_point(aes(lag,Varianza),color="#BDBDBD",size=1)+
  geom_point(aes(lagZ,VarianzaZ),color="#F78181",size=1)+
  geom_point(aes(lagB,VarianzaB),color="#A9BCF5",size=1)+
  stat_smooth(aes(lag,Varianza),color="black",lwd=1.5)+
  stat_smooth(aes(lagZ,VarianzaZ),color="red",lwd=1.5)+ 
  stat_smooth(aes(lagB,VarianzaB),color="blue",lwd=1.5)+
  xlim(c(3,18))+ylim(c(5,15))+ggtitle("Varianza respecto lag")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )

#Soria
ggplot(ggdf4) + geom_point(aes(lag,Varianza),color="#A9BCF5",size=1)+
  stat_smooth(aes(lag,Varianza),color="blue",lwd=1.5)+
  ggtitle("Varianza respecto lag")+ ylab("Varianza")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )
  
  
#Zaragoza
ggplot(ggdf4) + geom_point(aes(lag,VarianzaZ),color="#A9BCF5",size=1)+
  stat_smooth(aes(lag,VarianzaZ),color="blue",lwd=1.5)+
  ggtitle("Varianza respecto lag")+ ylab("Varianza")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )

#Bilbao
ggplot(ggdf4) + geom_point(aes(lag,VarianzaB),color="#A9BCF5",size=1)+
  stat_smooth(aes(lag,VarianzaB),color="blue",lwd=1.5)+
  ggtitle("Varianza respecto lag")+ ylab("Varianza")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )



#Varianza respecto Year

ggdf5<-data.frame(Tmax.data$YEAR,epsilon_cuadrado,epsilon_cuadradoZ,epsilon_cuadradoB)
names(ggdf5)<-c("YEAR","Varianza","VarianzaZ","VarianzaB")
aux.varianza<-tapply(ggdf5$Varianza,ggdf5$YEAR,mean,na.rm=T)
aux.varianzaZ<-tapply(ggdf5$VarianzaZ,ggdf5$YEAR,mean,na.rm=T)
aux.varianzaB<-tapply(ggdf5$VarianzaB,ggdf5$YEAR,mean,na.rm=T)

ggdf6<-data.frame(c(1951:2020),aux.varianza,aux.varianzaZ,aux.varianzaB)
names(ggdf6)<-c("YEAR","aux.v","aux.vZ","aux.vB")  

ggplot(ggdf6) + geom_point(aes(YEAR,aux.v),color="black",alpha=1/5)+ stat_smooth(aes(YEAR,aux.v),color="black")+
  geom_point(aes(YEAR,aux.vZ),color="red",alpha=1/5)+ stat_smooth(aes(YEAR,aux.vZ),color="red")+
  geom_point(aes(YEAR,aux.vB),color="blue",alpha=1/5)+ stat_smooth(aes(YEAR,aux.vB),color="blue")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )+ xlab("Año")+
  ylab("Varianza")+ggtitle("Varianza media según el año")

#Soria
ggplot(ggdf6) + geom_point(aes(YEAR,aux.v),color="blue",alpha=1/5)+
  stat_smooth(aes(YEAR,aux.v),color="blue",method=loess)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )+ xlab("Año")+
  ylab("Varianza")+ggtitle("Varianza media según el año")

#Zaragoza
ggplot(ggdf6) + geom_point(aes(YEAR,aux.vZ),color="blue",alpha=1/5)+
  stat_smooth(aes(YEAR,aux.vZ),color="blue",method=loess)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )+ xlab("Año")+
  ylab("Varianza")+ggtitle("Varianza media según el año")

#Bilbao
ggplot(ggdf6) + geom_point(aes(YEAR,aux.vB),color="blue",alpha=1/5)+
  stat_smooth(aes(YEAR,aux.vB),color="blue",method=loess)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(face="bold",color="black",size=10),
        axis.text.y = element_text(face="bold",color="black",size=10),
        axis.title=element_text(size=10,face="bold") )+ xlab("Año")+
  ylab("Varianza")+ggtitle("Varianza media según el año")
  
