library("bamlss")
library("xtable")

setwd("C:/Users/Javier/Desktop/TFG")
#####Soria
Tmax.data<-read.table('Datos-Soria.txt',header=TRUE,dec='.',sep=',')
Tmax.data<-fun_depurar_datos(Tmax.data)

#Primer modelo bayesiano (sin AR)
f2<-list(TX ~ sin1t + cos1t + (sin2t + cos2t)*YEAR,
         sigma ~ (sin1t + cos1t): YEAR)
bam2<-bamlss(f2,family="gaussian",data=Tmax.data,light=T)
summary(bam2)


#Segundo modelo bayesiano (varianza constante)
fmedia<-(TX ~ YEAR + sin1t + cos1t + sin2t + cos2t + Ylag*(sin1t + cos1t))
bam.media<-bamlss(fmedia,family="gaussian",data=Tmax.data,light=T)
summary(bam.media)


#Modelo bayesiano final, con stepwise
f3<-list(TX ~ YEAR + sin1t + cos1t + sin2t + cos2t + Ylag*(sin1t + cos1t),
         sigma ~ Ylag*(sin1t + cos1t + sin2t + cos2t))

bam3<-bamlss(f3,family="gaussian",data=Tmax.data,light=T)
summary(bam3)

#Para representación de los parámetros
xtable((summary(bam3))$"model.matrix"$mu[c(2,3,4,5,6,7,8,9),c(1,2)],digits=3,caption="Distribución posteriori de mu")
xtable((summary(bam3))$"model.matrix"$sigma[c(2,3,4,5,6,7,8,9,10),c(1,2)],digits=4,caption="Distribución posteriori de sigma")



dic.SO<-DIC(bam2,bam.media,bam3) #Comparar mejor modelo
dic.SO#Notar que el mejor DIC es el menor (bam3)

###Generación de gráficos para representar las interacciones entre términos
#Parámetros para la tendencia lineal
sa.year<-samples(bam3,term="YEAR")

library(latex2exp)#Para letra LaTex
#Ylag-Estacionalidad (Media)
plot(density(sa.ylag[,1],weights = rep(1/1000000,1001)),xlim=c(0.70,0.76),
     main=TeX(r'(Coeficiente de $Y_{t-1}$ según la estación, modelo de $\mu$)'),bty = "L",ylim=c(0,0.08),xlab=TeX(r'(Coeficiente de $Y_{t-1})'),ylab="Densidad",type="n",font=2,cex.lab=1.3,cex.main=1.3)
rect(xleft=quantile(sa.ylag[,1],p=0.025),xright=quantile(sa.ylag[,1],p=0.975),ybottom=0,ytop=0.08, col="gray",border=F,)
abline(v=quantile(sa.ylag[,1],p=c(0.025,0.5,0.975)),color="black",lty=3,lwd=2)
abline(v=quantile(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*180/365)+sa.ylag[,3]*cos(2*pi*180/365),p=0.5),col="red",lty=2,lwd=3)
abline(v=quantile(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*363/365)+sa.ylag[,3]*cos(2*pi*363/365),p=0.5),col="blue",lty=2,lwd=3)
abline(v=quantile(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*110/365)+sa.ylag[,3]*cos(2*pi*110/365),p=0.5),col="green",lty=2,lwd=3)
abline(v=quantile(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*266/365)+sa.ylag[,3]*cos(2*pi*266/365),p=0.5),col="yellow",lty=2,lwd=3)
polygon(density(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*363/365)+sa.ylag[,3]*cos(2*pi*363/365),weights = rep(1/1000000,1001)),
        col = "blue" , density = 50, angle = 90)
polygon(density(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*180/365)+sa.ylag[,3]*cos(2*pi*180/365),weights = rep(1/1000000,1001)),
        col = "red" , density = 50, angle = 90)
polygon(density(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*110/365)+sa.ylag[,3]*cos(2*pi*110/365),weights = rep(1/1000000,1001)),
        col = "green" , density = 50, angle = 90)
polygon(density(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*266/365)+sa.ylag[,3]*cos(2*pi*266/365),weights = rep(1/1000000,1001)),
        col = "yellow" , density = 50, angle = 90)
lines(density(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*363/365)+sa.ylag[,3]*cos(2*pi*363/365),weights = rep(1/1000000,1001)),col="blue",lwd=2)#29Dec
lines(density(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*180/365)+sa.ylag[,3]*cos(2*pi*180/365),weights = rep(1/1000000,1001)),col="red",lwd=2)#29JUN
lines(density(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*110/365)+sa.ylag[,3]*cos(2*pi*110/365),weights = rep(1/1000000,1001)),col="green",lwd=2)#20April
lines(density(sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*266/365)+sa.ylag[,3]*cos(2*pi*266/365),weights = rep(1/1000000,1001)),col="yellow",lwd=2)#23Sep


#Para obtener su valor
aux.matrix<-as.matrix(data.frame(sa.ylag[,1],
                                 sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*110/365)+sa.ylag[,3]*cos(2*pi*110/365),
                                 sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*180/365)+sa.ylag[,3]*cos(2*pi*180/365), 
                                 sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*266/365)+sa.ylag[,3]*cos(2*pi*266/365),
                                 sa.ylag[,1]+sa.ylag[,2]*sin(2*pi*363/365)+sa.ylag[,3]*cos(2*pi*363/365)))

round(quantile(aux.matrix[,1],c(0.5,0.025,0.975)),digits=3)
round(quantile(aux.matrix[,2],c(0.5,0.025,0.975)),digits=3)#20A
round(quantile(aux.matrix[,3],c(0.5,0.025,0.975)),digits=3)#29J
round(quantile(aux.matrix[,4],c(0.5,0.025,0.975)),digits=3)#23S
round(quantile(aux.matrix[,5],c(0.5,0.025,0.975)),digits=3)#29D




#Ylag-Estacionalidad (Varianza)
plot(density(sa.ylag[,4],weights = rep(1/1000000,1001)),xlim=c(0,0.02),ylim=c(0,0.27),
     main=TeX(r'(Coeficiente de $Y_{t-1}$ según la estación, modelo de $\sigma$)'),bty = "L",xlab=TeX(r'(Coeficiente de $Y_{t-1}$)'),ylab="Densidad",type="n",font=2,cex.lab=1.3,cex.main=1.3)
rect(xleft=quantile(sa.ylag[,4],p=0.025),xright=quantile(sa.ylag[,4],p=0.975),ybottom=0,ytop=300, col="gray",border=F,)
abline(v=quantile(sa.ylag[,4],p=c(0.025,0.5,0.975)),color="black",lty=3,lwd=2)
abline(v=quantile(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*180/365)+sa.ylag[,6]*cos(2*pi*180/365)+sa.ylag[,7]*sin(4*pi*180/365)+sa.ylag[,8]*cos(4*pi*180/365),p=0.5),col="red",lty=2,lwd=3)
abline(v=quantile(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*363/365)+sa.ylag[,6]*cos(2*pi*363/365)+sa.ylag[,7]*sin(4*pi*363/365)+sa.ylag[,8]*cos(4*pi*363/365),p=0.5),col="blue",lty=2,lwd=3)
abline(v=quantile(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*110/365)+sa.ylag[,6]*cos(2*pi*110/365)+sa.ylag[,7]*sin(4*pi*110/365)+sa.ylag[,8]*cos(4*pi*110/365),p=0.5),col="green",lty=2,lwd=3)
abline(v=quantile(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*266/365)+sa.ylag[,6]*cos(2*pi*266/365)+sa.ylag[,7]*sin(4*pi*266/365)+sa.ylag[,8]*cos(4*pi*266/365),p=0.5),col="yellow",lty=2,lwd=3)
polygon(density(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*180/365)+sa.ylag[,6]*cos(2*pi*180/365)+sa.ylag[,7]*sin(4*pi*180/365)+sa.ylag[,8]*cos(4*pi*180/365),weights = rep(1/1000000,1001)),
        col = "red" , density = 50, angle = 90)
polygon(density(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*363/365)+sa.ylag[,6]*cos(2*pi*363/365)+sa.ylag[,7]*sin(4*pi*363/365)+sa.ylag[,8]*cos(4*pi*363/365),weights = rep(1/1000000,1001)),
        col = "blue" , density = 50, angle = 90)
polygon(density(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*110/365)+sa.ylag[,6]*cos(2*pi*110/365)+sa.ylag[,7]*sin(4*pi*110/365)+sa.ylag[,8]*cos(4*pi*110/365),weights = rep(1/1000000,1001)),
        col = "green" , density = 50, angle = 90)
polygon(density(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*266/365)+sa.ylag[,6]*cos(2*pi*266/365)+sa.ylag[,7]*sin(4*pi*266/365)+sa.ylag[,8]*cos(4*pi*266/365),weights = rep(1/1000000,1001)),
        col = "yellow" , density = 50, angle = 90)
lines(density(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*363/365)+sa.ylag[,6]*cos(2*pi*363/365)+sa.ylag[,7]*sin(4*pi*363/365)+sa.ylag[,8]*cos(4*pi*363/365),weights = rep(1/1000000,1001)),col="blue",lwd=2)#29Dec
lines(density(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*180/365)+sa.ylag[,6]*cos(2*pi*180/365)+sa.ylag[,7]*sin(4*pi*180/365)+sa.ylag[,8]*cos(4*pi*180/365),weights = rep(1/1000000,1001)),col="red",lwd=2)#29JUN
lines(density(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*110/365)+sa.ylag[,6]*cos(2*pi*110/365)+sa.ylag[,7]*sin(4*pi*110/365)+sa.ylag[,8]*cos(4*pi*110/365),weights = rep(1/1000000,1001)),col="green",lwd=2)#20April
lines(density(sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*266/365)+sa.ylag[,6]*cos(2*pi*266/365)+sa.ylag[,7]*sin(4*pi*266/365)+sa.ylag[,8]*cos(4*pi*266/365),weights = rep(1/1000000,1001)),col="yellow",lwd=2)#23Sep


#Para obtener su valor
aux.matrix<-as.matrix(data.frame(sa.ylag[,4],
                                 sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*180/365)+sa.ylag[,6]*cos(2*pi*180/365)+sa.ylag[,7]*sin(4*pi*180/365)+sa.ylag[,8]*cos(4*pi*180/365),
                                 sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*363/365)+sa.ylag[,6]*cos(2*pi*363/365)+sa.ylag[,7]*sin(4*pi*363/365)+sa.ylag[,8]*cos(4*pi*363/365),
                                 sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*110/365)+sa.ylag[,6]*cos(2*pi*110/365)+sa.ylag[,7]*sin(4*pi*110/365)+sa.ylag[,8]*cos(4*pi*110/365),
                                 sa.ylag[,4]+sa.ylag[,5]*sin(2*pi*266/365)+sa.ylag[,6]*cos(2*pi*266/365)+sa.ylag[,7]*sin(4*pi*266/365)+sa.ylag[,8]*cos(4*pi*266/365)))



round(quantile(aux.matrix[,1],c(0.5,0.025,0.975)),digits=3)
round(quantile(aux.matrix[,2],c(0.5,0.025,0.975)),digits=3)#20A
round(quantile(aux.matrix[,3],c(0.5,0.025,0.975)),digits=3)#29J
round(quantile(aux.matrix[,4],c(0.5,0.025,0.975)),digits=3)#23S
round(quantile(aux.matrix[,5],c(0.5,0.025,0.975)),digits=3)#29D





##Zaragoza
Tmax.dataZ<-read.table('Datos-Zaragoza.txt',header=TRUE,dec='.',sep=',')
Tmax.dataZ<-fun_depurar_datos(Tmax.dataZ)
#Primer modelo bayesiano (sin AR)
f2<-list(TX ~ sin1t + cos1t + (sin2t + cos2t)*YEAR,
         sigma ~ (sin1t + cos1t): YEAR)
bam2<-bamlss(f2,family="gaussian",data=Tmax.dataZ,light=T)
summary(bam2)


#Segundo modelo bayesiano (varianza constante)
fmedia<-(TX~ YEAR  + Ylag*( sin1t + cos1t + sin2t + cos2t)+YEAR:(sin1t+cos1t))
bam.media<-bamlss(fmedia,family="gaussian",data=Tmax.dataZ,light=T)
summary(bam.media)


#Modelo bayesiano final, con stepwise
f3Z<-list(TX ~ YEAR  + Ylag*( sin1t + cos1t + sin2t + cos2t)+YEAR:(sin1t+cos1t),
         sigma ~ Ylag*(sin1t + cos1t + sin2t + cos2t))

bam3Z<-bamlss(f3Z,family="gaussian",data=Tmax.dataZ,light=T)
summary(bam3Z)

#Para representación de los parámetros
xtable((summary(bam3Z))$"model.matrix"$mu[2:13,c(1,2,4)],digits=3,caption="Distribución posteriori de mu")
xtable((summary(bam3Z))$"model.matrix"$sigma[2:11,c(1,2,4)],digits=4,caption="Distribución posteriori de sigma")

dic.Z<-DIC(bam2,bam.media,bam3Z)#Para comparar el mejor modelo
dic.Z #El mejor tiene menor DIC




#Box-Plot de distrib a posteriori de Year e interacciones

saZ.year<-samples(bam3Z,term="YEAR")
saZ.ylag<-samples(bam3Z,term="Ylag")
head(saZ.year)
head(saZ.ylag)



library(latex2exp)
##Box-Plot media Ylag 
aux.matrix<-as.matrix(data.frame(saZ.ylag[,1],
                                 saZ.ylag[,1]+saZ.ylag[,2]*sin(2*pi*110/365)+saZ.ylag[,3]*cos(2*pi*110/365)+saZ.ylag[,4]*sin(4*pi*110/365)+saZ.ylag[,5]*cos(4*pi*110/365),
                                 saZ.ylag[,1]+saZ.ylag[,2]*sin(2*pi*180/365)+saZ.ylag[,3]*cos(2*pi*180/365)+saZ.ylag[,4]*sin(4*pi*180/365)+saZ.ylag[,5]*cos(4*pi*180/365),
                                 saZ.ylag[,1]+saZ.ylag[,2]*sin(2*pi*266/365)+saZ.ylag[,3]*cos(2*pi*266/365)+saZ.ylag[,4]*sin(4*pi*266/365)+saZ.ylag[,5]*cos(4*pi*266/365),
                                 saZ.ylag[,1]+saZ.ylag[,2]*sin(2*pi*363/365)+saZ.ylag[,3]*cos(2*pi*363/365)+saZ.ylag[,4]*sin(4*pi*363/365)+saZ.ylag[,5]*cos(4*pi*363/365)))
                                  

#Para obtener su valor
round(quantile(aux.matrix[,1],c(0.5,0.025,0.975)),digits=3)
round(quantile(aux.matrix[,2],c(0.5,0.025,0.975)),digits=3)#20A
round(quantile(aux.matrix[,3],c(0.5,0.025,0.975)),digits=3)#29J
round(quantile(aux.matrix[,4],c(0.5,0.025,0.975)),digits=3)#23S
round(quantile(aux.matrix[,5],c(0.5,0.025,0.975)),digits=3)#29D



boxplot(aux.matrix, col=c("black","green","red","yellow","blue"),
        main=TeX(r'(Coefieciente de $Y_{t-1}$ ($\mu$))'),
        names=c( TeX(r'($Y_{t-1}$)'),"20A","29J","23S","29D"))
rect(ybottom=quantile(saZ.ylag[,1],p=0.025),ytop=quantile(saZ.ylag[,1],p=0.975),xleft=0,xright=10, col="gray",border=F,)
boxplot(aux.matrix,col=c("black","green","red","yellow","blue"),
        names=c( TeX(r'($Y_{t-1}$)'),"20A","29J","23S","29D"),add=T)

##Box-Plot media Year 
aux.matrix<-as.matrix(data.frame(saZ.year[,1],
                                 saZ.year[,1]+saZ.year[,2]*sin(2*pi*110/365)+saZ.year[,3]*cos(2*pi*110/365),
                                 saZ.year[,1]+saZ.year[,2]*sin(2*pi*180/365)+saZ.year[,3]*cos(2*pi*180/365), 
                                 saZ.year[,1]+saZ.year[,2]*sin(2*pi*266/365)+saZ.year[,3]*cos(2*pi*266/365),
                                 saZ.year[,1]+saZ.year[,2]*sin(2*pi*363/365)+saZ.year[,3]*cos(2*pi*363/365)))


#Para obtener su valor
round(quantile(aux.matrix[,1],c(0.5,0.025,0.975)),digits=3)
round(quantile(aux.matrix[,2],c(0.5,0.025,0.975)),digits=3)#20A
round(quantile(aux.matrix[,3],c(0.5,0.025,0.975)),digits=3)#29J
round(quantile(aux.matrix[,4],c(0.5,0.025,0.975)),digits=3)#23S
round(quantile(aux.matrix[,5],c(0.5,0.025,0.975)),digits=3)#29D


boxplot(aux.matrix, col=c("black","green","red","yellow","blue"),
        main=TeX(r'(Coefieciente de $Year_{t}$ ($\mu$))'),
        names=c( TeX(r'($Year_{t}$)'),"20A","29J","23S","29D"))

rect(ybottom=quantile(saZ.year[,1],p=0.025),ytop=quantile(saZ.year[,1],p=0.975),xleft=0,xright=10, col="gray",border=F,)
boxplot(aux.matrix,col=c("black","green","red","yellow","blue"),
        names=c( TeX(r'($Year_{t}$)'),"20A","29J","23S","29D"),add=T)



##Box-Plot sigma Ylag 
aux.matrix<-as.matrix(data.frame(saZ.ylag[,6],
                                 saZ.ylag[,6]+saZ.ylag[,7]*sin(2*pi*110/365)+saZ.ylag[,8]*cos(2*pi*110/365)+saZ.ylag[,9]*sin(4*pi*110/365)+saZ.ylag[,10]*cos(4*pi*110/365),
                                 saZ.ylag[,6]+saZ.ylag[,7]*sin(2*pi*180/365)+saZ.ylag[,8]*cos(2*pi*180/365)+saZ.ylag[,9]*sin(4*pi*180/365)+saZ.ylag[,10]*cos(4*pi*180/365), 
                                 saZ.ylag[,6]+saZ.ylag[,7]*sin(2*pi*266/365)+saZ.ylag[,8]*cos(2*pi*266/365)+saZ.ylag[,9]*sin(4*pi*266/365)+saZ.ylag[,10]*cos(4*pi*266/365),
                                 saZ.ylag[,6]+saZ.ylag[,7]*sin(2*pi*363/365)+saZ.ylag[,8]*cos(2*pi*363/365)+saZ.ylag[,9]*sin(4*pi*363/365)+saZ.ylag[,10]*cos(4*pi*363/365)))

#Para obtener su valor
round(quantile(aux.matrix[,1],c(0.5,0.025,0.975)),digits=3)
round(quantile(aux.matrix[,2],c(0.5,0.025,0.975)),digits=3)#20A
round(quantile(aux.matrix[,3],c(0.5,0.025,0.975)),digits=3)#29J
round(quantile(aux.matrix[,4],c(0.5,0.025,0.975)),digits=3)#23S
round(quantile(aux.matrix[,5],c(0.5,0.025,0.975)),digits=3)#29D



boxplot(aux.matrix, col=c("black","green","red","yellow","blue"),
        main=TeX(r'(Coefieciente de $Y_{t-1}$ ($\sigma$))'),
        names=c( TeX(r'($Y_{t-1}$)'),"20A","29J","23S","29D"))
rect(ybottom=quantile(saZ.ylag[,6],p=0.025),ytop=quantile(saZ.ylag[,6],p=0.975),xleft=0,xright=10, col="gray",border=F,)
boxplot(aux.matrix,col=c("black","green","red","yellow","blue"),
        names=c( TeX(r'($Y_{t-1}$)'),"20A","29J","23S","29D"),add=T)
abline(h=0,col="red", lty=2)




#####Bilbao
Tmax.dataB<-read.table('Datos-Bilbao.txt',header=TRUE,dec='.',sep=',')
Tmax.dataB<-fun_depurar_datos(Tmax.dataB)
#Primer modelo bayesiano (sin AR)
f2<-list(TX ~ sin1t + cos1t + (sin2t + cos2t)*YEAR,
         sigma ~ (sin1t + cos1t): YEAR)
bam2<-bamlss(f2,family="gaussian",data=Tmax.dataB,light=T)
summary(bam2)

#Primer modelo bayesiano (varianza constante)
fmedia<-(TX~ YEAR  + Ylag*( sin1t + cos1t + sin2t + cos2t)+YEAR:(sin1t+cos1t))
bam.media<-bamlss(fmedia,family="gaussian",data=Tmax.dataB,light=T)
summary(bam.media)


#Modelo bayesiano final, con stepwise
f3B<-list(TX ~ YEAR + Ylag + sin2t+ cos2t + Ylag:( sin1t + cos1t)+YEAR:(sin1t+cos1t),
          sigma ~ YEAR:( sin2t + cos2t) + Ylag*(sin1t + cos1t))

bam3B<-bamlss(f3B,family="gaussian",data=Tmax.dataB,light=T)
summary(bam3B)

#Para la representación de los paráemtros
xtable((summary(bam3B))$"model.matrix"$mu[2:9,c(1,2,4)],digits=3,caption="Distribución posteriori de mu")
xtable((summary(bam3B))$"model.matrix"$sigma[2:8,c(1,2,4)],digits=4,caption="Distribución posteriori de sigma")

dic.B<-DIC(bam2,bam.media,bam3B)
dic.B



#Box-Plot de distrib a posteriori de Year e interacciones

saB.year<-samples(bam3B,term="YEAR")
saB.ylag<-samples(bam3B,term="Ylag")
head(saB.year)
head(saB.ylag)




##Box-Plot media Ylag 
aux.matrix<-as.matrix(data.frame(saB.ylag[,1],
                        saB.ylag[,1]+saB.ylag[,2]*sin(2*pi*110/365)+saB.ylag[,3]*cos(2*pi*110/365),
                        saB.ylag[,1]+saB.ylag[,2]*sin(2*pi*180/365)+saB.ylag[,3]*cos(2*pi*180/365), 
                        saB.ylag[,1]+saB.ylag[,2]*sin(2*pi*266/365)+saB.ylag[,3]*cos(2*pi*266/365),
                        saB.ylag[,1]+saB.ylag[,2]*sin(2*pi*363/365)+saB.ylag[,3]*cos(2*pi*363/365)))

#Para obtener su valor
round(quantile(aux.matrix[,1],c(0.5,0.025,0.975)),digits=3)
round(quantile(aux.matrix[,2],c(0.5,0.025,0.975)),digits=3)#20A
round(quantile(aux.matrix[,3],c(0.5,0.025,0.975)),digits=3)#29J
round(quantile(aux.matrix[,4],c(0.5,0.025,0.975)),digits=3)#23S
round(quantile(aux.matrix[,5],c(0.5,0.025,0.975)),digits=3)#29D


boxplot(aux.matrix, col=c("black","green","red","yellow","blue"),
        main=TeX(r'(Coefieciente de $Y_{t-1}$ ($\mu$))'),
        names=c( TeX(r'($Y_{t-1}$)'),"20A","29J","23S","29D"))
rect(ybottom=quantile(saB.ylag[,1],p=0.025),ytop=quantile(saB.ylag[,1],p=0.975),xleft=0,xright=10, col="gray",border=F,)
boxplot(aux.matrix,col=c("black","green","red","yellow","blue"),
        names=c( TeX(r'($Y_{t-1}$)'),"20A","29J","23S","29D"),add=T)



##Box-Plot media Year 
aux.matrix<-as.matrix(data.frame(saB.year[,1],
                                 saB.year[,1]+saB.year[,2]*sin(2*pi*110/365)+saB.year[,3]*cos(2*pi*110/365),
                                 saB.year[,1]+saB.year[,2]*sin(2*pi*180/365)+saB.year[,3]*cos(2*pi*180/365), 
                                 saB.year[,1]+saB.year[,2]*sin(2*pi*266/365)+saB.year[,3]*cos(2*pi*266/365),
                                 saB.year[,1]+saB.year[,2]*sin(2*pi*363/365)+saB.year[,3]*cos(2*pi*363/365)))

#Para obtener su valor
round(quantile(aux.matrix[,1],c(0.5,0.025,0.975)),digits=3)
round(quantile(aux.matrix[,2],c(0.5,0.025,0.975)),digits=3)#20A
round(quantile(aux.matrix[,3],c(0.5,0.025,0.975)),digits=3)#29J
round(quantile(aux.matrix[,4],c(0.5,0.025,0.975)),digits=3)#23S
round(quantile(aux.matrix[,5],c(0.5,0.025,0.975)),digits=3)#29D


boxplot(aux.matrix, col=c("black","green","red","yellow","blue"),
        main=TeX(r'(Coefieciente de $Year_{t}$ ($\mu$))'),
        names=c( TeX(r'($Year_{t}$)'),"20A","29J","23S","29D"))
rect(ybottom=quantile(saB.year[,1],p=0.025),ytop=quantile(saB.year[,1],p=0.975),xleft=0,xright=10, col="gray",border=F,)
boxplot(aux.matrix,col=c("black","green","red","yellow","blue"),
        names=c( TeX(r'($Year_{t}$)'),"20A","29J","23S","29D"),add=T)

#Box-plot sigma ylag
aux.matrix<-as.matrix(data.frame(saB.ylag[,4],
                                 saB.ylag[,4]+saB.ylag[,5]*sin(2*pi*110/365)+saB.ylag[,6]*cos(2*pi*110/365),
                                 saB.ylag[,4]+saB.ylag[,5]*sin(2*pi*180/365)+saB.ylag[,6]*cos(2*pi*180/365), 
                                 saB.ylag[,4]+saB.ylag[,5]*sin(2*pi*266/365)+saB.ylag[,6]*cos(2*pi*266/365),
                                 saB.ylag[,4]+saB.ylag[,5]*sin(2*pi*363/365)+saB.ylag[,6]*cos(2*pi*363/365)))

#Para obtener su valor
round(quantile(aux.matrix[,1],c(0.5,0.025,0.975)),digits=3)
round(quantile(aux.matrix[,2],c(0.5,0.025,0.975)),digits=3)#20A
round(quantile(aux.matrix[,3],c(0.5,0.025,0.975)),digits=3)#29J
round(quantile(aux.matrix[,4],c(0.5,0.025,0.975)),digits=3)#23S
round(quantile(aux.matrix[,5],c(0.5,0.025,0.975)),digits=3)#29D


boxplot(aux.matrix, col=c("black","green","red","yellow","blue"),
        main=TeX(r'(Coefieciente de $Y_{t-1}$ ($\sigma$))'),
        names=c( TeX(r'($Y_{t-1}$)'),"20A","29J","23S","29D") )
rect(ybottom=quantile(saB.ylag[,4],p=0.025),ytop=quantile(saB.ylag[,4],p=0.975),xleft=0,xright=10, col="gray",border=F,)
boxplot(aux.matrix,col=c("black","green","red","yellow","blue"),
        names=c( TeX(r'($Y_{t-1}$)'),"20A","29J","23S","29D"),add=T)
