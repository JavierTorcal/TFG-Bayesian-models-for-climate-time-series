library("bamlss")
library("xtable")
library(coda)
library(latex2exp)


#Soria
setwd("C:/Users/Javier/Desktop/TFG")
Tmax.data<-read.table('Datos-Soria.txt',header=TRUE,dec='.',sep=',')
Tmax.data<-fun_depurar_datos(Tmax.data)

f3<-list(TX ~ YEAR + sin1t + cos1t + sin2t + cos2t + Ylag*(sin1t + cos1t),
         sigma ~ Ylag*(sin1t + cos1t + sin2t + cos2t))

set.seed(123)
bam3.1<-bamlss(f3,family="gaussian",data=Tmax.data,light=T)
set.seed(248)
bam3.2<-bamlss(f3,family="gaussian",data=Tmax.data,light=T)
set.seed(259)
bam3.3<-bamlss(f3,family="gaussian",data=Tmax.data,light=T)

summary(bam3.1)


dev.off()
#media
par(mfrow=c(2,4))
for(i in 2:9){
  a<-signif(gelman.diag(c(bam3.3$"samples"[,i],bam3.1$"samples"[,i],bam3.2$"samples"[,i]))$"psrf"[[1]],4)
  traceplot(c(bam3.3$"samples"[,i],bam3.1$"samples"[,i],bam3.2$"samples"[,i]),col=c(3,2,4), ylab=names(parameters(bam3.1))[i], 
            main=bquote(paste(~bold(hat(R)),"=",.(a))),font.lab=2,cex.lab=1.5,xlab=" ",sub="Iteraciones")
}

#varianza
par(mfrow=c(3,3))
for(i in 14:22){
  a<-signif(gelman.diag(c(bam3.3$"samples"[,i],bam3.1$"samples"[,i],bam3.2$"samples"[,i]))$"psrf"[[1]],4)
  traceplot(c(bam3.3$"samples"[,i],bam3.1$"samples"[,i],bam3.2$"samples"[,i]),col=c(3,2,4), ylab=names(parameters(bam3.1))[i-3], 
            main=bquote(paste(~bold(hat(R)),"=",.(a))),font.lab=2,cex.lab=1.5,xlab=" ",sub="Iteraciones")
            }



#Zaragoza
Tmax.dataZ<-read.table('Datos-Zaragoza.txt',header=TRUE,dec='.',sep=',')
Tmax.dataZ<-fun_depurar_datos(Tmax.dataZ)

f3Z<-list(TX ~ YEAR  + Ylag*( sin1t + cos1t + sin2t + cos2t)+YEAR:(sin1t+cos1t),
          sigma ~ Ylag*(sin1t + cos1t + sin2t + cos2t))

set.seed(123)
bam3Z.1<-bamlss(f3Z,family="gaussian",data=Tmax.dataZ,light=T)
set.seed(245)
bam3Z.2<-bamlss(f3Z,family="gaussian",data=Tmax.dataZ,light=T)
set.seed(259)
bam3Z.3<-bamlss(f3Z,family="gaussian",data=Tmax.dataZ,light=T)


dev.off()
#media
par(mfrow=c(2,3))
for(i in 2:7){
  a<-signif(gelman.diag(c(bam3Z.3$"samples"[,i],bam3Z.1$"samples"[,i],bam3Z.2$"samples"[,i]))$"psrf"[[1]],4)
  traceplot(c(bam3Z.3$"samples"[,i],bam3Z.1$"samples"[,i],bam3Z.2$"samples"[,i]),col=c(3,2,4), ylab=names(parameters(bam3Z.1))[i], 
            main=bquote(paste(~bold(hat(R)),"=",.(a))),font.lab=2,cex.lab=1.5,xlab=" ",sub="Iteraciones")
}

for(i in 8:13){
  a<-signif(gelman.diag(c(bam3Z.3$"samples"[,i],bam3Z.1$"samples"[,i],bam3Z.2$"samples"[,i]))$"psrf"[[1]],4)
  traceplot(c(bam3Z.3$"samples"[,i],bam3Z.1$"samples"[,i],bam3Z.2$"samples"[,i]),col=c(3,2,4), ylab=names(parameters(bam3Z.1))[i], 
            main=bquote(paste(~bold(hat(R)),"=",.(a))),font.lab=2,cex.lab=1.5,xlab=" ",sub="Iteraciones")
}

#varianza
par(mfrow=c(3,3))
for(i in 18:26){
  a<-signif(gelman.diag(c(bam3Z.3$"samples"[,i],bam3Z.1$"samples"[,i],bam3Z.2$"samples"[,i]))$"psrf"[[1]],4)
  traceplot(c(bam3Z.3$"samples"[,i],bam3Z.1$"samples"[,i],bam3Z.2$"samples"[,i]),col=c(3,2,4), ylab=names(parameters(bam3Z.1))[i-3], 
            main=bquote(paste(~bold(hat(R)),"=",.(a))),font.lab=2,cex.lab=1.5,xlab=" ",sub="Iteraciones")
}


#Bilbao
Tmax.dataB<-read.table('Datos-Bilbao.txt',header=TRUE,dec='.',sep=',')
Tmax.dataB<-fun_depurar_datos(Tmax.dataB)

f3B<-list(TX ~ YEAR + Ylag + sin2t+ cos2t + Ylag:( sin1t + cos1t)+YEAR:(sin1t+cos1t),
          sigma ~ YEAR:( sin2t + cos2t) + Ylag*(sin1t + cos1t))

set.seed(123)
bam3B.1<-bamlss(f3B,family="gaussian",data=Tmax.dataB,light=T)
set.seed(245)
bam3B.2<-bamlss(f3B,family="gaussian",data=Tmax.dataB,light=T)
set.seed(259)
bam3B.3<-bamlss(f3B,family="gaussian",data=Tmax.dataB,light=T)

dev.off()
#media
par(mfrow=c(2,4))
for(i in 2:9){
  a<-signif(gelman.diag(c(bam3B.3$"samples"[,i],bam3B.1$"samples"[,i],bam3B.2$"samples"[,i]))$"psrf"[[1]],4)
  traceplot(c(bam3B.3$"samples"[,i],bam3B.1$"samples"[,i],bam3B.2$"samples"[,i]),col=c(3,2,4), ylab=names(parameters(bam3B.1))[i], 
            main=bquote(paste(~bold(hat(R)),"=",.(a))),font.lab=2,cex.lab=1.5,xlab=" ",sub="Iteraciones")
}


#varianza
par(mfrow=c(2,4))
for(i in 14:20){
  a<-signif(gelman.diag(c(bam3B.3$"samples"[,i],bam3B.1$"samples"[,i],bam3B.2$"samples"[,i]))$"psrf"[[1]],4)
  traceplot(c(bam3B.3$"samples"[,i],bam3B.1$"samples"[,i],bam3B.2$"samples"[,i]),col=c(3,2,4), ylab=names(parameters(bam3B.1))[i-3], 
            main=bquote(paste(~bold(hat(R)),"=",.(a))),font.lab=2,cex.lab=1.5,xlab=" ",sub="Iteraciones")
}


#Tamaño de muestra efectivo
effectiveSize(bam3.1$"samples")
effectiveSize(bam3Z.1$"samples")
effectiveSize(bam3B.1$"samples")




     