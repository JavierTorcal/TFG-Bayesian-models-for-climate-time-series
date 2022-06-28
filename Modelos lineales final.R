#Vamos a estudiar los modelos lineales finales de Soria, Bilbao y Zaragoza

#Cargar los datos
setwd("C:/Users/Javier/Desktop/TFG")
Tmax.data<-read.table('Datos-Soria.txt',header=TRUE,dec='.',sep=',')
Tmax.dataZ<-read.table('Datos-Zaragoza.txt',header=TRUE,dec='.',sep=',')
Tmax.dataB<-read.table('Datos-Bilbao.txt',header=TRUE,dec='.',sep=',')

#Adecuarlos
Tmax.data<-fun_depurar_datos(Tmax.data)
Tmax.dataZ<-fun_depurar_datos(Tmax.dataZ)
Tmax.dataB<-fun_depurar_datos(Tmax.dataB)

##MODELO LINEAL FINAL (PARA LA MEDIA)

###Soria

model7<-lm(TX ~ YEAR*(sin1t + cos1t + sin2t + cos2t) + Ylag*(sin1t + cos1t + sin2t + cos2t),data=Tmax.data) 
summary(model7)
#Hacemos procedimiento stepwise
step(model7)
#El modelo resultante para Soria tras el procedimiento de selección de variables:
model7.SO <-lm(TX ~ YEAR + Ylag*(sin1t+cos1t) + sin2t + cos2t, data=Tmax.data)
#Notar que los armónicos o sus interacciones los incluimos por parejas.
summary(model7.SO)#Consultar R^2
aux.coefSO<-coefficients(model7.SO)


###Zaragoza 

model7<-lm(TX ~ YEAR*(sin1t + cos1t + sin2t + cos2t) + Ylag*(sin1t + cos1t + sin2t + cos2t),data=Tmax.dataZ) 
#Hacemos procedimiento stepwise
step(model7)
#El modelo resultante para Zaragoza tras el procedimiento de selección de variables:
model7.Z <-lm(TX ~ YEAR+ YEAR:(sin1t+cos1t)+ Ylag*(sin1t+cos1t+ sin2t + cos2t),data=Tmax.dataZ )
summary(model7.Z)#Consultar R^2
aux.coefZ<-coefficients(model7.Z)



###Bilbao

model7<-lm(TX ~ YEAR*(sin1t + cos1t + sin2t + cos2t) + Ylag*(sin1t + cos1t + sin2t + cos2t),data=Tmax.dataB) 
#Hacemos procedimiento stepwise
step(model7)
#El modelo resultante para Zaragoza tras el procedimiento de selección de variables:
model7.B<-lm(TX ~ YEAR+ YEAR:(sin1t+cos1t)+ Ylag*(sin1t+cos1t+ sin2t + cos2t),data=Tmax.dataB)
summary(model7.B)#Consultar R^2
aux.coefB<-coefficients(model7.B)


#Coeficientes de los parámetros con 2 cifras significativas
signif(coefficients(model7.SO),2)
signif(coefficients(model7.Z),2)
signif(coefficients(model7.B),2)

#ACF de los residuos
acf(residuals(model7.SO),na.action = na.pass,1,plot=F)
acf(residuals(model7.Z),na.action = na.pass,1,plot=F)
acf(residuals(model7.B),na.action = na.pass,1,plot=F)


#Ahora pasamos a los modelos para la varianza.
#Se utilizará como variable respuesta el cuadrado de los residuos de los modelos finales para cada ciudad.

##MODELOS PARA LA VARIANZA

###Soria
#Predecimos los valores ajustados para cada día.
model7.predict<-predict(model7.SO,newdata=Tmax.data)
#Conseguimos los residuos del modelo lineal
resid7<-Tmax.data$TX-model7.predict
#Nuestra nueva variable respuesta:
epsilon_cuadrado<-resid7^2
lm.var7<-lm(log(epsilon_cuadrado)~YEAR*(sin1t + cos1t + sin2t + cos2t) + Ylag*(sin1t + cos1t + sin2t + cos2t),data=Tmax.data)
#Procedimiento para seleccionar las variables:
step(lm.var7)
#Modelo resultante:
lm.var7.step.SO<-lm(log(epsilon_cuadrado)~ Ylag*(sin1t + cos1t + sin2t + cos2t),data=Tmax.data)
summary(lm.var7.step.SO)

###Zaragoza
#Predecimos los valores ajustados para cada día.
model7.predictZ<-predict(model7.Z,newdata=Tmax.dataZ)
#Conseguimos los residuos del modelo lineal
resid7Z<-Tmax.dataZ$TX-model7.predictZ
#Nuestra nueva variable respuesta:
epsilon_cuadradoZ<-resid7Z^2
#Se decide no incluir término YEAR
lm.var7Z<-lm(log(epsilon_cuadradoZ)~Ylag*(sin1t + cos1t + sin2t + cos2t),Tmax.dataZ)
#Procedimiento para seleccionar las variables:
step(lm.var7Z)
#Modelo resultante:
lm.var7.step.Z<-lm(log(epsilon_cuadrado)~ Ylag*(sin1t + cos1t + sin2t + cos2t),data=Tmax.dataZ)
summary(lm.var7.step.Z)

###Bilbao
#Predecimos los valores ajustados para cada día.
model7.predictB<-predict(model7.B,newdata=Tmax.dataB)
#Conseguimos los residuos del modelo lineal
resid7B<-Tmax.dataB$TX-model7.predictB
#Nuestra nueva variable respuesta:
epsilon_cuadradoB<-resid7B^2
lm.var7B<-lm(log(epsilon_cuadradoB)~YEAR*(sin1t + cos1t + sin2t + cos2t) + Ylag*(sin1t + cos1t + sin2t + cos2t),Tmax.dataB)
#Procedimiento para seleccionar las variables:
step(lm.var7B)
#Modelo resultante(no se prescinde de ningún término)
lm.var7.step.B<-lm.var7B
#Procedimiento para seleccionar las variables:
summary(lm.var7.step.B)


#Coeficientes de los parámetros con 2 cifras significativas
signif(coefficients(lm.var7.step.SO),2)
signif(coefficients(lm.var7.step.Z),2)
signif(coefficients(model7.B),2)
