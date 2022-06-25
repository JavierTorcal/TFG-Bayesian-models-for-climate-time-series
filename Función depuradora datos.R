#Este archivo recoge la función utilizada para preparar los conjunto de datos 
#de acuerdo a nuestras preferencias.
# Buscamos poner los datos -9999ºC como NA, dividir las
#temperaturas entre 10 ya que vienen dadas en décimas de grado.

#Con la librería lubridate se consigue un formato adecuado para las fechas.

#También buscamos elimininar los 29 Feb para conseguir años de 365 días y 
#queremos conseguir los datos correspondientes a los años 1951-2020.

#Por último se incluiran las covariables correspondientes como armónicos e Ylag.

fun_depurar_datos<-function(datos){

require(lubridate)
#Eliminar columnas que no queremos.
datos$SOUID<-NULL
datos$STAID<-NULL
datos$Q_TX<-NULL

#Formato de fechas mediante Lubridate
datos$DATE<-ymd(datos$DATE)
datos$YEAR<-year(datos$DATE)
datos$MONTH<-month(datos$DATE)
datos$DAY<-day(datos$DATE)


#Eliminar las observaciones -9999 y dividir entre 10:
datos[datos$TX==-9999,]$TX<-NA
datos$TX<-datos$TX/10

#Eliminar 29 Feb
diabisiesto<-is.element(datos$MONTH,2)&is.element(datos$DAY,29)
datos<-datos[!diabisiesto,]

#Años 1951-2020
datos<-datos[(datos$YEAR>=1951) & (datos$YEAR<=2020),]

#Incluir los armónicos en el conjunto de datos
n <- length(datos$TX)
t <- 1:n
datos$sin1t <- sin(2 * pi * t / 365)
datos$cos1t <- cos(2 * pi * t / 365)
datos$sin2t <- sin(4 * pi * t / 365)
datos$cos2t <- cos(4 * pi * t / 365)

#Añadir el día previo (Ylag)
datos$Ylag<-lag(c(NA,datos$TX[-length(datos$TX)]),k=1)

#Poner el número adecuado a los días
rownames(datos)<-c(1:25550)
datos$YEAR_DAY<-rep(1:365,70)

#Devuelve el conjunto de datos listo para su uso.
return(datos)}

#Aplicar la función:
Tmax.data<-fun_depurar_datos(Tmax.data)

#Resumen numérico de las temperaturas
summary(Tmax.data$TX)

