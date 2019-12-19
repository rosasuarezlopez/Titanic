#Instalamos los paquetes y las librerías.

library("nortest")
library(boxplotdbl)
library(sqldf)
library(gam)
library(dplyr)
library(funModeling)
library(readr)

##########################################################
#LECTURA Y EXPLORATORIO DE DATOS
##########################################################
#Leemos los datos 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
data<- read_csv("train.csv")

#Veamos qué tipo de datos tenemos
head(data)
#y las dimensiones del dataset: Tenemos 891 registros con 12 variables
#La descripción de cada una de las variables se puede encontrar en el pdf complementario
dim.data.frame(data)
#Así como los nombres de las columnas
names(data)

#Vemos el tipo de dato de cada variable. 
sapply(data, function(x) class(x))


##########################################################
#LIMPIEZA Y DEPURACION DE DATOS
##########################################################

##Vamos a realizar una serie de comprobaciones sobre nuestros datos para ver si tenemos
#que tomar alguna medida sobre la calidad de los datos:

#1) Comprobemos la unicidad de los registros:
#Con el comando unique() eliminamos las muestras duplicadas  y vemos que sigue teniendo
#891 muestras. Por tanto, no hay duplicados. OK
datasindup<-unique(data)
dim.data.frame(datasindup)

#2) Veamos un resumen de los valores que tienen las  12 variables de nuestro dataset
summary(data)

df_status(data)

#Observamos tambien que tenemos variables personales, que no nos aportarían nada en un 
#proyecto de analítica de datos puesto que definen univocamente cada registro. Son
#las variables "Passengerld","Name" y "Ticket", y las vamos a eliminar de nuestro dataset.
data<-select(data,-PassengerId,-Name,-Ticket)

#Tenemos algunas variables que aunque a priori aparecen como “numeric” o 
#“character” deberíamos convertir a “Factor”, son Survived, Pclass,Embarked y Sex
data$Sex<-as.factor(data$Sex)
data$Survived<-as.factor(data$Survived)
data$Pclass<-as.factor(data$Pclass)
data$Embarked<-as.factor(data$Embarked)

#En el df_status vemos que Age tiene un 19% de nulos y cabin tiene un 77%  
#son los casos más llamativos que tenemos que solucionar. 
#También tenemos 2 nulos en Embarked.

#Tenemos un alto número de ceros en SibSp y Parch, pero son valores válidos para estas
#variables, pues cuentan número de acompañantes del pasajero. Hay también un % pequeño
#de ceros en Fare (tarifa), podría tener algún sentido por lo que en principio vamos
#a dejar presentes estos ceros.



#3) Actuaciones en la variable "Age":
#Tenemos 177 casos de nulos 

#En el siguiente gráfico vemos que la edad tiene bastante relación con la clase
#en que se viaja, `puesto que a mayor edad  mejor clase (clase alta). Por
#lo que según esto podríamos hacer una imputación por clase y el resultado
#sería más acertado:`
boxplot(data$Age~data$Pclass,xlab="Pclass",ylab="Age",col=c("blue","yellow","red"))

data$Age[is.na(data$Age)&data$Pclass==1]<-mean(data$Age[!is.na(data$Age)&data$Pclass==1])
data$Age[is.na(data$Age)&data$Pclass==2]<-mean(data$Age[!is.na(data$Age)&data$Pclass==2])
data$Age[is.na(data$Age)&data$Pclass==3]<-mean(data$Age[!is.na(data$Age)&data$Pclass==3])


#Ahora veamos con summary los resultados para Age y cómo han variado: Los valores de media
#y mediana han sufrido  ligeras variaciones.
summary(data)

#4) Actuaciones en la variable "Cabin":
#Dado que tiene un 77% de blancos y además no parece una variabe muy relevante, vamos
#a prescindir de ella:
data<-select(data,-Cabin)


#5)Actualiciones sobre "Embarked":
#Dado que solo tenemos 2 nulos y además la variable solo toma 3 valores vamos a imputar
#el valor que más se repite: Embarked=S (Southampton)

data %>% group_by(Embarked) %>% count(Embarked)

data$Embarked[is.na(data$Embarked)]<-"S"


#Vemos ahora que ya  no tenemos nulos y que hemos eliminado los ceros en Embarked.
df_status(data)

#6)Tratamiento de valores outliers:
#Vamos a separar las variables numéricas
lista<-sapply(data, is.numeric) 

#ahora solo nos quedamos con esas y las vemos en boxplot
data_num<-data[,lista] 
boxplot(data_num,las=2)

#Según el gráfico podría haber outilers en Fare

#Si nos vamos al método más habitual de detectar outlier, nos dice que consideraremos 
#outlier aquellos que estén por encima de la media más 3 desviaciones típicas 
#(o por debajo),veamos:
data_out<-as.data.frame(data_num$Fare)
data_out$outlier<-FALSE
for (i in 1:ncol(data_out)-1){
  columna = data_out[,i]
  if (is.numeric(columna)){
    media = mean(columna)
    desviacion = sd(columna)
    data_out$outlier = ( columna>(media+3*desviacion) | columna<(media-3*desviacion))
  }
}
# Marcamos los TRUE y FALSE
table(data_out$outlier)
#Podríamos tener 20 outliers, pero según los valores qu enos devuelve la función
#boxplot.stats todos parecen bastante razonables, hasta el maximo 512
boxplot.stats(data$Fare)$out
#y además los valores están bien distribuidos, los más altos están en la primera
#clase, más elevada.
boxplot(data$Fare~data$Pclass,xlab="Pclass",ylab="Fare",col=c("blue","yellow","red"))
#Por estos motivos no vamos a tomar ninguna medida con estos outliers.

##veamos siguiendo el mismo procedimiento qué resultados tenemos con la edad:
data_out<-as.data.frame(data_num$Age)
data_out$outlier<-FALSE
for (i in 1:ncol(data_out)-1){
  columna = data_out[,i]
  if (is.numeric(columna)){
    media = mean(columna)
    desviacion = sd(columna)
    data_out$outlier = ( columna>(media+3*desviacion) | columna<(media-3*desviacion))
  }
}
# Marcamos los TRUE y FALSE
table(data_out$outlier)

#Podríamos tener 7 outliers, pero según los valores qu enos devuelve la función
#boxplot.stats todos no los consideraremos outliers, son razonables
boxplot.stats(data$Age)$out

##########################################################
#ANALISIS
##########################################################




