# Antes de nada, limpiamos el workspace
rm(list = ls())

#Instalamos los paquetes y las librerías.

if(!require(knitr)){
  install.packages('knitr')
  library(knitr)
}
if(!require(corrplot)){
  install.packages('corrplot')
  library(corrplot)
}
if(!require(sqldf)){
  install.packages('sqldf')
  library(sqldf)
}
if(!require(dplyr)){
  install.packages('dplyr')
  library(dplyr)
}
if(!require(funModeling)){
  install.packages('funModeling')
  library(funModeling)
}
if(!require(readr)){
  install.packages('readr')
  library(readr)
}
if(!require(ggplot2)){
  install.packages('ggplot2')
  library(ggplot2)
}
if(!require(scales)){
  install.packages('scales')
  library(scales)
}
if(!require(mice)){
  install.packages('mice')
  library(mice)
}


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
kable(sapply(data, function(x) class(x)))


##########################################################
#LIMPIEZA Y DEPURACION DE DATOS
##########################################################

##Vamos a realizar una serie de comprobaciones sobre nuestros datos para ver si tenemos
#que tomar alguna medida sobre la calidad de los datos:

#1) Comprobemos la unicidad de los registros:
#Con el comando unique() eliminamos las muestras duplicadas  y vemos que sigue teniendo
#891 muestras. Por tanto, no hay duplicados. OK
data_unique<-unique(data)
dim.data.frame(data_unique)
remove(data_unique)
#2) Veamos un resumen de los valores que tienen las  12 variables de nuestro dataset
summary(data)

df_status(data)

#Observamos tambien que tenemos variables personales, que no nos aportarían nada en un 
#proyecto de analítica de datos puesto que definen univocamente cada registro. Son
#las variables "Passengerld","Name" y "Ticket", y las vamos a eliminar de nuestro dataset.
#Pero la variable Ticket aporta la agrupación de las personas que viajan con un mismo ticket,
#es decir podemos calcular el precio por persona diviendo Fare / num

#Agrupamos por Ticket y Fare y obtenemos el nº de filas 
nrow(table(data$Ticket, data$Fare))  #Devuelve 681 filas
length(unique(data$Ticket))          #Devuelve 681 valores únicos de Ticket
#También se puede obtener así
data %>% 
  group_by(Ticket, Fare) %>% 
  filter(row_number() == 1)

#Concluímos que se puede obtener el Precio por Persona, dividiendo Fare / integrantes del Ticket
ticket_personas <- as.data.frame(data %>% 
                                   group_by(Ticket) %>% 
                                   dplyr::summarize(PersonasTicket=n()))
#Tenemos un nuevo dataframe con el Ticket y nº de registros (personas) para ese ticket
df_status(ticket_personas)   #Las columnas son Ticket y PersonasTicket 

#Ahora vamos a hacer un Merge entre nuestro Dataset original y el de Tickets/Personas
#Lo hacemos a través de la columna Ticket (aparece la nueva columnas PersonasTicket que usamos para 
#calular, y luego eliminaremos)
data <- merge(data, ticket_personas, by = "Ticket")
remove(ticket_personas)

#Y ahora generamos la nueva columna de precio (Price para mantener el idioma de columnas)
data$Price <- data$Fare / data$PersonasTicket

#Y después de esto ahora sí finalmente nos disponemos a eliminar aquellas columnas que no vamos 
#a utilizar en nuestro análisis
data<-select(data,-PassengerId,-Name,-Ticket,-PersonasTicket)

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
#variables, pues cuentan número de familiares del pasajero. Hay también un % pequeño
#de ceros en Fare (tarifa), podría tener algún sentido por lo que en principio vamos
#a dejar presentes estos ceros.

#Vamos a definir el tamaño familiar de la gente que viaja, sumando SibSp y Parch
#SibSp: (Number of Siblings/Spouses Aboard)
#   Sibling: Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
#   Spouse: Husband or Wife of Passenger Aboard Titanic
#Parch: (Number of Parents/Children Aboard)
#   Parent: Mother or Father of Passenger Aboard Titanic
#   Child: Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic

#Incluimos al pasajero en cuestión. Es decir siempre va de 1 a N la columna FamilySize
data$FamilySize = data$SibSp + data$Parch + 1


#3)Actualiciones sobre "Embarked":
#Dado que solo tenemos 2 nulos y además la variable solo toma 3 valores vamos a imputar
#el valor que más se repite: Embarked=S (Southampton)
dat_Embarked = sort(table(data$Embarked,useNA = "ifany"),decreasing = TRUE)
dat_Embarked
dat_plot = as.data.frame(dat_Embarked)
ggplot(dat_plot, aes(x=Var1, y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity", color="black") +
  geom_text(aes(label=Freq), vjust=-0.4, color="black", size=3) +
  labs(x='Embarked', y='Count') + theme(legend.position='none')

data$Embarked[is.na(data$Embarked)]<-names(dat_Embarked[1]) #Es decir asignar el más frecuente (o sea "S")


#4) Actuaciones en la variable "Cabin":
#Dado que tiene un 77% de blancos y además no parece una variabe muy relevante, vamos
#a prescindir de ella:
data<-select(data,-Cabin)


#5) Actuaciones en la variable "Age":
#Tenemos 177 casos de nulos 

#Nos quedamos inicialmente un data.frame con los registros que no tienen valores nulos en Age
data_NoNA = data[which(!is.na(data$Age)),]

#Comprobamos gráficamente como se distribuye la variable Age (en el dataset que no son nulos)
ggplot(data_NoNA, aes(Age)) + 
  geom_histogram(aes(y = ..density..), bins=50, fill="steelblue", color="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_NoNA$Age), sd = sd(data_NoNA$Age)))

#Gráficamente ya vemos que no sigue una distribución normal, igualmente podemos aplicar el test 
#de Shapiro-Wilk 
#La hipótesis nula es que la muestra sobre la que se hace el test sigue una distribución normal
#Si el p-valor es inferior al nivel alpha elegido (convencionalmente se usa 0.05) entonces se rechaza
#la hipótesis nula
resultado_test <- shapiro.test(data_NoNA$Age)
resultado_test
resultado_test$p.value
#El p-valor es inferior a 0.05, rechazamos la hipótesis nula, y entonces la variable no sigue
#una distribución normal (como ya habíamos comprobado gráficamente)
qqnorm(data_NoNA$Age)
qqline(data_NoNA$Age, col=2)

#Al no seguir una distribución normal, el valor a imputar será la mediana en lugar de la media

#Comprobamos gráficamente la relación entre la edad y el género (no se aprecian diferencias)
titulo <- 'Age vs Sex'
ggplot(data_NoNA, aes(y=Age, x=Sex, fill=Sex)) + geom_boxplot() +
  labs(title = paste0('Boxplot ',titulo)) + ylab("Age") + xlab("Sex")

#Comprobamos gráficamente la relación entre la edad y el puerto de Embarque (no se aprecian casi diferencias)
titulo <- 'Age vs Embarked'
ggplot(data_NoNA, aes(y=Age, x=Embarked, fill=Embarked)) + geom_boxplot() +
  labs(title = paste0('Boxplot ',titulo)) + ylab("Age") + xlab("Embarked")
  #theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

#En cambio en el siguiente gráfico vemos que la edad tiene bastante relación con la clase
#en que se viaja, `puesto que a mayor edad  mejor clase (clase alta). Por
#lo que según esto podríamos hacer una imputación por clase y el resultado
#sería más acertado:
titulo <- 'Age vs Pclass'
ggplot(data_NoNA, aes(y=Age, x=Pclass, fill=Pclass)) + geom_boxplot() +
  labs(title = paste0('Boxplot ',titulo)) + ylab("Age") + xlab("Pclass")

#Boxplot equivalente:
#  boxplot(data_NoNA$Age~data_NoNA$Pclass,xlab="Pclass",ylab="Age",col=c("blue","yellow","red"))

#Obtenemos las variables numéricas
lista<-sapply(data, is.numeric) 

data_num_NoNA = data_NoNA[,lista]

#Vamos a mirar la correlación que existe entre las variables numéricas
corrplot.mixed(cor(data_num_NoNA))

#Podemos ver que existe correlación negativa con SibSp y con Parch (también con FamilySize, 
#pero eso es completamente normal porque FamilySize es una transformación lineal de las otras 2)
#También hay correlación positiva con Price, pero entendemos que podemos tener en cuenta la parte 
#familiar por el tema de esposa, hijos, hermanos, etc puede tener efecto en la edad

data$Age1 <- data$Age
data$Age2 <- data$Age  
data$Age3 <- data$Age
data$Age4 <- data$Age

#Caso 1: Imputando la media a todos los elementos faltantes
data$Age1[is.na(data$Age1)] <- mean(data_NoNA$Age) 

ggplot(data, aes(Age1)) + 
  geom_histogram(aes(y = ..density..), bins=50, fill="steelblue", color="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$Age1), sd = sd(data$Age1)))

#Caso 2: Imputando la media pero por cada Pclass 
data$Age2[is.na(data$Age2)&data$Pclass==1]<-mean(data$Age2[!is.na(data$Age2)&data$Pclass==1])
data$Age2[is.na(data$Age2)&data$Pclass==2]<-mean(data$Age2[!is.na(data$Age2)&data$Pclass==2])
data$Age2[is.na(data$Age2)&data$Pclass==3]<-mean(data$Age2[!is.na(data$Age2)&data$Pclass==3])

ggplot(data, aes(Age2)) + 
  geom_histogram(aes(y = ..density..), bins=50, fill="steelblue", color="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$Age2), sd = sd(data$Age2)))

#Caso 3: Imputando datos de Age, teniendo en cuenta Pclass, Parch y SibSp

medias_clase_fam <- as.data.frame(data_NoNA %>% 
                                    group_by(Pclass, SibSp, Parch) %>% 
                                    dplyr::summarize(Media_clase_fam=mean(Age)))

#Ahora vamos a hacer un Merge entre nuestro Dataset original y el de Medias (por Pclass,SibSp y Parch)
#Lo hacemos a través de las columnas usadas en la agrupación 
#Aparecerá una nueva columna (llamada Media_clase_fam)
#El merge es un LEFT JOIN ya que puede que no existan todas las combinaciones en medias_clase_fam
data <- merge(data, medias_clase_fam, by = c("Pclass", "SibSp", "Parch"), all.x = TRUE)
data$Age3[is.na(data$Age3)] <- data$Media_clase_fam[is.na(data$Age3)]
df_status(data)
#Como es posible que nos hayan quedado alguno sin poder asignar (por no existir la combinación
# Pclass + SibSp + Parch) entonces a los que faltan (7) los asignamos directamente por la Pclass
# sin tener en cuenta los otros 2 valores
data$Age3[is.na(data$Age3)&data$Pclass==1]<-mean(data$Age3[!is.na(data$Age3)&data$Pclass==1])
data$Age3[is.na(data$Age3)&data$Pclass==2]<-mean(data$Age3[!is.na(data$Age3)&data$Pclass==2])
data$Age3[is.na(data$Age3)&data$Pclass==3]<-mean(data$Age3[!is.na(data$Age3)&data$Pclass==3])
df_status(data)

#Ya tenemos todos los valores de Age (caso 3) imputados.
ggplot(data, aes(Age3)) + 
  geom_histogram(aes(y = ..density..), bins=50, fill="steelblue", color="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$Age3), sd = sd(data$Age3)))

#Caso 4: Imputando datos de Age, con MICE (Multivariate Imputation via Chained Equations)
#En este caso se predicen los valores con el resto de valores observados
columnas <- c('Pclass', 'SibSp', 'Parch', 'Sex', 'Age')
mice_imputar <- mice(data = data[,columnas], method = "rf")
mice_completo <- complete(mice_imputar)
summary(mice_completo$Age)
data$Age4[is.na(data$Age4)]<-mice_completo$Age[is.na(data$Age4)]
df_status(data)
#Ya tenemos todos los valores de Age (caso 4) imputados.
ggplot(data, aes(Age4)) + 
  geom_histogram(aes(y = ..density..), bins=50, fill="steelblue", color="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$Age4), sd = sd(data$Age4)))

#Ahora veamos con summary los resultados para los diferentes valores de Age y cómo han variado: 
#Los valores de media y mediana han sufrido  ligeras variaciones.
summary(data$Age)  #Este es el original
summary(data$Age1) 
summary(data$Age2)
summary(data$Age3)
summary(data$Age4)

#Es decir hemos ejecutado 4 imputaciones diferentes, nos quedamos con una de ellas
data$Age <- data$Age4

#Ahora ya nos sobran algunas columnas que usamos para calcular el resultado
data$Age1 <- NULL
data$Age2 <- NULL
data$Age3 <- NULL
data$Age4 <- NULL
data$Media_clase_fam <- NULL

remove(data_num_NoNA)
remove(data_NoNA)
remove(medias_clase_fam)
remove(mice_imputar)
remove(mice_completo)

#Vemos ahora que ya  no tenemos nulos y que hemos eliminado los ceros en Embarked.
df_status(data)

#6)Tratamiento de valores outliers:
#Vamos a separar las variables numéricas
lista<-sapply(data, is.numeric) 

#ahora solo nos quedamos con esas y las vemos en boxplot
data_num<-data[,lista] 
boxplot(data_num,las=2)

#Según el gráfico podría haber outilers en Fare y en Price (y además menos, valor calculado desde Fare, aunque)

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

#Incluso con más motivo entonces con Price
data_out<-as.data.frame(data_num$Price)
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
#Podríamos tener 13 outliers, pero según los valores qu enos devuelve la función
#boxplot.stats todos parecen bastante razonables, hasta el maximo 221
boxplot.stats(data$Price)$out
boxplot(data$Price~data$Pclass,xlab="Pclass",ylab="Price",col=c("blue","yellow","red"))

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

#Podríamos tener 7 outliers, pero según los valores que nos devuelve la función
#boxplot.stats todos no los consideraremos outliers, son razonables
boxplot.stats(data$Age)$out

##########################################################
#ANALISIS
##########################################################

#Histograma de Age
ggplot(data, aes(Age)) + 
  geom_histogram(color="black", fill="steelblue3", binwidth = 5) +
  ggtitle("Histograma de Age")

#Histograma de Age en función del género
ggplot(data, aes(Age, fill = Sex)) +
  geom_histogram(position = "dodge", colour = "black", binwidth = 5, alpha=0.7) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) + 
  ggtitle("Histograma de Age (teniendo en cuenta Sex)")

#Histograma de Fare
ggplot(data, aes(Fare)) + 
  geom_histogram(color="black", fill="steelblue3", bins=30)

#Histograma de Price
ggplot(data, aes(Price)) + 
  geom_histogram(color="black", fill="steelblue3", bins=30)

#Correlacion de las variables numéricas
corrplot(cor(data_num), method = "number")

#Comparamos la relación que hay entre Pclass y Survived
#Hipótesis nula H0 --> Las variables son independientes
test_chisq <- chisq.test(data$Pclass, data$Survived)
test_chisq
test_chisq$p.value
#4.549252e-23
#Como el p-value es inferior a 0.05 rechazamos la hipótesis nula 
#Los grados de libertad los podemos obtener de la tabla de contingencias:
#table(data$Pclass, data$Survived) donde hay que multiplicar (filas-1)*(columnas-1)
#Que es lo mismo que obtener los diferentes valores de cada variable (menos 1) y multiplicarlos

df_chisq <- (length(levels(data$Pclass))-1) * (length(levels(data$Survived))-1)
valorcritico_chisq <- qchisq(p=0.05, df=df_chisq,lower.tail = FALSE)
test_chisq$statistic
#Si el estadístico chi-cuadrado calculado es superior al valor critico, entonces se rechaza 
#la hipótesis nula, y por tanto concluimos que las variables sí son dependientes
cat(sprintf("El estadístico obtenido es: %f, y el valor crítico es: %f", 
            test_chisq$statistic, valorcritico_chisq))

#######################################################################
#Test de independencia entre Embarked y Survived
#Hipótesis nula: Embarked y Survived son independientes
test_chisq <- chisq.test(data$Embarked, data$Survived)
test_chisq
test_chisq$p.value
df_chisq <- (length(levels(data$Pclass))-1) * (length(levels(data$Survived))-1)
valorcritico_chisq <- qchisq(p=0.05, df=df_chisq,lower.tail = FALSE)
test_chisq$statistic
cat(sprintf("El estadístico obtenido es: %f, y el valor crítico es: %f", 
            test_chisq$statistic, valorcritico_chisq))

