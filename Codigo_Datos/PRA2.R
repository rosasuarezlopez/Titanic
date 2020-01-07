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
if(!require(nortest)){
  install.packages('nortest')
  library(nortest)
}
if(!require(car)){
  install.packages('car')
  library(car)
}
if(!require(caret)){
  install.packages('caret')
  library(caret)
}
if(!require(pROC)){
  install.packages('pROC')
  library(pROC)
}
if(!require(xgboost)) {
  install.packages('xgboost')
  library(xgboost)
}
if(!require(randomForest)) {
  install.packages('randomForest')
  library(randomForest)
}
if(!require(rpart.plot)){
  install.packages('rpart.plot')
  library(rpart.plot)
}
if(!require(tidyr)) {
  install.packages('tidyr')
  library(tidyr)
}
if(!require(tibble)){
  install.packages('tibble')
  library(tibble)
}
if(!require(gridExtra)){
  install.packages('gridExtra')
  library(gridExtra)
}
if(!require(beeswarm)){
  install.packages('beeswarm', repos='http://cran.us.r-project.org')
  library(beeswarm)
} 
if(!require(gmodels)){
  install.packages('gmodels', repos='http://cran.us.r-project.org')
  library(gmodels)
}
##########################################################
#LECTURA Y EXPLORATORIO DE DATOS
##########################################################
#Leemos los datos 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
data<- read_csv("train.csv")

#Veamos unos pocos datos tipo de datos tenemos
head(data)

#Vemos las dimensiones del dataframe: Tenemos 891 registros con 12 variables
#La descripción de cada una de las variables se puede encontrar 
#en el pdf complementario
dim.data.frame(data)
#Así como los nombres de las columnas
names(data)
sapply(data, function(x) class(x))
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
data<-dplyr::select(data,-PassengerId,-Name,-Ticket,-PersonasTicket)

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

summary(data)
df_status(data)

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

#Es decir asignar el más frecuente (o sea "S")
data$Embarked[is.na(data$Embarked)]<-names(dat_Embarked[1]) 


#4) Actuaciones en la variable "Cabin":
#Dado que tiene un 77% de blancos y además no parece una variabe muy relevante, vamos
#a prescindir de ella:
data<-dplyr::select(data,-Cabin)


#5) Actuaciones en la variable "Age":
#Tenemos 177 casos de nulos 

#Nos quedamos inicialmente un data.frame con los registros que 
#no tienen valores nulos en Age
data_NoNA = data[which(!is.na(data$Age)),]

#Comprobamos gráficamente como se distribuye la variable Age 
#(en el dataset que no son nulos)
ggplot(data_NoNA, aes(Age)) + 
  geom_histogram(aes(y = ..density..), bins=50, fill="steelblue", color="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(data_NoNA$Age), 
                                         sd = sd(data_NoNA$Age)))

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


#Comprobamos gráficamente la relación entre la edad y el género 
#(no se aprecian diferencias)
titulo <- 'Age vs Sex'
ggplot(data_NoNA, aes(y=Age, x=Sex, fill=Sex)) + geom_boxplot() +
  labs(title = paste0('Boxplot: ',titulo)) + ylab("Age") + xlab("Sex")

#Comprobamos gráficamente la relación entre la edad y el puerto de Embarque 
#(no se aprecian casi diferencias)
titulo <- 'Age vs Embarked'
ggplot(data_NoNA, aes(y=Age, x=Embarked, fill=Embarked)) + geom_boxplot() +
  labs(title = paste0('Boxplot: ',titulo)) + ylab("Age") + xlab("Embarked")
  #theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

#En cambio en el siguiente gráfico vemos que la edad tiene bastante relación 
#con la clase #en que se viaja, puesto que a mayor edad  mejor clase 
#(clase alta). Por lo que según esto podríamos hacer una imputación 
#por clase y el resultado podría ser más acertado:
titulo <- 'Age vs Pclass'
ggplot(data_NoNA, aes(y=Age, x=Pclass, fill=Pclass)) + geom_boxplot() +
  labs(title = paste0('Boxplot: ',titulo)) + ylab("Age") + xlab("Pclass")

#Boxplot equivalente:
#  boxplot(data_NoNA$Age~data_NoNA$Pclass,xlab="Pclass",ylab="Age",col=c("blue","yellow","red"))

#Obtenemos las variables numéricas
lista<-sapply(data, is.numeric) 

data_num_NoNA = data_NoNA[,lista]

#Vamos a mirar la correlación que existe entre las variables numéricas
corrplot(cor(data_num_NoNA), method="number")
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
  stat_function(fun = dnorm, args = list(mean = mean(data$Age1), 
                                         sd = sd(data$Age1)))

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

#Ahora vamos a hacer un Merge entre nuestro Dataset original y el de Medias 
#(por Pclass,SibSp y Parch)
#Lo hacemos a través de las columnas usadas en la agrupación 
#Aparecerá una nueva columna (llamada Media_clase_fam)
#El merge es un LEFT JOIN ya que puede que no existan todas las combinaciones en 
#medias_clase_fam
data <- merge(data, medias_clase_fam, by = c("Pclass", "SibSp", "Parch"), all.x = TRUE)
data$Age3[is.na(data$Age3)] <- data$Media_clase_fam[is.na(data$Age3)]
df_status(data)
#Como es posible que nos hayan quedado alguno sin poder asignar (por no existir la combinación
# Pclass + SibSp + Parch) entonces a los que faltan (7) los asignamos directamente 
#por la Pclass sin tener en cuenta los otros 2 valores
data$Age3[is.na(data$Age3)&data$Pclass==1]<-mean(data$Age3[!is.na(data$Age3)&data$Pclass==1])
data$Age3[is.na(data$Age3)&data$Pclass==2]<-mean(data$Age3[!is.na(data$Age3)&data$Pclass==2])
data$Age3[is.na(data$Age3)&data$Pclass==3]<-mean(data$Age3[!is.na(data$Age3)&data$Pclass==3])
df_status(data)

#Ya tenemos todos los valores de Age (caso 3) imputados.
ggplot(data, aes(Age3)) + 
  geom_histogram(aes(y = ..density..), bins=50, fill="steelblue", color="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$Age3), 
                                         sd = sd(data$Age3)))

#Caso 4: Imputando datos de Age, con MICE (Multivariate Imputation via Chained Equations)
#En este caso se predicen los valores con el resto de valores observados
columnas <- c('Pclass', 'SibSp', 'Parch', 'Sex', 'Age')
mice_imputar <- mice(data = data[,columnas], method = "rf")
mice_completo <- mice::complete(mice_imputar)
data$Age4[is.na(data$Age4)]<-mice_completo$Age[is.na(data$Age4)]
df_status(data)
#Ya tenemos todos los valores de Age (caso 4) imputados.
ggplot(data, aes(Age4)) + 
  geom_histogram(aes(y = ..density..), bins=50, fill="steelblue", color="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(data$Age4), 
                                         sd = sd(data$Age4)))

summary(mice_completo$Age)
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
summary(data)
#6)Tratamiento de valores outliers:
#Vamos a separar las variables numéricas
lista<-sapply(data, is.numeric) 

#ahora solo nos quedamos con esas y las vemos en boxplot
data_num<-data[,lista] 
boxplot(data_num,las=2)
var.continuas <- vector()

#Pero además vamos a separar cuales son variables continuas y discretas
for (i in 1:ncol(data_num)) {
  #diferentes = sum(data_num[,i] != as.integer(data_num[,i]))
  #if (diferentes==)
  columna_integer <- as.integer(data_num[,i])
  columna = names(data_num)[i]
  if (isTRUE(all.equal(data_num[,i], columna_integer))) {
     #Cambiamos la variable 
    data[,columna] <- as.integer(data[,columna])
  } else {
    var.continuas <- c(var.continuas, columna)
  }
}

cat("Las variables continuas son:", paste(var.continuas, collapse = ", "))

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
#Podríamos tener 20 outliers, pero según los valores que nos devuelve la función
#boxplot.stats todos parecen bastante razonables, hasta el maximo 512
boxplot.stats(data$Fare)$out
#y además los valores están bien distribuidos, los más altos están en la primera
#clase, más elevada.
boxplot(data$Fare~data$Pclass,xlab="Pclass",ylab="Fare",
        col=c("blue","yellow","red"))
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

#Podríamos tener 4 outliers, pero según los valores que nos devuelve la función
#boxplot.stats todos no los consideraremos outliers, son razonables
boxplot.stats(data$Age)$out

##########################################################
#ANALISIS
##########################################################

#Definimos posibles grupos para analizar
#Dentro del dataset vamos a añadir una variable Child
edad_corte = 8
data$Child[data$Age <= edad_corte] <- 1
data$Child[data$Age > edad_corte] <- 0
data$Child <- as.factor(data$Child)

#Agrupando por el género
Mujeres <- data[which(data$Sex=='female'),]
Hombres <- data[which(data$Sex=='male'),]

#Agrupando por Embarked
EmbarqueC <- data[which(data$Embarked=='C'),]
EmbarqueQ <- data[which(data$Embarked=='Q'),]
EmbarqueS <- data[which(data$Embarked=='S'),]

#Por clase
FirstClass <- data[which(data$Pclass==1),]
SecondClass <- data[which(data$Pclass==2),]
ThirdClass <- data[which(data$Pclass==3),]

#Definir grupos por edades. Añadir una columna al dataset (AgeInterval)
intervalos_edad <- c(0,13,18,40,55) 
data$AgeInterval <- findInterval(data$Age,intervalos_edad)
#data$AgeInterval <- as.ordered(data$AgeInterval)
data$AgeInterval <- as.factor(data$AgeInterval)
table(data$AgeInterval)

# menores <- data[which(data$Age <= 12),]
# mayores <- data[which(data$Age > 12),]
labelsAgeInterval <- c()
for (k in 1:(length(intervalos_edad))) {
  if (k==1) {
      texto <- paste('<',intervalos_edad[k+1])
  } else if (k==length(intervalos_edad)) {
      texto <- paste('>',intervalos_edad[k])
  } else {
      texto <- paste0(intervalos_edad[k],'-',intervalos_edad[k+1]-1)
  }
  labelsAgeInterval <- c(labelsAgeInterval, texto)
}
labelsAgeInterval

tabla.normalidad <- data.frame('Variable'=character(),
                               'Test de Normalidad'=character(),
                               'Valor Estadístico'=numeric(),
                               'p-Value'=numeric(),
                               stringsAsFactors=FALSE)
str(tabla.normalidad)

#Vamos a comprobar la normalidad de las variables continuas
for (i in 1:length(var.continuas)) {
  variable=var.continuas[i]
  #Test Shapiro-Wilk
  test = shapiro.test(data[,variable])
  tabla.normalidad[nrow(tabla.normalidad)+1,] = c(variable, test$method, 
                                                  test$statistic, test$p.value)
  
  #Test Anderson-Darling
  test = ad.test(data[,variable])
  tabla.normalidad[nrow(tabla.normalidad)+1,] = c(variable, test$method, 
                                                  test$statistic, test$p.value)
  
  #Test Kolmogorov-Smirnov'
  test = ks.test(data[,variable], "pnorm", mean=mean(data[,variable]), sd=sd(data[,variable]))
  tabla.normalidad[nrow(tabla.normalidad)+1,] = c(variable, test$method, 
                                                  test$statistic, test$p.value)
  
}
#Tabla de los test de normalidad (Shapiro-Wilk, Anderson-Darling y Kolmogorov-Smirnov)
kable(tabla.normalidad)

#De todas formas para cada variable, vamos a dibujar su histograma, su curva de densidad y 
#las graficas Q-Q
for (i in 1:length(var.continuas)) {
  variable=var.continuas[i]
  #Histograma
  print(ggplot(data, aes(data[,variable])) + 
    geom_histogram(aes(y = ..density..), bins=50, fill="steelblue", color="blue") + xlab(variable) +
    stat_function(fun = dnorm, args = list(mean = mean(data[,variable]), sd = sd(data[,variable]))))
  #Gráfica Q-Q
  qqnorm(data[,variable], main=paste0('Normal Q-Q Plot ', '(Variable "', variable,'")'))
  qqline(data[,variable], col=2)
  
  print(variable)
}



#Correlacion de las variables numéricas
corrplot(cor(data_num), method = "number")

#Comprobar las varianzas edad entre los grupos sobreviven/mueren
leveneTest(data = data, Age ~ Survived, center = mean)
colores_defecto_ggplot = (hue_pal()(2))
#Como se distribuyen las muestras en función de la edad y supervivencia
ggplot(data, aes(x = Survived, y = Age)) +
  geom_jitter(aes(color=Survived), position = position_jitter(0.05)) +
  scale_color_manual(values = colores_defecto_ggplot) + 
  scale_x_discrete(name="Survived", breaks=c("0", "1"), labels=c("No", "Yes"))
fligner.test(Age ~ Survived, data=data)

#Estudio homogeneidad de varianzas entre Age y como grupo Embarked
leveneTest(data = data, Age ~ Embarked, center = mean)
leveneTest(y=data$Age, group = data$Embarked, center=mean)
fligner.test(Age ~ Embarked, data=data)
#fligner.test(data$Age ~ data$Embarked, data=data)

# ggplot(data, aes(x = Embarked, y = Age)) +
#   geom_jitter(aes(color=Embarked), position = position_jitter(0.05)) +
#   scale_color_manual(values = (hue_pal()(3))) 
# 
# ggplot(data, aes(x = Pclass, y = Age)) +
#   geom_jitter(aes(color=Pclass), position = position_jitter(0.05)) +
#   scale_color_manual(values = (hue_pal()(3))) 



#Estudio de la homogeneidad de varianzas de Age en función de Pclass
leveneTest(data = data, Age ~ Pclass, center = mean)
fligner.test(Age ~ Pclass, data=data)

#Estudio de la homogeneidad de varianzas de Age en función de Sex
leveneTest(y=data$Age, group=data$Sex, center="mean", data=data)

#Estudio de la homogeneidad de varianzas de Age en función de Sex
leveneTest(y=data$Age, group=data$Sex, center=mean)
fligner.test(Age ~ Sex, data=data)

#boxplot(data$Age ~ data$Embarked)

#Estudio de la homogeneidad de las varianzas
# No tiene distribucion normal leveneTest(y=data$Fare, group=data$Sex, center=mean, data=data)
fligner.test(Fare ~ Sex, data=data)
fligner.test(Fare ~ Survived, data=data)
fligner.test(Fare ~ Embarked, data=data)
fligner.test(Fare ~ Pclass, data=data)
fligner.test(Price ~ Sex, data=data)
fligner.test(Price ~ Survived, data=data)
fligner.test(Price ~ Embarked, data=data)
fligner.test(Price ~ Pclass, data=data)



#Antes comparamos si las varianzas son iguales
var.test(Mujeres$Age, Hombres$Age)  #Con F-test

#Antes comparamos si las varianzas son iguales
leveneTest(Age ~ Sex, data=data)  #Con leveneTest

#Comparamos la media de edades entre mujeres y hombres
t.test(Mujeres$Age, Hombres$Age, var.equal = TRUE)
t.test(Hombres$Age, Mujeres$Age, alternative = "greater", var.equal = TRUE)
t.test(Mujeres$Age, Hombres$Age, alternative = "less", var.equal = TRUE)
t.test(data$Age ~ data$Sex, alternative = "less", var.equal = TRUE)


#Proporcion de mujeres que mueren (no sobreviven) coindice con la proporcion 
#de los hombres que viven
fisher.test(table(data$Sex, data$Survived)) #p-Value < 0.05 se rechaza hipotesis nula

#Proporcion de mujeres que mueren (no sobreviven) es mayor que la de hombre que mueren?
fisher.test(table(data$Sex, data$Survived), alternative = 'less')

fisher.test(table(data$Sex, data$Survived), alternative = 'greater')
tab.contingencia =  table(data$Sex, data$Survived)
odd_ratio = (tab.contingencia[1,1]/tab.contingencia[1,2]) / 
            (tab.contingencia[2,1]/tab.contingencia[2,2])
odd_ratio

#Intervalo de confianza, al 99% 
test = t.test(data$Age, conf.level = 0.99)
test$conf.int


#Comparamos la relación que hay entre Pclass y Survived
#Hipótesis nula H0 --> Las variables son independientes
test_chisq <- chisq.test(data$Pclass, data$Survived)
test_chisq
test_chisq$p.value
#4.549252e-23
#Como el p-value es inferior a 0.05 rechazamos la hipótesis nula 
#Los grados de libertad los podemos obtener de la tabla de contingencias:
#table(data$Pclass, data$Survived) donde hay que multiplicar (filas-1)*(columnas-1)
#Que es lo mismo que obtener los diferentes valores de cada variable (menos 1) 
#y multiplicarlos

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

table(data$Child)
table(data$Child, data$Survived)
#data$Child[data$Age <= 8] <- 1
#data$Child[data$Age > 8] <- 0
#data$Child <- as.factor(data$Child)

#table(data$Age[data$Child==0])

##############################################################
#Proporcion de niños que mueren (no sobreviven) coindice con la proporcion 
#de los que no son niños (hemos hecho un corte a los 8 años)
fisher.test(table(data$Child, data$Survived)) #p-Value < 0.05 se rechaza hipotesis nula

#Proporcion de no-niños que mueren (no sobreviven) es menor a 
#la proporcion de niños (hasta 8 años) que mueren?
fisher.test(table(data$Child, data$Survived), alternative="greater")

tab.contingencia =  table(data$Child, data$Survived)
tab.contingencia
odd_ratio = (tab.contingencia[1,1]/tab.contingencia[1,2]) / 
  (tab.contingencia[2,1]/tab.contingencia[2,2])
odd_ratio


#Edad media de los muertos equivalente a la media de los supervivientes
#Son equivalentes las 2 siguientes instrucciones
t.test(Age~Survived,data = data)
t.test(data$Age[which(data$Survived==0)], data$Age[which(data$Survived==1)])
#Prueba no paramétrica Mann-Whitney
wilcox.test(x = data$Age[which(data$Survived==0)],
            y = data$Age[which(data$Survived==1)],
            paired=FALSE)



#Este modelo ANOVA de la edad en función de Embarked arroja un p-value superior a 0.05
#Además parecen cumplirse las condiciones
modelo_anova <- aov(formula = Age ~ Embarked, data = data)
resumen_anova <- summary(modelo_anova)
resumen_anova
plot(modelo_anova)

#Boxplot de Age (por Embarked)
ggplot(data, aes(x=Embarked, y=Age)) +
  geom_boxplot(fill=(hue_pal()(3))) +
  ggtitle("Boxplot Age (by Embarked)")

#Modelo anova de Price en función de Pclass (el p-value es inferior a 0.05)
#Además no se cumplen las condiciones
modelo_anova <- aov(formula = Price ~ Pclass, data = data)
resumen_anova <- summary(modelo_anova)
resumen_anova
plot(modelo_anova)
#Test no paramétrico, Kruskal Wallis
modelo_krustal <- kruskal.test(formula=Price ~ Pclass, data=data)
modelo_krustal

#Boxplot de Price (por Pclass)
ggplot(data, aes(x=Pclass, y=Price)) +
  geom_boxplot(fill=(hue_pal()(3))) +
  ggtitle("Boxplot Price (by Pclass)")
#################################################################################
#################################################################################
# Variables originales
# Pclass+SibSp+Parch+Sex+Age+Fare+Embarked
#################################################################################
df_status(data)

#Creamos el conjunto de entramiento y de test (75-25)
set.seed(123)
index_train <- createDataPartition(data$Survived, p = .75, list = FALSE)
data_train <- data[index_train,]
data_test <- data[-index_train,]



#############################################################################
## Modelo regresion logistica 1
#############################################################################
modelo_glm1 <- glm(formula = Survived ~ Pclass+SibSp+Parch+Sex+Age+Fare+Embarked,
                   data = data_train, family = binomial(link = "logit"))
summary(modelo_glm1)
predict_glm1 <- predict.glm(object=modelo_glm1, newdata=data_test, type = "response")
r1 = pROC::roc(response=data_test$Survived, predictor = predict_glm1)
threshold_r1 <- coords(r1, "best", ret = "threshold")
predict_glm1 <- ifelse(predict_glm1 < threshold_r1, 0, 1)
mat.confusion_glm1 <- table(data_test$Survived, predict_glm1)
mat.confusion_glm1
pct.correcto_glm1 <- 100 * sum(diag(mat.confusion_glm1)) / sum(mat.confusion_glm1)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",
              pct.correcto_glm1))


csstab_glm1 <- CrossTable(data_test$Survived, predict_glm1,
                          prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE, 
                          dnn = c('Reality', 'Prediction'))

#############################################################################
## Función para imprimir la matriz de confusión a partir de un CrossTable
## Devuelve un gráfico doble (precision por porcentaje y las muestras)
#############################################################################
plot_matriz_confusion <- function(par_crosstable) {
  #Obtenemos la matriz de confusion en porcentaje
  datplotconfusion <- as.data.frame(par_crosstable[2]$prop.row)
  colnames(datplotconfusion) <- c('Observacion','Prediccion','Valor')
  datplotconfusion$Valor <- datplotconfusion$Valor*100
  #Ordenamos los factores de las clases
  datplotconfusion <- datplotconfusion %>% arrange(Observacion)
  clasesord <- sort(as.character(levels(datplotconfusion$Observacion)),decreasing = FALSE)
  datplotconfusion$Observacion <- factor(datplotconfusion$Observacion, levels = clasesord)
  #Ordenar las clases descendente (para el plot)
  clasesinv<-rev(as.character(unique(datplotconfusion$Observacion)))
  
  plot1 <- ggplot() +
    geom_tile(aes(x=Observacion, y=Prediccion,fill=Valor), 
              data=datplotconfusion, color="black",size=0.1) +
    labs(x="Observación real (%)",y="Predicción (%)") +
    scale_y_discrete(limits=clasesinv)
  
  plot1 = plot1 + 
    geom_text(aes(x=Observacion, y=Prediccion, label=sprintf("%.2f", Valor)), 
              data=datplotconfusion, size=3, colour="black") +
    scale_fill_gradient(low="gray",high="red")
  
  #Obtenemos la matriz de confusion en numero de muestras
  datplotconfusion <- as.data.frame(par_crosstable[1]$t)
  colnames(datplotconfusion) <- c('Observacion','Prediccion','Valor')
  #Ordenamos los factores de las clases
  datplotconfusion <- datplotconfusion %>% arrange(Observacion)
  clasesord <- sort(as.character(levels(datplotconfusion$Observacion)),decreasing = FALSE)
  datplotconfusion$Observacion <- factor(datplotconfusion$Observacion, levels = clasesord)
  #Ordenar las clases descendente (para el plot)
  clasesinv<-rev(as.character(unique(datplotconfusion$Observacion)))
  plot2 <- ggplot() +
    geom_tile(aes(x=Observacion, y=Prediccion,fill=Valor), 
              data=datplotconfusion, color="black",size=0.1) +
    labs(x="Observación real (nº)",y="Predicción (nº)") +
    scale_y_discrete(limits=clasesinv)
  
  plot2 = plot2 + 
    geom_text(aes(x=Observacion, y=Prediccion, label=sprintf("%d", Valor)), 
              data=datplotconfusion, size=3, colour="black") +
    scale_fill_gradient(low="gray",high="red")
  return(grid.arrange(plot1, plot2, ncol=2))
}
print(plot_matriz_confusion(csstab_glm1))

#############################################################################
## Modelo regresion logistica 2
#############################################################################
modelo_glm2 <- glm(formula = Survived ~ Pclass+SibSp+Sex+Age,
                   data = data_train, family = binomial(link = "logit"))
summary(modelo_glm2)
predict_glm2 <- predict.glm(object=modelo_glm2, newdata=data_test, type = "response")
r2 = pROC::roc(response=data_test$Survived, predictor = predict_glm2)
threshold_r2 <- coords(r2, "best", ret = "threshold")
predict_glm2 <- ifelse(predict_glm2 < threshold_r2, 0, 1)
mat.confusion_glm2 <- table(data_test$Survived, predict_glm2)
mat.confusion_glm2
pct.correcto_glm2 <- 100 * sum(diag(mat.confusion_glm2)) / sum(mat.confusion_glm2)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",
              pct.correcto_glm2))
csstab_glm2 <- CrossTable(data_test$Survived, predict_glm2,
                          prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE, 
                          dnn = c('Reality', 'Prediction'))
print(plot_matriz_confusion(csstab_glm2))

#############################################################################
## Modelo regresion logistica 3
#############################################################################
modelo_glm3 <- glm(formula = Survived ~ Sex+Pclass+Age+Child+FamilySize, 
                   data = data_train, family = binomial(link = "logit"))
summary(modelo_glm3)
predict_glm3 <- predict.glm(object=modelo_glm3, newdata=data_test, type = "response")
r3 = pROC::roc(response=data_test$Survived, predictor = predict_glm3)
threshold_r3 <- coords(r3, "best", ret = "threshold")
predict_glm3 <- ifelse(predict_glm3 < threshold_r3, 0, 1)
mat.confusion_glm3 <- table(data_test$Survived, predict_glm3)
mat.confusion_glm3
pct.correcto_glm3 <- 100 * sum(diag(mat.confusion_glm3)) / sum(mat.confusion_glm3)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",
              pct.correcto_glm3))

csstab_glm3 <- CrossTable(data_test$Survived, predict_glm3,
                          prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE, 
                          dnn = c('Reality', 'Prediction'))
print(plot_matriz_confusion(csstab_glm3))

#############################################################################
# Antes de crear el modelo nº4 de glm, vamos a ver la interaccion de variables
#
#interaction.plot(data$SibSp, data$Child, data$SurvivedNum)
#

#Para ello antes de nada vamos a definir una variable 'objetivo' pero numérica
data$SurvivedNum <- as.integer(as.character(data$Survived))

#Agrupamos el dataset por las variables Sex y Pclass
data.agrup <- data %>% group_by(Sex, Pclass)

#Calculamos la media creando un campo 'Mean_Survived' de la media
data.mediasurvived <- summarise(.data = data.agrup, Mean_Survived = mean(SurvivedNum))
kable(data.mediasurvived)

plot1 <- ggplot(data=data.mediasurvived, aes(x=Pclass, y = Mean_Survived, group=Sex))+
  geom_line(aes(color=Sex), size=1) + 
  geom_point(aes(color=Sex, shape=Sex), size=2) + 
  theme(legend.position="top") +
  theme(legend.title = element_text(size=12), legend.text = element_text(size = 11))
plot2 <- ggplot(data=data.mediasurvived, aes(x=Sex, y = Mean_Survived, group=Pclass))+
  geom_line(aes(color=Pclass), size=1) + 
  geom_point(aes(color=Pclass, shape=Pclass), size=2) +
  theme(legend.position="top", legend.title = element_text(size=12)) + 
  theme(legend.text = element_text(size = 11))
#library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

#Repetimos la operación pero para AgeInterval y Sex
#Agrupamos el dataset por las variables AgeInterval y Sex
data.agrup <- data %>% group_by(AgeInterval, Sex)

#Calculamos la media creando un campo 'Mean_Survived' de la media
data.mediasurvived <- summarise(.data = data.agrup, Mean_Survived = mean(SurvivedNum))
kable(data.mediasurvived)
#Recordemos los cortes de edades intervalos_edad <- c(0,13,18,40,55) de AgeInterval
data[data$Age==55,'AgeInterval']
plot1 <- ggplot(data=data.mediasurvived, aes(x=AgeInterval, y = Mean_Survived, group=Sex))+
  geom_line(aes(color=Sex), size=1) + 
  geom_point(aes(color=Sex, shape=Sex), size=2) + 
  scale_x_discrete(name="AgeInterval", breaks=c("1", "2", "3", "4", "5"), 
                   labels=labelsAgeInterval)+
  theme(legend.position="top") +
  theme(legend.title = element_text(size=12), legend.text = element_text(size = 11))
plot2 <- ggplot(data=data.mediasurvived, aes(x=Sex, y = Mean_Survived, group=AgeInterval))+
  geom_line(aes(color=AgeInterval), size=1) + 
  geom_point(aes(color=AgeInterval, shape=AgeInterval), size=2) +
  theme(legend.position="top", legend.title = element_text(size=12)) + 
  theme(legend.text = element_text(size = 11))
#library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)
#############################################################################
## Modelo regresion logistica 4
#############################################################################
modelo_glm4 <- glm(formula = Survived ~ Sex+Parch+SibSp+AgeInterval:Sex+Sex:Pclass, 
                   data = data_train, family = binomial(link = "logit"))
summary(modelo_glm4)
predict_glm4 <- predict.glm(object=modelo_glm4, newdata=data_test, type = "response")
r4 = pROC::roc(response=data_test$Survived, predictor = predict_glm4)
threshold_r4 <- coords(r4, "best", ret = "threshold")
predict_glm4 <- ifelse(predict_glm4 < threshold_r4, 0, 1)
mat.confusion_glm4 <- table(data_test$Survived, predict_glm4)
mat.confusion_glm4
pct.correcto_glm4 <- 100 * sum(diag(mat.confusion_glm4)) / sum(mat.confusion_glm4)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",
              pct.correcto_glm4))

csstab_glm4 <- CrossTable(data_test$Survived, predict_glm4,
                          prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE, 
                          dnn = c('Reality', 'Prediction'))
print(plot_matriz_confusion(csstab_glm4))
#library(pROC)

r1$auc
r2$auc
r3$auc
r4$auc

#coords(r3, "best", ret = "threshold")
colores <- (hue_pal()(4))
# plot(r1, col=colores[1], lty=1, lwd=2)
# plot(r2, col=colores[2], lty=2, lwd=2, add=TRUE)
# plot(r3, col=colores[3], lty=4, lwd=2, add=TRUE)
# plot(r4, col=colores[4], lty=5, lwd=2, add=TRUE)

#Curvas ROC
ggroc(list(glm1=r1, glm2=r2, glm3=r3, glm4=r4), 
            aes=c("color"), linetype="dotted", size = 1.1) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
               color="grey", linetype="dashed") +
  labs(color = "Modelos")

g1<-ggroc(list(glm1=r1, glm2=r2), 
      aes=c("color"), linetype="solid", size = 1) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
               color="grey", linetype="dashed") +
  labs(color = "Modelos") +
  facet_grid(.~name)
g2<-ggroc(list(glm3=r3, glm4=r4), 
      aes=c("color"), linetype="solid", size = 1) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
               color="grey", linetype="dashed") +
  labs(color = "Modelos") +
  facet_grid(.~name)
grid.arrange(g1, g2, nrow=2)


data$SurvivedNum <- NULL

################################################################
#ARBOL DE DECISION
################################################################
#Arbol de decision
################################################################
tctrl <- caret::trainControl(method = "repeatedcv",
                             number=10, repeats = 3)
model_tree1 <- caret::train(Survived~Sex+FamilySize+Child+Age+Pclass, 
                            data=data_train, 
                            method="rpart",
                            trControl = tctrl)
summary(model_tree1)
#library(rpart.plot)
rpart.plot(model_tree1$finalModel)

#Realizamos la predicción con el modelo construido con rpart y el dataset test
predict_tree1 <-predict(model_tree1, data_test)

mat.confusion_tree1 <- table(data_test$Survived, predict_tree1)
mat.confusion_tree1
pct.correcto_tree1 <- 100 * sum(diag(mat.confusion_tree1)) / sum(mat.confusion_tree1)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",
              pct.correcto_tree1))
csstab_tree1 <- CrossTable(data_test$Survived, predict_tree1,
                          prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE, 
                          dnn = c('Reality', 'Prediction'))
print(plot_matriz_confusion(csstab_tree1))
#################################################################
# Arbol decision2
#################################################################
tctrl <- caret::trainControl(method = "repeatedcv",
                             number=10, repeats = 3)
tgrid <- data.frame(maxdepth = seq(2,10,1))

model_tree2<- train(Survived ~ Sex+FamilySize+Child+Age+Pclass, 
                    data=data_train,
                    trControl=tctrl, 
                    tuneGrid=tgrid,
                    method="rpart2")

rpart.plot(model_tree2$finalModel)
summary(model_tree2)

#Realizamos la predicción con el modelo construido con rpart y el dataset test
predict_tree2 <-predict(model_tree2, data_test)
mat.confusion_tree2 <- table(data_test$Survived, predict_tree2)
mat.confusion_tree2
pct.correcto_tree2 <- 100 * sum(diag(mat.confusion_tree2)) / sum(mat.confusion_tree2)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",
              pct.correcto_tree2))
print(paste0("El mejor parámetro para el árbol de clasificación ha sido: ",
            names(model_tree2$bestTune),"=", model_tree2$bestTune))
csstab_tree2 <- CrossTable(data_test$Survived, predict_tree2,
                           prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE, 
                           dnn = c('Reality', 'Prediction'))
print(plot_matriz_confusion(csstab_tree2))

#Importancia de las variables
datosVarImp <- varImp(model_tree2)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall)
datosVarImp$Variable <- reorder(datosVarImp$rowname, datosVarImp$Overall)
ggplot(datosVarImp)+
  geom_col(aes(x = Variable, y = Overall), fill="steelblue", color="blue")+
  coord_flip() + ggtitle("Importancia de las variables")

###########################################################################
# Random Forest
###########################################################################
tctrl <- caret::trainControl(method = "repeatedcv",
                             number=10, repeats = 3)
model_rf <- caret::train(Survived ~ Sex+Parch+SibSp+Child+Age+Pclass, 
                          data = data_train, 
                          method = "rf", 
                          trControl = tctrl,
                          verbose = FALSE)
plot(model_rf)
predict_rf <- predict(model_rf, data_test)
mat.confusion_rf <- table(data_test$Survived, predict_rf)
mat.confusion_rf
pct.correcto_rf<-100 * sum(diag(mat.confusion_rf)) / sum(mat.confusion_rf)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",
              pct.correcto_rf))
csstab_rf <- CrossTable(data_test$Survived, predict_rf,
                        prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE, 
                        dnn = c('Reality', 'Prediction'))
print(plot_matriz_confusion(csstab_rf))
#Importancia de las variables
datosVarImp <- varImp(model_rf)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall)
datosVarImp$Variable <- reorder(datosVarImp$rowname, datosVarImp$Overall)
ggplot(datosVarImp)+
  geom_col(aes(x = Variable, y = Overall), fill="steelblue", color="blue")+
  coord_flip() + ggtitle("Importancia de las variables")

#################################################################
#Vamos a utilizar otros conjuntos train y test
#porque hay que hacer conversiones numéricas (las categóricas a numéricas)
train_xgb <- data_train
test_xgb <- data_test


train_xgb$Sex <- as.integer(ifelse((train_xgb$Sex == "male"),1,0))
test_xgb$Sex <- as.integer(ifelse((test_xgb$Sex == "male"),1,0))
train_xgb$Pclass <- as.integer(as.character(train_xgb$Pclass))
test_xgb$Pclass <- as.integer(as.character(test_xgb$Pclass))
train_xgb$Embarked <- as.integer(ifelse((train_xgb$Embarked == "C"),0,
                                        ifelse(train_xgb$Embarked=='Q',1,2)))
test_xgb$Embarked <- as.integer(ifelse((test_xgb$Embarked == "C"),0,
                                       ifelse(test_xgb$Embarked=='Q',1,2)))
train_xgb$Child <- as.integer(as.character(train_xgb$Child))
test_xgb$Child <- as.integer(as.character(test_xgb$Child))
train_xgb$AgeInterval <- as.integer(as.character(train_xgb$AgeInterval))
test_xgb$AgeInterval <- as.integer(as.character(test_xgb$AgeInterval))
train_xgb$Survived <- as.integer(as.character(train_xgb$Survived))
test_xgb$Survived <- as.integer(as.character(test_xgb$Survived))

df_status(train_xgb)  #Ya están convertidas las variables categóricas a integer


predictores <- c('Sex', 'Pclass','AgeInterval','Child', 'Fare')                           #84.23 #82.43 #83.33
var.objetivo <- train_xgb$Survived
predictores <- c('Sex', 'Child', 'Pclass', 'AgeInterval','Fare') #82.43
model_xgb <- xgboost(data = as.matrix(train_xgb[,predictores]), 
                     label = var.objetivo,
                     objective = "binary:logistic",
                     eval_metric = "logloss",
                     max_depth = 8,
                     nfold=4,
                     nrounds = 80) 
predict_xgb <- predict(model_xgb, as.matrix(test_xgb[,predictores]))
rxgb = pROC::roc(response=test_xgb$Survived, predictor = predict_xgb)
plot(rxgb, lty=2, lwd=2)
print(rxgb$auc)
valor_threshold <- coords(rxgb, "best", ret = "threshold")

predict_xgb <- ifelse(predict_xgb <= valor_threshold, 0, 1)
mat.confusion_xgb <- table(test_xgb$Survived, predict_xgb)
mat.confusion_xgb
porcentaje.correcto_xgb <- 100 * sum(diag(mat.confusion_xgb)) / sum(mat.confusion_xgb)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",
              porcentaje.correcto_xgb))

csstab_xgboost <- CrossTable(test_xgb$Survived, predict_xgb,
                             prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE, 
                             dnn = c('Reality', 'Prediction'))
print(plot_matriz_confusion(csstab_xgboost))
ggroc(list(xgboost=rxgb), 
      aes=c("color"), linetype="solid", size = 1) +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
               color="grey", linetype="dashed") +
  labs(color = "Modelo") + theme(legend.position="none") +
  ggtitle('Curva ROC - Modelo xgboost')
#################################################################
#################################################################


df_status(train_xgb)
table(train_xgb$Pclass)
table(train_xgb$Embarked)
train_xgb <- data_train
test_xgb <- data_test
train_xgb$Sex <- as.integer(ifelse((train_xgb$Sex == "male"),1,0))
test_xgb$Sex <- as.integer(ifelse((test_xgb$Sex == "male"),1,0))
train_xgb$Pclass <- as.integer(as.character(train_xgb$Pclass))
test_xgb$Pclass <- as.integer(as.character(test_xgb$Pclass))
train_xgb$Embarked <- as.integer(ifelse((train_xgb$Embarked == "C"),0,ifelse(train_xgb$Embarked=='Q',1,2)))
test_xgb$Embarked <- as.integer(ifelse((test_xgb$Embarked == "C"),0,ifelse(test_xgb$Embarked=='Q',1,2)))
train_xgb$Child <- as.integer(as.character(train_xgb$Child))
test_xgb$Child <- as.integer(as.character(test_xgb$Child))
train_xgb$AgeInterval <- as.integer(as.character(train_xgb$AgeInterval))
test_xgb$AgeInterval <- as.integer(as.character(test_xgb$AgeInterval))
train_xgb$Survived <- as.integer(as.character(train_xgb$Survived))
test_xgb$Survived <- as.integer(as.character(test_xgb$Survived))
train_xgb$HombreAdultoSolo <- NULL
test_xgb$HombreAdultoSolo <- NULL
library(xgboost)
predictores <- c('Pclass', 'Parch', 'SibSp','Fare', 'Sex')  #81.98
predictores <- c('Sex', 'Child', 'Pclass', 'AgeInterval','Fare')  #84.23
#predictores <- c('Pclass', 'Embarked', 'Parch', 'SibSp','Fare', 'Sex')  #80.63
var.objetivo <- train_xgb$Survived
modelo.XGB <- xgboost(data = as.matrix(train_xgb[,predictores]), 
                      label = var.objetivo,
                      objective = "binary:logistic",
                      eval_metric = "logloss",
                      max_depth = 8,
                      nfold=4,
                      nrounds = 100) 
prediccion_xgb <- predict(modelo.XGB, as.matrix(test_xgb[,predictores]))
rxgb = roc(response=test_xgb$Survived, predictor = prediccion_xgb)
plot(rxgb, lty=2, lwd=2)
print(rxgb$auc)
valor_threshold <- coords(rxgb, "best", ret = "threshold")

predict_xgb <- ifelse(prediccion_xgb <= valor_threshold, 0, 1)
mat.confusion_xgb <- table(test_xgb$Survived, predict_xgb)
mat.confusion_xgb
porcentaje.correcto_xgb <- 100 * sum(diag(mat.confusion_xgb)) / sum(mat.confusion_xgb)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",
              porcentaje.correcto_xgb))





############################################################################
#Graficos
###########################################################################
colores_defecto_ggplot = (hue_pal()(2))
colores_defecto_ggplot_3 = (hue_pal()(3))
#Supervivencia
#ggplot(data, aes(x=Survived))+
#  geom_bar(color="black", fill=colores_defecto_ggplot) +
#  ggtitle("Survived (0=No/1=Yes)")

###########################################################################
# Survived
###########################################################################
#Survived (barras y tarta)
tabdatos <- data.frame(table(data$Survived))
tabdatos$Pct <- (tabdatos$Freq / sum(tabdatos$Freq)) * 100

p1<-ggplot(tabdatos, aes(x=Var1, y = Freq)) +
  geom_bar(stat="identity", colour = "black", alpha=0.7, fill=colores_defecto_ggplot) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5),color="black", size=4)+
  geom_text(aes(label = sprintf("(%.2f %%)", Pct)), 
            position = position_stack(vjust = 0.35),color="black", size=3)+
  scale_x_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  labs(y="Passengers")+
  ggtitle("Passengers Survived (No/Yes)")

p2<-ggplot(data.frame(tabdatos), aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), color="black", size=3) +
  coord_polar(theta = "y", start=0) + ggtitle("Passengers Survived (No/Yes)") + 
  xlab("") + ylab("") + labs(fill = 'Survived') + 
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_discrete(name="Survived",
                      breaks=c("0", "1"),
                      labels=c("No", "Yes"))
grid.arrange(p1, p2, ncol=2)

###########################################################################
# Age
###########################################################################
#Histograma de Age
p1 <- ggplot(data, aes(Age)) + 
  geom_histogram(color="black", fill="steelblue3", binwidth = 2) +
  ggtitle("Histograma de Age")

#Boxplot de Age
p2 <- ggplot(data, aes(y=Age)) + 
  geom_boxplot(color="black", fill="steelblue3", width=0.1) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  ggtitle("Boxplot de Age")

grid.arrange(p1, p2, ncol=2)

###########################################################################
# Fare
###########################################################################
#Histograma de Fare
p1 <- ggplot(data, aes(Fare)) + 
  geom_histogram(color="black", fill="steelblue3", bins=30) +
  ggtitle("Histograma de Fare")
#Boxplot de Fare
p2 <- ggplot(data, aes(y=Fare)) + 
  geom_boxplot(color="black", fill="steelblue3", width=0.1) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  ggtitle("Boxplot de Fare")
grid.arrange(p1, p2, ncol=2)

###########################################################################
# Price
###########################################################################
#Histograma de Price
p1 <- ggplot(data, aes(Price)) + 
  geom_histogram(color="black", fill="steelblue3", bins=30) +
  ggtitle("Histograma de Price")

#Boxplot de Price
p2 <- ggplot(data, aes(y=Price)) + 
  geom_boxplot(color="black", fill="steelblue3", width=0.1) +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  ggtitle("Boxplot de Price")
grid.arrange(p1, p2, ncol=2)

###########################################################################
# Sex
###########################################################################
#Pasajeros por Sex (barras)
tabdatos <- data.frame(table(data$Sex))
tabdatos$Pct <- (tabdatos$Freq / sum(tabdatos$Freq)) * 100

p1<-ggplot(tabdatos, aes(x=Var1, y = Freq)) +
  geom_bar(stat="identity", colour = "black", alpha=0.7, fill=colores_defecto_ggplot) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.75),color="black", size=4)+
  geom_text(aes(label = sprintf("(%.2f %%)", Pct)), 
            position = position_stack(vjust = 0.35),color="black", size=3)+
  labs(y="Passengers")+scale_x_discrete(name="Sex")+
  ggtitle("Passengers (by Sex)")

#Tarta de Pasajeros por Sex
p2<-ggplot(tabdatos, aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), color="black", size=3) +
  coord_polar(theta = "y", start=0) + ggtitle("Passengers (by Sex)") + 
  xlab("") + ylab("") + labs(fill = 'Sex') + 
  theme(axis.text.x = element_text(size = 7))
grid.arrange(p1, p2, ncol=2)
###########################################################################
# Embarked
###########################################################################
#Pasajeros por Embarked (barras)
tabdatos <- data.frame(table(data$Embarked))
tabdatos$Pct <- (tabdatos$Freq / sum(tabdatos$Freq)) * 100

p1<-ggplot(tabdatos, aes(x=Var1, y = Freq)) +
  geom_bar(stat="identity", colour = "black", alpha=0.7, fill=colores_defecto_ggplot_3) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.75),color="black", size=4)+
  geom_text(aes(label = sprintf("(%.2f %%)", Pct)), 
            position = position_stack(vjust = 0.35),color="black", size=3)+
  scale_x_discrete(name="Embarked",breaks=c("C", "Q", "S"),
                   labels=c("C = Cherbourg", "Q = Queenstown", "S = Southampton"))+
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  labs(y="Passengers")+
  ggtitle("Passengers (by Embarked)")

#Tarta de Pasajeros por Embarked
p2<-ggplot(data.frame(table(data$Embarked)), aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), color="black", size=3) +
  coord_polar(theta = "y", start=0) + ggtitle("Passengers (by Embarked)") + 
  xlab("") + ylab("") + labs(fill = 'Embarked') + 
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_discrete(name="Embarked",
                      breaks=c("C", "Q", "S"),
                      labels=c("C = Cherbourg", "Q = Queenstown", "S = Southampton"))

#Tarta de Pasajeros por Embarked Proporciones
p3<-ggplot(data.frame(round(prop.table(table(data$Embarked))*100,2)), aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(Freq,'%')), position = position_stack(vjust = 0.5), color="black", size=3) +
  coord_polar(theta = "y", start=0) + ggtitle("% Passengers (by Embarked)") + 
  xlab("") + ylab("") + labs(fill = 'Embarked') + 
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_discrete(name="Embarked",
                      breaks=c("C", "Q", "S"),
                      labels=c("C = Cherbourg", "Q = Queenstown", "S = Southampton"))

grid.arrange(p1, arrangeGrob(p2, p3, ncol=1), ncol=2)

###########################################################################
# Pclass
###########################################################################
#Pasajeros por Embarked (barras)
tabdatos <- data.frame(table(data$Pclass))
tabdatos$Pct <- (tabdatos$Freq / sum(tabdatos$Freq)) * 100

p1<-ggplot(tabdatos, aes(x=Var1, y = Freq)) +
  geom_bar(stat="identity", colour = "black", alpha=0.7, fill=colores_defecto_ggplot_3) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.75),color="black", size=4)+
  geom_text(aes(label = sprintf("(%.2f %%)", Pct)), 
            position = position_stack(vjust = 0.35),color="black", size=3)+
  scale_x_discrete(name="Pclass",
                   breaks=c("1", "2", "3"),
                   labels=c("1st class", "2nd class", "3rd class"))+
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  labs(y="Passengers")+
  ggtitle("Passengers (by Pclass)")
#Tarta de Pasajeros por Pclass
p2<-ggplot(data.frame(table(data$Pclass)), aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), color="black", size=3) +
  coord_polar(theta = "y", start=0) + ggtitle("Passengers (by Pclass)") + 
  xlab("") + ylab("") + labs(fill = 'Pclass') + 
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_discrete(name="Pclass",
                      breaks=c("1", "2", "3"),
                      labels=c("1st class", "2nd class", "3rd class"))
#Proporcion Tarta de Pasajeros por Pclass
p3<-ggplot(data.frame(round(prop.table(table(data$Pclass))*100,2)), aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(Freq,'%')), position = position_stack(vjust = 0.5), color="black", size=3) +
  coord_polar(theta = "y", start=0) + ggtitle("%Passengers (by Pclass)") + 
  xlab("") + ylab("") + labs(fill = 'Pclass') + 
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_discrete(name="Pclass",
                      breaks=c("1", "2", "3"),
                      labels=c("1st class", "2nd class", "3rd class"))

grid.arrange(p1, arrangeGrob(p2, p3, ncol=1), ncol=2)
  
###########################################################################
# SibSp y Parch
###########################################################################

tabdatos <- data.frame(table(data$SibSp))
tabdatos$Pct <- (tabdatos$Freq / sum(tabdatos$Freq)) * 100

p1<-ggplot(tabdatos, aes(x=Var1, y = Freq)) +
  geom_bar(stat="identity", colour = "black", alpha=0.7, fill=(hue_pal()(nrow(tabdatos)))) +
  geom_text(aes(label = Freq), vjust = -0.4,color="black", size=3)+
  geom_text(aes(label = sprintf("(%.2f %%)", Pct)), 
            vjust = 1,color="black", size=3)+
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_discrete(name='SibSp')+
  labs(y="Passengers")+
  ggtitle("Passengers (by SibSp)")

## Parch
tabdatos <- data.frame(table(data$Parch))
tabdatos$Pct <- (tabdatos$Freq / sum(tabdatos$Freq)) * 100

p2<-ggplot(tabdatos, aes(x=Var1, y = Freq)) +
  geom_bar(stat="identity", colour = "black", alpha=0.7, fill=(hue_pal()(nrow(tabdatos)))) +
  geom_text(aes(label = Freq), vjust = -0.4,color="black", size=3)+
  geom_text(aes(label = sprintf("(%.2f %%)", Pct)), 
            vjust = 1,color="black", size=3)+
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  scale_x_discrete(name='Parch')+
  labs(y="Passengers")+
  ggtitle("Passengers (by Parch)")

grid.arrange(p1, p2, ncol=2)
###########################################################################
# AgeInterval
###########################################################################
tabdatos <- data.frame(table(data$AgeInterval))
tabdatos$Pct <- (tabdatos$Freq / sum(tabdatos$Freq)) * 100

p1<-ggplot(data=tabdatos, aes(x=Var1, y = Freq))+
  geom_bar(stat="identity", colour = "black", alpha=0.7, fill=(hue_pal()(nrow(tabdatos)))) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5),color="black", size=4)+
  geom_text(aes(label = sprintf("(%.2f %%)", Pct), y=-7), color="black", size=3)+
  scale_x_discrete(name="AgeInterval (years)", breaks=c("1", "2", "3", "4", "5"), 
                   labels=labelsAgeInterval)+
  labs(y="Passengers")+
  ggtitle("Passengers (by AgeInterval)")

p2<-ggplot(data.frame(round(prop.table(table(data$AgeInterval))*100,2)), aes(x="", y=Freq, fill=Var1)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(Freq,'%'),x=1.3), position = position_stack(vjust = 0.5), color="black", size=3) +
  coord_polar(theta = "y", start=0) + ggtitle("%Passengers (by AgeInterval)") + 
  xlab("") + ylab("") + labs(fill = 'Pclass') + 
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_discrete(name="AgeInterval (years)", breaks=c("1", "2", "3", "4", "5"), 
                      labels=labelsAgeInterval)
grid.arrange(p1, p2, ncol=2)

###########################################################################
# Child
###########################################################################
tabdatos <- data.frame(table(data$Child))
tabdatos$Pct <- (tabdatos$Freq / sum(tabdatos$Freq)) * 100
tabdatos <- tabdatos[order(tabdatos$Var1,decreasing = TRUE),]
tabdatos$Var1 <- as.integer(tabdatos$Var1)

p1<-ggplot(tabdatos, aes(x=reorder(Var1, -Var1), y = Freq)) +
  geom_bar(stat="identity", colour = "black", alpha=0.7, fill=colores_defecto_ggplot) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5),color="black", size=4)+
  geom_text(aes(label = sprintf("(%.2f %%)", Pct), y=-11),color="black", size=3)+
  scale_x_discrete(name="Child",
                   labels=c(paste0("Yes (<=",edad_corte,' years)'), 
                            paste0("No (>",edad_corte,' years)')))+
  scale_y_continuous(breaks = seq(0, 900, 100)) +
  labs(y="Passengers")+
  ggtitle("Passengers (Childs Yes/No)")

p2<-ggplot(data.frame(table(data$Child)), aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5), 
            color="black", size=3) +
  coord_polar(theta = "y", start=0) + ggtitle("Passengers (Childs Yes/No)") + 
  xlab("") + ylab("") + labs(fill = 'Child') + 
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_discrete(name="Child",
                      breaks=c("0", "1"),
                      labels=c(paste0("No (>",edad_corte,' years)'),
                               paste0("Yes (<=",edad_corte,' years)')))
grid.arrange(p1, p2, ncol=2)

###########################################################################
###########################################################################
###########################################################################

#Histograma de Age en función del género
p1<-ggplot(data, aes(Age, fill = Sex)) +
  geom_histogram(position = "dodge", colour = "black", binwidth = 5, alpha=0.7) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) + 
  labs(y='Passengers') + 
  ggtitle("Histograma de Age (por Sex)")


p2<-ggplot(data, aes(Age, fill = Sex)) +
  geom_histogram(colour = "black", binwidth = 5, alpha=0.7) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) + 
  labs(y='Passengers') + 
  ggtitle("Histograma de Age (por Sex)")

p3<-ggplot(data, aes(Age, fill = Survived)) + 
  geom_histogram(binwidth = 5, colour="black") + 
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 120, 20)) +
  scale_fill_discrete(name="Survived", breaks=c("0", "1"), labels=c("No", "Yes"))+
  facet_grid(~Sex) +
  labs(y='Passengers')

grid.arrange(arrangeGrob(p1, p2, ncol=2), p3, nrow=2)
################################################################################
#Supervivencia por sexo
################################################################################
# ggplot(data=data,aes(x=Sex,fill=Survived))+
#   geom_bar() +
#   ggtitle('Survived (by Sex)')
#Supervivencia por sexo
p1<-ggplot(data.frame(table(data$Sex, data$Survived)), aes(x=Var1, y = Freq, fill=Var2)) +
  geom_bar(stat="identity", colour = "black", alpha=0.7) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5),color="black", size=3)+
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  labs(y='Passengers', x='Sex')+
  ggtitle("Survived (by Sex)")

#Pasajeros por Sexo y Supervivencia (proporciones)
p2<-ggplot(as.data.frame(prop.table(table(data$Sex,data$Survived),margin = 1)), 
       aes(x=Var1, y = Freq, fill=Var2)) + 
  geom_bar(stat="identity", position="fill") + 
  geom_text(aes(label = paste(round(Freq*100,2),'%')), position=position_stack(vjust = 0.5), 
            color="black", size=3)+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  labs(x='Sex', fill='Survived') +
  ggtitle('% Survived (by Sex)')

p3<-ggplot(data.frame(table(data$Sex, data$Survived)), aes(x=Var1, y = Freq, fill=Var2)) +
  geom_bar(stat="identity", position = "dodge", colour = "black", alpha=0.7) +
  geom_text(aes(label = Freq), position = position_dodge(width=1),vjust=1.5,color="black", size=3)+
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  labs(y='Passengers', x='Sex')+
  ggtitle("Survived (by Sex)")
grid.arrange(arrangeGrob(p1, p2, ncol=2), p3, nrow=2)
################################################################################
#Boxplot de supervivencia (por edad)
p1<-ggplot(data = data, aes(x=Survived, y=Age))+
  scale_x_discrete(name ="Survived", labels=c("0"="No","1"="Yes"))+
  geom_boxplot(color="black", fill=c(colores_defecto_ggplot[1],colores_defecto_ggplot[2])) +
  ggtitle("Boxplot Survived (by Age)")


#Boxplot de edad de supervivencia por género
p2<-ggplot(data = data, aes(x=Survived, y=Age, fill=Sex))+
  geom_boxplot() + 
  scale_x_discrete(name ="Survived", labels=c("0"="No","1"="Yes"))+
  ggtitle("Boxplot Survived (by Age & Sex)")
grid.arrange(p1, p2, ncol=2)


################################################################################
# Beeswarm Age vs Survived
################################################################################
# Boxplot/Beeswarm Boxplot/Beeswarm (Age vs Survived)
vector.color <- rep(hue_pal()(2)[1], length(data$Age))
vector.color[data$Survived==1] <- hue_pal()(2)[2]

beeswarm(data$Age, method = 'swarm', spacing=0.5, cex=0.6, 
         pch = 16, pwcol = vector.color,
         xlab = '', ylab = 'Age', main = 'Boxplot/Beeswarm (Age vs Survived)')
boxplot(data$Age, add=TRUE, col=rgb(0.8,0.8,0.8, alpha=0.1))
legend("topright", c("Died", "Survived"), adj = c(0, 0.6), col = hue_pal()(2),
       text.col = "black", pch = c(16, 16), cex = 0.75, plot=TRUE)
grid(NA, 10, lwd = 1, col='lightgrey') 
################################################################################
# Boxplot/Beeswarm Boxplot/Beeswarm (Age vs Sex vs Survived)
beeswarm(data$Age ~ data$Sex, data = data, method = 'swarm', spacing=0.6, cex=0.5, 
         pch = 16, pwcol = vector.color,
         xlab = '', ylab = 'Age', main = 'Boxplot/Beeswarm (Age vs Sex vs Survived)')
boxplot(data$Age ~ data$Sex, data = data, add = TRUE,
        names = c("",""), col=rgb(0.8,0.8,0.8, alpha=0.1))
legend("topright", c("Died", "Survived"), adj = c(0, 0.6), col = hue_pal()(2),
       text.col = "black", pch = c(16, 16), cex = 0.75, plot=TRUE)
grid(NA, 10, lwd = 1, col='lightgrey') 

################################################################################
# Boxplot/Beeswarm (Age vs Pclass)
color.clase <- as.data.frame(cbind(levels(data$Pclass), hue_pal()(3)))
colnames(color.clase) <- c('class', 'color')
vector.color <- color.clase[match(data$Pclass, color.clase$class),'color']
vector.color <- as.character(vector.color)

titulo = 'Age vs Pclass'
beeswarm(data$Age ~ data$Pclass, data = data,
         method = 'swarm', spacing=0.5, cex=0.6, 
         pch = 16, pwcol = vector.color,
         xlab = 'Pclass', ylab = 'Age',
         labels=c('1st','2nd','3rd'))
boxplot(data$Age ~ data$Pclass, data = data, add = TRUE,
        names = c("","",""), col=rgb(0.8,0.8,0.8, alpha=0.1), 
        main=paste0("Boxplot/Beeswarm - ",titulo))
grid(NA, 10, lwd = 1, col='lightgrey')

################################################################################
# Beeswarm (Age vs Pclass vs Survived)
vector.color <- rep(hue_pal()(2)[1], length(data$Age))
vector.color[data$Survived==1] <- hue_pal()(2)[2]
titulo = 'Age vs Pclass'
beeswarm(data$Age ~ data$Pclass, data = data,
         method = 'swarm', spacing=0.5, cex=0.6, 
         pch = 16, pwcol = vector.color, main="Beeswarm (Age vs Pclass vs Survived)",
         xlab = 'Pclass', ylab = 'Age',
         labels=c('1st','2nd','3rd'))
#boxplot(data$Age ~ data$Pclass, data = data, add = TRUE,
#        names = c("","",""), col=rgb(0.8,0.8,0.8, alpha=0.1), 
#        main=paste0("Boxplot/Beeswarm - ",titulo))
legend("topright", c("Died", "Survived"), adj = c(0, 0.6), col = hue_pal()(2),
       text.col = "black", pch = c(16, 16), cex = 0.75, plot=TRUE)
grid(NA, 10, lwd = 1, col='lightgrey')

################################################################################
# FamilySize vs Survived
################################################################################
p1<-ggplot(data, aes(x = FamilySize, fill = Survived)) +
  geom_bar(position='dodge') +
  scale_x_continuous(breaks=c(min(data$FamilySize):max(data$FamilySize))) +
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  labs(x = 'FamilySize', y='Passengers') +
  ggtitle('Passengers (by FamilySize)')

p2<-ggplot(data, aes(x = FamilySize, fill = Survived)) +
  geom_bar(position='fill') +
  scale_x_continuous(breaks=c(min(data$FamilySize):max(data$FamilySize))) +
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  labs(x = 'FamilySize', y='Proportion') +
  ggtitle('Survived (by FamilySize)')
grid.arrange(p1, p2, nrow=2)

################################################################################
# Pclas vs Embarked vs Survived
################################################################################
#Supervivencia por Pclass & Embarked
p1<-ggplot(data = data,aes(x=Embarked,fill=Survived))+
  geom_bar()+
  labs(title='Survived (by Pclass & Embarked)', y='Passengers')+
  scale_fill_discrete(name="Survived",
                      breaks=c("0", "1"),
                      labels=c("No", "Yes"))+
  facet_wrap(~Pclass, labeller = pclass_to_string) +
  ggtitle('Survivors (by Embarked & Pclass)')

#Supervivencia por Pclass & Embarked (proporciones)
pclass_to_string <- as_labeller(c('1' = "1st class", '2' = "2nd class", '3'='3rd class'))
p2<-ggplot(data = data,aes(x=Embarked,fill=Survived))+
  geom_bar(position="fill")+ 
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  labs(y='Proportion of survivors (Embarked/Pclass)')+
  facet_wrap(~Pclass, labeller = pclass_to_string)+
  ggtitle('Survivors (by Embarked & Pclass)')

grid.arrange(p1, p2, nrow=2)

################################################################################
# Pclass vs Sex vs Survived
################################################################################
#Supervivencia por Sex y Pclass
p1<-ggplot(data = data,aes(x=Pclass,fill=Survived))+
  geom_bar()+ 
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  labs(y='Passengers')+
  scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
  facet_wrap(~Sex)+ theme(legend.position = "none") +
  ggtitle('Survivors (by Sex & Pclass)')

p2<-ggplot(data = data,aes(x=Pclass,fill=Survived))+
  geom_bar(position="fill")+ 
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  labs(y='Proportion of survivors (Embarked/Pclass)')+
  facet_wrap(~Sex)+
  ggtitle('Survivors (by Sex & Pclass)')
grid.arrange(p1, p2, ncol=2)


################################################################################
# Survived vs Embarked
################################################################################
#Supervivencia por puerto de embarque
#ggplot(data, aes(Embarked, fill = Survived)) +
#  geom_bar(position = "dodge", colour = "black", alpha=0.7) +
#  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
#  labs(y='Passengers')+
#  ggtitle("Survived (by Embarked)")
p1<-ggplot(data.frame(table(data$Embarked, data$Survived)), aes(x=Var1, y = Freq, fill=Var2)) +
  geom_bar(stat="identity", position = "dodge", colour = "black", alpha=0.7) +
  geom_text(aes(label = Freq), position = position_dodge(width=1),vjust=1.5,
            color="black", size=3)+
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  labs(y='Passengers', x='Embarked')+theme(legend.position = "none") +
  ggtitle("Survived (by Embarked)")

p2<-ggplot(data, aes(Embarked, fill = Survived)) +
  geom_bar(position = "fill", colour = "black", alpha=0.7) +
  scale_fill_discrete(name="Survived",
                      breaks=c("0", "1"),
                      labels=c("No", "Yes"))+
  labs(y='Proportion of survivors (by Embarked)')+
  ggtitle("Survived (by Embarked)")
grid.arrange(p1, p2, ncol=2)

# #Survived by Embarked
# ggplot(data.frame(table(data$Embarked, data$Survived)), aes(x=Var1, y = Freq, fill=Var2)) +
#   geom_bar(stat="identity", position = "dodge", colour = "black", alpha=0.7) +
#   geom_text(aes(label = Freq), position = position_dodge(width=1),vjust=1.5,color="black", size=3)+
#   scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
#   labs(y='Passengers', x='Embarked')+
#   ggtitle("Survived (by Embarked)")

################################################################################
# Survived vs Pclass
################################################################################
#Survived by Pclass
p1<-ggplot(data.frame(table(data$Pclass, data$Survived)), aes(x=Var1, y = Freq, fill=Var2)) +
  geom_bar(stat="identity", position = "dodge", colour = "black", alpha=0.7) +
  geom_text(aes(label = Freq), position = position_dodge(width=1),vjust=1.5,
            color="black", size=3)+
  scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
  scale_x_discrete(name="Pclass",breaks=c("1", "2", "3"),labels=c("1st", "2nd", "3rd"))+
  labs(y='Passengers', x='Pclass')+theme(legend.position = "none") +
  ggtitle("Survived (by Pclass)")

p2<-ggplot(data, aes(Pclass, fill = Survived)) +
  geom_bar(position = "fill", colour = "black", alpha=0.7) +
  scale_fill_discrete(name="Survived",
                      breaks=c("0", "1"),
                      labels=c("No", "Yes"))+
  scale_x_discrete(name="Pclass",breaks=c("1", "2", "3"),labels=c("1st", "2nd", "3rd"))+
  labs(y='Proportion of survivors (by Pclass)')+
  ggtitle("Survived (by Pclass)")

grid.arrange(p1, p2, ncol=2)

################################################################################
# Boxplot Age, Pclass y Sex ... y la media por sexos
################################################################################
#Boxplot de edad de supervivencia por género, Pclass (y medias)
ggplot(data, aes(x = Pclass, y = Age, fill=Sex)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=mean(data$Age[data$Sex=="male"])), 
             colour=colores_defecto_ggplot[2], linetype='dotted', lwd=1.2) +
  geom_hline(aes(yintercept=mean(data$Age[data$Sex=="female"])), 
             colour=colores_defecto_ggplot[1], linetype='dotted', lwd=1.2) +
  scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd")) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggtitle("Boxplot (Age vs Pclass by Sex) and Mean by Sex")

#Supervivencia por Pclass
# as.data.frame(table(data$Pclass, data$Survived))
# ggplot(data = as.data.frame(table(data$Pclass, data$Survived)),aes(x=Var2,y=Freq,fill=Var1))+
#   geom_bar(stat="identity") +
#   scale_fill_discrete(name="Survived",breaks=c("0", "1"),labels=c("No", "Yes"))+
#   labs(y='Passengers')+
#   scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
#   facet_wrap(~Sex)+
#   ggtitle('Survivors (by Sex & Pclass)')

################################################################################
# Survived vs Child
################################################################################
p1<-ggplot(data[data$Child==1,],aes(x=Pclass,fill=Survived))+
  geom_bar()+
  labs(y='Passengers')+
  scale_fill_discrete(name="Survived", breaks=c("0", "1"), labels=c("No", "Yes"))+
  facet_wrap(~Child, labeller = pchild_to_string) +
  scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
  theme(legend.position = "none") +
  ggtitle('Survivors (Child by Pclass)')


p2<-ggplot(data[data$Child==1,], aes(Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_fill_discrete(name="Survived",
                      breaks=c("0", "1"),
                      labels=c("No", "Yes"))+
  facet_wrap(~Child, labeller = pchild_to_string) +
  scale_x_discrete(name="Pclass",breaks=c("1", "2", "3"),labels=c("1st", "2nd", "3rd"))+
  labs(y='Proportion of survivors (by Pclass)')+
  ggtitle("Survived (Child by Pclass)")
grid.arrange(p1, p2, ncol=2)


################################################################################
# Survived vs Child vs Class
################################################################################
#Supervivientes por Clase y Corte Edad niños (8 años)
pchild_to_string <- as_labeller(c('0' = "Age > 8 years old", '1' = "Age <= 8 years old"))
p1<-ggplot(data,aes(x=Pclass,fill=Survived))+
  geom_bar()+
  labs(title='Survived (by Pclass & Child)', y='Passengers')+
  scale_fill_discrete(name="Survived", breaks=c("0", "1"), labels=c("No", "Yes"))+
  facet_wrap(~Child, labeller = pchild_to_string) +
  scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))
#ggtitle('Survivors (by Pclass & Child)')

p2<-ggplot(data,aes(x=Pclass,fill=Survived))+
  geom_bar(position='fill')+
  labs(title='Survived (by Pclass & Child)', y='%Passengers')+
  scale_fill_discrete(name="Survived", breaks=c("0", "1"), labels=c("No", "Yes"))+
  facet_wrap(~Child, labeller = pchild_to_string) +
  scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))
#ggtitle('Survivors (by Pclass & Child)')
grid.arrange(p1, p2, nrow=2)

################################################################################
# Sex vs Pclass
################################################################################
#Pasajeros por Sexo y Pclass
tabdatos <- data.frame(table(data$Sex,data$Pclass))
tabdatos$Pct <- round((tabdatos$Freq / sum(tabdatos$Freq)) * 100,2)
tabdatos
# ggplot(as.data.frame(table(data$Sex,data$Pclass)), 
#        aes(x=Var1, y = Freq, fill=Var2)) + 
#   geom_bar(stat="identity") + 
#   geom_text(aes(label = Freq),position = position_stack(vjust = 0.5), color="black", size=3) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_fill_discrete(name="Pclass",
#                       breaks=c("1", "2", "3"),
#                       labels=c("1st class", "2nd class", "3rd class"))+
#   labs(x = "Sex", y = "Passengers", fill = "Pclass")

p1<-ggplot(tabdatos, aes(x=Var1, y = Freq, fill=Var2)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = Freq), hjust=1.3, position = position_stack(vjust = 0.5), color="black", size=3.5) +
  geom_text(aes(label = paste0("(",Pct," %)")), hjust=-0.1, position = position_stack(vjust = 0.5), color="black", size=2.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete(name="Pclass",
                      breaks=c("1", "2", "3"),
                      labels=c("1st class", "2nd class", "3rd class"))+
  labs(x = "Sex", y = "Passengers", fill = "Pclass") +
  theme(legend.position = "none") +
  ggtitle('Passengers (by Sex & Pclass)')

#Pasajeros por Sexo y Pclass (proporciones)
p2<-ggplot(as.data.frame(prop.table(table(data$Sex,data$Pclass),margin = 1)), 
       aes(x=Var1, y = Freq, fill=Var2)) + 
  geom_bar(stat="identity", position="fill") + 
  geom_text(aes(label = paste(round(Freq*100,2),'%')), position=position_stack(vjust = 0.5), color="black", size=3)+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete(name="Pclass",
                      breaks=c("1", "2", "3"),
                      labels=c("1st class", "2nd class", "3rd class"))+
  labs(x = "Sex", y = "% Passengers", fill = "Pclass") + 
  ggtitle('% Passengers (by Sex/Pclass)')
grid.arrange(p1, p2, ncol=2)
#########################
# Variables continuas vs Survived
#######
ggplot(data, aes(x = Age, y = Pclass)) + 
  scale_y_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  geom_jitter(aes(colour = Survived)) + 
  labs(title = "Age by Pclass")

ggplot(data, aes(x = Fare, y = Pclass)) + 
  scale_y_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
  geom_jitter(aes(colour = Survived)) + 
  labs(title = "Fare by Pclass")

ggplot(data, aes(x = Price, y = Pclass)) + 
  scale_y_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
  geom_jitter(aes(colour = Survived)) + 
  labs(title = "Price by Pclass")






p1<-ggplot(data[data$Pclass!=1,], aes(Pclass, y=Price)) +
  geom_count() + scale_size_area() +
  scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
  coord_flip() +
  ggtitle("Price (by Pclass 2&3)")

#Precio medio por clase y precio medio
p2<-ggplot(data, aes(Pclass, y=Price)) +
  geom_count() + scale_size_area() +
  scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
  coord_flip() +
  ggtitle("Price (by Pclass)")

grid.arrange(p1, p2, nrow=2)
#




# ggplot(data, aes(x = Price, y = Pclass)) + 
#   geom_jitter(aes(colour = Survived)) + 
#   theme(legend.title = element_blank())+
#   labs(x = "Age", y = "Pclass", title = "Price vs Pclass")
# 
#   scale_fill_discrete(name = "Survived") + 
#   scale_x_continuous(name="Fare", limits=c(0, 270), breaks=c(0, 40, 80, 120, 160, 200, 240, 280))

precio_medio_pclass <- data.frame(aggregate(data$Price, by=list(data$Pclass), FUN=mean))
names(precio_medio_pclass) <- c('Pclass', 'PrecioMedio')
precio_medio_pclass

p1<-ggplot(data[data$Pclass!=1,], aes(Pclass, y=Price)) +
  geom_point(color='blue') + 
  geom_point(data=precio_medio_pclass[precio_medio_pclass$Pclass!=1,], 
             aes(x = Pclass, y = PrecioMedio), col="black", fill='red', size=3, shape=23)+
  geom_segment(aes(x = 0.3, y = mean(data$Price[data$Pclass!=1]), 
                   xend = 2.7, yend = mean(data$Price[data$Pclass!=1])),
               colour="orange",size=1.2,linetype='dashed',
               data = data[data$Pclass!=1,])+
  geom_segment(aes(x = 0.3, y = mean(data$Price), 
                   xend = 2.7, yend = mean(data$Price)),
               colour="red",size=1.2,linetype='dashed',
               data = data)+
  scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
  coord_flip()+
  ggtitle("Price (by Pclass 2&3)")

p2<-ggplot(data, aes(Pclass, y=Price)) +
  geom_point(color='blue') + 
  geom_point(data=precio_medio_pclass, 
             aes(x = Pclass, y = PrecioMedio), col="black", fill='red', size=3, shape=23)+
  geom_segment(aes(x = 0.3, y = mean(data$Price), 
                   xend = 3.7, yend = mean(data$Price)),
               colour="red",size=1.2,linetype='dashed',
               data = data)+
  scale_x_discrete(name="Pclass", breaks=c("1", "2", "3"), labels=c("1st", "2nd", "3rd"))+
  coord_flip()+
  ggtitle("Price (by Pclass)")

grid.arrange(p1, p2, nrow=2)

#################################################################
# Grabar los datos que hemos limpiado y enriquecido 
# en un nuevo fichero CSV
#################################################################
fichero.nuevo <- "titanic_enriquecido.csv"
# Grabar los datos
write.csv(data, file=fichero.nuevo, row.names = FALSE)



#################################################################
# Realizar la prediccion con el modelo elegido en todo el dataset
#################################################################

#Realizamos la predicción con el modelo construido con rpart y el dataset total
predict_fin <-predict(model_tree2, data)
#Juntamos realidad y prediccion
#datos.real.predict <- cbind(as.integer(as.character(data$Survived)), 
#                            as.integer(as.character(predict_fin)))

#Lo que vamos a hacer es añadir la columna de la prediccion a nuestro 
#dataset limpio y enriquecido y lo vamos a guardar en nuevo fichero
data$Survived_Predicted <- predict_fin
#################################################################
# Grabar los datos que hemos limpiado y enriquecido + predicción
# en un nuevo fichero CSV
#################################################################
fichero.nuevo <- "titanic_enriquecido_predict.csv"
# Grabar los datos
write.csv(data, file=fichero.nuevo, row.names = FALSE)

