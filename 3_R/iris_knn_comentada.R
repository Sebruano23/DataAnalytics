

#Iris: La base de datos iris contiene las mediciones en centímetros 
#de las variables longitud (Length) y ancho (Width) de los pétalos 
#(Petal) y sépalos (Sepal) de 150 flores de cada una de las 3 especies 
#(Species) del género Iris: Iris setosa, Iris versicolor e Iris virginica.

#First we need to load some libraries.
#install.packages('knitr')
#install.packages('class')
#install.packages('tidyverse')
#install.packages('GGally')   

library(knitr) #te permite crear un documento que 
              #es una mezcla de texto y algunos fragmentos de código.
library(class) #es el plano que ayuda a crear un objeto y 
              #contiene su variable miembro junto con los atributos
library(tidyverse) #Es un conjunto de librerías de R diseñadas para la 
                  #“Ciencia de datos (Data Science)”. Todas las librerías 
                  #de este conjunto comparten la misma filosofía de trabajo, 
                  #la misma gramática y las mismas estructuras de datos.
library(GGally) #Gramatica de Graficos 

            #El paquete R 'ggplot2' es un sistema de trazado basado 
            #en la gramática de gráficos. 'GGally' amplía 'ggplot2' 
            #añadiendo varias funciones para reducir 
            #la complejidad de combinar objetos geométricos con 
            #datos transformados. Algunas de estas funciones 
            #incluyen una matriz de gráficos por pares, 
            #una matriz de gráficos por pares de dos grupos, 
            #un gráfico de coordenadas paralelas, un gráfico de 
            #supervivencia y varias funciones para trazar redes.

library(readr)# Analogo a Pandas en phyton

#El objetivo de 'readr' es proporcionar una forma rápida 
#y sencilla de leer datos rectangulares 
#(como 'csv', 'tsv' y 'fwf'). Está diseñado para analizar 
#de manera flexible muchos tipos de datos que se encuentran 
#en la naturaleza, sin dejar de fallar limpiamente cuando 
#los datos cambian inesperadamente.

#----------------------------------------------------------------------------#

iris <- read_csv(file = 'Iris.csv') #leer la base de datos
iris$Id <- NULL

# Let's get an idea of what we're working with.

head(iris) # Mostrar los primeros datos o primeros ejemplos

# Last rows 
tail(iris) # Mostrarlos últimos datos o primeros ejemplos

# Summary
summary(iris) # Arroja un resumen de las variables: cuenta 
              #las cualitiativas y de las cuantitativas hace
              #un registro estadístico con los cuartiles, máximo,
              #mínimo, media y mediana.

# Structure 
str(iris) # Arroja un resumen de la información, másgeneral que "summary"

#----------------------------------------------------------------------------#

# Some minor changes in the data set

#Se realizaron cambios en los nombres de las columnas y variables cualitativas
#por cuestiones de practicidad.

iris2 <- iris %>%
  rename(Sepallength=SepalLengthCm,
         Sepalwidth=SepalWidthCm,
         Petallength=PetalLengthCm,
         Petalwidth=PetalWidthCm) %>%
  mutate(Species=fct_recode(Species, "setosa"="Iris-setosa",
                            "versicolor" = "Iris-versicolor",
                            "virginica"="Iris-virginica"))

#----------------------------------------------------------------------------#
# Histogram for each species

#Se presentan 3 histogramas, uno por cada especie,
#en los que se analizan las frecuencias de las medidas
#de los sépalos y los pétalos.

iris2 %>%
  gather(Attributes, Value, 1:4) %>%
  ggplot(aes(x=Value, fill=Attributes)) +
  geom_histogram(colour="black") +
  facet_wrap(~Species) +
  theme_bw() +
  labs(x="Values", y="Frequency",
       title="Iris data set",
       subtitle="Histogram for each species") +
  theme(legend.title=element_blank(),
        legend.position="bottom")
#----------------------------------------------------------------------------#

# Density plot for each species
#En esta parte se puede evidenciar la densidad de cada tipo
# de Iris con respecto a cada una de las 4 características.
iris2 %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=value, fill=Species)) +
  geom_density(colour="black", alpha=0.5) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Iris data set",
       subtitle="Density plot for each attribute") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank())
#----------------------------------------------------------------------------#

# Violin plot for each attribute
# Esta función sirve para hacer un diagrama de violin
# de cada característica del total de Iris
iris2 %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
  geom_violin(show.legend=FALSE) +
  geom_boxplot(width=0.05, fill="white") +
  labs(title="Iris data set",
       subtitle="Violin plot for each attribute") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())
#----------------------------------------------------------------------------#

# Boxplot for each attribute
# Esta función sirve para hacer un diagrama de cajas y
# cadenas de cada característica del total de Iris.
iris2 %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Iris data set",
       subtitle="Boxplot for each attribute") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())
#----------------------------------------------------------------------------#

# Scatter plot and correlations
#Con esta función se hace la correlación entre las
#4 características junto a sus diagramas de densidad.
#Lo cuarioso es que incluyen las variables de 
#correlación calcuadas.

ggpairs(cbind(iris2, Cluster=as.factor(iris2$Species)),
        columns=1:4, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        axisLabels="none", switch="both") +
  theme_bw() 

#----------------------------------------------------------------------------#
# Data preparation

#Con la función "scale" se normalizan las 4 caracteísticas
# de medida.
dataNorm <- iris
dataNorm[, -5] <- scale(iris[, -5])

#----------------------------------------------------------------------------#
# Reproducible results

set.seed(1234) # es para que los datos de entrenamiento y 
              #prueba sean los mismos.


ind <- sample(2, nrow(dataNorm), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dataNorm[ind==1,]
testData <- dataNorm[ind==2,]
# se separan los datos en 
# 70% de entrenamiento = 112 ejemplos
# 30% de prueba = 38 ejmplos

#----------------------------------------------------------------------------#

# **k-NN execution**

#Se preparan 5 modelos cada uno con un k diferente que varía del 1 al 5.

# Execution of k-NN with k=1
KnnTestPrediction_k1 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=1, prob=TRUE)

# Execution of k-NN with k=2
KnnTestPrediction_k2 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=2, prob=TRUE)

# Execution of k-NN with k=3
KnnTestPrediction_k3 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=3, prob=TRUE)

# Execution of k-NN with k=4
KnnTestPrediction_k4 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=4, prob=TRUE)

#-----------------------------------------------------------------------------#

# Evaluation
#Se crea una Matriz de Confusion con cada valor de K para comparar
#cual obtuvo un mejor desarrollo.
#Después se evidencia la exctitud de la evaluación 
#de manera que se suman los resultados correctamente clasificados
#y se dividen entre el total de Iris analizadas.Así con cada valor de k.

# Confusion matrix of KnnTestPrediction_k1
table(testData$Species, KnnTestPrediction_k1) 

# Classification accuracy of KnnTestPrediction_k1
sum(KnnTestPrediction_k1==testData$Species)/length(testData$Species)*100

# The results of the other classifications:
  
# Confusion matrix of KnnTestPrediction_k2
table(testData$Species, KnnTestPrediction_k2)

# Classification accuracy of KnnTestPrediction_k2
sum(KnnTestPrediction_k2==testData$Species)/length(testData$Species)*100

# Confusion matrix of KnnTestPrediction_k3
table(testData$Species, KnnTestPrediction_k3)

# Classification accuracy of KnnTestPrediction_k3
sum(KnnTestPrediction_k3==testData$Species)/length(testData$Species)*100

# Confusion matrix of KnnTestPrediction_k4
table(testData$Species, KnnTestPrediction_k4)

# Classification accuracy of KnnTestPrediction_k4
sum(KnnTestPrediction_k4==testData$Species)/length(testData$Species)*100


#De acuerdo con los resultados, se puede ver que todos, menos el modelo con
#el k=2, tuvieron un éxito en el 94.73684 en las predicciones de acuerdo a 
#los datos de prueba.Es decir, que el modelo tiende a confundir los datos de
#la Iris Virginica con respecto a la Versicolor. Esto puede deberse a que 
#las características físicas de los ejemplos pudieron ser muy parecidas o 
#que efectivamente las especies pueden tener una proporción en cieto aspecto
#similar.

#----------------------------------------------------------------------------#

#CAMBIO DE K DEL 1 AL 100 #---------------------------------------------------
#Las siguientes lineas se plantea realizar un gráfico para comparar el 
#porcentaje de éxito en las predicciones de los modelos con valores de k del 1 
#al 100.

# Empty variables
KnnTestPrediction <- list()
accuracy <- numeric()

# From k=1 to k=100...
for(k in 1:100){
  
  # KnnTestPrediction for each k
  KnnTestPrediction[[k]] <- knn(trainData[,-5], testData[,-5], trainData$Species, k, prob=TRUE)
  
  # Accuracy for each k   
  accuracy[k] <- sum(KnnTestPrediction[[k]]==testData$Species)/length(testData$Species)*100
  
}

# Accuracy vs Choice of k
plot(accuracy, type="b", col="dodgerblue", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification accuracy", 
     main="Accuracy vs Neighbors")

# Add lines indicating k with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)

# Add line for max accuracy seen
abline(h=max(accuracy), col="grey", lty=2)

# Add line for min accuracy seen 
abline(h=min(accuracy), col="grey", lty=2)

# We see that 10 different values of `k` achieve the highest accuracy. Also notice that, 
# as `k` increases, the accuracy decreases. 

#ANÁLISIS DE LOS RESULTADOS: 

#Como se pudo ver en el gráfico desarrollado, con el aumento de "k", las 
#predicciones iban teniendo un mayor grado de éxito, esto tiene
#sentido si se plantea que el modelo podría estar subentenado (underfitting). 
#Sin embargo, a partir del valor k=15, debido al overfitting que 
#causa el crecimiento del hiperparámetro,
#las predicciones comienzan a ser menos exitosas, por lo que el procentaje de 
#aciertos tiende a cero.

#Aterrizando el modelo a la problemática planteada del Dataset Iris, se puede 
#ver que el modelo no alcanzará una predicción del 100% a pesar de que se varíe
#el hiperparametro "k". Aunque, para esta problemática no se necesita una 
#presición excta debido a que estamos comparando flores.No obstante, el modelo
#nos indica que existe una relación en las proporciones de medida de 
#pétalo y sépalo entre las especies Virginica con respecto a la Versicolor. Es 
#por ello que si se quiere seguir analizando estos tipos de planta se deben 
#escoger más parámetros para su reconocimiento.
#---------------------------------------------------------------------------#

#Prueba con k=5

# Execution of k-NN with k=5
KnnTestPrediction_k5r <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=5, prob=TRUE)
table(testData$Species, KnnTestPrediction_k5r)

# Classification accuracy of KnnTestPrediction_k5r
sum(KnnTestPrediction_k5r==testData$Species)/length(testData$Species)*100
