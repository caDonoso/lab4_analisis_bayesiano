library("ggplot2")
library("e1071")
library("corrplot")

#LECTURA DE DATOS
columnas <- c("class","cap-shape","cap-surface","cap-color","bruises","odor","gill-attachment","gill-spacing","gill-size","gill-color"
              ,"stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring"
              ,"stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population","habitat")
datos <- read.csv("D:/Universidad/Analisis de datos/Lab 4/agaricus-lepiota.data", header=FALSE, 
                  sep=",", col.names = columnas)

#Porcentaje de datos inicial:
#   - edible: 51.8% (4208)
#   - poisonous: 48.2% (3916)

#LIMPIEZA DE DATOS
#Se eliminan las tuplas o registros que en el atributo "stalk-root" no tienen ning?n valor registrado.

#Se filtran 2480 datos, es decir, nos quedamos con el 69,5% de los datos.
#El porcentaje de datos por cada variable de clase, despu?s de filtrado:
#   - edible: 61,8% (3488)
#   - poisonous: 38.2% (2156)
filtered.datos <- datos[datos$stalk.root != "?",]

mushroom <- sample(2, nrow(filtered.datos), replace = TRUE, prob = c(0.62, 0.38))
trainD <- filtered.datos[mushroom==1,]
testD <- filtered.datos[mushroom==2,]

nrow(trainD)
nrow(testD)

conditionalProbabilities <- naiveBayes(filtered.datos ~ .,data=trainD)
conditionalProbabilities