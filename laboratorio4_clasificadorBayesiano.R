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

#Se elimina el atributo veil-type ya que es una elemento de tipo constante por lo que no aporta al estudio.
#Se mantiene el número total de datos, no hay variación absoluta en este.
filtered.datos <- subset(filtered.datos, select = -c(veil.type))


columnas <- c("cap.shape","cap.surface","cap.color","bruises","odor","gill.attachment","gill.spacing","gill.size","gill.color"
              ,"stalk.shape","stalk.root","stalk.surface.above.ring","stalk.surface.below.ring","stalk.color.above.ring"
              ,"stalk.color.below.ring","veil.color","ring.number","ring.type","spore.print.color","population","habitat")


#Conjunto de entrenamiento del 95%
mush_test <- sample(1:nrow(filtered.datos), nrow(filtered.datos)*0.95)
nb_mush <- naiveBayes(filtered.datos[-mush_test, columnas], filtered.datos[-mush_test, "class"])
pred_mush <- predict(nb_mush, filtered.datos[mush_test, columnas])

datosTabla <- filtered.datos[mush_test,]
tabla1.c <- table(pred_mush,datosTabla$class)
tabla1.c

clasificadosCorrectamente <- sum(pred_mush==filtered.datos[mush_test, "class"])
totalDatos <- nrow(filtered.datos)*0.95
clasificadosCorrectamente
totalDatos
precision <- (clasificadosCorrectamente / totalDatos) * 100
precision

#Conjunto de entrenamiento del 80%
mush_test <- sample(1:nrow(filtered.datos), nrow(filtered.datos)*0.8)
nb_mush <- naiveBayes(filtered.datos[-mush_test, columnas], filtered.datos[-mush_test, "class"])
pred_mush <- predict(nb_mush, filtered.datos[mush_test, columnas])

datosTabla <- filtered.datos[mush_test,]
tabla1.c <- table(pred_mush,datosTabla$class)
tabla1.c

clasificadosCorrectamente <- sum(pred_mush==filtered.datos[mush_test, "class"])
totalDatos <- nrow(filtered.datos)*0.8
clasificadosCorrectamente
totalDatos
precision <- (clasificadosCorrectamente / totalDatos) * 100
precision

#Conjunto de entrenamiento del 60%
mush_test <- sample(1:nrow(filtered.datos), nrow(filtered.datos)*0.6)
nb_mush <- naiveBayes(filtered.datos[-mush_test, columnas], filtered.datos[-mush_test, "class"])
pred_mush <- predict(nb_mush, filtered.datos[mush_test, columnas])

datosTabla <- filtered.datos[mush_test,]
tabla1.c <- table(pred_mush,datosTabla$class)
tabla1.c

clasificadosCorrectamente <- sum(pred_mush==filtered.datos[mush_test, "class"])
totalDatos <- nrow(filtered.datos)*0.6
clasificadosCorrectamente
totalDatos
precision <- (clasificadosCorrectamente / totalDatos) * 100
precision

#Conjunto de entrenamiento del 40%
mush_test <- sample(1:nrow(filtered.datos), nrow(filtered.datos)*0.4)
nb_mush <- naiveBayes(filtered.datos[-mush_test, columnas], filtered.datos[-mush_test, "class"])
pred_mush <- predict(nb_mush, filtered.datos[mush_test, columnas])

datosTabla <- filtered.datos[mush_test,]
tabla1.c <- table(pred_mush,datosTabla$class)
tabla1.c

clasificadosCorrectamente <- sum(pred_mush==filtered.datos[mush_test, "class"])
totalDatos <- nrow(filtered.datos)*0.4
clasificadosCorrectamente
totalDatos
precision <- (clasificadosCorrectamente / totalDatos) * 100
precision

#Conjunto de entrenamiento del 20%
mush_test <- sample(1:nrow(filtered.datos), nrow(filtered.datos)*0.2)
nb_mush <- naiveBayes(filtered.datos[-mush_test, columnas], filtered.datos[-mush_test, "class"])
pred_mush <- predict(nb_mush, filtered.datos[mush_test, columnas])

datosTabla <- filtered.datos[mush_test,]
tabla1.c <- table(pred_mush,datosTabla$class)
tabla1.c

clasificadosCorrectamente <- sum(pred_mush==filtered.datos[mush_test, "class"])
totalDatos <- nrow(filtered.datos)*0.2
clasificadosCorrectamente
totalDatos
precision <- (clasificadosCorrectamente / totalDatos) * 100
precision
