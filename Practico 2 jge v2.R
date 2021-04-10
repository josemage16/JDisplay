###### PRACTICO 2 ---- JOSEMARIA GONZALEZ (1913780-2)#########

# seteo el working directory
setwd("C:/Users/josem/Dropbox/JGE/Master DATA SCIENCE UM/Data Science/Practico 2")

#Cargo todas las Librerias por las dudas
library(ggplot2)
library(mice)
library(ggcorrplot)
library(caret)
library(tidyverse)
library(zoo)
library(PerformanceAnalytics)

#cargo dataset 
DataAll <- read.csv("C:/Users/josem/Dropbox/JGE/Master DATA SCIENCE UM/Data Science/Practico 2/winequality-red.csv")

# Hago boxplot de todas las variables
boxplot(DataAll)

histogram(DataAll$quality)
# Analizando visualmente el boxplot de cada variable se oberva que en varias 
# de las variables tienen un numero importante de "outliers" los cuales 
#deberian ser analizados para poder individualmente antes de usarse para predecir otros valores
#Por otro lado, aquellas variables que no aparentan tener outliers, parecería que tienen
# muy pocas registros como para ser utiles.
# Por su parte de observar el Histograma se podría concluir que la distribución no es normal
# tendiendo mas hacia la derecha.
# Como soluciones posibles se sugiere analizar cada variable indivdualmente,
# y evaluar que "poder de predicción" tiene cada una sobre la Calidad, pudiendo
# pasar que alguna no aporte demasiado por lo cual no justifica estudiar.

# Separa 20% de los registros para usar como "testigo"
# veo cuantos registros son
nrow(DataAll)

#me da 1599, el 20% seria 320 aprox deberian quedar 1279

testigo <- DataAll[sample(nrow(DataAll), 320), ]
View(testigo)
nrow(testigo)

#Le saco las rows de testigo de la base principal
rt<-row.names(testigo)
rt
db <- DataAll
db <-  db[!(row.names(db) %in% rt), ]
names(db)
nrow(db)

#aplico algoritmos de clasificacion
# Regresion logistica , random forest, support vector machine SVN, KNN


#Divido base entre test y Train
set.seed(26)
dbPart <- createDataPartition(db$quality,p=0.70,list = FALSE)
test <- db[-dbPart, ]
train <- db[dbPart, ]


## Pruebo distintos modelos

#View(test)

#Paso a factor la variable quality
train$quality= as.factor(train$quality)
test$quality = as.factor(test$quality)

library(caret)

fit_rpart <- train(quality~.,data=train, method = "rpart", metric = "Accuracy")
predictions_rpart <- predict(fit_rpart, test[1:11], type="raw")

table(predictions_rpart, test$quality)

#confusionMatrix(as.factor(actual),as.factor(predicted))
#?confusionMatrix

confusionMatrix(as.factor(test$quality),as.factor(predictions_rpart))

# modelo Random Forest
fit_RF <- train(quality~.,data=train, method = "rf", metric = "Accuracy")
predictions_RF <- predict(fit_RF, test[1:11], type="raw")
table(predictions_RF, test$quality)

confusionMatrix(as.factor(test$quality),as.factor(predictions_RF))

# modelo lda
fit_lda <- train(quality~.,data=train, method = "lda", metric = "Accuracy")
predictions_lda <- predict(fit_lda, test[1:11], type="raw")
table(predictions_lda, test$quality)

confusionMatrix(as.factor(test$quality),as.factor(predictions_lda))


#modelo knn
fit_knn <- train(quality~.,data=train, method = "knn", metric = "Accuracy")
predictions_knn <- predict(fit_knn, test[1:11], type="raw")
table(predictions_knn, test$quality)

confusionMatrix(as.factor(test$quality),as.factor(predictions_knn))


# modelo superior vector machine
fit_svm <- train(quality~.,data=train, method = "svmRadial", metric = "Accuracy")
predictions_svm <- predict(fit_svm, test[1:11], type="raw")
table(predictions_svm, test$quality)

confusionMatrix(as.factor(test$quality),as.factor(predictions_svm))


#graf resultados

scales <- list(x=list(relation="free"), y=list(relation="free"))
Resultados <-resamples(list(RF=fit_RF, KNN=fit_knn, SVM=fit_svm,  LDA = fit_lda, RPART = fit_rpart))
dotplot(Resultados, scales=scales)

#Conclusiones
# Luego de correr 5 modelos diferentes se observa que el que se ajsuatria mejor es 
# es Random Forest, ya que con él se obtienen los mayores valores de 
#Accuracy y Kappa, tal como se observa en el grafico comparativo.


