
####################### Datos
Datos = TrainingSet1001_MIC

DatosPrueba = ValidationSet999_MIC

#######################Import the libraries
library(tidyverse)
library(caret)
library(GGally)
library(ROCR) #for plotting ROC curves.
library(corrplot)
library(rpart)
library(rpart.plot)
library(partykit)
library(NeuralNetTools) ##Useful tools for plotting and analyzing neural networks
library(nnet)

Datos$Y <- as.factor(Datos$Y)

str(Datos)


ggpairs(Datos, aes(color = Y))

boxplot_X10 <- boxplot(Datos$X10)
boxplot_X10$out

boxplot_X9 <- boxplot(Datos$X9)
boxplot_X9$out

boxplot_X6 <- boxplot(Datos$X6)
boxplot_X6$out

Datos <- Datos %>%
  filter(X10 < 8, X9 < 2.77, X6 < 2.8, X6 > -2.7) 

ggcorr(Datos, label = TRUE)



show(Datos)



ctrl <- trainControl(method = "cv",                        
                     number = 10,                          
                     summaryFunction = defaultSummary,     
                     classProbs = TRUE)   

trainIndex <- createDataPartition(Datos$Y,      
                                  p = 0.8,      
                                  list = FALSE, 
                                  times = 1)   
#obtain training and test sets
fTR <- Datos[trainIndex,]
fTS <- Datos[-trainIndex,]

#Create dataset to include model predictions
fTR_eval <- fTR
fTS_eval <- fTS


inputs = c(6,9,10)
tree.fit <- train(x = fTR[,inputs],  #Input variables.
                  y = fTR$Y,   #Output variable
                  method = "rpart",   #Decision tree with cp as tuning parameter
                  control = rpart.control(minsplit = 5,  # Minimum number of obs in node to keep cutting
                                          minbucket = 5), # Minimum number of obs in a terminal node
                  parms = list(split = "gini"),          # impuriry measure
                  #tuneGrid = data.frame(cp = 0.1), # TRY this: tuneGrid = data.frame(cp = 0.25),
                  #tuneLength = 10,
                  tuneGrid = data.frame(cp = seq(0,0.1,0.0005)),
                  trControl = ctrl, 
                  metric = "Accuracy")
tree.fit #information about the resampling settings
ggplot(tree.fit) #plot the summary metric as a function of the tuning parameter
summary(tree.fit)

varImp(tree.fit,scale = FALSE)

fTR_eval <- fTR
fTR_eval$tree_prob <- predict(tree.fit, type="prob", newdata = fTR) # predict probabilities
fTR_eval$tree_pred <- predict(tree.fit, type="raw", newdata = fTR) # predict classes 
#test
fTS_eval <- fTS
fTS_eval$tree_prob <- predict(tree.fit, type="prob", newdata = fTS) # predict probabilities
fTS_eval$tree_pred <- predict(tree.fit, type="raw", newdata = fTS) # predict classes 

## Performance measures --------------------------------------------------------------------------------

#######confusion matices
# Training
confusionMatrix(data = fTR_eval$tree_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
# test
confusionMatrix(fTS_eval$tree_pred, 
                fTS_eval$Y, 
                positive = "YES")


Solucion1 <- predict(tree.fit, type="raw", newdata = DatosPrueba)

write.table(Solucion1 ,"T25.csv", col.names = FALSE, row.names = FALSE)




