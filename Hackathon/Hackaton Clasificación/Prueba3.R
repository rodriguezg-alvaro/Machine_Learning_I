
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

Datos <- Datos %>%
  filter(X10 < 8, X9 < 2.77, X6 < 2.8, X6 > -2.7, X3 > -3.02, X3 < 2.93) 

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











mlp.fit = train(form = Y ~ X3 + X6 + X9 + X10, #formula for specifying inputs and outputs.
                 data = fTR,   #Training dataset 
                 method = "nnet",
                 preProcess = c("center","scale"),
                 maxit = 250,    # Maximum number of iterations
                 #tuneGrid = data.frame(size =5, decay = 0),
                 tuneGrid = expand.grid(size = seq(5,25,length.out = 5),
                                        decay=c(10^(-9),0.0001,0.001,0.01,0.1,1)),
                 trControl = ctrl, 
                 metric = "Accuracy")

mlp.fit

mlp.fit$finalModel

fTR_eval$mlp_prob = predict(mlp.fit, type="prob" , newdata = fTR) # predict probabilities
fTR_eval$mlp_pred = predict(mlp.fit, type="raw" , newdata = fTR) # predict classes 
#test
fTS_eval$mlp_prob = predict(mlp.fit, type="prob" , newdata = fTS) # predict probabilities
fTS_eval$mlp_pred = predict(mlp.fit, type="raw" , newdata = fTS) # predict classes 




## Performance measures --------------------------------------------------------------------------------

#######confusion matices
# Training
confusionMatrix(data = fTR_eval$mlp_pred, #Predicted classes
                reference = fTR_eval$Y, #Real observations
                positive = "YES") #Class labeled as Positive
# test
confusionMatrix(fTS_eval$mlp_pred, 
                fTS_eval$Y, 
                positive = "YES")


Solucion3 <- predict(mlp.fit, type="raw", newdata = DatosPrueba)

write.table(Solucion3 ,"T25_3.csv", col.names = FALSE, row.names = FALSE)
