# Cargamos librerias

library(caret)
library(ggplot2)
library(GGally)
library(leaps)
library(glmnet)
library(pls)
library(car)
library(corrplot)
library(MLTools)
library(readxl)
library(ggplot2)
library(caret)
library(GGally)
library(splines)
library(NeuralNetTools)
library(gridExtra)
library(MLTools)

# Cargamos datos
library(readr)
datos <- TRdataEEMhourly

set.seed(2021)

summary(datos)

str(datos)

trainIndex <- createDataPartition(datos$WG,      #output variable. createDataPartition creates proportional partitions
                                  p = 0.8,      #split probability for training
                                  list = FALSE, #Avoid output as a list
                                  times = 1)    #only one partition

fTR <- datos[trainIndex,]
fTS <- datos[-trainIndex,]
fTR_eval <- fTR
fTS_eval <- fTS

ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          returnResamp = "final",              #Return final information about resampling
                          savePredictions = TRUE)              #

set.seed(2021) #For replication

lm.fit <- train(form = WG~.,
                data = fTR, 
                method = "lm", #Linear model
                tuneGrid = data.frame(intercept = TRUE), 
                #preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
lm.fit #information about the resampling settings
summary(lm.fit) 
vif(lm.fit$finalModel)
fTR_eval$lm_pred <- predict(lm.fit,  newdata = fTR)  
fTS_eval$lm_pred <- predict(lm.fit,  newdata = fTS)
PlotModelDiagnosis(fTR, fTR$WG, fTR_eval$lm_pred,
                   together = TRUE)
ggplot(fTR_eval)+geom_point(aes(x=bmi,y=charges),alpha = 0.5)+
  geom_point(aes(x=bmi,y=lm_pred), alpha =0.5, color="red")


# Segundo

lm2.fit <- train(form = WG ~  Hour + TL3H80 + TL1H80  + TL10H80 +  WSL1H80 + WSL3H80 + WSL4H80  + WSL7H80+  WSL10H80 + WDL2H80 + WDL4H80+ WDL7H80,
                data = fTR,
                 method = "lm", #Linear model
                tuneGrid = data.frame(intercept = TRUE), 
                #preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
lm2.fit #information about the resampling settings
summary(lm2.fit) 
vif(lm2.fit$finalModel)
fTR_eval$lm2_pred <- predict(lm2.fit,  newdata = fTR)  
fTS_eval$lm2_pred <- predict(lm2.fit,  newdata = fTS)
PlotModelDiagnosis(fTR, fTR$WG, fTR_eval$lm2_pred,
                   together = TRUE)
ggplot(fTR_eval)+geom_point(aes(x=bmi,y=charges),alpha = 0.5)+
  geom_point(aes(x=bmi,y=lm_pred), alpha =0.5, color="red")

# RedNeuronal
set.seed(2021)

mlp.fit = train(form = WG ~ TL3H80 + TL1H80 +TL4H80 + TL10H80 +  WSL1H80 +  WSL2H80+ WSL3H80 + WSL4H80  + WSL7H80+  WSL10H80 + WDL2H80 + WDL4H80+ WDL7H80,
                data = fTR, 
                method = "nnet",
                linout = TRUE, # Hay que decir que la salida es lineal!!!!
                maxit = 250,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(20,50,length.out = 5), decay=10^seq(-10,-5, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
mlp.fit #information about the resampling settings
#  25    1e-07  18.88480  0.8989549  13.93013

ggplot(mlp.fit) + scale_x_log10()

fTR_eval$mlp_pred = predict(mlp.fit,  newdata = fTR)  
fTS_eval$mlp_pred = predict(mlp.fit,  newdata = fTS)  
library(NeuralSens)
SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis
PlotModelDiagnosis(fTR[,varindex], fTR$y, 
                   fTR_eval$mlp_pred, together = TRUE)



set.seed(2021)

mlp2.fit = train(form = WG ~ WSL1H80 + WSL2H80 + TL1H80 + WSL4H80 + TL3H80 + TL4H80 + TL10H80,
                data = fTR, 
                method = "nnet",
                linout = TRUE, # Hay que decir que la salida es lineal!!!!
                maxit = 250,
                #tuneGrid = data.frame(size = 10, decay = 0),
                tuneGrid = expand.grid(size = seq(20,50,length.out = 5), decay=10^seq(-10,-5, length.out=6)),
                #tuneLength = 5,
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
mlp2.fit #information about the resampling settings
#  25    1e-07  18.88480  0.8989549  13.93013

ggplot(mlp2.fit) + scale_x_log10()

fTR_eval$mlp2_pred = predict(mlp2.fit,  newdata = fTR)  
fTS_eval$mlp2_pred = predict(mlp2.fit,  newdata = fTS)  
library(NeuralSens)
SensAnalysisMLP(mlp2.fit) #Statistical sensitivity analysis
vif(mlp2.fit$finalModel)

set.seed(2021)
lm3.fit <- train(form = WG~WSL1H80 + WSL2H80 + TL10H80 + poly(TL3H80, degree = 2, raw = TRUE)  ,
                data = fTR, 
                method = "lm", #Linear model
                tuneGrid = data.frame(intercept = TRUE), 
                #preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
lm3.fit #information about the resampling settings

summary(lm3.fit) 
vif(lm3.fit$finalModel)
fTR_eval$lm3_pred <- predict(lm3.fit,  newdata = fTR)  
fTS_eval$lm3_pred <- predict(lm3.fit,  newdata = fTS)
index = c(2, 4,5, 11, 12, 13, 15)
PlotModelDiagnosis(fTR[ ,index], fTR$WG, fTR_eval$lm3_pred,
                   together = TRUE)


# TL3H80 + TL1H80+ TL4H80 + TL10H80 + WSL1H80 + WSL2H80 

lm4.fit <- train(form = WG~TL3H80 + TL1H80+ TL4H80 + TL10H80 + WSL1H80 + WSL2H80  ,
                 data = fTR, 
                 method = "lm", #Linear model
                 tuneGrid = data.frame(intercept = TRUE), 
                 #preProcess = c("center","scale"),
                 trControl = ctrl_tune, 
                 metric = "RMSE")
lm4.fit #information about the resampling settings

summary(lm4.fit) 
vif(lm4.fit$finalModel)
fTR_eval$lm4_pred <- predict(lm4.fit,  newdata = fTR)  
fTS_eval$lm4_pred <- predict(lm4.fit,  newdata = fTS)
index = c(2, 4,5, 11, 12, 13, 15)
PlotModelDiagnosis(fTR[ ,index], fTR$WG, fTR_eval$lm3_pred,
                   together = TRUE)

# Tree

library(rpart)
library(rpart.plot)
library(partykit)
set.seed(2021) #For replication
tree.fit = train(form = WG~.,
                 data = fTR, 
                 method = "rpart", #Linear model
                 tuneGrid = data.frame(cp = seq(0,0.05,0.0005)),
                 trControl = ctrl_tune, 
                 metric = "RMSE")

tree.fit #information about the resampling settings
ggplot(tree.fit) #plot the summary metric as a function of the tuning parameter
summary(tree.fit)  #information about the model trained
tree.fit$finalModel #Cuts performed and nodes. Also shows the number and percentage of cases in each node.
#Basic plot of the tree:
plot(tree.fit$finalModel, uniform = TRUE, margin = 0)
text(tree.fit$finalModel, use.n = TRUE, all = TRUE, cex = .8)
#Advanced plots
rpart.plot(tree.fit$finalModel, type = 2, fallen.leaves = FALSE, box.palette = "Oranges")


#Measure for variable importance
varImp(tree.fit,scale = FALSE)
plot(varImp(tree.fit,scale = FALSE))


#Evaluate the model with training sets and diagnosis
fTR_eval$tree_pred = predict(tree.fit,  newdata = fTR)  
fTS_eval$tree_pred = predict(tree.fit,  newdata = fTS)  

PlotModelDiagnosis(fTR[,varindex], fTR$y,
                   fTR_eval$tree_pred, together = TRUE)

ggplot(fTR_eval) + geom_point(aes(x=lm_pred, y=y, color="lm"))+
  geom_point(aes(x=tree_pred, y=y, color="tree"))+
  geom_abline()




# svm 

library(kernlab)
set.seed(2021) #For replication
svm.fit = train(form = WG~.,
                data = fTR,
                method = "svmRadial",
                #tuneLength = 5,
                #tuneGrid =  data.frame( sigma=10, C=1),  
                tuneGrid = expand.grid(C = 10^seq(-1,2,length.out = 6), sigma=10^seq(-3,1,length.out=5)),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
svm.fit #information about the resampling
ggplot(svm.fit) #plot the summary metric as a function of the tuning parameter
fTR_eval$svm = predict(svm.fit,  newdata = fTR)  
fTS_eval$svm = predict(svm.fit,  newdata = fTS)  
PlotModelDiagnosis(fTR, fTR$WG, 
                   fTR_eval$svm, together = TRUE)

set.seed(2021)
svm2.fit = train(form = WG~WSL1H80 + WSL4H80 + WSL10H80 + WSL3H80 + WSL2H80 + WDL3H80 + WSL8H80 + WSL5H80 + Hour + WDL4H80 + WDL5H80 + WSL7H80 + WSL9H80 + WSL6H80 + TL5H80 + TL2H80,
                data = fTR,
                method = "svmRadial",
                #tuneLength = 5,
                #tuneGrid =  data.frame( sigma=10, C=1),  
                tuneGrid = expand.grid(C = 10^seq(-1,2,length.out = 6), sigma=10^seq(-3,1,length.out=5)),
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE")
svm2.fit #information about the resampling
ggplot(svm2.fit) #plot the summary metric as a function of the tuning parameter

library(tidyverse)
datosfinales <- TVdataEEMhourlyInput
datosfinales$svm2_pred = predict(svm2.fit,  newdata = datosfinales)  
datossubir <- datosfinales %>%
  select(svm2_pred)
datossubir$svm2_pred <- ifelse(datossubir$svm2_pred < 0, 0, datossubir$svm2_pred)
write.table(datossubir ,"T3.csv", col.names = FALSE, row.names = FALSE)



