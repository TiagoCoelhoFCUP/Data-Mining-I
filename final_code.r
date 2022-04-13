library(readr)
library(dplyr)
library(na.tools)
library(tidyimpute)
library(imputeTS)
library(readxl)
library(ggplot2)

AQD <- read.table("PRSA_Data_20130301-20170228/PRSA_Data_Huairou_20130301-20170228.csv", header = TRUE, sep = ",", dec = ".", na.strings = "NA", )

AQD2  <- select(AQD,-"wd")

for (l in 1:1461){
  pdia <- filter(AQD2 ,No<=l*24,No>l*24-24)
  
  for (s in 1:length(pdia)){
    
    if (n_na(pdia[,s])>15){
      AQD2[l*24-24:l*24,s]=na_ma(AQD2[l*24-24:l*24,s],k=6) }
    
    else {
      AQD2[l*24-24:l*24,s]=na_ma(AQD2[l*24-24:l*24,s],k=1) }
  }
}


vaqi <- matrix(5, nrow=0, ncol=6)
colnames(vaqi) <- c("PM2.5","PM10","SO2","NO2","CO","O3")
molec <- c("PM2.5","PM10","SO2","NO2","CO","O3")

Aqi <- read_xlsx("PRSA_Data_20130301-20170228/Aqi.xlsx")

for (l in 1:1461){
  pdia <- filter(AQD2,No<=l*24,No>l*24-24)
  
  pdia_namean <-  pdia
  aqidia1 <- c(0,0,0,0,0,0)
  for (i in molec){
    if (i=="PM2.5"){
      c = mean(select(pdia_namean,i)[,1])
      for (a in 2:7){
        if (select(Aqi,i)[a-1,1] < c && c <= select(Aqi,i)[a,1]){
          aqidia1[1]=((select(Aqi,I)[a,1]-select(Aqi,I)[a-1,1])/(select(Aqi,i)[a,1]-select(Aqi,i)[a-1,1])*(c-select(Aqi,i)[a-1,1])+select(Aqi,I)[a-1,1])[1,1]
        }
      }
    }
    if (i=="PM10"){
      c = mean(select(pdia_namean,i)[,1])
      for (a in 2:7){
        if (select(Aqi,i)[a-1,1] < c && c <= select(Aqi,i)[a,1]){
          aqidia1[2]=((select(Aqi,I)[a,1]-select(Aqi,I)[a-1,1])/(select(Aqi,i)[a,1]-select(Aqi,i)[a-1,1])*(c-select(Aqi,i)[a-1,1])+select(Aqi,I)[a-1,1])[1,1]
        }
      }
    }
    if (i=="NO2"){
      c = mean(select(pdia_namean,i)[,1])
      for (a in 2:7){
        if (select(Aqi,i)[a-1,1] < c && c <= select(Aqi,i)[a,1]){
          aqidia1[3]=((select(Aqi,I)[a,1]-select(Aqi,I)[a-1,1])/(select(Aqi,i)[a,1]-select(Aqi,i)[a-1,1])*(c-select(Aqi,i)[a-1,1])+select(Aqi,I)[a-1,1])[1,1]
        }
      }
    }
    if (i=="SO2"){
      c = mean(select(pdia_namean,i)[,1])
      for (a in 2:7){
        if (select(Aqi,i)[a-1,1] < c && c <= select(Aqi,i)[a,1]){
          aqidia1[4]=((select(Aqi,I)[a,1]-select(Aqi,I)[a-1,1])/(select(Aqi,i)[a,1]-select(Aqi,i)[a-1,1])*(c-select(Aqi,i)[a-1,1])+select(Aqi,I)[a-1,1])[1,1]
        }
      }
    }
    if (i=="CO"){
      c = max(select(pdia_namean,i)[,1])
      for (a in 2:7){
        if (select(Aqi,i)[a-1,1] < c && c <= select(Aqi,i)[a,1]){
          aqidia1[5] = ((select(Aqi,I)[a,1]-select(Aqi,I)[a-1,1])/(select(Aqi,i)[a,1]-select(Aqi,i)[a-1,1])*(c-select(Aqi,i)[a-1,1])+select(Aqi,I)[a-1,1])[1,1]
        }
      }
    }
    if (i=="O3"){
      c = max(select(pdia_namean,i)[,1])
      for (a in 2:7){
        if (select(Aqi,i)[a-1,1] < c && c <= select(Aqi,i)[a,1]){
          aqidia1[6] = ((select(Aqi,I)[a,1]-select(Aqi,I)[a-1,1])/(select(Aqi,i)[a,1]-select(Aqi,i)[a-1,1])*(c-select(Aqi,i)[a-1,1])+select(Aqi,I)[a-1,1])[1,1]
        }
      }
    }
  } 
  vaqi <- rbind(vaqi,aqidia1)
}
vaqi <- as.data.frame(vaqi)
vaqi <- mutate(vaqi, AQUI=0)
vaqi <- mutate(vaqi,componente=0)
for (m in 1:length(vaqi[,1])){
  vaqi[m,7]= max(vaqi[m,1:6])
  for (b in molec){
    if (select(vaqi,"AQUI")[m,]==select(vaqi,b)[m,]){
      vaqi[m,8]=b
    }
  }
}


vaqi <- mutate(vaqi, season=0)
for (m in 1:length(vaqi[,1])){
  if (select(AQD2,"month")[m*24,]<3){
    vaqi[m,9]="Winter"
  }
  if (select(AQD2,"month")[m*24,]==12){
    vaqi[m,9]="Winter"
  }
  if (select(AQD2,"month")[m*24,]>=3 && select(AQD2,"month")[m*24,]<6){
    vaqi[m,9]="Spring"
  }
  if (select(AQD2,"month")[m*24,]>=6 && select(AQD2,"month")[m*24,]<10){
    vaqi[m,9]="Summer"
  }
  if (select(AQD2,"month")[m*24,]>=10 && select(AQD2,"month")[m*24,]<=11){
    vaqi[m,9]="Fall"
  }
}
x <- c("Winter","Spring","Summer","Fall")
xf <- factor(select(vaqi,"season")[,1], levels=x)
vaqi <- mutate(vaqi, season=xf)




vaqi <- mutate(vaqi, poluicao=0)
for (m in 1:length(vaqi[,1])){
  if (select(vaqi,"AQUI")[m,]<=50){
    vaqi[m,10]="Excellent"
  }
  if (select(vaqi,"AQUI")[m,]>50 && select(vaqi,"AQUI")[m,]<=100){
    vaqi[m,10]="Good"
  }
  if (select(vaqi,"AQUI")[m,]>100 && select(vaqi,"AQUI")[m,]<=150){
    vaqi[m,10]="Lightly Polluted"
  }
  if (select(vaqi,"AQUI")[m,]>150 && select(vaqi,"AQUI")[m,]<=200){
    vaqi[m,10]="Moderately Polluted"
  }
  if (select(vaqi,"AQUI")[m,]>200 && select(vaqi,"AQUI")[m,]<=300){
    vaqi[m,10]="Heavily Polluted"
  }
  if (select(vaqi,"AQUI")[m,]>300){
    vaqi[m,10]="Severely Polluted"
  }
}
x <- c("Excellent","Good","Lightly Polluted","Moderately Polluted","Heavily Polluted","Severely Polluted")
xf <- factor(select(vaqi,"poluicao")[,1], levels=x)
vaqi <- mutate(vaqi, poluico=xf)





vaqi <- mutate(vaqi,No=select(AQD2,"No")[1:1461,])
casestudy <- matrix(1,nrow=0,ncol=5)
colnames(casestudy) <- c("TEMPM","PRESM","RAIN","WSPM","DEWP")
for (l in 1:1461){ 
  t=c(0)
  i=l*24-24
  f=l*24
  t=c(mean(select(AQD2,"TEMP")[i:f,1]),mean(select(AQD2,"PRES")[i:f,1]),mean(select(AQD2,"RAIN")[i:f,1]),mean(select(AQD2,"WSPM")[i:f,1]),mean(select(AQD2,"DEWP")[i:f,1]))
  casestudy <- rbind(casestudy,t)
}
casestudy <- as.data.frame(casestudy)
casestudy <- mutate(casestudy,AQUI=select(vaqi,AQUI)[,1])
casestudy <- mutate(casestudy,poluicao=select(vaqi,"poluicao")[,1])
casestudy <- mutate(casestudy,season=select(vaqi,"season")[,1])
casestudy <- mutate(casestudy,componente=select(vaqi,"componente")[,1])




library(grid)
library(gridExtra)

p1 <- ggplot(vaqi, aes(x = AQUI, fill=poluicao)) +  geom_histogram(breaks=c(0,50,100,150,200,300,500),closed="right") + xlab("AQI level") + ylab("frequency") + theme(plot.title = element_text(size=10),axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x = element_text(size = 8))

p2 <-ggplot(vaqi, aes(x = componente, fill=componente)) + geom_bar() + facet_grid("season") + ggtitle("Polutants that originate the daily AQI by season") + xlab("pollutant") +  theme(plot.title = element_text(size=8),axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x = element_text(size = 8))

grid.arrange(p1, p2, ncol = 2)



vaqi <- mutate(vaqi, PRESM=select(casestudy,"PRESM")[,1])
vaqi <- mutate(vaqi, TEMPM=select(casestudy,"TEMPM")[,1])
vaqi <- mutate(vaqi, RAIN=select(casestudy,"RAIN")[,1])
vaqi <- mutate(vaqi, WSPM=select(casestudy,"WSPM")[,1])
vaqi <- mutate(vaqi, DEWP=select(casestudy,"DEWP")[,1])
tblprev <- select(vaqi,"RAIN","PRESM","TEMPM","DEWP","WSPM","season","poluicao")
tblprev<- mutate(tblprev,AQI=vaqi$AQUI)
tblprev <- mutate(tblprev,aqi1=0,aqi2=0,aqi3=0)
for (i in 4:1461){
  tblprev$aqi3[i] = tblprev$AQI[i-3]
  tblprev$aqi2[i] = tblprev$AQI[i-2]
  tblprev$aqi1[i] = tblprev$AQI[i-1]
  
}
tblprev <- tblprev[4:1461,]
M <- cor(select(tblprev, RAIN, PRESM, TEMPM, DEWP, WSPM, AQI,aqi1,aqi2,aqi3))

library(corrplot)
# Plot the correlation plot with `M`
corrplot(M, method="circle")

library(rpart)
library(rpart.plot)
library(caret)
library(doParallel)
library(e1071)
library(caretEnsemble) 
library(cutpointr)
library(caTools)
library(DMwR)

tblprev <-mutate(tblprev,bin=0)
for (i in 1:1458){
  if(tblprev$poluicao[i]=="Excellent" || tblprev$poluicao[i]=="Good" || tblprev$poluicao[i]=="Lightly Polluted"){
    tblprev$bin[i]="fewer_pollution"
  }
  if( tblprev$poluicao[i]=="Moderately Polluted" || tblprev$poluicao[i]=="Heavily Polluted" || tblprev$poluicao[i]=="Severely Polluted"){
    tblprev$bin[i]="more_pollution"
  }
}
x <- c("fewer_pollution","more_pollution")
xf <- factor(select(tblprev,"bin")[,1], levels=x)
tblprev <- mutate(tblprev, poluicao=xf)
tblprev <- select(tblprev, -bin)

#One hot encoding for multiclass factors
tblprev <- mutate(tblprev,isPrimavera=0,isVerao=0,isOutono=0,isInverno=0)

for (i in 1:1458){
  if(as.character(tblprev$season[i]) == "Spring")
    tblprev$isPrimavera[i] = 1
  if(as.character(tblprev$season[i]) == "Summer")
    tblprev$isVerao[i] = 1
  if(as.character(tblprev$season[i]) == "Fall")
    tblprev$isOutono[i] = 1
  if(as.character(tblprev$season[i]) == "Winter")
    tblprev$isInverno[i] = 1
}

tblprev <- select(tblprev, - season)

#setting the seed for replicatable results and randomizing entries
set.seed(222)
dataset <- select(tblprev, - c(AQI))
dataset<-na_ma(dataset, k=1)
dataset_rand <- dataset[sample(1:nrow(dataset)), ]

#create test set and trainning set (using Synthetic minority class oversample to balance the trainnig set)
indexes = createDataPartition(dataset$poluicao, p = .80, list = FALSE)
train = dataset[indexes, ]
train <- SMOTE(poluicao~.,train,perc.over=100)
test = dataset[-indexes, ]

#Setup parallel processing and cross validation
registerDoParallel(4)
getDoParWorkers()

my_control <- trainControl(method = "repeatedcv", # for "cross-validation"
                           number = 10, # number of k-folds
                           index = createFolds(train$poluicao, 10),
                           repeats = 3,
                           savePredictions = "final",
                           allowParallel = TRUE,
                           classProbs = TRUE)

#Create a model list using "Accuracy" as the summary metric using the models rpart, svmLinera, svmPoly, rf, naieve_bayes, knn, nnet
set.seed(222)
model_list <- caretList(poluicao~., data = train,
                        trControl = my_control,
                        methodList = c("rpart2","svmLinear","svmPoly","rf","naive_bayes","knn","nnet"),
                        tuneList = NULL,
                        continue_on_fail = FALSE, 
                        preProcess = c("center","scale"))


#Sample the performance of the model list and plot it
resamples <- resamples(model_list)
dotplot(resamples, metric = "Accuracy")


#Train an ensemble model as a linear combination of the previous models
set.seed(222)
ensemble_1 <- caretEnsemble(model_list, 
                            metric = "Accuracy", 
                            trControl = my_control)

#Predictions for each particular model using the test set
pred_rpart <- predict.train(model_list$rpart2, newdata = test[,-6])
pred_knn <- predict.train(model_list$knn, newdata = test[,-6])
pred_naive_bayes <- predict.train(model_list$naive_bayes, newdata = test[,-6])
pred_nnet <- predict.train(model_list$nnet, newdata = test[,-6])
pred_rf <- predict.train(model_list$rf, newdata = test[,-6])
pred_svmLinear <- predict.train(model_list$svmLinear, newdata = test[,-6])
pred_svmPoly <- predict.train(model_list$svmPoly, newdata = test[,-6])
pred_ens1 <- predict(ensemble_1, newdata = test[,-6])


#Calculate the accuracy values for each model
confM_ens1 <- table(test[, 6], pred_ens1)
confM_knn <- table(test[, 6], pred_knn)
confM_naive_bayes <- table(test[, 6], pred_naive_bayes)
confM_nnet <- table(test[, 6], pred_nnet)
confM_svmLinear <- table(test[, 6], pred_svmLinear)
confM_svmPoly <- table(test[, 6], pred_svmPoly)
confM_rf <- table(test[, 6], pred_rf)
confM_rpart <- table(test[, 6], pred_rpart)


#Create a data frame with the accuracy values per model for the test set
pred_ACC <- data.frame(ensemble_1 = sum(diag(confM_ens1))/sum(confM_ens1),
                       KNN = sum(diag(confM_knn))/sum(confM_knn),
                       SVM_Linear = sum(diag(confM_svmLinear))/sum(confM_svmLinear),
                       SVM_Poly = sum(diag(confM_svmPoly))/sum(confM_svmPoly),
                       NNET = sum(diag(confM_nnet))/sum(confM_nnet),
                       NAIVE_BAYES = sum(diag(confM_naive_bayes))/sum(confM_naive_bayes),
                       RF = sum(diag(confM_rf))/sum(confM_rf),
                       RPART = sum(diag(confM_rpart))/sum(confM_rpart))


#Create a data frame with the recall values per model for the test set
pred_RECALL <- data.frame(ensemble_1 = caret::recall(pred_ens1, test[,6], relevant = "more_pollution"),
                          KNN = caret::recall(pred_knn, test[,6], relevant = "more_pollution"),
                          SVM_Linear = caret::recall(pred_svmLinear, test[,6], relevant = "more_pollution"),
                          SVM_Poly = caret::recall(pred_svmPoly, test[,6], relevant = "more_pollution"),
                          NNET = caret::recall(pred_nnet, test[,6], relevant = "more_pollution"),
                          NAIVE_BAYES = caret::recall(pred_naive_bayes, test[,6], relevant = "more_pollution"),
                          RF = caret::recall(pred_rf, test[,6], relevant = "more_pollution"),
                          RPART = caret::recall(pred_rpart, test[,6], relevant = "more_pollution"))


#variable importance analysis 
varImp(ensemble_1)


#Create a new trainControl
my_control2 <- trainControl(method = "repeatedcv", 
                            number = 10,
                            index = createFolds(train$poluicao, 10),
                            repeats = 3,
                            savePredictions = "final",
                            allowParallel = TRUE,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE)

#Create a model list using "Spec" as the summary metric using the models rpart, svmLinera, svmPoly, rf, naieve_bayes, knn, nnet
set.seed(222)
model_list2 <- caretList(poluicao~., data = train,
                         trControl = my_control2,
                         metric = "Spec",
                         methodList = c("rpart2","svmLinear","svmPoly","rf","naive_bayes","knn","nnet"),
                         tuneList = NULL,
                         continue_on_fail = FALSE, 
                         preProcess = c("center","scale"))


#Sample the performance of the model list and plot it
resamples2 <- resamples(model_list2)
dotplot(resamples2,metric = "Spec")


#Train an ensemble model as a linear combination of the previous models
set.seed(222)
ensemble_2 <- caretEnsemble(model_list2, 
                            metric = "Spec", 
                            trControl = my_control2)

#Predictions for each particular model using the test set
pred2_rpart <- predict.train(model_list2$rpart2, newdata = test[,-6])
pred2_knn <- predict.train(model_list2$knn, newdata = test[,-6])
pred2_naive_bayes <- predict.train(model_list2$naive_bayes, newdata = test[,-6])
pred2_nnet <- predict.train(model_list2$nnet, newdata = test[,-6])
pred2_rf <- predict.train(model_list2$rf, newdata = test[,-6])
pred2_svmLinear <- predict.train(model_list2$svmLinear, newdata = test[,-6])
pred2_svmPoly <- predict.train(model_list2$svmPoly, newdata = test[,-6])
pred2_ens2 <- predict(ensemble_2, newdata = test[,-6])


#Calculate the accuracy values for each model
confM2_ens2 <- table(test[, 6], pred2_ens2)
confM2_knn <- table(test[, 6], pred2_knn)
confM2_naive_bayes <- table(test[, 6], pred2_naive_bayes)
confM2_nnet <- table(test[, 6], pred2_nnet)
confM2_svmLinear <- table(test[, 6], pred2_svmLinear)
confM2_svmPoly <- table(test[, 6], pred2_svmPoly)
confM2_rf <- table(test[, 6], pred2_rf)
confM2_rpart <- table(test[, 6], pred2_rpart)


#Create a data frame with the accuracy values per model for the test set
pred2_ACC <- data.frame(ensemble_2 = sum(diag(confM2_ens2))/sum(confM2_ens2),
                        KNN = sum(diag(confM2_knn))/sum(confM2_knn),
                        SVM_Linear = sum(diag(confM2_svmLinear))/sum(confM2_svmLinear),
                        SVM_Poly = sum(diag(confM2_svmPoly))/sum(confM2_svmPoly),
                        NNET = sum(diag(confM2_nnet))/sum(confM2_nnet),
                        NAIVE_BAYES = sum(diag(confM2_naive_bayes))/sum(confM2_naive_bayes),
                        RF = sum(diag(confM2_rf))/sum(confM2_rf),
                        RPART = sum(diag(confM2_rpart))/sum(confM2_rpart))


#Create a data frame with the recall values per model for the test set
pred2_RECALL <- data.frame(ensemble_2 = caret::recall(pred2_ens2, test[,6], relevant = "more_pollution"),
                           KNN = caret::recall(pred2_knn, test[,6], relevant = "more_pollution"),
                           SVM_Linear = caret::recall(pred2_svmLinear, test[,6], relevant = "more_pollution"),
                           SVM_Poly = caret::recall(pred2_svmPoly, test[,6], relevant = "more_pollution"),
                           NNET = caret::recall(pred2_nnet, test[,6], relevant = "more_pollution"),
                           NAIVE_BAYES = caret::recall(pred2_naive_bayes, test[,6], relevant = "more_pollution"),
                           RF = caret::recall(pred2_rf, test[,6], relevant = "more_pollution"),
                           RPART = caret::recall(pred2_rpart, test[,6], relevant = "more_pollution"))


#variable importance analysis 
varImp(ensemble_2)

#Comparing results (The ensemble trainned for accuracy has the best accuracy/specificity combination)
pred_ACC
pred_RECALL
pred2_ACC
pred2_RECALL
