#==================================================================================
#*******************------ Machine Learning ------*********************
#==================================================================================

rm(list = ls())
#setwd("X:/My Documents/Machine Learning") #at School

#Packages
#-------------------------------------------------------------
#install.packages("stargazer") #package to create Latex tables
library("stargazer")
#install.packages("SDMTools") #package to obtain the confusion matrix
library("SDMTools")
#install.packages("verification") #package to use the brier score function
library("verification")
#install.packages("randomForest") #package to use RandomForest
library("randomForest")
#install.packages("e1071") #package to use NaiveBayes 
library("e1071")
#install.packages("MASS")
library("MASS")
#install.packages("pROC") #package to get the ROC curve
library("pROC")
#install.packages("ROCR") #Another package to get the ROC curve
library("ROCR")
#install.packages("neuralnet")
library("neuralnet")
#install.packages("mixdist") #package for weibulparinv
library("mixdist")
#install.packages("caret") #Package to use knn
library("caret")
#install.packages("class")
library("class")
#install.packages("xgboost") #Package to use boosting logistic regression
library("xgboost")
#install.packages("kknn") #Another Package to use knn (and get the predicted probabilities)
library("kknn")
#install.packages("unbalanced") #package to use ubBalanced function (to create balanced data)
library("unbalanced")
#install.packages("fitdistrplus") #package to fit distributions
library("fitdistrplus")

#-------------------------------------------------------------

# Importing the data
d <- read.csv("diabeticData.csv", na.strings = "?")
df <- read.csv("diabetic_data.csv")

#If we want to balance the dataset somewhat, consider a readmition as being readmitted disregards time
# d2 <- read.csv("readmitted.csv")
# d2$readmitted_dummy <- rep(0, ncol(d2))
# d2$readmitted_dummy[d2$readmitted!="NO"] <- 1
#d$readmitted_dummy <- d2$readmitted_dummy

any(is.na(d)) #Check for missing values 

# Data preparation function for Naive bayes
PrepDataNaiveBayes <- function(data, balanced=c("FALSE", "TRUE")){
  
  balanced <- match.arg(balanced)
  
  df <- data
  
  df$encounter_id <- NULL;
  df$patient_nbr <- NULL;
  df$weight <- NULL;
  df$payer_code <- NULL;
  
  df$race <- as.factor(df$race);
  
  df$age <- ifelse(df$age == "[0-10)",  0, df$age);
  df$age <- ifelse(df$age == "[10-20)", 10, df$age);
  df$age <- ifelse(df$age == "[20-30)", 20, df$age);
  df$age <- ifelse(df$age == "[30-40)", 30, df$age);
  df$age <- ifelse(df$age == "[40-50)", 40, df$age);
  df$age <- ifelse(df$age == "[50-60)", 50, df$age);
  df$age <- ifelse(df$age == "[60-70)", 60, df$age);
  df$age <- ifelse(df$age == "[70-80)", 70, df$age);
  df$age <- ifelse(df$age == "[80-90)", 80, df$age);
  df$age <- ifelse(df$age == "[90-100)", 90, df$age);
  df$age <- as.factor(df$age);
  
  df$gender <- as.factor(df$gender);
  
  # df$weight <- ifelse(df$weight == "[75-100)",  75, df$weight);
  # df$weight <- ifelse(df$weight == "[50-75)",   50, df$weight);
  # df$weight <- ifelse(df$weight == "[25-50)",   25, df$weight);
  # df$weight <- ifelse(df$weight == "[0-25)",    0, df$weight);
  # df$weight <- ifelse(df$weight == "[100-125)", 100, df$weight);
  # df$weight <- ifelse(df$weight == "[125-150)", 125, df$weight);
  # df$weight <- ifelse(df$weight == "[150-175)", 150, df$weight);
  # df$weight <- ifelse(df$weight == "[175-200)", 175, df$weight);
  # df$weight <- ifelse(df$weight == ">200",      -25, df$weight);
  # df$weight <- as.factor(df$weight);
  
  df$admission_type_id <- as.factor(as.factor(df$admission_type_id));
  df$discharge_disposition_id <- as.factor(as.factor(df$discharge_disposition_id));
  df$admission_source_id <- as.factor(as.factor(df$admission_source_id));
  df$time_in_hospital <- as.factor(df$time_in_hospital);
 # df$payer_code <- as.factor(as.factor(df$payer_code));
  df$medical_specialty <- as.factor(as.factor(df$medical_specialty));
  df$num_lab_procedures <- as.factor(df$num_lab_procedures);
  df$num_procedures <- as.factor(df$num_procedures);
  df$num_medications <- as.factor(df$num_medications);
  df$number_outpatient <- as.factor(df$number_outpatient);
  df$number_emergency <- as.factor(df$number_emergency);
  df$number_inpatient <- as.factor(df$number_inpatient);
  df$diag_1 <- as.factor(as.factor(df$diag_1));
  df$diag_2 <- as.factor(as.factor(df$diag_2));
  df$diag_3 <- as.factor(as.factor(df$diag_3));
  df$number_diagnoses <- as.factor(df$number_diagnoses);
  
  df$max_glu_serum <- ifelse(df$max_glu_serum == "None",  0, df$max_glu_serum);
  df$max_glu_serum <- ifelse(df$max_glu_serum == "Norm",  100, df$max_glu_serum);
  df$max_glu_serum <- ifelse(df$max_glu_serum == ">200",  200, df$max_glu_serum);
  df$max_glu_serum <- ifelse(df$max_glu_serum == ">300",  300, df$max_glu_serum);
  df$max_glu_serum <- as.factor(df$max_glu_serum);
  
  df$A1Cresult <- ifelse(df$A1Cresult == "None",  0, df$A1Cresult);
  df$A1Cresult <- ifelse(df$A1Cresult == "Norm",  5, df$A1Cresult);
  df$A1Cresult <- ifelse(df$A1Cresult == ">7",    7, df$A1Cresult);
  df$A1Cresult <- ifelse(df$A1Cresult == ">8",    8, df$A1Cresult);
  df$A1Cresult <- as.factor(df$A1Cresult);
  
  #columns <- c("metformin", "repaglinide", "nateglinide", "chlorpropamide", "glimepiride", "acetohexamide", "glipizide", "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone", "acarbose", "miglitol", "troglitazone", "tolazamide", "examide", "citoglipton", "insulin", "glyburide-metformin", "glipizide-metformin", "glimepiride-pioglitazone", "metformin-rosiglitazone", "metformin-pioglitazone");
  columns <- colnames(df)[c(21:43)]
  for( c in columns){
    df[[c]] <- ifelse(df[[c]] == "Up",     +10, df[[c]]);
    df[[c]] <- ifelse(df[[c]] == "Down",   -10, df[[c]]);
    df[[c]] <- ifelse(df[[c]] == "Steady", +0, df[[c]]);
    df[[c]] <- ifelse(df[[c]] == "No",     -20, df[[c]]);
    df[[c]] <- as.factor(df[[c]]);
  }
  
  df$change <- ifelse(df$change == "No", -1, df$change);
  df$change <- ifelse(df$change == "Ch", +1, df$change);
  df$change <- as.factor(df$change);
  
  df$diabetesMed <- ifelse(df$diabetesMed == "Yes", +1, df$diabetesMed);
  df$diabetesMed <- ifelse(df$diabetesMed == "No",  -1, df$diabetesMed);
  df$diabetesMed <- as.factor(df$diabetesMed);
  
  
  if(balanced==TRUE){
    df$readmitted <- ifelse(df$readmitted != "NO", 1, 0); # ">30", "<30", "NO"
    df$readmitted <- as.factor(df$readmitted)
  }else{
    df$readmitted <- ifelse(df$readmitted == "<30", 1, 0); # ">30", "<30", "NO"
    df$readmitted <- as.factor(df$readmitted) 
  }
  
  data2 <- df
  colnames(data2)[46] <- "readmitted_dummy"
  
  return(data2)
}

#===========================Functions of the Algortihms=================================
#The output of the algorithm functions provide us a dataframe which consist of the predictions (1s and 0s)

#Logistic Regression Algorithm 
LogitpredFunction <- function(traindata, testdata, bagging=c("FALSE", "TRUE")){
  
  bagging <- match.arg(bagging) 
  if(bagging==TRUE){
    randomset <- sort(sample(nrow(traindata), nrow(traindata), replace = T))
    traindata <- traindata[randomset,]
  }
  
  selectvar <- c(4:16, 18:20, 22:24, 27, 28, 30, 31, 36, 40:43, 47, 53:63)
  indep <- colnames(traindata)[selectvar] #
  modelformula <- as.formula(paste(colnames(traindata)[65], "~", paste(indep, collapse = "+"), sep = ""))
  
  model <- glm(modelformula, data=traindata, family=binomial(link="logit")) #The model to predict
  
  if(any(is.na(model$coefficients))){
    omit <-  which(is.na(model$coefficients))
    indep <- indep[-(omit-1)]
    modelformula <- as.formula(paste(colnames(traindata)[55], "~", paste(indep, collapse = "+"), sep = ""))
    model <- glm(modelformula, data=traindata, family=binomial(link="logit")) #The model to predict
  }
  
  pred.prob <- predict(model, testdata, type="response") #predict the GLM model with the testing data

  roccurve <- roc(response=testdata$readmitted_dummy, predictor=pred.prob) #In order to determine the optimal cutoff
  cut <- max(coords(roccurve, "best", ret="threshold", best.method="youden"))
  # cut <- mean(as.numeric(test$readmitted_dummy)-1)  #Based on the observations
  
  prediction <- ifelse(pred.prob > cut,1,0)
  auroc <- auc(testdata$readmitted_dummy, pred.prob)[1]
  
  LR.prob <- pred.prob
  LR.pred <- prediction
  
  return(list(pred=data.frame(LR.pred), prob=data.frame(LR.prob), AUC=auroc))
}

#NaiveBayes Algorithm 
NaiveBayesFunction <- function(traindata, testdata, bagging=c("FALSE", "TRUE")){
  
  bagging <- match.arg(bagging) 
  if(bagging==TRUE){
    randomset <- sort(sample(nrow(traindata), nrow(traindata), replace = T))
    traindata <- traindata[randomset,]
  }
  
  model <- naiveBayes(readmitted_dummy ~ ., data = traindata, laplace=1)
  prediction <- as.numeric(predict(model, testdata)) -1
  
  pred.prob <- predict(model, testdata, type='raw')
  result.roc <- roc(testdata$readmitted_dummy, pred.prob[,2]) # Draw ROC curve.
  auroc <- result.roc$auc[1]
  
  NB.pred <- prediction
  NB.prob <- pred.prob[,2]
  
  #Plot Roc Curve
  # pred <- prediction(pred.prob[, 2], testdata$readmitted_dummy)
  # nb.prff <- performance(pred, "tpr", "fpr")
  # plot(nb.prff, col="blue")
  # lines(x = c(0,1), y = c(0,1))
  
  return(list(pred=data.frame(NB.pred), prob=data.frame(NB.prob), AUC=auroc))
}

#Random Forest Algorithm 
RandomForestFunction <- function(modelformula, traindata, testdata, numTrees, bagging=c("FALSE", "TRUE")){
  
  bagging <- match.arg(bagging) 
  if(bagging==TRUE){
    randomset <- sort(sample(nrow(traindata), nrow(traindata), replace = T))
    traindata <- traindata[randomset,]
  }
  
  model <- randomForest(modelformula, data=traindata, importance=TRUE, ntree=numTrees, method = "class",type="prob")

  prediction <- as.numeric(predict(model, testdata)) -1
  
  #The AUC for the Random Forest
  pred.prob <- predict(model, testdata, type="prob") # Prediction
  result.roc <- roc(testdata$readmitted_dummy, pred.prob[,2]) # Draw ROC curve.
  auroc <- result.roc$auc[1]
  # plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")
  # result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
  # print(result.coords)#to get threshold and accuracy
  
  RF.pred <- prediction
  RF.prob <- pred.prob[,2]
  
  return(list(pred=data.frame(RF.pred), prob=data.frame(RF.prob), AUC=auroc))
}

#KNN Algorithm
kNNFunction <- function(traindata, testdata, k, bagging=c("FALSE", "TRUE")){
  
  bagging <- match.arg(bagging) 
  if(bagging==TRUE){
    randomset <- sort(sample(nrow(traindata), nrow(traindata), replace = T))
    traindata <- traindata[randomset,]
  }
  
  D_traintarget = traindata$readmitted_dummy
  D_testtarget = testdata$readmitted_dummy
  
  model <- knn(traindata, testdata, cl= D_traintarget, k, prob=T)
  pred.prob <- attributes(model)$prob
  
  prediction <- as.numeric(model) -1 
  
  result.roc <- multiclass.roc(response=D_testtarget, as.ordered(model))
  auroc <- result.roc$auc[1]
  
  kNN.pred <- prediction
  kNN.prob <- pred.prob
  
  return(list(pred=data.frame(kNN.pred),  prob=data.frame(kNN.prob), AUC=auroc))
}

#The Extreme Gradient Boosting Algorithm
EGBfunction <- function(traindata, testdata, rounds, bagging=c("FALSE", "TRUE")){
  
  bagging <- match.arg(bagging) 
  if(bagging==TRUE){
    randomset <- sort(sample(nrow(traindata), nrow(traindata), replace = T))
    traindata <- traindata[randomset,]
  }
  
  train <- traindata[,-64]; test <- testdata[,-64]
  
  #Change the class in numerical for both the train and test data
  for(i in 1:ncol(train)){
    train[,i] <- as.numeric(train[,i])  
  }
  for(i in 1:ncol(test)){
    test[,i] <- as.numeric(test[,i])  
  }
  
  #Store the train and test data in matrices
  dtrain <- xgb.DMatrix(as.matrix(train[,4:63]), label = train$readmitted_dummy) 
  dtest <- xgb.DMatrix(as.matrix(test[,4:63]), label = test$readmitted_dummy) 
  
  watchlist <- list(train = dtrain);
  
  param <- list(
    objective           = "binary:logistic",
    booster             = "gbtree",
    eta                 = 0.03,
    max_depth           = 5,
    eval_metric         = "auc",
    min_child_weight    = 150,
    alpha               = 0.00,
    subsample           = 0.80,
    colsample_bytree    = 0.70
  );
  
  clf <- xgb.cv(  params                = param,
                  data                  = dtrain,
                  nrounds               = rounds,
                  verbose               = 1,
                  watchlist             = watchlist,
                  maximize              = TRUE,
                  nfold                 = 10,
                  #folds                 = list(dt, dt2),
                  nthread               = 8,
                  print_every_n         = 50,
                  stratified            = TRUE,
                  early_stopping_rounds = 15,
                  prediction = TRUE
  );
  
  #obtain the best parameters from the algorithm
  max.aux <- which.max(clf$evaluation_log[,test_auc_mean])
  
  #Using the best parameters, fit the model again
  bst <- xgboost(param=param, data=dtrain, nrounds=max.aux, verbose=1)
  
  #Obtain the Predicted probabilities  
  pred.prob <- predict(bst, dtest)
  #pred.prob <- clf$pred
  
  #The cut off value for the predicted probabilities based on the ROC-curve
  roccurve <- roc(response=test$readmitted_dummy, predictor=pred.prob) #In order to determine the optimal cutoff
  cut <- max(coords(roccurve, "best", ret="threshold", best.method="closest.topleft"))
  
  prediction <- ifelse(pred.prob > cut,1,0)
  
  auroc <- auc(testdata$readmitted_dummy, pred.prob)[1]
  
  EGB.pred <- prediction
  EGB.prob <- pred.prob
  
  return(list(pred=data.frame(EGB.pred),  prob=data.frame(EGB.prob), AUC=auroc))
}


#=========================== Ensemble (Bagging with Majority Voting) =================================

#Each Ensemble function differs in the 5 different algorithms 

#Ensemble With all five different algorithms
Ensemble1 <- function(modelformula, data, data2, data3, iterations, k, numtrees, rounds, bagging){
  
  n <- iterations
  split <- 0.8
  p <- data.frame(FP=rep(NA,n), FN=rep(NA,n), TP=rep(NA,n), TN=rep(NA,n), accuracy=rep(NA,n),
                  acc.LR=rep(NA,n), acc.NB=rep(NA,n), acc.RF=rep(NA,n), acc.kNN=rep(NA,n), acc.EGB=rep(NA,n),
                  AUC.LR=rep(NA,n), AUC.NB=rep(NA,n), AUC.RF=rep(NA,n), AUC.kNN=rep(NA,n), AUC.EGB=rep(NA,n), AUC=rep(NA,n))
  
  for(i in 1:n){
    
    cat("iteration ", i, ";")
    
    #Random sample from the observations in order to construct the train and test data
    dt = sort(sample(nrow(data), nrow(data)*split))
    
    #Obtain the observations for the test data
    #dt2 <- c(1:nrow(data))
    #dt2 <- dt2[! dt2 %in% dt] #Remove all elements with the same value as in dt
    
    #Subset the data
    train <- data[dt,] #This dataset consist of the split percentage of the data
    train2 <- data2[dt,]
    train3 <- data3[dt,]
    test <- data[-dt,] #This dataset consist of the remaining data
    test2 <- data2[-dt,]
    test3 <- data3[-dt,]
    
    #Run the different algorithms
    Logit <- LogitpredFunction(train, test, bagging=bagging)
    Naive <- NaiveBayesFunction(train2, test2, bagging=bagging)
    RandomForest <- RandomForestFunction(modelformula, train, test, numtrees, bagging=bagging)
    kNN <- kNNFunction(train3, test3, k, bagging=bagging)
    EGB <- EGBfunction(train, test, rounds, bagging=bagging)
    
    p$acc.LR[i] <- mean(Logit$pred == test$readmitted_dummy)
    p$acc.NB[i] <- mean(Naive$pred == test$readmitted_dummy)
    p$acc.RF[i] <- mean(RandomForest$pred == test$readmitted_dummy)
    p$acc.kNN[i] <- mean(kNN$pred == test$readmitted_dummy)
    p$acc.EGB[i] <- mean(EGB$pred == test$readmitted_dummy)
    
    #The AUC
    p$AUC.LR[i] <- Logit$AUC
    p$AUC.NB[i] <- Naive$AUC
    p$AUC.RF[i] <- RandomForest$AUC
    p$AUC.kNN[i] <- kNN$AUC
    p$AUC.EGB[i] <- EGB$AUC
    
    
    #Combine the outputs from the algorithms
    Results  <- cbind(Logit$pred,  RandomForest$pred,  Naive$pred,  kNN$pred,  EGB$pred)# Store all final prediction results
    Results$PredReadmition <- rep(NA, nrow(Results))
    
    #Sum the predictions of all algorithms
    for(j in 1:nrow(Results)){
      Results$PredReadmition[j] <- sum(Results[j,(1:(ncol(Results)-1))])
    }
    
    Results$Output <- rep(0, nrow(Results))
    #Calculate the final prediction based on majority vote
    Results$Output[Results$PredReadmition > (ncol(Results)-2)/2] <- 1
    
    #Average predicted probability for the Ensemble
    Results <- cbind(Results, Logit$prob,  RandomForest$prob,  Naive$prob,  kNN$prob,  EGB$prob)
    Results$Ens.prob <- (Results[,8] + Results[,9] + Results[,10] + Results[,11] + Results[,12])/5
    
    p$AUC[i] <- auc(test$readmitted_dummy, Results$Ens.prob)[1]
    
    #Compute the Confusion matrix and accuracy and store the results
    conmatrix <- confusion.matrix(test$readmitted_dummy, Results$Output)
    p$FP[i] <- conmatrix[2,1]
    p$FN[i] <- conmatrix[1,2]
    p$TP[i] <- conmatrix[2,2]
    p$TN[i] <- conmatrix[1,1]
    
    p$accuracy[i] <- (p$TP[i]+p$TN[i])/sum(p$FP[i], p$FN[i], p$TP[i], p$TN[i]) #Accuracy
    
  } 
  return(p)
} 

#Ensemble With four different algorithms (No kNN)
Ensemble2 <- function(modelformula, data, data2, iterations, numtrees, rounds, bagging){
  
  n <- iterations
  split <- 0.8
  p <- data.frame(FP=rep(NA,n), FN=rep(NA,n), TP=rep(NA,n), TN=rep(NA,n), accuracy=rep(NA,n),
                  acc.LR=rep(NA,n), acc.NB=rep(NA,n), acc.RF=rep(NA,n), acc.EGB=rep(NA,n),
                  AUC.LR=rep(NA,n), AUC.NB=rep(NA,n), AUC.RF=rep(NA,n), AUC.EGB=rep(NA,n), AUC=rep(NA,n))
  
  for(i in 1:n){
    
    cat("iteration ", i, ";")
    
    #Random sample from the observations in order to construct the train and test data
    dt = sort(sample(nrow(data), nrow(data)*split))
    
    #Obtain the observations for the test data
    #dt2 <- c(1:nrow(data))
    #dt2 <- dt2[! dt2 %in% dt] #Remove all elements with the same value as in dt
    
    #Subset the data
    train <- data[dt,] #This dataset consist of the split percentage of the data
    train2 <- data2[dt,]
    test <- data[-dt,] #This dataset consist of the remaining data
    test2 <- data2[-dt,]
    
    #Run the different algorithms
    Logit <- LogitpredFunction(train, test, bagging=bagging)
    Naive <- NaiveBayesFunction(train2, test2, bagging=bagging)
    RandomForest <- RandomForestFunction(modelformula, train, test, numtrees, bagging=bagging)
    EGB <- EGBfunction(train, test, rounds, bagging=bagging)
    
    p$acc.LR[i] <- mean(Logit$pred == test$readmitted_dummy)
    p$acc.NB[i] <- mean(Naive$pred == test$readmitted_dummy)
    p$acc.RF[i] <- mean(RandomForest$pred == test$readmitted_dummy)
    p$acc.EGB[i] <- mean(EGB$pred == test$readmitted_dummy)
    
    #The AUC
    p$AUC.LR[i] <- Logit$AUC
    p$AUC.NB[i] <- Naive$AUC
    p$AUC.RF[i] <- RandomForest$AUC
    p$AUC.EGB[i] <- EGB$AUC
    
    
    #Combine the outputs from the algorithms
    Results  <- cbind(Logit$pred,  RandomForest$pred,  Naive$pred, EGB$pred)# Store all final prediction results
    Results$PredReadmition <- rep(NA, nrow(Results))
    
    #Sum the predictions of all algorithms
    for(j in 1:nrow(Results)){
      Results$PredReadmition[j] <- sum(Results[j,(1:(ncol(Results)-1))])
    }
    
    Results$Output <- rep(0, nrow(Results))
    #Calculate the final prediction based on majority vote
    Results$Output[Results$PredReadmition > (ncol(Results)-1.99)/2] <- 1
    
    #Average predicted probability for the Ensemble
    Results <- cbind(Results, Logit$prob,  RandomForest$prob,  Naive$prob, EGB$prob)
    Results$Ens.prob <- (Results[,7] + Results[,8] + Results[,9] + Results[,10])/4
    
    p$AUC[i] <- auc(test$readmitted_dummy, Results$Ens.prob)[1]
    
    #Compute the Confusion matrix and accuracy and store the results
    conmatrix <- confusion.matrix(test$readmitted_dummy, Results$Output)
    p$FP[i] <- conmatrix[2,1]
    p$FN[i] <- conmatrix[1,2]
    p$TP[i] <- conmatrix[2,2]
    p$TN[i] <- conmatrix[1,1]
    
    p$accuracy[i] <- (p$TP[i]+p$TN[i])/sum(p$FP[i], p$FN[i], p$TP[i], p$TN[i]) #Accuracy
    
  } 
  return(p)
} 

#Ensemble With four different algorithms (No EGB) 
Ensemble3 <- function(modelformula, data, data2, data3, iterations, k, numtrees){
  
  n <- iterations
  split <- 0.8
  p <- data.frame(accuracy=rep(NA,n), FP=rep(NA,n), FN=rep(NA,n), TP=rep(NA,n), TN=rep(NA,n), 
                  acc.logit=rep(NA,n), acc.naive=rep(NA,n), acc.randomforest=rep(NA,n), acc.knn=rep(NA,n))
  
  for(i in 1:n){
    
    cat("iteration ", i, ";")
    
    #Random sample from the observations in order to construct the train and test data
    dt = sort(sample(nrow(data), nrow(data)*split))
    
    #Obtain the observations for the test data
    #dt2 <- c(1:nrow(data))
    #dt2 <- dt2[! dt2 %in% dt] #Remove all elements with the same value as in dt
    
    #Subset the data
    train <- data[dt,] #This dataset consist of the split percentage of the data
    train2 <- data2[dt,]
    train3 <- data3[dt,]
    test <- data[-dt,] #This dataset consist of the remaining data
    test2 <- data2[-dt,]
    test3 <- data3[-dt,]
    
    #Run the different algorithms
    ResultsLogit <- LogitpredFunction(train, test, bagging=TRUE)
    ResultsNaive <- NaiveBayesFunction(train2, test2, bagging=TRUE)
    ResultsRandomForest <- RandomForestFunction(modelformula, train, test, numtrees, bagging=TRUE)
    ResultskNN <- kNNFunction(train3, test3, k, bagging=TRUE)
    
    p$acc.logit[i] <- mean(ResultsLogit == test$readmitted_dummy)
    p$acc.naive[i] <- mean(ResultsNaive == test$readmitted_dummy)
    p$acc.randomforest[i] <- mean(ResultsRandomForest == test$readmitted_dummy)
    p$acc.knn[i] <- mean(ResultskNN == test$readmitted_dummy)
    
    #Combine the outputs from the algorithms
    Results <- cbind(ResultsLogit, ResultsRandomForest, ResultsNaive, ResultskNN, ResultskNN) # Store all results
    Results$PredReadmition <- rep(NA, nrow(Results))
    
    #Sum the predictions of all algorithms
    for(j in 1:nrow(Results)){
      Results$PredReadmition[j] <- sum(Results[j,(1:(ncol(Results)-1))])
    }
    
    Results$Output <- rep(0, nrow(Results))
    #Calculate the final prediction based on majority vote
    Results$Output[Results$PredReadmition > (ncol(Results)-2)/2] <- 1
    
    #Compute the Confusion matrix and accuracy and store the results
    conmatrix <- confusion.matrix(test$readmitted_dummy, Results$Output)
    p$FP[i] <- conmatrix[2,1]
    p$FN[i] <- conmatrix[1,2]
    p$TP[i] <- conmatrix[2,2]
    p$TN[i] <- conmatrix[1,1]
    
    p$accuracy[i] <- (p$TP[i]+p$TN[i])/sum(p$FP[i], p$FN[i], p$TP[i], p$TN[i]) #Accuracy
    
  } 
  return(p)
} 

#Ensemble With 3 different algorithms (LR, NB and KNN)
Ensemble4 <- function(data, data2, data3, iterations, k, bagging){
  
  n <- iterations
  split <- 0.8
  p <- data.frame(FP=rep(NA,n), FN=rep(NA,n), TP=rep(NA,n), TN=rep(NA,n), accuracy=rep(NA,n),
                  acc.LR=rep(NA,n), acc.NB=rep(NA,n), acc.kNN=rep(NA,n),
                  AUC.LR=rep(NA,n), AUC.NB=rep(NA,n), AUC.kNN=rep(NA,n), AUC=rep(NA,n))
  
  for(i in 1:n){
    
    cat("iteration ", i, ";")
    
    #Random sample from the observations in order to construct the train and test data
    dt = sort(sample(nrow(data), nrow(data)*split))
    
    #Obtain the observations for the test data
    #dt2 <- c(1:nrow(data))
    #dt2 <- dt2[! dt2 %in% dt] #Remove all elements with the same value as in dt
    
    #Subset the data
    train <- data[dt,] #This dataset consist of the split percentage of the data
    train2 <- data2[dt,]
    train3 <- data3[dt,]
    test <- data[-dt,] #This dataset consist of the remaining data
    test2 <- data2[-dt,]
    test3 <- data3[-dt,]
    
    #Run the different algorithms
    Logit <- LogitpredFunction(train, test, bagging=bagging)
    Naive <- NaiveBayesFunction(train2, test2, bagging=bagging)
    kNN <- kNNFunction(train3, test3, k, bagging=bagging)
    
    p$acc.LR[i] <- mean(Logit$pred == test$readmitted_dummy)
    p$acc.NB[i] <- mean(Naive$pred == test$readmitted_dummy)
    p$acc.kNN[i] <- mean(kNN$pred == test$readmitted_dummy)
    
    #The AUC
    p$AUC.LR[i] <- Logit$AUC
    p$AUC.NB[i] <- Naive$AUC
    p$AUC.kNN[i] <- kNN$AUC
    
    
    #Combine the outputs from the algorithms
    Results  <- cbind(Logit$pred,  Naive$pred,  kNN$pred)# Store all final prediction results
    Results$PredReadmition <- rep(NA, nrow(Results))
    
    #Sum the predictions of all algorithms
    for(j in 1:nrow(Results)){
      Results$PredReadmition[j] <- sum(Results[j,(1:(ncol(Results)-1))])
    }
    
    Results$Output <- rep(0, nrow(Results))
    #Calculate the final prediction based on majority vote
    Results$Output[Results$PredReadmition > (ncol(Results)-2)/2] <- 1
    
    #Average predicted probability for the Ensemble
    Results <- cbind(Results, Logit$prob,  Naive$prob,  kNN$prob)
    Results$Ens.prob <- (Results[,6] + Results[,7] + Results[,8])/3
    
    p$AUC[i] <- auc(test$readmitted_dummy, Results$Ens.prob)[1]
    
    #Compute the Confusion matrix and accuracy and store the results
    conmatrix <- confusion.matrix(test$readmitted_dummy, Results$Output)
    p$FP[i] <- conmatrix[2,1]
    p$FN[i] <- conmatrix[1,2]
    p$TP[i] <- conmatrix[2,2]
    p$TN[i] <- conmatrix[1,1]
    
    p$accuracy[i] <- (p$TP[i]+p$TN[i])/sum(p$FP[i], p$FN[i], p$TP[i], p$TN[i]) #Accuracy
    
  } 
  return(p)
} 

#Ensemble With 3 different algorithms (LR, NB and RF)
Ensemble5 <- function(modelformula, data, data2, numtrees, iterations, bagging){
  
  n <- iterations
  split <- 0.8
  p <- data.frame(FP=rep(NA,n), FN=rep(NA,n), TP=rep(NA,n), TN=rep(NA,n), accuracy=rep(NA,n),
                  acc.LR=rep(NA,n), acc.NB=rep(NA,n), acc.RF=rep(NA,n),
                  AUC.LR=rep(NA,n), AUC.NB=rep(NA,n), AUC.RF=rep(NA,n), AUC=rep(NA,n))
  
  for(i in 1:n){
    
    cat("iteration ", i, ";")
    
    #Random sample from the observations in order to construct the train and test data
    dt = sort(sample(nrow(data), nrow(data)*split))
    
    #Obtain the observations for the test data
    #dt2 <- c(1:nrow(data))
    #dt2 <- dt2[! dt2 %in% dt] #Remove all elements with the same value as in dt
    
    #Subset the data
    train <- data[dt,] #This dataset consist of the split percentage of the data
    train2 <- data2[dt,]
    test <- data[-dt,] #This dataset consist of the remaining data
    test2 <- data2[-dt,]
    
    #Run the different algorithms
    Logit <- LogitpredFunction(train, test, bagging=bagging)
    Naive <- NaiveBayesFunction(train2, test2, bagging=bagging)
    RandomForest <- RandomForestFunction(modelformula, train, test, numtrees, bagging=bagging)
    
    p$acc.LR[i] <- mean(Logit$pred == test$readmitted_dummy)
    p$acc.NB[i] <- mean(Naive$pred == test$readmitted_dummy)
    p$acc.RF[i] <- mean(RandomForest$pred == test$readmitted_dummy)
    
    #The AUC
    p$AUC.LR[i] <- Logit$AUC
    p$AUC.NB[i] <- Naive$AUC
    p$AUC.RF[i] <- RandomForest$AUC
    
    
    #Combine the outputs from the algorithms
    Results  <- cbind(Logit$pred,  Naive$pred,  RandomForest$pred)# Store all final prediction results
    Results$PredReadmition <- rep(NA, nrow(Results))
    
    #Sum the predictions of all algorithms
    for(j in 1:nrow(Results)){
      Results$PredReadmition[j] <- sum(Results[j,(1:(ncol(Results)-1))])
    }
    
    Results$Output <- rep(0, nrow(Results))
    #Calculate the final prediction based on majority vote
    Results$Output[Results$PredReadmition > (ncol(Results)-2)/2] <- 1
    
    #Average predicted probability for the Ensemble
    Results <- cbind(Results, Logit$prob,  Naive$prob,  RandomForest$prob)
    Results$Ens.prob <- (Results[,6] + Results[,7] + Results[,8])/3
    
    p$AUC[i] <- auc(test$readmitted_dummy, Results$Ens.prob)[1]
    
    #Compute the Confusion matrix and accuracy and store the results
    conmatrix <- confusion.matrix(test$readmitted_dummy, Results$Output)
    p$FP[i] <- conmatrix[2,1]
    p$FN[i] <- conmatrix[1,2]
    p$TP[i] <- conmatrix[2,2]
    p$TN[i] <- conmatrix[1,1]
    
    p$accuracy[i] <- (p$TP[i]+p$TN[i])/sum(p$FP[i], p$FN[i], p$TP[i], p$TN[i]) #Accuracy
    
  } 
  return(p)
} 

#Ensemble6 <- function(modelformula, data, data2, numtrees, iterations, bagging){
  
  n <- iterations
  split <- 0.8
  p <- data.frame(FP=rep(NA,n), FN=rep(NA,n), TP=rep(NA,n), TN=rep(NA,n), accuracy=rep(NA,n),
                  acc.LR=rep(NA,n), acc.NB=rep(NA,n), acc.RF=rep(NA,n),
                  AUC.LR=rep(NA,n), AUC.NB=rep(NA,n), AUC.RF=rep(NA,n), AUC=rep(NA,n))
  
  for(i in 1:n){
    
    cat("iteration ", i, ";")
    
    #Random sample from the observations in order to construct the train and test data
    dt = sort(sample(nrow(data), nrow(data)*split))
    
    #Obtain the observations for the test data
    #dt2 <- c(1:nrow(data))
    #dt2 <- dt2[! dt2 %in% dt] #Remove all elements with the same value as in dt
    
    #Subset the data
    train <- data[dt,] #This dataset consist of the split percentage of the data
    train2 <- data2[dt,]
    test <- data[-dt,] #This dataset consist of the remaining data
    test2 <- data2[-dt,]
    
    input.train <- train[,-65]
    output.train <- as.factor(train[,65])
    
    balanced.train <- ubBalance(input.train, output.train, type="ubSMOTE",
                                percOver = 100, percUnder = 200, verbose = T)
    bal.train <- cbind(balanced.train$X, balanced.train$Y)
    train <- bal.train
    
    #Run the different algorithms
    Logit <- LogitpredFunction(train, test, bagging=bagging)
    Naive <- NaiveBayesFunction(train2, test2, bagging=bagging)
    RandomForest <- RandomForestFunction(modelformula, train, test, numtrees, bagging=bagging)
    
    p$acc.LR[i] <- mean(Logit$pred == test$readmitted_dummy)
    p$acc.NB[i] <- mean(Naive$pred == test$readmitted_dummy)
    p$acc.RF[i] <- mean(RandomForest$pred == test$readmitted_dummy)
    
    #The AUC
    p$AUC.LR[i] <- Logit$AUC
    p$AUC.NB[i] <- Naive$AUC
    p$AUC.RF[i] <- RandomForest$AUC
    
    
    #Combine the outputs from the algorithms
    Results  <- cbind(Logit$pred,  Naive$pred,  RandomForest$pred)# Store all final prediction results
    Results$PredReadmition <- rep(NA, nrow(Results))
    
    #Sum the predictions of all algorithms
    for(j in 1:nrow(Results)){
      Results$PredReadmition[j] <- sum(Results[j,(1:(ncol(Results)-1))])
    }
    
    Results$Output <- rep(0, nrow(Results))
    #Calculate the final prediction based on majority vote
    Results$Output[Results$PredReadmition > (ncol(Results)-2)/2] <- 1
    
    #Average predicted probability for the Ensemble
    Results <- cbind(Results, Logit$prob,  Naive$prob,  RandomForest$prob)
    Results$Ens.prob <- (Results[,6] + Results[,7] + Results[,8])/3
    
    p$AUC[i] <- auc(test$readmitted_dummy, Results$Ens.prob)[1]
    
    #Compute the Confusion matrix and accuracy and store the results
    conmatrix <- confusion.matrix(test$readmitted_dummy, Results$Output)
    p$FP[i] <- conmatrix[2,1]
    p$FN[i] <- conmatrix[1,2]
    p$TP[i] <- conmatrix[2,2]
    p$TN[i] <- conmatrix[1,1]
    
    p$accuracy[i] <- (p$TP[i]+p$TN[i])/sum(p$FP[i], p$FN[i], p$TP[i], p$TN[i]) #Accuracy
    
  } 
  return(p)
} 

#========================= Variable Selection ==============================

#---------- Random forest Model formulation  (Includes all variables in the dataset)

# indep <- colnames(d)[c(4:54)]
# modelformula <- as.formula(paste(colnames(d)[55], "~", paste(indep, collapse = "+"), sep = ""))

modelformula <- as.factor(readmitted_dummy) ~ genderMale + 
  ageYoung +
  ageMiddle +
  ageOld +
  raceCaucasian +
  raceAfricanAmerican +
  raceAsian +
  raceHispanic +
  raceOther +
  raceMissing +
  time_in_hospital +
  num_lab_procedures +
  num_procedures +
  num_medications +
  number_outpatient +
  number_emergency +
  number_inpatient +
  number_diagnoses +
  max_glu_serum_none + 
  max_glu_serum_normal +
  max_glu_serum_200 +
  max_glu_serum_300 + 
  A1Cresult_none + 
  A1Cresult_normal + 
  A1Cresult_7 +
  A1Cresult_8 +
  metformin_dummy +
  repaglinide_dummy +
  nateglinide_dummy +
  chlorpropamide_dummy +
  glimepiride_dummy +
  acetohexamide_dummy +
  glipizide_dummy +
  glyburide_dummy +
  tolbutamide_dummy +
  pioglitazone_dummy +
  rosiglitazone_dummy +
  acarbose_dummy +
  miglitol_dummy +
  troglitazone_dummy +
  tolazamide_dummy +
  examide_dummy +
  citoglipton_dummy +
  insulin_dummy +
  glyburide.metformin_dummy +
  glipizide.metformin_dummy +
  glimepiride.pioglitazone_dummy +
  metformin.rosiglitazone_dummy +
  metformin.pioglitazone_dummy +
  change_dummy +
  diabetesMed_dummy +
  Diabetes +
  Neoplasms +
  Other +
  Circulatory +
  Respiratory +
  Injury +
  Musculoskeletal +
  Digestive +
  Genitourinary 

#----------  Naive Bayes Variable selection (Uses all variables)
data2 <- PrepDataNaiveBayes(df, balanced="FALSE")
#data2 <- PrepDataNaiveBayes(df, balanced="TRUE")

#----------  kNN Algorithm model formulation (With the optimal set of variables)

myvars <- c("raceCaucasian", "raceAfricanAmerican", "raceAsian", "raceHispanic", "raceOther", "raceMissing", 
            "time_in_hospital", "num_lab_procedures", "num_procedures","num_medications", "number_outpatient", 
            "number_emergency", "number_inpatient", "number_diagnoses", "genderMale", "ageYoung", "ageMiddle", 
            "max_glu_serum_none", "max_glu_serum_normal", "max_glu_serum_200", "A1Cresult_none", "A1Cresult_normal",
            "A1Cresult_7", "metformin_dummy", "repaglinide_dummy", "acetohexamide_dummy", "glyburide_dummy", 
            "pioglitazone_dummy", "acarbose_dummy", "tolazamide_dummy", "change_dummy", "diabetesMed_dummy", 
            "Diabetes", "Neoplasms", "Other", "Circulatory", "Respiratory", "Injury",
            "Musculoskeletal", "Digestive", "Genitourinary", "readmitted_dummy")
data3 <- d[myvars] 

#----------  Logistic regression Variable selection (Based on the AIC-value)

# logit <- glm(modelformula, data=train, family=binomial(link="logit")) #the model which includes all variables
# step <- stepAIC(logit, direction="both") #Obtain the AIC Vaulues for different models
# 
# keepx <- names(step$model)[-1] #To obtain the best model based on AIC
# index <- match(keepx, names(d)) #Matches the column numbers with the variables for the best model
# selectvar <- which(names(d) %in% keepx) #Store the variables for the best model

#Eventually, these columns of the entire data set provide the lowest AIC score 
selectvar <- c(4:16, 18:20, 22:24, 27, 28, 30, 31, 36, 40:43, 47, 53:64)
indep2 <- colnames(d)[selectvar]
modelformula2 <- as.formula(paste(colnames(d)[55], "~", paste(indep2, collapse = "+"), sep = ""))

#========================= Run the Ensemble and obtain Results ==============================

#Run the Ensemble (The output consist of the accuracy and the elements of the confusion matrix)
#Note that:
#   - modelformula is used for the RandomForest algorithm
#   - iterations denotes the total number of times the ensemble should run
#   - k denotes the total number of neighbours for the kNN algorithm
#   - numtrees denotes the total number of trees

# #Recall: 
#     - Ensemble1: all 5 algorithms (LR, NB, RF, kNN, EGB)
#     - Ensemble2: LR, NB, RF and EGB
#     - Ensemble3: LR, NB, kNN and RF
#     - Ensemble4: LR, NB and KNN
#     - Ensemble5: LR, NB and RF
 

# Ensmb11 <- Ensemble1(modelformula, d, data2, data3, iterations=2, k=20, numtrees=10, rounds=15, bagging="TRUE")
# Ensmb12 <- Ensemble1(modelformula, d, data2, data3, iterations=1, k=20, numtrees=10, rounds=15, bagging="FALSE")

#This ensemble yields the best results
Ensmb21 <- Ensemble2(modelformula, d, data2, iterations=100, numtrees=10, rounds=20, bagging="TRUE")
Ensmb22 <- Ensemble2(modelformula, d, data2, iterations=30, numtrees=10, rounds=20, bagging="FALSE")

# Ensmb41 <- Ensemble4(d, data2, data3, iterations=2, k=20, bagging="TRUE")
# Ensmb42 <- Ensemble4(d, data2, data3, iterations=2, k=20, bagging="FALSE")

# Ensmb51 <- Ensemble5(modelformula, d, data2, numtrees=10, iterations=2, bagging="TRUE")
# Ensmb52 <- Ensemble5(modelformula, d, data2, numtrees=10, iterations=2, bagging="FALSE")

#write.csv(Ensmb21, file = "output.csv" ,row.names=F) #Write the results into a csv file

#========================= From R to latex ==============================

# names(d)
# demographics <- c(4, 5, 6, 7, 12, 18, 19, 20, 21, 47, 55)
# stargazer(d[,demographics], omit.summary.stat = "N" ) #descriptive statistics to Latex

#========================= Combine all the output into a dataframe =========================
#Code to combine all seperate result files from a folder into one dataframe
# for(i in 1:9){
#   if(i==1){ #empty dataframe for the first iteration
#     oldoutput <- data.frame(accuracy=rep(NA,0), FP=rep(NA,0), FN=rep(NA,0), TP=rep(NA,0), TN=rep(NA,0),
#                  acc.logit=rep(NA,0), acc.naive=rep(NA,0), acc.randomforest=rep(NA,0), acc.knn=rep(NA,0), acc.egb=rep(NA,0))
#   }
#   name <- paste("output",i,".csv", sep="") # Load the output data
#   newoutput <- read.csv(name)
# 
#   output <- rbind(oldoutput, newoutput) #Combine the output data
#   oldoutput <- output
# }
#========================= Mean Results and Confidence intervals =========================

#Results Averages from the ensemble output
av.con <- c(mean(output[,1]), mean(output[,2]), mean(output[,3]), mean(output[,4]))
av.auc <- c(mean(output[,14]), mean(output[,10]), mean(output[,11]), mean(output[,12]), mean(output[,13]))
round(av.auc,3)
round(av.con)

#Fit distributions for all different outputs in order to construct confience intervals
fit1 <- fitdist(output$AUC, "norm"); fit2 <- fitdist(output$AUC, "weibull")
fit10 <- fitdist(output[,10], "norm"); fit20 <- fitdist(output[,10], "weibull")
fit11 <- fitdist(output[,11], "norm"); fit21 <- fitdist(output[,11], "weibull")
fit12 <- fitdist(output[,12], "norm"); fit22 <- fitdist(output[,12], "weibull")
fit13 <- fitdist(output[,13], "norm"); fit23 <- fitdist(output[,13], "weibull")

#Compare the AIC and we can conclude that the normal distr. has a good fit 
fit1$aic; fit2$aic #Ensemble
fit10$aic; fit20$aic #LR
fit11$aic; fit21$aic #NB
fit12$aic; fit22$aic #RF
fit13$aic; fit23$aic #EGB

#Therefore, the Normal distribution is used to confidence intervals for all output
confint(fit1); cat("(",round(confint(fit1)[1,],4), ")")
confint(fit10); cat("(",round(confint(fit10)[1,],4), ")")
confint(fit11); cat("(",round(confint(fit11)[1,],4), ")")
confint(fit12); cat("(",round(confint(fit12)[1,],4), ")")
confint(fit13); cat("(",round(confint(fit13)[1,],4), ")")

#If we use the Weibull distribution:
#Calculate the mean of the Weibull distribution using the parameters given in the CI
#weibullparinv(shape=86.38591051, scale=0.6182255, loc = 0)

#95% CI for the TN, FP and FN
fTN1 <- fitdist(output$TN, "norm"); fTN2 <- fitdist(output$TN, "weibull")
fFN1 <- fitdist(output$FN, "norm"); fFN2 <- fitdist(output$FN, "weibull")
fFP1 <- fitdist(output$FP, "norm"); fFP2 <- fitdist(output$FP, "weibull")
fTP1 <- fitdist(output$TP, "norm"); fTP2 <- fitdist(output$TP, "weibull")

#Compare the AIC and we can conclude that the normal distr. has the best fit 
fTN1$aic; fTN2$aic
fFN1$aic; fFN2$aic
fFP1$aic; fFP2$aic
fTP1$aic; fTP2$aic

#As the AIC is smaller for the Normal distribution in all cases use the normal distr.
confint(fTN1); cat("(",round(confint(fTN1)[1,]), ")")
confint(fFN1); cat("(",round(confint(fFN1)[1,]), ")")
confint(fFP1); cat("(",round(confint(fFP1)[1,]), ")")
confint(fTP1); cat("(",round(confint(fTP1)[1,]), ")")
