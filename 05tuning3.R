## Title -----------------------------------------------------------------------
##
## name:    05tuning3.R
## author:  Dennis F Langley
## date:    2018-06-04
## what:    Tune Models for Chapter 3
##       
##
####


## Preamble --------------------------------------------------------------------
rm(list = ls())
dev.off()
options(scipen = 999)
setwd("~/Documents/Dissertation/Data")

library(foreign)
library(readstata13)
library(tidyverse)
library(plyr)
library(gdata)
library(mice)
library(VIM)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(texreg)
library(lmtest)
library(caret)
library(data.table)
library(reshape2)
library(foreach)
library(parallel)
library(doParallel)
library(elasticnet)
library(Hmisc)
library(randomForest)
library(gbm)
library(Cubist)
library(xtable)
library(beepr)
library(pls)
load("~/Documents/Dissertation/Analysis/01imputeddata.RData")


## Custom Functions ------------------------------------------------------------
error<-function(pred.Y, Y) {
  sqrt(sum((pred.Y-Y)^2)/nrow(Y))
}

rm(d.cces.old)

d.cces.old <- d.cces

cat("\014")

## Machine Learning ------------------------------------------------------------

# Only keep the variables I'll train on

d.cces <- dplyr::select(d.cces, c(cces.pri.es, cces.pri.ae, psi, female, minority, married, party.strength, ideo.strength, church_freq, interest, income, educ_cat))

set.seed(32308)

folds <- createFolds(d.cces$psi, k = 5, list = TRUE, returnTrain = FALSE)
iter<-list(c(folds$Fold1),  c(folds$Fold2), c(folds$Fold3), c(folds$Fold4), c(folds$Fold5))
ctrlParallel<-trainControl(method="cv", n=5, predictionBounds = c(0, NA), 
                           savePredictions="final", allowParallel=T, selectionFunction = "oneSE")

model.names<-c("null", "nullpstr", "ols", "pls", "ridge", "lasso", "enet", "knn", "cart", "rf", "bsttree", "cub", "svm", "nnet")

registerDoParallel(cores = detectCores() - 2)

i <- 1

tune<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %dopar% {
  
  test<-d.cces[iter[[i]],]
  train<-d.cces[-iter[[i]],]
  train <- dplyr::select(train, -cces.pri.es, -cces.pri.ae)
  
  x.test<-dplyr::select(test, -psi, -cces.pri.es, -cces.pri.ae)
  y.test<-dplyr::select(test, psi)
  
  # nested cross validation
  # 1 Null
  set.seed(32308)
  
  nullModel<-train(psi~., 
                   data=train, 
                   method="null",
                   trControl=ctrlParallel)
  
  # 2 null_pid
  set.seed(32308)
  
  nullpstrModel<-train(psi~party.strength, 
                       data=train, 
                       method="lm",
                       trControl=ctrlParallel)
  
  # 3 Linear regression
  set.seed(32308)
  
  olsModel<-train(psi~., 
                  data=train,
                  method="lm",
                  trControl=ctrlParallel)
  
  # 4 PLS
  set.seed(32308)
  
  plsModel<-train(psi~., 
                  data=train,
                  method="pls",
                  trControl=ctrlParallel,
                  tuneLength= 10,
                  preProcess=c("center", "scale"))
  
  # 5 Ridge
  set.seed(32308)
  
  ridgeModel<-train(psi~., 
                    data=train,
                    method="ridge",
                    trControl=ctrlParallel,
                    preProcess=c("center", "scale"))
  
  # 6 Lasso
  set.seed(32308)
  
  lassoModel<-train(psi~., 
                    data=train,
                    method="lasso",
                    trControl=ctrlParallel,
                    preProcess=c("center", "scale"))
  
  # 7 Elastic Net
  set.seed(32308)
  
  enetModel<-train(psi~., 
                   data=train,
                   method="enet",
                   trControl=ctrlParallel,
                   preProcess=c("center", "scale"))
  
  # 8 KNN
  set.seed(32308)
  
  knnModel<-train(psi~., 
                  data=train, 
                  method="knn", 
                  trControl=ctrlParallel,
                  preProcess=c("center", "scale"))
  
  # 9 CART tuning over max depth
  set.seed(32308)
  
  cartModel<-train(psi~., 
                   data=train, 
                   method="rpart2",
                   trControl=ctrlParallel)
  
  # 10 Random Forest
  set.seed(32308)
  
  rfModel<-train(psi~., 
                 data=train, 
                 method="rf", 
                 trControl=ctrlParallel, 
                 importance=T)
  
  # 11 Boosted Tree
  set.seed(32308)
  
  bstModel<-train(psi~.,
                  data=train,
                  method="gbm",
                  trControl=ctrlParallel, 
                  verbose=F)
  
  # 12 Cubist
  set.seed(32308)
  
  cubModel<-train(psi~., 
                  data=train,  
                  method="cubist", 
                  trControl=ctrlParallel)
  
  # 13 SVM r
  set.seed(32308)
  
  svmModel<-train(psi~.,
                  data=train,
                  trControl=ctrlParallel,
                  method="svmRadial",
                  preProcess=c("center", "scale"))
  
  # 14 aNN
  set.seed(32308)
  
  nnetModel<-train(psi~.,
                   data=train,
                   trControl=ctrlParallel,
                   method="avNNet",
                   preProcess=c("center", "scale"),
                   linout=1,
                   trace=F)
  
  # Preliminary Models
  model_fold<-list(nullModel, #1
                   nullpstrModel,# 2
                   olsModel, # 3
                   plsModel,  #4
                   ridgeModel, # 5
                   lassoModel, # 6
                   enetModel, # 7
                   knnModel,  # 8
                   cartModel, # 9
                   rfModel, # 10
                   bstModel, # 11
                   cubModel, # 12
                   svmModel, # 13
                   nnetModel) # 14
  
  
  # grab predictions
  pred.fold<-foreach(j=1:length(model_fold)) %do%
    predict.train(model_fold[[j]], x.test)
  
  pred.mat<-as.matrix(do.call(cbind, pred.fold))
  
  # rmse for the candidate learners
  rmse.learners<-foreach(j=1:length(model_fold)) %do%
    error(pred.mat[,j], y.test)
  
  rmse.mat<-do.call(rbind, rmse.learners)
  
  # tune
  par<-foreach(j=1:length(model_fold)) %do%
    model_fold[[j]]$bestTune
  
  #
  out<-list("rmse"=rmse.mat, "pred"=pred.mat, "observed"=y.test, "tune"=par)
  
  out
})

par<-lapply(tune, '[', 'tune')

{
  nullTune<-list() 
  nullpstrTune<-list()
  olsTune<-list()
  plsTune<-list()
  plsTune<-list()
  ridgeTune<-list()
  lassoTune<-list()
  enetTune<-list()
  knnTune<-list()
  cartTune<-list()
  rfTune<-list()
  bstTune<-list()
  cubTune<-list()
  svmTune<-list()
  nnetTune<-list()
}

for(i in 1: length(par)){
  fold<-par[[i]]
  
  nullTune[[i]]<-fold$tune[[1]] 
  nullpstrTune[[i]]<-fold$tune[[2]]
  olsTune[[i]]<-fold$tune[[3]]
  plsTune[[i]]<-fold$tune[[4]]
  ridgeTune[[i]]<-fold$tune[[5]]
  lassoTune[[i]]<-fold$tune[[6]]
  enetTune[[i]]<-fold$tune[[7]]
  knnTune[[i]]<-fold$tune[[8]]
  cartTune[[i]]<-fold$tune[[9]]
  rfTune[[i]]<-fold$tune[[10]]
  bstTune[[i]]<-fold$tune[[11]]
  cubTune[[i]]<-fold$tune[[12]]
  svmTune[[i]]<-fold$tune[[13]]
  nnetTune[[i]]<-fold$tune[[14]]
}

tune.list<-list(nullTune, nullpstrTune, olsTune, plsTune, ridgeTune, lassoTune, enetTune, knnTune, cartTune, rfTune, bstTune, cubTune, svmTune, nnetTune)

i = 1
j = 1

final<-suppressWarnings(foreach(i=1:length(iter), .packages=c('dplyr', 'foreach', 'caret')) %dopar%  {
  
  tune.fold<-foreach(j=1:length(folds)) %do% {
    test<-d.cces[iter[[j]],]
    train<-d.cces[-iter[[j]],]
    train <- dplyr::select(train, -cces.pri.es, -cces.pri.ae)
    
    x.test<-dplyr::select(test, -psi)
    y.test<-dplyr::select(test, psi)
    
    set.seed(32308)
    
    # 1 Null
    nullModel<-train(psi~., 
                     data=train, 
                     method="null", 
                     trControl=ctrlParallel)
    
    # 2 Null cinc
    set.seed(32308)
    
    nullpstrModel<-train(psi~party.strength, 
                         data=train, 
                         method="lm",
                         trControl=ctrlParallel)
    
    # 3 Linear regression
    set.seed(32308)
    
    olsModel<-train(psi~., 
                    data=train,
                    method="lm",
                    trControl=ctrlParallel)
    
    # 4 PLS
    set.seed(32308)
    
    plsModel<-train(psi~., 
                    data=train,
                    method="pls",
                    trControl=ctrlParallel,
                    tuneGrid=plsTune[[i]],
                    preProcess=c("center", "scale"))
    
    # 5 Ridge
    set.seed(32308)
    
    ridgeModel<-train(psi~., 
                      data=train,
                      method="ridge",
                      trControl=ctrlParallel,
                      tuneGrid=ridgeTune[[i]],
                      preProcess=c("center", "scale"))
    
    # 6 Lasso
    set.seed(32308)
    
    lassoModel<-train(psi~., 
                      data=train,
                      method="lasso",
                      trControl=ctrlParallel,
                      tuneGrid=lassoTune[[i]],
                      preProcess=c("center", "scale"))
    
    # 7 Elastic Net
    set.seed(32308)
    
    enetModel<-train(psi~., 
                     data=train,
                     method="enet",
                     trControl=ctrlParallel,
                     tuneGrid=enetTune[[i]],
                     preProcess=c("center", "scale"))
    
    # 8 KNN
    set.seed(32308)
    
    knnModel<-train(psi~., 
                    data=train, 
                    method="knn", 
                    tuneGrid=knnTune[[i]], 
                    trControl=ctrlParallel, 
                    preProcess=c("center", "scale"))
    
    # 9 CART tuning over max depth
    set.seed(32308)
    
    cartModel<-train(psi~., 
                     data=train, 
                     method="rpart2",
                     trControl=ctrlParallel, 
                     tuneGrid=cartTune[[i]])
    
    # 10 Random Forest
    set.seed(32308)
    
    rfModel<-train(psi~., 
                   data=train, 
                   method="rf", 
                   trControl=ctrlParallel, 
                   tuneGrid=rfTune[[i]])
    
    # 11 Boosted Tree
    set.seed(32308)
    
    bstModel<-train(psi~.,
                    data=train,
                    method="gbm",
                    tuneGrid=bstTune[[i]],
                    trControl=ctrlParallel)
    
    # 12 Cubist
    set.seed(32308)
    
    cubModel<-train(psi~., 
                    data=train,  
                    method="cubist", 
                    trControl=ctrlParallel, 
                    tuneGrid = cubTune[[i]])
    
    # 13 SVM r
    set.seed(32308)
    
    svmModel<-train(psi~.,
                    data=train,
                    trControl=ctrlParallel,
                    method="svmRadial",
                    tuneGrid=svmTune[[i]],
                    preProcess=c("center", "scale"))
    
    # 14 aNN
    set.seed(32308)
    
    nnetModel<-train(psi~.,
                     data=train,
                     trControl=ctrlParallel,
                     tuneGrid=nnetTune[[i]],
                     method="avNNet",
                     preProcess=c("center", "scale"),
                     linout=1,
                     trace=F)
    
    # Preliminary Models
    model_fold<-list(nullModel, #1
                     nullpstrModel,# 2
                     olsModel, # 3
                     plsModel,  # 4
                     ridgeModel, # 5
                     lassoModel, # 6
                     enetModel, # 7
                     knnModel,  # 8
                     cartModel, # 9
                     rfModel, # 9
                     bstModel, # 10
                     cubModel, # 11
                     svmModel, # 12
                     nnetModel) # 13
    
    
    # grab predictions
    pred.fold<-foreach(k=1:length(model_fold)) %do%
      predict.train(model_fold[[k]], x.test)
    
    pred.mat<-as.matrix(do.call(cbind, pred.fold))
    
    # rmse for the candidate learners
    rmse.learners<-foreach(k=1:length(model_fold)) %do%
      error(pred.mat[,k], y.test)
    
    rmse.mat<-do.call(rbind, rmse.learners)
    
    #
    rmse.mat
    
    
  }
  
  rmse.par<-do.call(cbind, tune.fold)
  
  rmse.avg<-apply(rmse.par, 1, mean)
  
  rmse.sd<-apply(rmse.par, 1, sd)
  
  out<-list("RMSE"=rmse.avg, "SD"=rmse.sd)
  
  out
})

# extract relevant info
rmse<-do.call(cbind, lapply(final, '[[', 'RMSE'))
sd<-do.call(cbind, lapply(final, '[[', 'SD'))

rmse.tune<-rmse[1:nrow(rmse),]
sd.tune<-sd[1:nrow(sd),]


par<-foreach(i=4:nrow(rmse.tune)) %do%{
  num<-which(rmse.tune[i,]==min(rmse.tune[i,]))
  unlist(tune.list[[i]][num][1])
}

names(par)<-model.names[4:length(model.names)]

# get the min RMSE for each learner
rmse.min<-foreach(i=1:nrow(rmse.tune), .combine=rbind) %do%{
  a<-rmse.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
  b<-sd.tune[i, which(rmse.tune[i,]==min(rmse.tune[i,]))][1]
  c<-which(rmse.tune[i,]==min(rmse.tune[i,]))[1]
  unlist(c(a, b, c))
}
rownames(rmse.min)<-model.names
colnames(rmse.min)<-c("rmse", "sd", "fold")

best.tune.params<-list()

for (i in 1:length(tune.list)){
  best.tune.params[i] <- tune.list[[i]][rmse.min[i,3]]
}


pre.null <- vector()
for (i in 1:nrow(rmse.min)){
  pre.null[i] <- ((rmse.min[1,1]-rmse.min[i,1])/rmse.min[1,1])
}

table<-cbind(rmse.min[,1:2], pre.null)
rownames(table)<-model.names

table.rmse<-round(table, 5)
table.names<-c("Null",
               "Null + p.str",
               "OLS", 
               "PLS", 
               "Ridge", 
               "Lasso", 
               "Elastic Net", 
               "KNN", 
               "CART", 
               "RF",
               "Boosted Trees",
               "Cubist",
               "SVM Radial",
               "Neural Networks")

rownames(table.rmse)<-table.names
colnames(table.rmse)<-c("RMSE", "SD", "PRE")

table.rmse.sort.rmse <- table.rmse[order(-table.rmse[,1]),]
table.rmse.sort.rmse

table.paper <- xtable(table.rmse.sort.rmse, digits=4)


set.seed(32308)
bstModel<-train(psi~.,
                data=dplyr::select(d.cces, -cces.pri.es, -cces.pri.ae),
                method="gbm",
                tuneGrid=bstTune[[rmse.min[which(rownames(rmse.min)=="bsttree"),3]]],
                trControl=ctrlParallel)

d.anes.ts$psi <- predict(bstModel, newdata = d.anes.ts)


save.image("~/Documents/Dissertation/Analysis/05tuneddata.RData")
