library(tidyverse)
library(lubridate)

#load data
#lcdf <- read.csv('D:/LEARNING STUDY/UIC STUDY/IDS 572/Assignment 2/lcdf_2.csv')
lcdf <- read.csv('/Users/vanisa/Documents/Doc_MSBA_Fall2020/IDS572_DataMining/Assignment_2/lcdf_2.csv')

lcdf <- subset(lcdf, select = -c(total_rec_prncp,earliest_cr_line,debt_settlement_flag,total_rec_int,funded_amnt_inv,
                                 total_rec_late_fee,installment,total_pymnt_inv,revol_util,dti,
                                 annual_inc,tot_hi_cred_lim,num_op_rev_tl,pct_tl_nvr_dlq ,
                                 total_il_high_credit_limit,issue_d,total_bal_ex_mort,
                                 total_bc_limit,num_rev_tl_bal_gt_0,num_actv_bc_tl,
                                 mo_sin_old_rev_tl_op,num_bc_tl,num_bc_sats,num_accts_ever_120_pd,
                                 mths_since_last_delinq,num_tl_90g_dpd_24m,mo_sin_rcnt_tl,
                                 num_actv_rev_tl,mo_sin_rcnt_rev_tl_op,num_tl_30dpd,num_tl_op_past_12m 
                                 )) 

#delete single value (single category)
lcdf <- subset(lcdf, select = -c(disbursement_method,term,X)) 
summary(lcdf)
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))==1] 
#save lcdf for problem2 i.e. backup of lcdf
lcdfx <- lcdf
#head(lcdfx$actualReturn[lcdfx$actualReturn<0])  # checking how negative actual returns
str(lcdfx) #var = 19
########################################### 1 #############################################################
############################## Loan Status prediction #####################################################
################################ Splitting ################################################################
#Splitting Training Data set and Testing Data Set
lcdf <- lcdf %>% mutate_if(is.character, as.factor)

nr=nrow(lcdf)
TRG_PCT=0.7
set.seed(1789)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) 
lcdfTrn = lcdf[trnIndex,] #var = 20  
lcdfTst = lcdf[-trnIndex,] #var = 20

eva_lcdfTrn <- lcdfTrn
eva_lcdfTst <- lcdfTst

str(eva_lcdfTrn)
################################ a1 gbm : Dataset for gbm,glm ##############################################

lcdfTrn <- subset(lcdfTrn, select = -c(annRet,actualTerm,actualReturn, total_pymnt)) #var = 15
lcdfTst <- subset(lcdfTst, select = -c(annRet,actualTerm,actualReturn, total_pymnt)) #var = 15
lcdf <- subset(lcdf, select = -c(annRet,actualTerm,actualReturn, total_pymnt)) #var = 15


#“Bernoulli” distribution requires {0, 1} values for dependent variable
#lcdf$loan_status <- factor(lcdf$loan_status, levels=c(1, 0))
lcdf$loan_status <- ifelse(lcdf$loan_status=="Charged Off", 0, 1)
str(lcdf$loan_status) 
lcdfTrn$loan_status <- ifelse(lcdfTrn$loan_status=="Charged Off", 0, 1)
lcdfTst$loan_status <- ifelse(lcdfTst$loan_status=="Charged Off", 0, 1)
str(lcdfTrn) #15
str(lcdfTst) #15

#balance
library(ROSE)
set.seed(1789)
lcdfTrn <- ovun.sample(loan_status~.,data=lcdfTrn,method="both", p=0.5,N=71208,seed=1789)$data
table(lcdfTrn$loan_status)
#Check Training and Testing size
nrow(lcdfTrn)
nrow(lcdfTst)


################################ gbm : tuning parameters ##################################################

library(gbm)

#forloop gbm
paramGrid<-expand.grid(
  numtree= c(1000,1500),
  treeDepth= c(5,10),
  bagFraction= c(1),
  shrinkage = c(0.01),
  bestTree= 0,
  minRMSE= 0 
)
paramGrid

for(i in 1:nrow(paramGrid)) {
  gbm_paramTune <- gbm(formula=loan_status~., data=lcdfTrn, distribution = "bernoulli",cv.folds=4,
                       n.trees= paramGrid$numtree[i], 
                       interaction.depth= paramGrid$treeDepth[i],
                       bag.fraction= paramGrid$bagFraction[i],
                       shrinkage = paramGrid$shrinkage[i],
                       train.fraction= 0.7,
                       n.cores=NULL ) #use all available cores
  #add best tree and its RMSE to paramGrid
  paramGrid$bestTree[i] <-which.min(gbm_paramTune$valid.error)
  paramGrid$minRMSE[i] <-sqrt(min(gbm_paramTune$valid.error))
}
paramGrid

######### From parameter tuning we get minRMSE = 1.496046 for n.trees=1500, shrinkage =0.01 and tree depth = 10

set.seed(1789)
gbm_MM_b <- gbm(loan_status~., 
              data = lcdfTrn, distribution = "bernoulli", 
              n.trees=1500, shrinkage = 0.01,
              interaction.depth=10, bag.fraction=1,
              cv.folds=5, n.cores=16)

print(gbm_MM_b)
summary(gbm_MM_b, cbars=TRUE)

bestIter_b <- gbm.perf(gbm_MM_b, method='cv')    
print(bestIter_b) 

library(ROCR)
#Performance -ROC -lcdfTrn
scores_gbmTrn <- predict(gbm_MM_b, newdata=lcdfTrn, n.tree= bestIter_b, type="response")
head(scores_gbmTrn)
class(scores_gbmTrn)
pred_gbmTrn = prediction(scores_gbmTrn, lcdfTrn$loan_status, label.ordering= c(0, 1))
# label.ordering: The default ordering (cf.details) of the classes can be changed by 
# supplying a vector containing the negative and the positive class label.
# In our case 0(Charged Off) is negative and 1(Fully Paid) is positive
pred_gbmTrn
aucPerf_gbmTrn <- performance(pred_gbmTrn, "tpr", "fpr")
plot(aucPerf_gbmTrn)
aucPerf_gbmTrn <- performance(pred_gbmTrn, "auc")
print(aucPerf_gbmTrn)
aucPerf_gbmTrn@y.values
abline(a=0, b= 1)

#Performance -ROC -lcdfTst
scores_gbmTst<-predict(gbm_MM_b, newdata=lcdfTst, n.tree= bestIter_b, type="response")
scores_gbmTst
pred_gbmTst=prediction(scores_gbmTst, lcdfTst$loan_status, label.ordering= c(0, 1))
pred_gbmTst
aucPerf_gbmTst <-performance(pred_gbmTst, "tpr", "fpr")
plot(aucPerf_gbmTst)
aucPerf_gbmTst <-performance(pred_gbmTst, "auc")
print(aucPerf_gbmTst)
aucPerf_gbmTst@y.values
#abs
abline(a=0, b= 1)

################################ gbm : basic with 500 trees ##########################################################

set.seed(1789)
gbm_MM <- gbm(loan_status~., 
              data = lcdfTrn, distribution = "bernoulli", 
              n.trees=500, shrinkage = 0.01,
              interaction.depth=4, bag.fraction=0.5, 
              cv.folds=5, n.cores=16)

print(gbm_MM)
summary(gbm_MM, cbars=TRUE)

bestIter <- gbm.perf(gbm_MM, method='cv')    
print(bestIter) # bestIter means best iteration or n.tree = 492

library(ROCR)
#Performance -ROC -lcdfTrn
scores_gbmTrn <- predict(gbm_MM, newdata=lcdfTrn, n.tree= bestIter, type="response")
pred_gbmTrn = prediction(scores_gbmTrn, lcdfTrn$loan_status, label.ordering= c(0, 1))
aucPerf_gbmTrn <- performance(pred_gbmTrn, "tpr", "fpr")
plot(aucPerf_gbmTrn)
aucPerf_gbmTrn <- performance(pred_gbmTrn, "auc")
print(aucPerf_gbmTrn)
aucPerf_gbmTrn@y.values
#abs
abline(a=0, b= 1)

#Performance -ROC -lcdfTst
scores_gbmTst<-predict(gbm_MM, newdata=lcdfTst, n.tree= bestIter, type="response")
pred_gbmTst=prediction(scores_gbmTst, lcdfTst$loan_status, label.ordering= c(0, 1))
aucPerf_gbmTst <-performance(pred_gbmTst, "tpr", "fpr")
plot(aucPerf_gbmTst)
aucPerf_gbmTst <-performance(pred_gbmTst, "auc")
print(aucPerf_gbmTst)
aucPerf_gbmTst@y.values
#abs
abline(a=0, b= 1)

################################ xgboost : predicting status (use score for Q3) #####################################################
library(xgboost)
library(caret)

#str(lcdf)
#Re-order levels >> Levels: Fully Paid Charged Off
lcdf$loan_status <- factor(lcdf$loan_status, levels=c(1,0))
#unique(lcdf$loan_status)

#using one-hot encoding
#dummy-variables
fdum<-dummyVars(~.,data=lcdf%>% select(-loan_status))
dxlcdf<-predict(fdum, lcdf)

#levels(lcdf$loan_status)
fplcdf<-class2ind(lcdf$loan_status, drop2nd = TRUE)
#table(fplcdf) #level0 = 15341, level1 = 86385 

#Training, test subsets for xgboost (See trnIndex at the beginning)
dxlcdfTrn<-dxlcdf[trnIndex,]
fplcdfTrn<-fplcdf[trnIndex]
dxlcdfTst<-dxlcdf[-trnIndex,]
fplcdfTst<-fplcdf[-trnIndex]


dxTrn<-xgb.DMatrix(dxlcdfTrn, label=fplcdfTrn)
dxTst<-xgb.DMatrix(dxlcdfTst, label=fplcdfTst)


#xgbParamGrid
xgbParamGrid <- expand.grid(max_depth= c(1,3,5),
                            eta = c(0.1,0.01))

#Best Parameters
for(i in 1:nrow(xgbParamGrid)) {
  set.seed(1789)
  xgb_tune <- xgb.cv(data = dxTrn,objective= "binary:logistic", 
                     nrounds=100,
                     nfold = 5,
                     eta=xgbParamGrid$eta[i],
                     max_depth=xgbParamGrid$max_depth[i], 
                     early_stopping_rounds= 10)
  xgbParamGrid$bestTree[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$iter
  xgbParamGrid$bestPerf[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$test_error_mean
}

#view ParamGrid
xgbParamGrid


#watch list
xgbWatchlist<-list(train = dxTrn, eval= dxTst)
#list of parameters for the xgboost model
xgbParam<-list (
  max_depth= 5, eta = 0.01,
  objective = "binary:logistic",
  eval_metric="error", eval_metric= "auc")

#xgboost training/testing watchlist
set.seed(1789)
xgb_lsM1 <-xgb.train(xgbParam, dxTrn, nrounds= 500, xgbWatchlist, early_stopping_rounds= 10 )
#xgboost best iteration
xgb_lsM1$best_iteration #26

#Training predictions
xpredTrn <- predict(xgb_lsM1,dxTrn)
#Testing predictions
xpredTst <- predict(xgb_lsM1,dxTst)


#confusion matrix >0.5 = "Fully Paid" , <0.5 = "Charge Off"
table(pred=as.numeric(xpredTrg>0.5), act=fplcdfTrn)

#ROC, AUC performance
xpredTst<-predict(xgb_lsM1, dxTst)
pred_xgb_lsM1=prediction(xpredTst, lcdfTst$loan_status, label.ordering= c(0,1))
aucPerf_xgb_lsM1=performance(pred_xgb_lsM1, "tpr", "fpr")
plot(aucPerf_xgb_lsM1)
abline(a=0, b= 1)




################################ a2   cv.glmnet : Binomial  #######################################################
library(glmnet)
xD <- lcdfTrn %>% select(-loan_status) #47-lcdf$loan_status
yD <- lcdfTrn$loan_status
xDTst <- lcdfTst %>% select(-loan_status)
yDTst <- lcdfTst$loan_status

################################### cv.glmnet : alpha = 1 (Lasso) #######################################################
set.seed(1789)
cvglmnet_1 <- cv.glmnet(data.matrix(xD), yD, 
                        family = "binomial",
                        nfolds = 5,
                        alpha = 1)
plot(cvglmnet_1)

########## variable importance glmnet
library(vip)
tb1 <- vi_model(cvglmnet_1)
arrange(tb1,desc(Importance),Variable)
sort(tb1$Importance, decreasing = TRUE) %>% view()

library(caret)
#confusion_matrix for Trn
glm_M1_train_pred <- predict(cvglmnet_1, data.matrix(xD),s=cvglmnet_1$lambda.1se,type="class")
glm_M1_train_pred <- factor(glm_M1_train_pred, levels=c(1,0))
yD2 <- factor(yD, levels=c(1,0))                         
confusionMatrix (glm_M1_train_pred,yD2, positive="1")                           

#confusion_matrix for Tst
glm_M1_test_pred <- predict(cvglmnet_1, data.matrix(xDTst),s=cvglmnet_1$lambda.1se,type="class")
glm_M1_test_pred <- factor(glm_M1_test_pred, levels=c(1,0))
yDTst2 <- factor(yDTst, levels=c(1,0))                         
confusionMatrix (glm_M1_test_pred,yDTst2, positive="1")      



################################### cv.glmnet : alpha = 0.5 (elastic net) ########################################

set.seed(1789)
cvglmnet_2 <- cv.glmnet(data.matrix(xD), yD, 
                        family = "binomial",
                        nfolds = 5,
                        alpha = 0.5)

########## variable importance glmnet
library(vip)
tb1 <- vi_model(cvglmnet_2)
arrange(tb1,desc(Importance),Variable)
sort(tb1$Importance, decreasing = TRUE) %>% view()

library(caret)
#confusion_matrix for Trn
glm_M2_train_pred <- predict(cvglmnet_2, data.matrix(xD),s=cvglmnet_2$lambda.1se,type="class")
glm_M2_train_pred <- factor(glm_M2_train_pred, levels=c(1,0))
yD2 <- factor(yD, levels=c(1,0))                         
confusionMatrix (glm_M2_train_pred,yD2, positive="1")                           

#confusion_matrix for Tst
glm_M2_test_pred <- predict(cvglmnet_2, data.matrix(xDTst),s=cvglmnet_2$lambda.1se,type="class")
glm_M2_test_pred <- factor(glm_M2_test_pred, levels=c(1,0))
yDTst2 <- factor(yDTst, levels=c(1,0))                         
confusionMatrix (glm_M2_test_pred,yDTst2, positive="1")  


################################### cv.glmnet : alpha = 0.7 (elastic net) ######################################################

set.seed(1789)
cvglmnet_3 <- cv.glmnet(data.matrix(xD), yD, 
                        family = "binomial",
                        nfolds = 5,
                        alpha = 0.7)

########## variable importance glmnet
library(vip)
tb1 <- vi_model(cvglmnet_3)
arrange(tb1,desc(Importance),Variable)
sort(tb1$Importance, decreasing = TRUE) %>% view()

library(caret)
#confusion_matrix for Trn
glm_M3_train_pred <- predict(cvglmnet_3, data.matrix(xD),s=cvglmnet_3$lambda.1se,type="class")
glm_M3_train_pred <- factor(glm_M3_train_pred, levels=c(1,0))
yD2 <- factor(yD, levels=c(1,0))                         
confusionMatrix (glm_M3_train_pred,yD2, positive="1")                           

#confusion_matrix for Tst
glm_M3_test_pred <- predict(cvglmnet_3, data.matrix(xDTst),s=cvglmnet_3$lambda.1se,type="class")
glm_M3_test_pred <- factor(glm_M3_test_pred, levels=c(1,0))
yDTst2 <- factor(yDTst, levels=c(1,0))                         
confusionMatrix (glm_M3_test_pred,yDTst2, positive="1")  


################################### cv.glmnet : alpha = 0 (R) ######################################################

set.seed(1789)
cvglmnet_4 <- cv.glmnet(data.matrix(xD), yD, 
                        family = "binomial",
                        nfolds = 5,
                        alpha = 0)


########## variable importance glmnet
library(vip)
tb1 <- vi_model(cvglmnet_4)
arrange(tb1,desc(Importance),Variable)
sort(tb1$Importance, decreasing = TRUE) %>% view()

library(caret)
#confusion_matrix for Trn
glm_M4_train_pred <- predict(cvglmnet_4, data.matrix(xD),s=cvglmnet_4$lambda.1se,type="class")
glm_M4_train_pred <- factor(glm_M4_train_pred, levels=c(1,0))
yD2 <- factor(yD, levels=c(1,0))                         
confusionMatrix (glm_M4_train_pred,yD2, positive="1")                           

#confusion_matrix for Tst
glm_M4_test_pred <- predict(cvglmnet_4, data.matrix(xDTst),s=cvglmnet_4$lambda.1se,type="class")
glm_M4_test_pred <- factor(glm_M4_test_pred, levels=c(1,0))
yDTst2 <- factor(yDTst, levels=c(1,0))                         
confusionMatrix (glm_M4_test_pred,yDTst2, positive="1")  





############################### Random Forest with ranger for predicting loan status ##################################
library(ranger)

############################### Random Forest : Number of trees = 500 #########################
set.seed(1789)
rflcdf = ranger(loan_status ~ ., data = lcdfTrn, num.trees = 500, 
                importance='permutation', probability = TRUE)

#Variable Importance
importance(rflcdf) %>% sort() %>% view()

# ROC Performance
library(ROCR)
#########lcdfTrn random forest
pred_rflcdf = predict(rflcdf,lcdfTrn)$predictions

perfROC_rflcdf=performance(prediction(pred_rflcdf[,2], 
                                      lcdfTrn$loan_status,
                                      label.ordering=c(1,0)),"tpr", "fpr")

#plot ROC lcdfTrn 
plot(perfROC_rflcdf)
abline(a=0, b= 1)


#auc lcdfTrn 
aucPerf_rflcdf <- performance(prediction(pred_rflcdf[,2], 
                                         lcdfTrn$loan_status,
                                         label.ordering=c(1,0)), "auc")
print(aucPerf_rflcdf)
aucPerf_rflcdf@y.values #0.999846

#########lcdfTst random forest
pred_rflcdf_Tst = predict(rflcdf,lcdfTst)$predictions
perfROC_rflcdf_Tst=performance(prediction(pred_rflcdf_Tst[,2], 
                                          lcdfTst$loan_status,
                                          label.ordering=c(1,0)),"tpr", "fpr")

#plot ROC lcdfTst
plot(perfROC_rflcdf_Tst)
abline(a=0, b= 1)

#auc lcdfTst 
aucPerf_rflcdf <- performance(prediction(pred_rflcdf_Tst[,2], 
                                         lcdfTst$loan_status,
                                         label.ordering=c(1,0)), "auc")
print(aucPerf_rflcdf)
aucPerf_rflcdf@y.values #0.6567078


############################### Random Forest : Number of trees = 1000 #########################
set.seed(1789)
rflcdf_k = ranger(loan_status ~ ., data = lcdfTrn, num.trees = 1000, 
                importance='permutation', probability = TRUE)

#Variable Importance
importance(rflcdf_k) %>% sort() %>% view()

# ROC Performance
library(ROCR)
#########lcdfTrn random forest
pred_rflcdf = predict(rflcdf_k,lcdfTrn)$predictions
perfROC_rflcdf=performance(prediction(pred_rflcdf[,2], 
                                      lcdfTrn$loan_status,
                                      label.ordering=c(1,0)),"tpr", "fpr")

#plot ROC lcdfTrn 
plot(perfROC_rflcdf)
abline(a=0, b= 1)

#auc lcdfTrn 
aucPerf_rflcdf <- performance(prediction(pred_rflcdf[,2], 
                                         lcdfTrn$loan_status,
                                         label.ordering=c(1,0)), "auc")
print(aucPerf_rflcdf)
aucPerf_rflcdf@y.values #0.999846

#########lcdfTst random forest
pred_rflcdf_Tst = predict(rflcdf_k,lcdfTst)$predictions
pred_rflcdf_Tst
perfROC_rflcdf_Tst=performance(prediction(pred_rflcdf_Tst[,2], 
                                          lcdfTst$loan_status,
                                          label.ordering=c(1,0)),"tpr", "fpr")

#plot ROC lcdfTst
plot(perfROC_rflcdf_Tst)
abline(a=0, b= 1)

#auc lcdfTst 
aucPerf_rflcdf <- performance(prediction(pred_rflcdf_Tst[,2], 
                                         lcdfTst$loan_status,
                                         label.ordering=c(1,0)), "auc")
print(aucPerf_rflcdf)
aucPerf_rflcdf@y.values #0.6567078

#############################################################################################################




########################################### Q 2 ###############################################################
############################## 2 best Actual return prediction #####################################################
#load data
lcdf <- lcdfx
str(lcdf) #rows = 101726, var = 19

################################ xgboost: Actual Return #####################################################
library(xgboost)
library(caret)


lcdf <- lcdfx
str(lcdf) #rows = 101726, var = 19
#Delete : annRet, actualTerm, total_pymnt,loan_status ( unnecessary x ) and actualReturn(y)
lcdf_act <- subset(lcdf, select=-c(annRet, actualTerm, total_pymnt,loan_status,actualReturn))

#using one-hot encoding
fdum<-dummyVars(~.,data=lcdf_act)
dxlcdf<-predict(fdum, lcdf_act) #Matrix for x (lcdf_act)
actlcdf <- lcdf$actualReturn #Matrix for y


#Training, test subsets for xgboost (See trnIndex at the beginning)
dxlcdfTrn <- dxlcdf[trnIndex,] #Trn-x
dxlcdfTst <- dxlcdf[-trnIndex,] #Tst-x
actlcdfTrn <- actlcdf[trnIndex] #Trn-y
actlcdfTst <- actlcdf[-trnIndex] #Tst-y
#eva_lcdfTrn <- eva_lcdf[trnIndex,] #Value for evaluation 
#eva_lcdfTst <- eva_lcdf[-trnIndex,] #Value for evaluation


#make data matrix
dxTrn<-xgb.DMatrix(dxlcdfTrn, label=actlcdfTrn)
dxTst<-xgb.DMatrix(dxlcdfTst, label=actlcdfTst)

#which hyper-parameters work best –experiment with a grid of parameter values

#xgbParamGrid
xgbParamGrid <- expand.grid(max_depth= c(2,5),
                            eta = c(0.1, 0.01,0.001))

#Best Parameters
for(i in 1:nrow(xgbParamGrid)) {
  set.seed(1789)
  xgb_tune <- xgb.cv(data = dxTrn,objective= "reg:squarederror", 
                     nrounds=500,
                     nfold = 5,
                     eta=xgbParamGrid$eta[i],
                     max_depth=xgbParamGrid$max_depth[i], 
                     early_stopping_rounds= 10)
  xgbParamGrid$bestTree[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$iter
  xgbParamGrid$bestPerf[i] <- xgb_tune$evaluation_log[xgb_tune$best_iteration]$test_rmse_mean
}

#view ParamGrid
xgbParamGrid

#Select min param
best_index_ParamGrid <- which.min(xgbParamGrid$bestPerf)
best_index_ParamGrid

# we get max_depth =2 , bestTree = 67, ,eta= 0.1
best_rounds = xgbParamGrid$bestTree[best_index_ParamGrid]     # bestTree =67  
best_max.depth = xgbParamGrid$max_depth[best_index_ParamGrid] #max_depth = 2
best_eta = xgbParamGrid$eta[best_index_ParamGrid]             #eta= 0.1

#xgboost Training
set.seed(1789)
xgb_Mr <- xgboost( data = dxTrn, 
                   nrounds=best_rounds, 
                  max.depth=best_max.depth , 
                  eta=best_eta, 
                  objective="reg:squarederror")




#variable importance
xgb.importance(model=xgb_Mr) %>% view()

#evaluation Training
predXgbRet_Trn <- eva_lcdfTrn %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(predXgbRet=predict(xgb_Mr,dxTrn))

head(predXgbRet_Trn)
nrow(predXgbRet_Trn)

################## Finding RMSE for Train dataset ##########
diff_Xgb <- predXgbRet_Trn$actualReturn-predXgbRet_Trn$predXgbRet
#head(diff_Xgb)
sum_rmse_Xgb <- sum(diff_Xgb^2)
rmse_Xgb_act <- sqrt(sum_rmse/nrow(predXgbRet_Trn))
rmse_Xgb_act

predXgbRet_Trn <- predXgbRet_Trn %>% mutate(tile=ntile(-predXgbRet, 10))
predXgbRet_Trn %>% group_by(tile) %>% summarise(count=n(), 
                                               avgPredRet=mean(predXgbRet), 
                                               numDefaults=sum(loan_status=="Charged Off"), 
                                               avgActRet=mean(actualReturn), 
                                               minRet=min(actualReturn), 
                                               maxRet=max(actualReturn), 
                                               avgTer=mean(actualTerm), 
                                               totA=sum(grade=="A"), 
                                               totB=sum(grade=="B" ), 
                                               totC=sum(grade=="C"), 
                                               totD=sum(grade=="D"), 
                                               totE=sum(grade=="E"), 
                                               totF=sum(grade=="F") )


#evaluation Testing
predXgbRet_Tst <- eva_lcdfTst %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(predXgbRet=predict(xgb_Mr,dxTst))

head(predXgbRet_Tst)
nrow(predXgbRet_Tst)

################## Finding RMSE for Test dataset ##########
diff_Xgb <- predXgbRet_Tst$actualReturn-predXgbRet_Tst$predXgbRet
#head(diff_Xgb)
sum_rmse_Xgb <- sum(diff_Xgb^2)
rmse_Xgb_act <- sqrt(sum_rmse/nrow(predXgbRet_Tst))
rmse_Xgb_act

predXgbRet_Tst <- predXgbRet_Tst %>% mutate(tile=ntile(-predXgbRet, 10))
predXgbRet_Tst %>% group_by(tile) %>% summarise(count=n(), 
                                                avgPredRet=mean(predXgbRet), 
                                                numDefaults=sum(loan_status=="Charged Off"), 
                                                avgActRet=mean(actualReturn), 
                                                minRet=min(actualReturn), 
                                                maxRet=max(actualReturn), 
                                                avgTer=mean(actualTerm), 
                                                totA=sum(grade=="A"), 
                                                totB=sum(grade=="B" ), 
                                                totC=sum(grade=="C"), 
                                                totD=sum(grade=="D"), 
                                                totE=sum(grade=="E"), 
                                                totF=sum(grade=="F") )


################################ cv.glmnet : Actual Return ##############################################################################
library(glmnet)

#make sure >> not including loan_status for actualReturn modeling
xD <- eva_lcdfTrn %>% select(-actualReturn, -annRet, -actualTerm, -total_pymnt,-loan_status ) #var = 14
yD <- eva_lcdfTrn$actualReturn
xDTst <- eva_lcdfTst %>% select(-actualReturn, -annRet, -actualTerm, -total_pymnt, -loan_status) #var = 14
yDTst <- eva_lcdfTst$actualReturn

#cv.glmnet(x, y, type.measure = "mse", nfolds = 20)
paramGrid<-expand.grid(
  alpha= c(0,0.1,0.5,1),
  #nlambda = c(0.005352511),
  minRMSE= 0
)
paramGrid


for(i in 1:nrow(paramGrid)) {
  set.seed(1789)
  cvglmnet_tune <- cv.glmnet(data.matrix(xD), yD, 
                             family = "gaussian",
                             nfolds = 5,
                             alpha = paramGrid$alpha[i]
  )
  # RMSE to paramGrid
  paramGrid$minRMSE[i] <-min(cvglmnet_tune$cvm)
}
paramGrid


#Select best param (minimun cvm)
best_index_ParamGrid <- which.min(paramGrid$minRMSE)
best_index_ParamGrid # gives index of the minRMSE
best_alpha = paramGrid$alpha[best_index_ParamGrid]  # alpha =1

#cv.glmnet on training : alpha = 1
set.seed(1789)
cvglmnet_1 <- cv.glmnet(data.matrix(xD), yD, 
                        family = "gaussian",
                        nfolds = 5,
                        alpha = best_alpha)

#cv.glmnet on training : alpha = 1
set.seed(1789)
cvglmnet_1 <- cv.glmnet(data.matrix(xD), yD, 
                        family = "gaussian",
                        nfolds = 5,
                        alpha = 1)


plot(cvglmnet_1)




##################### Predicting on the Train dataset
pred_glm_x <- predict(cvglmnet_1,data.matrix(xD))
                   
pred_glm_Ret <- eva_lcdfTrn %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(pred_glm_x=predict(cvglmnet_1,data.matrix(xD)))

pred_glm_x <- predict(cvglmnet_1,data.matrix(xD))  # use it for expected return later
pred_glm_xTst <- predict(cvglmnet_1,data.matrix(xDTst))  # use it for expected return later


head(pred_glm_Ret$pred_glm_x)
#mean(pred_glm_Ret$pred_glm_x)

nrow(pred_glm_Ret)
pred_glm_Ret

diff_glm <- pred_glm_Ret$actualReturn-pred_glm_Ret$pred_glm_x
#head(diff_glm)
sum_rmse <- sum(diff_glm^2)
rmse_glm_act <- sqrt(sum_rmse/nrow(pred_glm_Ret))
rmse_glm_act

         
pred_glm_Ret <- pred_glm_Ret %>% mutate(tile=ntile(-pred_glm_x, 10))
pred_glm_Ret %>% group_by(tile) %>% summarise(count=n(), 
                                              avgPredRet=mean(pred_glm_x), 
                                              numDefaults=sum(loan_status=="Charged Off"), 
                                              avgActRet=mean(actualReturn), 
                                              minRet=min(actualReturn), 
                                              maxRet=max(actualReturn), 
                                              avgTer=mean(actualTerm), 
                                              totA=sum(grade=="A"), 
                                              totB=sum(grade=="B" ), 
                                              totC=sum(grade=="C"), 
                                              totD=sum(grade=="D"), 
                                              totE=sum(grade=="E"), 
                                              totF=sum(grade=="F") )

d=1

pRet_d<-pred_glm_Ret%>% filter(tile<=d)
pRet_d<-pRet_d%>% mutate(tile2=ntile(-pred_glm_x, 7121))

pRet_d %>% group_by(tile2) %>% summarise(count=n(), 
                                         avgPredRet=mean(pred_glm_x), 
                                         numDefaults=sum(loan_status=="Charged Off"), 
                                         avgActRet=mean(actualReturn),
                                         minRet=min(actualReturn), 
                                         maxRet=max(actualReturn), 
                                         avgTer=mean(actualTerm), 
                                         totA=sum(grade=="A"), 
                                         totB=sum(grade=="B" ), 
                                         totC=sum(grade=="C"), 
                                         totD=sum(grade=="D"), 
                                         totE=sum(grade=="E"))




#glmnet_test_pred <- predict(cvglmnet_1, data.matrix(xDTst)) #29518 rows

pred_glm_Ret_Tst <- eva_lcdfTst %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(pred_glm_xt=predict(cvglmnet_1,data.matrix(xDTst)))

################# Finding the RMSE for test dataset
diff_glm <- pred_glm_Ret_Tst$actualReturn-pred_glm_Ret_Tst$pred_glm_xt
#head(diff_glm)
sum_rmse <- sum(diff_glm^2)
rmse_glm_act_Tst <- sqrt(sum_rmse/nrow(pred_glm_Ret_Tst))
rmse_glm_act_Tst

pred_glm_Ret_Tst <- pred_glm_Ret_Tst %>% mutate(tile=ntile(-pred_glm_xt, 10))
pred_glm_Ret_Tst %>% group_by(tile) %>% summarise(count=n(), 
                                              avgPredRet=mean(pred_glm_xt), 
                                              numDefaults=sum(loan_status=="Charged Off"), 
                                              avgActRet=mean(actualReturn),
                                              minRet=min(actualReturn), 
                                              maxRet=max(actualReturn), 
                                              avgTer=mean(actualTerm), 
                                              totA=sum(grade=="A"), 
                                              totB=sum(grade=="B" ), 
                                              totC=sum(grade=="C"), 
                                              totD=sum(grade=="D"), 
                                              totE=sum(grade=="E"), 
                                              totF=sum(grade=="F") )


################################ Random Forest : Actual Return #########################

library(ranger)
str(eva_lcdfTrn)
lcdfTrn_rf <- eva_lcdfTrn %>% select(-loan_status,-actualTerm,-annRet,-total_pymnt)
lcdfTst_rf <- eva_lcdfTst %>% select(-loan_status,-actualTerm,-annRet,-total_pymnt)
str(lcdfTst_rf)


#Random Forest
paramGrid_rf<-expand.grid(
  mtry = c(3,5),
  num.trees = c(100,200), 
  minRMSE= 0
)
paramGrid_rf


for(i in 1:nrow(paramGrid_rf)) {
  set.seed(1789)
  rf_model = ranger(actualReturn ~ .,
                    data = lcdfTrn_rf,
                    mtry = paramGrid_rf$mtry[i],
                    num.trees = paramGrid_rf$num.trees[i],
                    importance ='impurity')
  paramGrid_rf$minRMSE[i] <- rf_model$prediction.error
}
paramGrid_rf


#Select min param
best_index_ParamGrid_rf <- which.min(paramGrid_rf$minRMSE)
best_index_ParamGrid_rf 


best_mtry = paramGrid_rf$mtry[best_index_ParamGrid_rf]
best_num.trees = paramGrid_rf$num.trees[best_index_ParamGrid_rf]


set.seed(1789)
rf_act = ranger(actualReturn ~ .,
                data = lcdfTrn_rf,
                mtry = best_mtry,
                num.trees = best_num.trees,
                importance ='impurity')


set.seed(1789)
rf_act = ranger(actualReturn ~ .,
                data = lcdfTrn_rf,
                mtry = 3,
                num.trees = 200,
                importance ='permutation')

rf_act$prediction.error

eva_lcdfTrn

pred_rf_Ret_Trn

#predict
pred_rf_Trn <- predict(rf_act,lcdfTrn_rf)
head(pred_rf_Trn)
pred_rf_Trn$predictions
head(pred_rf_Trn$predictions)


#avg_RMSE_act_fr = mean(pred_rf_Ret_Trn$rmse)
#RF Table for actualReturn (Train Dataset)
pred_rf_Ret_Trn <- eva_lcdfTrn %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(predicted_Ret = pred_rf_Trn$predictions)

################# Finding the RMSE for train dataset
diff_rf <- pred_rf_Ret_Trn$actualReturn-pred_rf_Ret_Trn$predicted_Ret
#head(diff_glm)
sum_rmse <- sum(diff_rf^2)
rmse_rf_act_Trn <- sqrt(sum_rmse/nrow(pred_rf_Ret_Trn))
rmse_rf_act_Trn

pred_rf_Ret_Trn <- pred_rf_Ret_Trn %>% mutate(tile=ntile(-predicted_Ret, 10))
pred_rf_Ret_Trn %>% group_by(tile) %>% summarise(count=n(), 
                                             avgPredRet=mean(predicted_Ret),
                                             numDefaults=sum(loan_status=="Charged Off"),
                                             avgActRet=mean(actualReturn),
                                             minRet=min(actualReturn), 
                                             maxRet=max(actualReturn), 
                                             avgTer=mean(actualTerm), 
                                             totA=sum(grade=="A"), 
                                             totB=sum(grade=="B" ), 
                                             totC=sum(grade=="C"), 
                                             totD=sum(grade=="D"), 
                                             totE=sum(grade=="E"), 
                                             totF=sum(grade=="F") )


#predict
pred_rf_Tst <- predict(rf_act,lcdfTst_rf)
head(pred_rf_Tst)
pred_rf_Tst$predictions
act_pred_compare <- pred_rf_Tst$predictions
head(pred_rf_Tst$predictions)

#RF Table for actualReturn (Test Dataset)
pred_rf_Ret_Tst <- eva_lcdfTst %>% select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(predicted_Ret_Tst = pred_rf_Tst$predictions)

################# Finding the RMSE for test dataset
diff_rf <- pred_rf_Ret_Tst$actualReturn-pred_rf_Ret_Tst$predicted_Ret_Tst
#head(diff_glm)
sum_rmse <- sum(diff_rf^2)
rmse_rf_act_Tst <- sqrt(sum_rmse/nrow(pred_rf_Ret_Tst))
rmse_rf_act_Tst


pred_rf_Ret_Tst <- pred_rf_Ret_Tst %>% mutate(tile=ntile(-predicted_Ret_Tst, 10))
pred_rf_Ret_Tst %>% group_by(tile) %>% summarise(count=n(), 
                                              avgPredRet=mean(predicted_Ret_Tst), 
                                              numDefaults=sum(loan_status=="Charged Off"), 
                                              avgActRet=mean(actualReturn),
                                              minRet=min(actualReturn), 
                                              maxRet=max(actualReturn), 
                                              avgTer=mean(actualTerm), 
                                              totA=sum(grade=="A"), 
                                              totB=sum(grade=="B" ), 
                                              totC=sum(grade=="C"), 
                                              totD=sum(grade=="D"), 
                                              totE=sum(grade=="E"), 
                                              totF=sum(grade=="F") )


############################## Q 3 ########################################################
library(dplyr)
################# Calling Data from Q1,Q2 ############################
#############Training##############
scores_gbmTrn <- predict(gbm_MM_b, newdata=lcdfTrn, n.tree= bestIter_b, type="response")


exp1 <- eva_lcdfTst

# Expected Return for Single model (Random Forest)
expected_single <- exp1 %>% mutate(EXP_rf = pred_rflcdf_Tst[,1] * act_pred_compare*loan_amnt - (1-pred_rflcdf_Tst[,1])*loan_amnt)
head(expected_single, 10)
mean(expected_single$EXP_rf)
sd(expected_single$EXP_r)

# Expected Return for combined model (GBM and random forest)
exp_combined <- exp1 %>% mutate(EXP_comb = scores_gbmTst*act_pred_compare*loan_amnt - (1-scores_gbmTst)*loan_amnt)
head(exp_combined)
mean(exp_combined$EXP_comb)
sd(exp_combined$EXP_comb)


#######################

class(scores_gbmTrn)
min(scores_gbmTrn)  #0.06381286
mean(scores_gbmTrn) #0.5024717
max(scores_gbmTrn)  #0.9471832


#glm
ann_Ret_best_glm = as.data.frame(pred_glm_x)
ann_Ret_best_glm <- ann_Ret_best_glm %>% rename( Prd_annRet_glm = "1")
head(ann_Ret_best_glm) 
#annRet from glmnet
min(pred_glm_x)     #0.0410057
mean(pred_glm_x)    #0.0464153
max(pred_glm_x)     #0.0580652
#not sure range pretty narrow, prediction nearly to mean, idt this works 

#rf
ann_Ret_best_rf = pred_rf_Trn$predictions
#class(pred_rf_Trn$predictions) #numeric
#annRet from rf
min(pred_rf_Trn$predictions)     #-0.1709431
mean(pred_rf_Trn$predictions)    #0.0464174
max(pred_rf_Trn$predictions)     #0.0586562
#if taking look on RFmodel range is closer to actualReturn (-0.17,0.06) 

#eva_lcdfTrn$actualReturn (-0.33,0.06)  
min(eva_lcdfTrn$actualReturn)     #-0.333333
mean(eva_lcdfTrn$actualReturn)    #0.0464174
max(eva_lcdfTrn$actualReturn)     #0.0586562


#############Testing#########################
#status score from gbm
class(scores_gbmTst)
min(scores_gbmTst)  #0.1088219
mean(scores_gbmTst) #0.5444628
max(scores_gbmTst)  #0.9595765

#annRet from glmnet
ann_Ret_best_glm_Tst = as.data.frame(pred_glm_xTst)
ann_Ret_best_glm_Tst <- ann_Ret_best_glm_Tst %>% rename( Prd_annRet_glm_tst = "1")

min(pred_glm_xTst)     #0.0410057
mean(pred_glm_xTst)    #0.0464174
max(pred_glm_xTst)     #0.0586562

#rf
ann_Ret_best_rf_Tst = pred_rf_Tst$predictions
#class(pred_rf_Trn$predictions) #numeric
#annRet from rf
min(pred_rf_Tst$predictions)     #-0.09297261
mean(pred_rf_Tst$predictions)    #0.04388332
max(pred_rf_Tst$predictions)     #0.1293119

#eva_lcdfTst$actualReturn (-0.33,0.37)  
min(eva_lcdfTst$actualReturn)     #-0.333333
mean(eva_lcdfTst$actualReturn)    #0.0479215
max(eva_lcdfTst$actualReturn)     #0.367361

################# Ending Retriev Data ############################




#Daframe exp_Ret Training
Table_exp_Ret <- eva_lcdfTrn %>%
  select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(Prd_loan_status = scores_gbmTrn, 
         Prd_annRet_glm = (ann_Ret_best_glm %>% select(Prd_annRet_glm)),
         Prd_annRet_rf = ann_Ret_best_rf,
         exp_Ret_glm = Prd_loan_status*Prd_annRet_glm,
         exp_Ret_rf = Prd_loan_status*Prd_annRet_rf)


head(Table_exp_Ret,30)

names(Table_exp_Ret)
#Re column 
Table_exp_Ret2 <- Table_exp_Ret[,c("grade","loan_status","actualTerm","actualReturn",
                    "Prd_annRet_rf",
                    "Prd_annRet_glm",
                      "int_rate",
                    "Prd_loan_status",
                    "exp_Ret_rf"
                    ,"exp_Ret_glm"
                    )]

#descending order (high-low)
Table_exp_Ret2 <- Table_exp_Ret2[order(-Table_exp_Ret2$exp_Ret_rf),]
#call head30
head(Table_exp_Ret2,30)

#groupby grade Training
Table_exp_Ret2 %>% group_by(grade) %>% summarise(count=n(), 
                                                 
                                                 numDefaults=sum(loan_status=="Charged Off"),
                                                 ActDefaultRate= sum(loan_status=="Charged Off")/n(),
                                                 #avgPredRet=mean(Prd_loan_status), 
                                                 PredDefaultRate = mean(Prd_loan_status),
                                                 avgActRet=mean(actualReturn),
                                                 avgPredRet_rf = mean(Prd_annRet_rf),
                                                 #minRet=min(actualReturn), 
                                                 #maxRet=max(actualReturn), 
                                                 #avgTer=mean(actualTerm),
)


#Daframe exp_Ret Testing
Table_exp_Ret_Tst <- eva_lcdfTst %>%
  select(grade, loan_status, actualReturn, actualTerm, int_rate) %>%
  mutate(Prd_loan_status = scores_gbmTst, 
         #Prd_annRet_glm = (ann_Ret_best_glm_Tst %>% select(Prd_annRet_glm_tst)),
         Prd_annRet_rf = ann_Ret_best_rf_Tst,
         #exp_Ret_glm = Prd_loan_status*Prd_annRet_glm,
         exp_Ret_rf = Prd_loan_status*Prd_annRet_rf,)


names(Table_exp_Ret_Tst)
# "grade","loan_status","actualReturn","actualTerm",
# "int_rate","Prd_loan_status","Prd_annRet_rf","exp_Ret_rf" 
Table_exp_Ret2_Tst <- Table_exp_Ret_Tst[,c("grade","loan_status","actualTerm","actualReturn",
                                   "Prd_annRet_rf",
                                   #"Prd_annRet_glm",
                                   "int_rate",
                                   "Prd_loan_status",
                                   "exp_Ret_rf"
                                   #,"exp_Ret_glm"
)]
head(Table_exp_Ret2,20)


##########################################################################

####################################### Q 4 ##############################

str(lcdfx)
#lcdf <- lcdf %>% mutate_if(is.character, as.factor)

lcdf_lowgrade <- lcdfx
lcdf_lowgrade <- lcdf_lowgrade %>% mutate_if(is.character, as.factor)

# Deleting rows of Grade A
tid <- which(lcdf_lowgrade$grade == "A")  # getting index where grade A
tid
lcdf_lowgrade[tid,]
lcdf_lowgrade <- lcdf_lowgrade[-c(tid), ]      # deleting rows of that index
head(lcdf_lowgrade)

# Similarly deleting rows of grade B
tid2 <- which(lcdf_lowgrade$grade == "B")
#tid2
#lcdf_lowgrade[tid2,]
lcdf_lowgrade <- lcdf_lowgrade[-c(tid2), ]      # deleting rows of that index
head(lcdf_lowgrade, 100)

lcdf_lowgrade %>% group_by(grade) %>% 
  summarise(nLoans=n(),
            defaults=sum(loan_status=="Charged Off"), 
            defaultRate = defaults/nLoans*100, avgInterst = mean(int_rate), 
            avgLoanAmt = mean(loan_amnt), avgRet = mean(annRet), avgActualRet = mean(actualReturn)*100, 
            avgActualTerm=mean(actualTerm),minActualRet=min(actualReturn)*100, maxActualRet=max(actualReturn)*100) %>% view()

# Grade F has a high default rate of 41.96078 % and average actual returns are also less compared to Grades C,D,E and G
# Therefore, we can remove F grade

tid3 <- which(lcdf_lowgrade$grade == "F")
#tid2
#lcdf_lowgrade[tid2,]
lcdf_lowgrade <- lcdf_lowgrade[-c(tid3), ]      # deleting rows of that index
head(lcdf_lowgrade, 100)

head(lcdf_lowgrade$actualReturn[lcdf_lowgrade$actualReturn<0])

nr=nrow(lcdf_lowgrade)
TRG_PCT=0.7
set.seed(1789)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) 
lcdfTrn_LG = lcdf_lowgrade[trnIndex,] #var = 20  
lcdfTst_LG = lcdf_lowgrade[-trnIndex,] #var = 20

str(lcdfTrn_LG)
str(lcdfTst_LG)
#eva_lcdfTrn <- lcdfTrn
#eva_lcdfTst <- lcdfTst


####### continuing Q 4 low grade gbm : Dataset for gbm,glm ##############################################

lcdfTrn_LG <- subset(lcdfTrn_LG, select = -c(annRet,actualTerm,actualReturn, total_pymnt)) #var = 15
lcdfTst_LG <- subset(lcdfTst_LG, select = -c(annRet,actualTerm,actualReturn, total_pymnt)) #var = 15
#lcdf <- subset(lcdf, select = -c(annRet,actualTerm,actualReturn, total_pymnt)) #var = 15

#“Bernoulli” distribution requires {0, 1} values for dependent variable
#lcdf$loan_status <- factor(lcdf$loan_status, levels=c(1, 0))
#lcdf$loan_status <- ifelse(lcdf$loan_status=="Charged Off", 0, 1)
#str(lcdf$loan_status) 
lcdfTrn_LG$loan_status <- ifelse(lcdfTrn_LG$loan_status=="Charged Off", 0, 1)
lcdfTst_LG$loan_status <- ifelse(lcdfTst_LG$loan_status=="Charged Off", 0, 1)
str(lcdfTrn_LG) #15
str(lcdfTst_LG) #15

#balance
library(ROSE)
set.seed(1789)
lcdfTrn_LG <- ovun.sample(loan_status~.,data=lcdfTrn_LG,method="both", p=0.5,N=71208,seed=1789)$data
table(lcdfTrn_LG$loan_status)
#Check Training and Testing size
nrow(lcdfTrn_LG)
nrow(lcdfTst_LG)

set.seed(1789)
gbm_MM_LG <- gbm(loan_status~., 
                 data = lcdfTrn_LG, distribution = "bernoulli", 
                 n.trees=1500, shrinkage = 0.01,
                 interaction.depth=10, bag.fraction=1,
                 cv.folds=5, n.cores=16)

print(gbm_MM_LG)
summary(gbm_MM_LG, cbars=TRUE)

bestIter_b <- gbm.perf(gbm_MM_LG, method='cv')    
print(bestIter_b) 

library(ROCR)
#Performance -ROC -lcdfTrn
scores_gbmTrn_LG <- predict(gbm_MM_b, newdata=lcdfTrn_LG, n.tree= bestIter_b, type="response")
head(scores_gbmTrn_LG)
class(scores_gbmTrn_LG)
pred_gbmTrn_LG = prediction(scores_gbmTrn_LG, lcdfTrn_LG$loan_status, label.ordering= c(0, 1))
pred_gbmTrn_LG
aucPerf_gbmTrn_LG <- performance(pred_gbmTrn_LG, "tpr", "fpr")
plot(aucPerf_gbmTrn_LG)
aucPerf_gbmTrn_LG <- performance(pred_gbmTrn_LG, "auc")
print(aucPerf_gbmTrn_LG)
aucPerf_gbmTrn_LG@y.values
abline(a=0, b= 1)

#Performance -ROC -lcdfTst
scores_gbmTst_LG<-predict(gbm_MM_LG, newdata=lcdfTst_LG, n.tree= bestIter_b, type="response")
pred_gbmTst_LG=prediction(scores_gbmTst_LG, lcdfTst_LG$loan_status, label.ordering= c(0, 1))
aucPerf_gbmTst_LG <-performance(pred_gbmTst_LG, "tpr", "fpr")
plot(aucPerf_gbmTst_LG)
aucPerf_gbmTst_LG <-performance(pred_gbmTst_LG, "auc")
print(aucPerf_gbmTst_LG)
aucPerf_gbmTst_LG@y.values
#abs
abline(a=0, b= 1)


#groupby grade Testing
Table_exp_Ret2_Tst %>% group_by(grade) %>% summarise(count=n(), 
                                                     
                                                     numDefaults=sum(loan_status=="Charged Off"),
                                                     ActDefaultRate= sum(loan_status=="Charged Off")/n(),
                                                     PredDefaultRate = mean(Prd_loan_status),
                                                     avgPredRet=mean(Prd_loan_status),
                                                     avgActRet=mean(actualReturn),
                                                     avgPredRet_rf = mean(Prd_annRet_rf),
                                                     #minRet=min(actualReturn), 
                                                     #maxRet=max(actualReturn), 
                                                     #avgTer=mean(actualTerm),
)



eva_lcdfTst %>% 
  summarise(nLoans=n(), 
            avgActRet=sum(actualReturn*loan_amnt)/sum(loan_amnt),
            avgTer=mean(actualTerm)) %>% 
  mutate(weighted_int_rate = (int_rate*loan_amnt)/sum(loan_amnt)) 


head(Table_exp_Ret_Tst)
overall <- Table_exp_Ret_Tst %>% mutate(tile=ntile(-exp_Ret_rf, 10))

#10decile
overall %>% group_by(tile) %>% summarise(count=n(), 
                                         avgPredRet_rf=mean(Prd_annRet_rf), 
                                         numDefaults=sum(loan_status=="Charged Off"), 
                                         avgActRet=mean(actualReturn),
                                         avgTer=mean(actualTerm), 
                                         totA=sum(grade=="A"), 
                                         totB=sum(grade=="B" ), 
                                         totC=sum(grade=="C"), 
                                         totD=sum(grade=="D"), 
                                         totE=sum(grade=="E"), 
                                         totF=sum(grade=="F") )
overall2 <- overall %>% mutate(eva_lcdfTst %>% select(loan_amnt)) 
overall2 %>% summarise(count=n(), sum_loan_amnt = sum(loan_amnt),
                       avgPredRet_rf=mean(Prd_annRet_rf), 
                       avgActRet=mean(actualReturn),
                       avgTer=mean(actualTerm))
head(overall2)


################################# END #####################################################