#set working directory ------------------------------------
setwd("D:/MSBA-Program/Fall-2020/IDS 575 Machine Learning Statistics/FinalProject")

library(dplyr)
library(caret)
library(parallel)
library(doSNOW)


#read the dataset
d <- read.csv("diabetic_data.csv")

#take a look at the data
head(d)

# functions used in this project --------------------------------------- 

#function for calculating confusion matrix and performance metric statistics
cf_matrix <- function(actual_vec, pred_prob_vec, positive_val,
                      cut_prob = 0.5, search_cut = FALSE) {
  if (search_cut == FALSE) {
    actual <- actual_vec == positive_val; pred <- pred_prob_vec >= cut_prob
    P <- sum(actual); N <- length(actual) - P; TP <- sum(actual & pred)
    FN <- P - TP; TN <- sum(!(actual) & !(pred)); FP <- N - TN
    if (TP != 0) { Precision <- TP/(TP + FP); Recall <- TP/(TP + FN)
    F1 <- 2*((Precision*Recall)/(Precision + Recall))}
    if(TP == 0) { Precision = 0; Recall = 0; F1 = 0 }
    1
    model_results <- list(confusion_matrix =
                            data.frame(metric = c("Correct", "Misclassified", "True Positive",
                                                  "True Negative","False Negative", "False Positive"),
                                       observations = c(TN + TP, FN + FP, TP, TN, FN, FP),
                                       rate = c((TN + TP)/(N + P), (FN + FP)/(N + P), TP/P, TN/N, FN/P, FP/N),
                                       pct_total_obs = c((TN + TP), (FN + FP), TP, TN, FN, FP)*(1/(N + P)),
                                       stringsAsFactors = FALSE),
                          F1_summary =
                            data.frame(metric = c("Precision", "Recall", "F1 Score"),
                                       value = c(Precision, Recall, F1),
                                       stringsAsFactors = FALSE))
    return(model_results) }
  if (search_cut == TRUE) {
    options("scipen"=100, "digits"=4)
    optimal_cut = data.frame(cut_prob = seq(0,1, by = 0.0025),
                             correct_rate = NA, F1_score = NA,
                             false_pos_rate = NA, false_neg_rate = NA)
    for (row in (1:nrow(optimal_cut))) {
      actual <- actual_vec == positive_val
      pred <- pred_prob_vec >= optimal_cut$cut_prob[row]
      P <- sum(actual); N <- length(actual) - P
      TP <- sum(actual & pred); FN <- P - TP
      TN <- sum(!(actual) & !(pred)); FP <- N - TN
      if (TP != 0) { Precision <- TP/(TP + FP); Recall <- TP/(TP + FN)
      F1 <- 2*((Precision*Recall)/(Precision + Recall))}
      if(TP == 0) { Precision = 0; Recall = 0; F1 = 0 }
      optimal_cut[row, 2:5] <- c((TN + TP)/(N + P), F1, FP/N, FN/P)
    }
    return(optimal_cut)
  }
}

#function calculate AUC
test_roc <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$readmitted, 
                 predict(model, data, type = "prob")[, "No"],
                 levels = c("Yes", "No"))
  ci(roc_obj)
}

#function set seed
reset.seed <- function(){
  # ensure results are repeatable
  set.seed(42)
}
# unregister cores
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister()
# Data Preprocessing ------------------------------------------------------

#readmitted - Only consider one that < 30 days
d$readmitted[d$readmitted==">30"] <- "NO"
d$readmitted[d$readmitted=="NO"] <- "0"
d$readmitted[d$readmitted=="<30"] <- "1"
table(d$readmitted)

#identify duplicate patients
length(d$patient_nbr)
length(unique(d$patient_nbr))

# Remove later visit of duplicated patients
d <- d[!duplicated(d$patient_nbr), ]


# find columns with one value
mylist <- mapply(table, d)
mylist


#droping columns with one value
#"metformin.rosiglitazone","glimepiride.pioglitazone" too few values of one of the classes
onevaluecolumns <- c("citoglipton","examide","glimepiride.pioglitazone")
d <- d %>% select(-onevaluecolumns)




#investigate gender
d$gender %>% unique()
table(d$gender)
#drop 3 rows with Unknown/Invalid gender
d <- d[!(d$gender=="Unknown/Invalid"), ]




#admission_type_id
table(d$admission_type_id)
hist(d$admission_type_id)

#admission type 5 is not available, 6 is null, and 8 is not mapped
#d <- d[!d$admission_type_id%in%c(5,6,8),]
#rename 1 as Emergency
d$admission_type_id[d$admission_type_id%in%c(1)] <- "Emergency"
#rename 2 as Urgent
d$admission_type_id[d$admission_type_id%in%c(2)] <- "Urgent"
#rename 3 as Elective
d$admission_type_id[d$admission_type_id%in%c(3)] <- "Elective"
#group unknown categories as UNKNOWN
d$admission_type_id[d$admission_type_id%in%c(5,6,8)] <- "NotAvailable"
#grouping 4 and 7 to urgent
d$admission_type_id[d$admission_type_id%in%c(4,7)] <- "Urgent"



#discharge_disposition_id
table(d$discharge_disposition_id)


#Removing rows of patient with discharge related to death or hospice - 11,13,14,19,20,21
unique(d$discharge_disposition_id)
d <- d[!d$discharge_disposition_id%in%c(11,13,14,19,20,21),]
table(d$discharge_disposition_id)
#removing rows of still patient categories - 9 and 15
d <- d[!d$discharge_disposition_id%in%c(9,15),]
#drop 7 because of leaving against medical advice
d <- d[!d$discharge_disposition_id%in%c(7),]
#grouping home categories
d$discharge_disposition_id[d$discharge_disposition_id%in%c(1,6,8,12,16,17)] <- "Home"
#grouping discharge to another hospital
d$discharge_disposition_id[d$discharge_disposition_id%in%c(2,5,23,30,27,28,29)] <- "AnotherHospitals"
#grouping discharge to rehab
d$discharge_disposition_id[d$discharge_disposition_id%in%c(3,4,10,22,24)] <- "RehabFacilities"
#discharge disposition 18 is null, 25 is not mapped, 26 is unknown/invalid
d$discharge_disposition_id[d$discharge_disposition_id%in%c(18,25,26)] <- "NotAvailable"


#admission_source_id
table(d$admission_source_id)

#admission_source_id 17 is Null, 20 is Not mapped, 21 is unknown/invalid
#9 and 15 is not available
#d <- d[!d$admission_source_id%in%c(9,15,17,20,21),]
d$admission_source_id[d$admission_source_id%in%c(9,15,17,20,21,8,11,13,14)] <- "Others/NotAvailable"

#admission from home&other facilities other another hospitals
d$admission_source_id[d$admission_source_id%in%c(1,2,3)] <- "Referral"

#admission from another hospital
d$admission_source_id[d$admission_source_id%in%c(4,5,6,10,18,19,22,25)] <- "AnotherHospitals"

#admission from emergency room
d$admission_source_id[d$admission_source_id%in%c(7)] <- "EmergencyRoom"


#replace ? with NA
d[d == "?"] <- NA

#investigate NA columns
names(d)[colMeans(is.na(d))>0]

#
names(d)[colMeans(is.na(d))>.5]
# weight has many missing values, convert it to available/notavailable.
d$weight[!is.na(d$weight)] <- "Available"
d$weight[is.na(d$weight)] <- "NotAvailable"
unique(d$weight)

#find missing columns with proportion > than 30%
names(d)[colMeans(is.na(d))>.3]
d <- d %>% select(-payer_code)

#find missing columns with proportion > than 0%
names(d)[colMeans(is.na(d))>0]

#race
unique(d$race)
table(d$race)
# elimate nas rows
d <- d[!is.na(d$race),]

#diag_1(Primary Diagnosis)
unique(d$diag_1) %>% sort()
table(d$diag_1)
d$diag_1 <- gsub("\\..*","",d$diag_1)
sum(is.na(d$diag_1))
d <- d[!is.na(d$diag_1),]
d$diag_1[d$diag_1%in%c(390:459,785)] <- "Circulatory"
d$diag_1[d$diag_1%in%c(460:519,786)] <- "Respiratory"
d$diag_1[d$diag_1%in%c(520:579,787)] <- "Digestive"
d$diag_1[d$diag_1%in%c(250)] <- "Diabetes"
d$diag_1[d$diag_1%in%c(800:999)] <- "Injury"
d$diag_1[d$diag_1%in%c(710:739)] <- "Musculoskeletal"
d$diag_1[d$diag_1%in%c(580:629,788)] <- "Genitourinary"
d$diag_1[d$diag_1%in%c(140:239)] <- "Neoplasms"
d$diag_1[d$diag_1%in%c(780,781,784,783,789,790:799,240:279,
                       680:709,782,001:139,290:319,280:289,
                       320:359,630:679,360:389,740:759)] <- "Other"
d$diag_1[substr(d$diag_1,1,1)%in%c("E","V")] <- "Other"
d <- d %>% subset(select = -c(diag_2,diag_3))


#droping encounter_id
d <- d[,-1:-2]

#
a <- data.frame(table(d$medical_specialty))#/length(d$medical_specialty)
a %>% arrange(desc(Freq)) %>% head(20)

#surgery, internal,family,emergency,cardiology, orthopedics
#medical_specialty is relevant, therefore, impute NotAvailable
sum(is.na(d$medical_specialty))
d$medical_specialty[is.na(d$medical_specialty)] <- "NotAvailable"
d$medical_specialty[d$medical_specialty%in%c("PhysicianNotFound")] <- "NotAvailable"
d$medical_specialty[d$medical_specialty%in%c("Cardiology","Cardiology-Pediatric")] <- "Cardiology"
d$medical_specialty[d$medical_specialty%in%c("Surgeon","Surgery-Cardiovascular","Surgery-Cardiovascular/Thoracic",
                                             "Surgery-Colon&Rectal","Surgery-General","Surgery-Maxillofacial","Surgery-Neuro",
                                             "Surgery-Pediatric","Surgery-Plastic","Surgery-PlasticwithinHeadandNeck","Surgery-Thoracic",
                                             "Surgery-Vascular","SurgicalSpecialty")] <- "Surgery"
d$medical_specialty[d$medical_specialty%in%c("Orthopedics","Orthopedics-Reconstructive")] <- "Orthopedics"
d$medical_specialty[d$medical_specialty%in%c("Obsterics&Gynecology-GynecologicOnco","Obstetrics","ObstetricsandGynecology")] <- "Obstetrics"
d$medical_specialty[d$medical_specialty%in%c("Pediatrics","Pediatrics-CriticalCare","Pediatrics-EmergencyMedicine","Pediatrics-Endocrinology",
                                             "Pediatrics-Hematology-Oncology", "Pediatrics-Neurology","Pediatrics-Pulmonology")] <- "Pediatrics"
d$medical_specialty[d$medical_specialty%in%c("Psychiatry","Psychiatry-Addictive","Psychiatry-Child/Adolescent")] <- "Psychiatry"
d$medical_specialty[d$medical_specialty%in%c("Radiologist","Radiology")] <- "Radiology"
d$medical_specialty[d$medical_specialty%in%c("Anesthesiology","Anesthesiology-Pediatric")] <- "Anesthesiology"
d$medical_specialty[d$medical_specialty%in%c("Resident","Family/GeneralPractice")] <- "Family/GeneralPractice"
data.frame(table(d$medical_specialty)) %>% arrange(desc(Freq))
# Group smaller groups into "others"
`%notin%` <- Negate(`%in%`)
d$medical_specialty[d$medical_specialty%notin%c("NotAvailable","InternalMedicine","","Family/GeneralPractice","Emergency/Trauma","Cardiology",
                                                "Surgery","Orthopedics")] <- "Others"
data.frame(table(d$medical_specialty)) %>% arrange(desc(Freq))


#max_glu_serum & A1Cresult
table(d$max_glu_serum)
table(d$A1Cresult)

d$max_glu_serum[d$max_glu_serum%in%c(">300")] <- 300
d$max_glu_serum[d$max_glu_serum%in%c(">200")] <- 200
d$max_glu_serum[d$max_glu_serum%in%c("Norm")] <- 100
d$max_glu_serum[d$max_glu_serum%in%c("None")] <- 0
d$max_glu_serum <- as.numeric(d$max_glu_serum)


d$A1Cresult[d$A1Cresult%in%c(">8")] <- 8
d$A1Cresult[d$A1Cresult%in%c(">7")] <- 7
d$A1Cresult[d$A1Cresult%in%c("Norm")] <- 5
d$A1Cresult[d$A1Cresult%in%c("None")] <- 0
d$A1Cresult <- as.numeric(d$A1Cresult)

#age
d$age
d$age <-  gsub("\\[", "",d$age) 
d$age <-gsub("\\)", "",d$age)
table(d$age)
d$age %>% unique()

d$age[d$age%in%c("0-10")] <- 5
d$age[d$age%in%c("10-20")] <- 15
d$age[d$age%in%c("20-30")] <- 25
d$age[d$age%in%c("30-40")] <- 35
d$age[d$age%in%c("40-50")] <- 45
d$age[d$age%in%c("50-60")] <- 55
d$age[d$age%in%c("60-70")] <- 65
d$age[d$age%in%c("70-80")] <- 75
d$age[d$age%in%c("80-90")] <- 85
d$age[d$age%in%c("90-100")] <- 95
d$age <- as.numeric(d$age)

#drop medications that are not common
d <- d %>% subset(select = -c(metformin.pioglitazone,metformin.rosiglitazone,glipizide.metformin,
                              tolazamide,troglitazone,miglitol,tolbutamide,acetohexamide,chlorpropamide,glyburide.metformin,acarbose,
                              nateglinide,repaglinide))


# to binary
d$change[d$change%in%c("No")] <- 0
d$change[d$change%in%c("Ch")] <- 1
d$change <- as.numeric(d$change)

# to binary
d$diabetesMed[d$diabetesMed%in%c("No")] <- 0
d$diabetesMed[d$diabetesMed%in%c("Yes")] <- 1
d$diabetesMed <- as.numeric(d$diabetesMed)

# to binary
d$gender[d$gender%in%c("Female")] <- 0
d$gender[d$gender%in%c("Male")] <- 1
d$gender <- as.numeric(d$gender)
str(d)

# to binary
d$readmitted[d$readmitted%in%c("0")] <- "No"
d$readmitted[d$readmitted%in%c("1")] <- "Yes"
d$readmitted <- factor(d$readmitted,levels = c("Yes","No"))
str(d)


#get datatype right
d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)], as.factor)

#data

set.seed(42)
nr<-nrow(d)
trnIndex<- sample(1:nr, size = round(0.7*nr), replace=FALSE)
dTrn <- d[trnIndex, ]
dTst <- d[-trnIndex, ]
table(dTrn$readmitted)


#variable selection
n_features <- length(setdiff(names(dTrn), "readmitted"))

library(ranger)
set.seed(42)
rf1 <- ranger(
  readmitted ~ ., 
  data = dTrn,
  num.trees=n_features * 10,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  importance = "permutation",
  probability = TRUE
)
a <- data.frame(rf1$variable.importance)
variable <- rownames(a)
importance <- a[,1]
a <- data.frame(variable,importance)
VARIMPplot <- ggplot(a, aes(x=reorder(variable,importance), y=importance))+ 
  geom_point(stat="identity")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")
VARIMPplot
a %>% filter(variable=="number_inpatient")
b <- a %>% subset(importance>=0.002084463)
toString(b$variable)


#age, admission_type_id, discharge_disposition_id,admission_source_id,medical_specialty,insulin,change,time_in_hospital,
#num_procedures, num_lab_procedures, num_medications, number_inpatient, number_diagnoses
str(d,list.len=ncol(d))
toString(colnames(d))
IMPVAR <- c("age", "admission_type_id", "discharge_disposition_id", "admission_source_id", 
            "time_in_hospital", "medical_specialty", "num_lab_procedures", "num_procedures", 
            "num_medications", "number_inpatient", "diag_1",
            "number_diagnoses")


d <- d %>% select(IMPVAR,readmitted)



#one hot encoding
str(d)
dim(d)

readmitted <- d$readmitted
dummy <- dummyVars(" ~.", data=d %>% subset(select=-readmitted))
d <- data.frame(predict(dummy, 
                        newdata = d))
d <- data.frame(d,readmitted)

str(d)
d <- d %>% subset(select=-c(admission_type_id.Emergency,discharge_disposition_id.Home,
                            admission_source_id.EmergencyRoom,medical_specialty.NotAvailable,diag_1.Circulatory))


#spliting
set.seed(42)
nr<-nrow(d)
trnIndex<- sample(1:nr, size = round(0.7*nr), replace=FALSE)
dTrn <- d[trnIndex, ]
dTst <- d[-trnIndex, ]
table(dTrn$readmitted)



# Modeling ------------------------------------------------------

#knn ---------------
library(kknn)
reset.seed()
#k = 7, distance = 2
knn_based <- kknn(readmitted~.,
                  train = dTrn,
                  test = dTst)
knn_based$response

baselineconf <-  confusionMatrix(knn_based$fitted.values,dTst$readmitted,positive = "Yes")
baselineconf$byClass
auc <- test_roc(knn_based,dTst) %>% as.vector()
auc


knn_tuneGrid <- expand.grid(kmax = 50,
                        distance = 2,
                        kernel = c('gaussian',
                                   'triangular',
                                   'rectangular',
                                   'optimal'))

#parallel processing
library(parallel)
library(doSNOW)

numberofcores = detectCores()
cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)
reset.seed()
knnFit <- train(readmitted~.,
                data = dTrn, 
                method = "kknn",
                metric="ROC",
                trControl = trainControl(method="cv",
                                         number = 10,
                                         verboseIter = T, 
                                         classProbs = TRUE, 
                                         summaryFunction = twoClassSummary,
                                         savePredictions = T), 
                preProcess = c("YeoJohnson","center","scale"),
                tuneGrid = knn_tuneGrid)
stopCluster(cl)

knnFit$finalModel

knnFit_pred_train <- predict(knnFit, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = knnFit_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)

knnFit_pred_test <- predict(knnFit, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = knnFit_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.175)


#rose
cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)
reset.seed()
knnFit_rose <- train(readmitted~.,
                     data = dTrn, 
                     method = "kknn",
                     metric="ROC",
                     trControl = trainControl(method="cv",
                                              number = 10,
                                              verboseIter = T, 
                                              classProbs = TRUE, 
                                              summaryFunction = twoClassSummary,
                                              savePredictions = T,
                                              sampling = "rose"), 
                     preProcess = c("YeoJohnson","center","scale"),
                     tuneGrid = knn_tuneGrid)

knnFit_rose$bestTune
stopCluster(cl)


knnFit_pred_train <- predict(knnFit_rose, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = knnFit_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)

knnFit_rose_pred_test <- predict(knnFit_rose, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = knnFit_rose_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.45)


#down
cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)
reset.seed()
knnFit_down <- train(readmitted~.,
                     data = dTrn, 
                     method = "kknn",
                     metric="ROC",
                     trControl = trainControl(method="cv",
                                              number = 10,
                                              verboseIter = T, 
                                              classProbs = TRUE, 
                                              summaryFunction = twoClassSummary,
                                              savePredictions = T,
                                              sampling = "down"), 
                     preProcess = c("YeoJohnson","center","scale"),
                     tuneGrid = knn_tuneGrid)
knnFit_down$bestTune
stopCluster(cl)


knnFit_pred_train <- predict(knnFit_down, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = knnFit_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)

knnFit_down_pred_test <- predict(knnFit_down, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = knnFit_down_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.5)

#knn models
knnmodels <- list(original = knnFit,
               ROSE = knnFit_rose,
               down = knnFit_down)

resampling <- resamples(knnmodels)
summary(resampling, metric = "ROC")

test_roc <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$readmitted, 
                 predict(model, data, type = "prob")[, "No"],
                 levels = c("Yes", "No"))
  ci(roc_obj)
}

test <- lapply(knnmodels, test_roc, data = dTst)
test <- lapply(test, as.vector)
test <- do.call("rbind", test)
colnames(test) <- c("lower", "ROC", "upper")
test <- as.data.frame(test)
test


bwplot(resampling,metric="ROC",main="KNN Models")


# Naive Bayes -----------------------------------------------------

tuneGrid <- expand.grid(
  usekernel = c(TRUE),
  fL = 0:5,
  adjust = 1:5
)

cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)

reset.seed()
nbFit <- train(readmitted~.,
               data = dTrn, 
               method = "nb",
               metric="ROC",
               trControl = trainControl(method="cv",
                                        number = 10,
                                        verboseIter = T, 
                                        classProbs = TRUE, 
                                        summaryFunction = twoClassSummary,
                                        savePredictions = T), 
               preProcess = c("YeoJohnson","center","scale"),
               tuneGrid = tuneGrid)
stopCluster(cl)
nbFit$bestTune

nbFit_pred_train <- predict(nbFit, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = nbFit_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)

nbFit_pred_test <- predict(nbFit, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = nbFit_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.00000008)



#rose
tuneGridROSE <- expand.grid(
  usekernel = c(TRUE),
  fL = 8:12,
  adjust = 3:5
)
cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)

reset.seed()
nbFit_rose <- train(readmitted~.,
                    data = dTrn, 
                    method = "nb",
                    metric="ROC",
                    trControl = trainControl(method="cv",
                                             number = 10,
                                             verboseIter = T, 
                                             classProbs = TRUE, 
                                             summaryFunction = twoClassSummary,
                                             savePredictions = T,
                                             sampling = "rose"), 
                    preProcess = c("YeoJohnson","center","scale"),
                    tuneGrid = tuneGridROSE)
stopCluster(cl)

nbFit_rose$bestTune

nbFit_rose_pred_train <- predict(nbFit_rose, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = nbFit_rose_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)

nb_rose_pred_test <- predict(nbFit_rose, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = nb_rose_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.1)


#down
tuneGriddown <- expand.grid(
  usekernel = c(TRUE),
  fL = 2:7,
  adjust = 2:6
)
cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)

reset.seed()
nbFit_down <- train(readmitted~.,
                    data = dTrn, 
                    method = "nb",
                    metric="ROC",
                    trControl = trainControl(method="cv",
                                             number = 10,
                                             verboseIter = T, 
                                             classProbs = TRUE, 
                                             summaryFunction = twoClassSummary,
                                             savePredictions = T,
                                             sampling = "down"), 
                    preProcess = c("YeoJohnson","center","scale"),
                    tuneGrid = tuneGriddown)
stopCluster(cl)

nbFit_down$bestTune

nbFit_down_pred_train <- predict(nbFit_down, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = nbFit_down_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)

nb_down_pred_test <- predict(nbFit_down, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = nb_down_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.325)


#nb models
nbmodels <- list(original = nbFit,
                 ROSE = nbFit_rose,
                 down = nbFit_down)


resampling <- resamples(nbmodels)
summary(resampling, metric = "ROC")



test <- lapply(nbmodels, test_roc, data = dTst)
test <- lapply(test, as.vector)
test <- do.call("rbind", test)
colnames(test) <- c("lower", "ROC", "upper")
test <- as.data.frame(test)
test
bwplot(resampling,metric="ROC",main="Naive Bayes Models")


#logistic -------------------------------------------------------

cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)

reset.seed()
glmnet_fit <- train(readmitted~.,
                    data = dTrn,
                    family = "binomial",
                    metric="ROC",
                    tuneLength = 20,
                    trControl = trainControl(method="cv",
                                             number = 10,
                                             verboseIter = T, 
                                             classProbs = TRUE, 
                                             summaryFunction = twoClassSummary,
                                             savePredictions = T),
                    preProcess = c("YeoJohnson"),
                    method = "glmnet")
stopCluster(cl)


glmnet_fit_pred_train <- predict(glmnet_fit, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = glmnet_fit_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)

glmnet_fit_pred_test <- predict(glmnet_fit, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = glmnet_fit_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.08196)

#my_roc <- roc(dTrn$readmitted, glmnet_fit_pred_train$Yes)
#best.coords <- coords(my_roc, "best", ret = "threshold",transpose = TRUE, best.method="youden")


#rose
cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)

reset.seed()
glmnet_fit_rose <- train(readmitted~.,
                         data = dTrn,
                         family = "binomial",
                         metric="ROC",
                         tuneLength = 20,
                         trControl = trainControl(method="cv",
                                                  number = 10,
                                                  verboseIter = T, 
                                                  classProbs = TRUE, 
                                                  summaryFunction = twoClassSummary,
                                                  savePredictions = T,
                                                  #new option
                                                  sampling = "rose"),
                         preProcess = c("YeoJohnson"),
                         method = "glmnet")
stopCluster(cl)

glmnet_fit_rose$bestTune


glmnet_fit_rose_pred_train <- predict(glmnet_fit_rose, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = glmnet_fit_rose_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)

glmnet_fit_rose_pred_test <- predict(glmnet_fit_rose, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = glmnet_fit_rose_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.5)


#down
cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)


reset.seed()
glmnet_fit_down <- train(readmitted~.,
                         data = dTrn,
                         family = "binomial",
                         metric="ROC",
                         tuneLength = 20,
                         trControl = trainControl(method="cv",
                                                  number = 10,
                                                  verboseIter = T, 
                                                  classProbs = TRUE, 
                                                  summaryFunction = twoClassSummary,
                                                  savePredictions = T,
                                                  #new option
                                                  sampling = "down",
                                                  search = "random"),
                         preProcess = c("YeoJohnson"),
                         method = "glmnet")
stopCluster(cl)
glmnet_fit_down$bestTune



glmnet_fit_down_pred_train <- predict(glmnet_fit_down, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = glmnet_fit_down_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)
#options(max.print=1000000)
#my_roc <- roc(dTrn$readmitted, glmnet_fit_down_pred_train$Yes)
#best.coords <- coords(my_roc, "best", ret = "threshold",transpose = TRUE, best.method="youden")
glmnet_fit_down_pred_test <- predict(glmnet_fit_down, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = glmnet_fit_down_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.4927)


#glmnet
glm_models <- list(original = glmnet_fit,
                   down = glmnet_fit_down,
                   ROSE = glmnet_fit_rose)

glm_resampling <- resamples(glm_models)

glm_test <- lapply(glm_models, test_roc, data = dTst)
glm_test <- lapply(glm_test, as.vector)
glm_test <- do.call("rbind", glm_test)
colnames(glm_test) <- c("lower", "ROC", "upper")
glm_test <- as.data.frame(glm_test)
glm_test
glmresult <- summary(glm_resampling, metric = "ROC")
glmresult
bwplot(glm_resampling,main="GLMNET Models",metric="ROC")

#SVM -----------------------
#svmRadial
cl <- makeCluster(numberofcores, type = "SOCK")
registerDoSNOW(cl)
reset.seed()
svmFit_down_Radial <- train(readmitted~.,
                            data = dTrn, 
                            method = "svmRadial",
                            metric="ROC",
                            trControl = trainControl(method="cv",
                                                     number = 10,
                                                     verboseIter = T, 
                                                     classProbs = TRUE, 
                                                     summaryFunction = twoClassSummary,
                                                     savePredictions = T,
                                                     sampling = "down",
                                                     search = "random"), 
                            preProcess = c("YeoJohnson","center","scale"),
                            tuneLength = 20)
svmFit_down_Radial$bestTune
stopCluster(cl)
svmFit_down_Radial$pred
svmFit_down_Radial_pred_train <- predict(svmFit_down_Radial, dTrn,type="prob")
cf_matrix(actual_vec = dTrn$readmitted,
          pred_prob_vec = svmFit_down_Radial_pred_train$Yes,
          positive_val = "Yes",
          search_cut = TRUE)
svmFit_down_Radial_pred_test <- predict(svmFit_down_Radial, dTst,type="prob")
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = svmFit_down_Radial_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.5)
cf_matrix(actual_vec = dTst$readmitted,
          pred_prob_vec = svmFit_down_Radial_pred_test$Yes,
          positive_val = "Yes",
          cut_prob = 0.4650)
'''
library(pROC)

my_roc <- roc(dTrn$readmitted, svmFit_down_Radial_pred_train$Yes)
best.coords <- coords(my_roc, "best", ret = "threshold",transpose = TRUE, best.method="youden")
coords <- coords(my_roc, "all",transpose = TRUE)
#my_roc <- roc(dTst$readmitted, svmFit_down_Radial_pred_test$Yes)
#coords(my_roc, "best", ret = "threshold",transpose = TRUE, best.method="youden")

plot(coords["threshold",], coords["specificity",], type="l", 
     col="red", xlab="Cutoff", ylab="Performance")
lines(coords["threshold",], coords["sensitivity",], type="l", 
      col="blue")
legend(x = "bottomright", c("Specificity", "Sensitivity"),col=c("red", "blue"), lty=1)
abline(v=best.coords["threshold"], lty=2, col="grey")
abline(h=best.coords["specificity"], lty=2, col="red")
abline(h=best.coords["sensitivity"], lty=2, col="blue")

my_roc <- roc(dTrn$readmitted, glmnet_fit_down_pred_train$Yes)
my_roc <- roc(dTst$readmitted, glmnet_fit_down_pred_test$Yes)
coords(my_roc, "best", ret = "threshold",transpose = TRUE, best.method="closest.topleft")
'''
 #Evaluation--------------------------------

allmodels <- list(original_knn = knnFit,
                  ROSE_knn = knnFit_rose,
                  down_knn = knnFit_down,
                  original_nb = nbFit,
                  ROSE_nb = nbFit_rose,
                  down_nb = nbFit_down,
                  original_glmnet = glmnet_fit,
                  down_glmnet = glmnet_fit_down,
                  ROSE_glmnet = glmnet_fit_rose,
                  down_radialsvm = svmFit_down_Radial)

resampling <- resamples(allmodels)
ROCresult <- summary(resampling,metric = "ROC")
ROCresult
thresholder(mod, 
            threshold = seq(.5, 1, by = 0.05), 
            final = TRUE)Sensresult <- summary(resampling,metric = "Sens")
Sensresult

dotplot(resampling,metric="ROC",main="All Models",pch = 19)
bwplot(resampling,metric=c("Sens","Spec"),main="All Models",pch = 19)

allmodels_test <- lapply(allmodels, test_roc, data = dTst)
allmodels_test <- lapply(allmodels_test, as.vector)
allmodels_test <- do.call("rbind", allmodels_test)
colnames(allmodels_test) <- c("lower", "ROC", "upper")
allmodels_test <- as.data.frame(allmodels_test)
allmodels_test



