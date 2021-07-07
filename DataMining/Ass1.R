library(tidyverse)
library(lubridate)


#lcdf <- read.csv('D:/LEARNING STUDY/UIC STUDY/IDS 572/Assignment 1/lcData5m.csv')
lcdf <- read.csv('/Users/vanisa/Documents/Doc_MSBA_Fall2020/IDS572_DataMining/Assignment_1/lcData5m.csv')

str(lcdf)
lcdf <- lcdf %>%  filter(loan_status !="Current")
lcdf %>% group_by(loan_status, grade) %>% tally()

###################################2a#########################################################

###################################2a(i)#########################################################

#What is the proportion of defaults ('charged off' vs 'fully paid' loans) in the data?

lcdf %>% group_by(loan_status) %>% summarise(nLoans=n()) %>% mutate(loan_prop=nLoans/sum(nLoans)) %>% view()

#How does default rate vary with loan grade? 
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), prop_defaults=sum(loan_status=="Charged Off")/nLoans*100) %>% view


#Does it vary with sub-grade? 

lcdf %>% group_by(sub_grade) %>% 
  summarise(nLoans=n(), prop_defaults=sum(loan_status=="Charged Off")/nLoans*100) %>% view


#And is this what you would expect, and why?
#Yes.

#grade_factor <- factor(lcdf$grade, ordered = TRUE, levels = c("A","B", "C" , "D", "E","F", "G"))

###################################2aii#########################################################

#How many loans are there in each grade?
lcdf %>% group_by(grade) %>% tally()


#And do loan amounts vary by grade?
#Yes,the loans amounts vary by grade. The higher grade(A to G) has higher amount than lower grade.
lcdf %>% 
  group_by(grade) %>% 
  summarise(nLoans=n(),sum_loan_amt=sum(loan_amnt),avg_loan=mean(loan_amnt)) %>% view()

dat <- data.frame(table(lcdf$loan_status, lcdf$grade))
names(dat) <- c("LoanStatus","Grade", "count")
ggplot(data=dat, aes(x=Grade, y=count, fill=LoanStatus)) + geom_bar(stat="identity") + xlab("Loan Grade") + ylab("Total Loans") + labs(fill = "Loan Status")


#Does interest rate for loans vary with grade?
#Yes, the interest rate vary with grade. The higher grade(A to G) has lower interest rate than lower grade.
lcdf %>% 
  group_by(grade) %>% 
  summarise(mean(int_rate)) %>% view

dat <- data.frame(table(lcdf$loan_status, lcdf$sub_grade))
names(dat) <- c("LoanStatus","SubGrade", "Count")
ggplot(data=dat, aes(x=SubGrade, y=Count, fill=LoanStatus)) + geom_bar(stat="identity") + xlab("Loan Sub Grade") + ylab("Total Loans") + labs(fill = "Loan Status")

#Does interest rate for loans vary with sub_grade?
#Yes, the interest rate vary with sub-grade. 
#In each grade, the higher sub-grade(1 to 5) has lower interest rate than lower grade.
lcdf %>% 
  group_by(sub_grade) %>% 
  summarise(mean(int_rate)) %>% view()

#And is this what you expect, and why?
#Yes, the results correspond to what we expected because 
#the customers who get the high grade have lower default rate.
#therefore, they should be accessible to higher amount and lower loan interest rate.

###################################2aiii#########################################################

#What are people borrowing money for (purpose)? 
unique(lcdf$purpose)

#Examine how many loans, average amounts, etc. by purpose?
  lcdf %>% 
  group_by(purpose) %>% 
  summarise(n=n(),mean(loan_amnt)) #%>% view()

#And within grade? 
#Yes, different grades have different purpose for being default
unique(lcdf$loan_status)

lcdf %>% 
  group_by(grade,purpose) %>% 
  summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"),
            nondefaults=sum(loan_status!="Charged Off"),percent_defaults_bygrade=defaults/nLoans) %>% 
  arrange(grade,desc(percent_defaults_bygrade)) #%>% view()

#Do defaults vary by purpose?
lcdf %>% 
    group_by(purpose) %>% 
    summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"),percent_defaults=defaults/nLoans*100) %>% 
    arrange(percent_defaults) #%>% view()

###################################2aiv#########################################################

#Calculate the annual return. 
#lcdf$annRet <- (lcdf$total_pymnt -lcdf$funded_amnt)*(12/36)
#Show how you calculate the percentage annual return.
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100

#Compare the average return values with the average interest_rate on loans 
lcdf %>% 
  summarise(Overall_annual_return=sum(total_pymnt -funded_amnt)*(12/36),
            Overall_annual_percent = sum(total_pymnt -funded_amnt)/sum(funded_amnt)*(12/36)*100, 
            Average_annual_return = mean(total_pymnt -funded_amnt)*(12/36),
            mean(int_rate)) %>% view()

##########RUN#ME#####################################################################################
#Create: annRet, actualTerm, actualReturn (nCol=145+3)

#annRet
lcdf$annRet <- (lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt*(12/36)*100
lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")

#actualTerm
lcdf$actualTerm <- ifelse(lcdf$loan_status == "Fully Paid", as.duration(lcdf$issue_d %--%lcdf$last_pymnt_d)/dyears(1),3)

#actualReturn
lcdf$actualReturn <- ifelse(lcdf$actualTerm>0,((lcdf$total_pymnt-lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm),0)
dim(lcdf)
#Noted: nCol=145+3 = 148

###############################################################################################

lcdf %>% select(loan_status, loan_amnt, funded_amnt, total_pymnt, int_rate, actualTerm, actualReturn ) #%>% view()

lcdf %>% summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"), 
                                       defaultRate = defaults/nLoans*100, avgInterst = mean(int_rate), 
                                       avgLoanAmt = mean(loan_amnt), avgRet = mean(annRet), avgActualRet = mean(actualReturn)*100, 
                                       avgActualTerm=mean(actualTerm),minActualRet=min(actualReturn)*100, maxActualRet=max(actualReturn)*100) %>% view()



lcdf %>% group_by(grade) %>% summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"), 
                                        defaultRate = defaults/nLoans*100, avgInterst = mean(int_rate), 
                                        avgLoanAmt = mean(loan_amnt), avgRet = mean(annRet), avgActualRet = mean(actualReturn)*100, 
                                        avgActualTerm=mean(actualTerm),minActualRet=min(actualReturn)*100, maxActualRet=max(actualReturn)*100) %>% view()


lcdf %>% group_by(sub_grade) %>% summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"), 
                                       defaultRate = defaults/nLoans*100, avgInterst = mean(int_rate), 
                                       avgLoanAmt = mean(loan_amnt), avgRet = mean(annRet), avgActualRet = mean(actualReturn)*100, 
                                       avgActualTerm=mean(actualTerm),minActualRet=min(actualReturn)*100, maxActualRet=max(actualReturn)*100) %>% view()

#– do you notice any differences, and how do you explain this?
#How do returns vary by grade, and by sub-grade. 
#If you wanted to invest in loans based on this data exploration, 
#which loans would you invest in?

##################################################################################################


###################################2av#########################################################

#########RUN#ME################################################################################
#Generate some new derived attributes which you think may be useful for predicting default., 
#and explain what these are.

#Previously Create: annRet, actualTerm, actualReturn 
#New Column: ratio_annualinc_loanam,ratio_install_loanamt (nCol=148+2=150)
#annRet, actualTerm, actualReturn,ratio_annualinc_loanam,ratio_install_loanamt

#ratio_annualinc_loanam
lcdf$ratio_annualinc_loanamt <- lcdf$annual_inc/lcdf$loan_amnt
#default rate
lcdf %>% 
  group_by(loan_status) %>% 
  summarise(mean(ratio_annualinc_loanamt)) #%>% view()

#ratio_install_loanamt 
lcdf$ratio_install_loanamt <- lcdf$installment/lcdf$loan_amnt
#grade,status
lcdf %>% 
  group_by(grade,loan_status) %>% 
  summarise(mean(ratio_install_loanamt)) %>% filter(loan_status!="Current") #%>% view()

###################################2b#########################################################
#Are there missing values? 
#What is the proportion of missing values in different variables?
#Explain how you will handle missing values for different variables.
#How to impute the data? 
#1) Consider the type of data
#2) Find the stat summary to impute the data.. 

summary(lcdf$mths_since_last_major_derog)
#impute with statistical data 
#and what missing values may arise from 

#missing value proportions in each column
colMeans(is.na(lcdf)) #%>% view()

#Data with No NA
#check column with no #NA or full 0f data 
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))==0]
#list of column with no #NA
toString(names(lcdf)[colMeans(is.na(lcdf))==0])
#Data with Full of NA
#check column with all #NA
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))==1]
#list of column with all #NA
toString(names(lcdf)[colMeans(is.na(lcdf))==1])


#Delete the column with all #NA
nm_na<-names(lcdf)[colMeans(is.na(lcdf))==1]
lcdf <- lcdf %>% select(-nm_na)
#check the no. of col
ncol(lcdf) #99

#Remove variables which have more than 60% missing values
nm_60<-names(lcdf)[colMeans(is.na(lcdf))>0.6]
lcdf <- lcdf %>% select(-nm_60)
#check the no. of col
ncol(lcdf) #93

#columns where there are missing values
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]

#Replace the missing value
#mths_since_last_delinq: has 48% missings, these pertain to no delinquincy, 
#so replace by max value (176) or a value higher than the max (500) -- 
#we will try this out on a temporary dataset lcx with the attributes that have misisng values
lcdf<- lcdf %>% replace_na(list(mths_since_last_delinq = 500))
#For revol_util, suppose we want to replace the misisng values by the median
lcdf<- lcdf %>% replace_na(list(revol_util=median(lcdf$revol_util, na.rm=TRUE)))

#columns where there are missing values
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]
colMeans(is.na(lcdf))


###################################3#########################################################
#Consider the potential for data leakage. You do not want to include variables in your model 
#which may not be available when applying the model; that is, 
#some data may not be available for new loans before they are funded. 
#Leakage may also arise from variables in the data which may have been updated during the loan period 
#(ie., after the loan is funded). For example, 
#it has been noted that the FICO scores on loan applicants are updated periodically, 
#and the data can carry thus FICO scores from after the loan issue_date. 
#So, even though FICO score can be useful, the values in the data may not be usable. 
#Identify and explain which variables will you exclude from the model.


##data leakage##
#delete potential Leakage
lcdf <- subset(lcdf, select=-c(emp_title,emp_length,delinq_2yrs,inq_last_6mths,
                               open_acc,pub_rec,revol_bal,title, zip_code, 
                               addr_state,out_prncp, out_prncp, out_prncp_inv,recoveries, collection_recovery_fee,
                               total_acc,last_pymnt_d,last_pymnt_amnt,last_credit_pull_d,collections_12_mths_ex_med,
                               acc_now_delinq,tot_coll_amt,tot_cur_bal,total_rev_hi_lim,acc_open_past_24mths,
                               avg_cur_bal,bc_open_to_buy,bc_util,chargeoff_within_12_mths,
                               delinq_amnt,mort_acc,mths_since_recent_bc, mo_sin_old_il_acct,num_tl_120dpd_2m,
                               percent_bc_gt_75,mths_since_recent_inq,hardship_flag))
#check the no. of col
ncol(lcdf) #57
#columns where there are missing values
colMeans(is.na(lcdf)) 
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]

#Remove the single value variables
lcdf <- subset(lcdf, select=-c(pymnt_plan,policy_code,application_type))
#check the no. of col
ncol(lcdf) #54
str(lcdf)

###################################4#########################################################
#Develop decision tree models to predict default.
#(a) Split the data into training and validation sets. What proportions do you consider, why?

library(rpart)
#summarize the lcdf
summary(lcdf)

#created attribute 
#$ annRet                 : num  2.61 2.78 1.98 10.13 7.24 ...
#$ actualTerm             : num  1.76 3.09 1 3.09 3.09 ...
#$ actualReturn           : num  0.0445 0.0271 0.0594 0.0985 0.0704 ...
#$ ratio_annualinc_loanamt: num  14.86 11.88 4.99 3.33 3.57 ...
#$ ratio_install_loanamt  : num  0.0305 0.0301 0.0314 0.0363 0.0339 ...

ncol(lcdf)
 
lcdfx <- subset(lcdf, select=-c(annRet,actualTerm,actualReturn,ratio_annualinc_loanamt,ratio_install_loanamt))
ncol(lcdfx) #49
str(lcdfx)
# lcdfx is the backup dataframe

lcdf <- lcdfx

#Ensure factor
str(lcdf)
lcdf= lcdf %>% mutate_if(is.character, as.factor)
str(lcdf)
dim(lcdf)

#Splitting Training Data set and Testing Data Set
nr=nrow(lcdf)
TRG_PCT=0.7
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) 
lcdfTrn=lcdf[trnIndex,]   
lcdfTst = lcdf[-trnIndex,]

#Check Training and Testing size
nrow(lcdfTrn)
nrow(lcdfTst)

names(lcdfTrn)

#Model1 the decision tree (15 secs)
#rpModel1=rpart(loan_status ~ ., data=lcdfTrn, method="class")

#RPart minsplit = 30
lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", 
               parms = list(split = "information"), control = rpart.control(minsplit = 30))
print(lcDT1)
library(rpart.plot)
rpart.plot::prp(lcDT1, type=2, extra=1)

lcDT1$variable.importance
printcp(lcDT1)

#RPart minsplit = 50
lcDT2 <- rpart(loan_status ~., data=lcdfTrn, method="class", 
               parms = list(split = "information"), control = rpart.control(minsplit = 50))
print(lcDT2)
#plot the decision tree
library(rpart.plot)
rpart.plot::prp(lcDT2, type=2, extra=1)
summary(lcDT2)

lcDT3 <- rpart(loan_status ~., data=lcdfTrn, method="class", 
               parms = list(split = "information"), control = rpart.control(minsplit = 80))
print(lcDT3)





# Pruned tree
lcDT1p<- prune.rpart(lcDT1, cp=0.0003)  
print(lcDT1p)
library(rpart.plot)
rpart.plot::prp(lcDT1p, type=2, extra=1)

#Model2 > Select the best cp by observing the graph (1min)
#rpModel2 = rpart(loan_status ~ ., data=lcdfTrn, method="class", control = rpart.control(cp = 0.0))
#printcp(rpModel2)
#plotcp(rpModel2)

#Model2_pruned > Prune the tree ()
#rpModel2_pruned = prune(rpModel2, cp=0.0015)
#rpart.plot::prp(rpModel2_pruned, type=2, extra=1)

#Performance Evaluation Model1?
predTrn=predict(lcDT1,lcdfTrn, type='class')
table(pred = predTrn, true=lcdfTrn$loan_status)
mean(predTrn == lcdfTrn$loan_status)  #0.9856477

#str(lcdfTst)
#levels(droplevels(lcdfTst$title))

table(pred = predict(lcDT1,lcdfTst, type='class'), true=lcdfTst$loan_status)
mean(predict(lcDT1,lcdfTst, type='class') ==lcdfTst$loan_status)  # 0.9838784

###########################

########################


#Performance Evaluation with different classification threshold
#print(predProbTrn[, 'Charged Off'])

CTHRESH=0.60
predProbTrn=predict(lcDT1,lcdfTrn, type='prob')
predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
table(predTrnCT , true=lcdfTrn$loan_status)
# Or, to set the predTrnCT values as factors, and then get the confusion matrix
table(predictions=factor(predTrnCT, levels=c("Fully Paid", "Charged Off")), actuals=lcdfTrn$loan_status)

#precision_recall_curve()

library(lattice)
library(ggplot2)
library(caret)
confusionMatrix(predTrn, lcdfTrn$loan_status)
confusionMatrix(predTrn, lcdfTrn$loan_status, positive="Charged Off")

# Accuracy : 0.9885

#confusionMatrix(pred, lcdfTst$loan_status)

##############################################
# C5.0
library(C50)
str(lcdfTrn)

#costMat <- matrix(c(0,23,37,0), nrow=2)

C5_DT1 <- C5.0(loan_status ~., data=lcdfTrn, control=C5.0Control(),rules=TRUE)
print(C5_DT1)
C5imp(C5_DT1) %>% view()

predTrn = predict(C5_DT1,lcdfTrn)
table(predTrn, lcdfTrn$loan_status)
predTst = predict(C5_DT1,lcdfTst)
table(predTst, lcdfTrn$loan_status)

ncol(lcdfTrn)

c_tree <- C5.0(as.factor(lcdfTrn$loan_status) ~., data = lcdfTrn, 
               method = "class", trials = 3, control=C5.0Control(CF=0.45,earlyStopping =FALSE))
print(c_tree)

##################ROCR##################

library('ROCR')
score=predict(lcDT1,lcdfTst, type="prob")[,"Charged Off"]
pred=prediction(score, lcdfTst$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels   

#ROC curve
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)

#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values

#Lift curve
liftPerf <-performance(pred, "lift", "rpp")
plot(liftPerf)

################################################ Q 5 ###############################

##################### Random Forest

library(tidyverse)
library(lubridate)


lcdf <- read.csv('/Users/usmcc/OneDrive/Desktop/IDS572/lcData5m.csv')
str(lcdf)
lcdf <- lcdf %>%  filter(loan_status !="Current")
lcdf %>% group_by(loan_status, grade) %>% tally()

###################################2a#########################################################

###################################2a(i)#########################################################

#What is the proportion of defaults ('charged off' vs 'fully paid' loans) in the data?

lcdf %>% group_by(loan_status) %>% summarise(nLoans=n()) %>% mutate(loan_prop=nLoans/sum(nLoans)) %>% view()

#How does default rate vary with loan grade? 
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), prop_defaults=sum(loan_status=="Charged Off")/nLoans*100) %>% view


#Does it vary with sub-grade? 

lcdf %>% group_by(sub_grade) %>% 
  summarise(nLoans=n(), prop_defaults=sum(loan_status=="Charged Off")/nLoans*100) %>% view


#And is this what you would expect, and why?
#Yes.

#grade_factor <- factor(lcdf$grade, ordered = TRUE, levels = c("A","B", "C" , "D", "E","F", "G"))

###################################2aii#########################################################

#How many loans are there in each grade?
lcdf %>% group_by(grade) %>% tally()


#And do loan amounts vary by grade?
#Yes,the loans amounts vary by grade. The higher grade(A to G) has higher amount than lower grade.
lcdf %>% 
  group_by(grade) %>% 
  summarise(nLoans=n(),sum_loan_amt=sum(loan_amnt),avg_loan=mean(loan_amnt)) %>% view()

dat <- data.frame(table(lcdf$loan_status, lcdf$grade))
names(dat) <- c("LoanStatus","Grade", "count")
ggplot(data=dat, aes(x=Grade, y=count, fill=LoanStatus)) + geom_bar(stat="identity") + xlab("Loan Grade") + ylab("Total Loans") + labs(fill = "Loan Status")


#Does interest rate for loans vary with grade?
#Yes, the interest rate vary with grade. The higher grade(A to G) has lower interest rate than lower grade.
lcdf %>% 
  group_by(grade) %>% 
  summarise(mean(int_rate)) %>% view

dat <- data.frame(table(lcdf$loan_status, lcdf$sub_grade))
names(dat) <- c("LoanStatus","SubGrade", "Count")
ggplot(data=dat, aes(x=SubGrade, y=Count, fill=LoanStatus)) + geom_bar(stat="identity") + xlab("Loan Sub Grade") + ylab("Total Loans") + labs(fill = "Loan Status")

#Does interest rate for loans vary with sub_grade?
#Yes, the interest rate vary with sub-grade. 
#In each grade, the higher sub-grade(1 to 5) has lower interest rate than lower grade.
lcdf %>% 
  group_by(sub_grade) %>% 
  summarise(mean(int_rate)) %>% view()

#And is this what you expect, and why?
#Yes, the results correspond to what we expected because 
#the customers who get the high grade have lower default rate.
#therefore, they should be accessible to higher amount and lower loan interest rate.

###################################2aiii#########################################################

#What are people borrowing money for (purpose)? 
unique(lcdf$purpose)

#Examine how many loans, average amounts, etc. by purpose?
lcdf %>% 
  group_by(purpose) %>% 
  summarise(n=n(),mean(loan_amnt)) #%>% view()

#And within grade? 
#Yes, different grades have different purpose for being default
unique(lcdf$loan_status)

lcdf %>% 
  group_by(grade,purpose) %>% 
  summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"),
            nondefaults=sum(loan_status!="Charged Off"),percent_defaults_bygrade=defaults/nLoans) %>% 
  arrange(grade,desc(percent_defaults_bygrade)) #%>% view()

#Do defaults vary by purpose?
lcdf %>% 
  group_by(purpose) %>% 
  summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"),percent_defaults=defaults/nLoans*100) %>% 
  arrange(percent_defaults) #%>% view()

###################################2aiv#########################################################

#Calculate the annual return. 
#lcdf$annRet <- (lcdf$total_pymnt -lcdf$funded_amnt)*(12/36)
#Show how you calculate the percentage annual return.
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100

#Compare the average return values with the average interest_rate on loans 
lcdf %>% 
  summarise(Overall_annual_return=sum(total_pymnt -funded_amnt)*(12/36),
            Overall_annual_percent = sum(total_pymnt -funded_amnt)/sum(funded_amnt)*(12/36)*100, 
            Average_annual_return = mean(total_pymnt -funded_amnt)*(12/36),
            mean(int_rate)) %>% view()

##########RUN#ME#####################################################################################
#Create: annRet, actualTerm, actualReturn (nCol=145+3)

#annRet
lcdf$annRet <- (lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt*(12/36)*100
lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")

#actualTerm
lcdf$actualTerm <- ifelse(lcdf$loan_status == "Fully Paid", as.duration(lcdf$issue_d %--%lcdf$last_pymnt_d)/dyears(1),3)

#actualReturn
lcdf$actualReturn <- ifelse(lcdf$actualTerm>0,((lcdf$total_pymnt-lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm),0)
dim(lcdf)
#Noted: nCol=145+3 = 148

###############################################################################################

lcdf %>% select(loan_status, loan_amnt, funded_amnt, total_pymnt, int_rate, actualTerm, actualReturn ) #%>% view()

lcdf %>% summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"), 
                   defaultRate = defaults/nLoans*100, avgInterst = mean(int_rate), 
                   avgLoanAmt = mean(loan_amnt), avgRet = mean(annRet), avgActualRet = mean(actualReturn)*100, 
                   avgActualTerm=mean(actualTerm),minActualRet=min(actualReturn)*100, maxActualRet=max(actualReturn)*100) %>% view()



lcdf %>% group_by(grade) %>% summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"), 
                                       defaultRate = defaults/nLoans*100, avgInterst = mean(int_rate), 
                                       avgLoanAmt = mean(loan_amnt), avgRet = mean(annRet), avgActualRet = mean(actualReturn)*100, 
                                       avgActualTerm=mean(actualTerm),minActualRet=min(actualReturn)*100, maxActualRet=max(actualReturn)*100) %>% view()


lcdf %>% group_by(sub_grade) %>% summarise(nLoans=n(),defaults=sum(loan_status=="Charged Off"), 
                                           defaultRate = defaults/nLoans*100, avgInterst = mean(int_rate), 
                                           avgLoanAmt = mean(loan_amnt), avgRet = mean(annRet), avgActualRet = mean(actualReturn)*100, 
                                           avgActualTerm=mean(actualTerm),minActualRet=min(actualReturn)*100, maxActualRet=max(actualReturn)*100) %>% view()

#– do you notice any differences, and how do you explain this?
#How do returns vary by grade, and by sub-grade. 
#If you wanted to invest in loans based on this data exploration, 
#which loans would you invest in?

##################################################################################################


###################################2av#########################################################

#########RUN#ME################################################################################
#Generate some new derived attributes which you think may be useful for predicting default., 
#and explain what these are.

#Previously Create: annRet, actualTerm, actualReturn 
#New Column: ratio_annualinc_loanam,ratio_install_loanamt (nCol=148+2=150)
#annRet, actualTerm, actualReturn,ratio_annualinc_loanam,ratio_install_loanamt

#ratio_annualinc_loanam
lcdf$ratio_annualinc_loanamt <- lcdf$annual_inc/lcdf$loan_amnt
#default rate
lcdf %>% 
  group_by(loan_status) %>% 
  summarise(mean(ratio_annualinc_loanamt)) #%>% view()

#ratio_install_loanamt 
lcdf$ratio_install_loanamt <- lcdf$installment/lcdf$loan_amnt
#grade,status
lcdf %>% 
  group_by(grade,loan_status) %>% 
  summarise(mean(ratio_install_loanamt)) %>% filter(loan_status!="Current") #%>% view()

###################################2b#########################################################
#Are there missing values? 
#What is the proportion of missing values in different variables?
#Explain how you will handle missing values for different variables.
#How to impute the data? 
#1) Consider the type of data
#2) Find the stat summary to impute the data.. 

summary(lcdf$mths_since_last_major_derog)
#impute with statistical data 
#and what missing values may arise from 

#missing value proportions in each column
colMeans(is.na(lcdf)) #%>% view()

#Data with No NA
#check column with no #NA or full 0f data 
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))==0]
#list of column with no #NA
toString(names(lcdf)[colMeans(is.na(lcdf))==0])
#Data with Full of NA
#check column with all #NA
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))==1]
#list of column with all #NA
toString(names(lcdf)[colMeans(is.na(lcdf))==1])


#Delete the column with all #NA
nm_na<-names(lcdf)[colMeans(is.na(lcdf))==1]
lcdf <- lcdf %>% select(-nm_na)
#check the no. of col
ncol(lcdf) #99

#Remove variables which have more than 60% missing values
nm_60<-names(lcdf)[colMeans(is.na(lcdf))>0.6]
lcdf <- lcdf %>% select(-nm_60)
#check the no. of col
ncol(lcdf) #93

#columns where there are missing values
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]

#Replace the missing value
#mths_since_last_delinq: has 48% missings, these pertain to no delinquincy, 
#so replace by max value (176) or a value higher than the max (500) -- 
#we will try this out on a temporary dataset lcx with the attributes that have misisng values
lcdf<- lcdf %>% replace_na(list(mths_since_last_delinq = 500))
#For revol_util, suppose we want to replace the misisng values by the median
lcdf<- lcdf %>% replace_na(list(revol_util=median(lcdf$revol_util, na.rm=TRUE)))

#columns where there are missing values
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]
colMeans(is.na(lcdf))


###################################3#########################################################
#Consider the potential for data leakage. You do not want to include variables in your model 
#which may not be available when applying the model; that is, 
#some data may not be available for new loans before they are funded. 
#Leakage may also arise from variables in the data which may have been updated during the loan period 
#(ie., after the loan is funded). For example, 
#it has been noted that the FICO scores on loan applicants are updated periodically, 
#and the data can carry thus FICO scores from after the loan issue_date. 
#So, even though FICO score can be useful, the values in the data may not be usable. 
#Identify and explain which variables will you exclude from the model.


##data leakage##
#delete potential Leakage
lcdf <- subset(lcdf, select=-c(emp_title,emp_length,delinq_2yrs,inq_last_6mths,
                               open_acc,pub_rec,revol_bal,title, zip_code, 
                               addr_state,out_prncp, out_prncp, out_prncp_inv,recoveries, collection_recovery_fee,
                               total_acc,last_pymnt_d,last_pymnt_amnt,last_credit_pull_d,collections_12_mths_ex_med,
                               acc_now_delinq,tot_coll_amt,tot_cur_bal,total_rev_hi_lim,acc_open_past_24mths,
                               avg_cur_bal,bc_open_to_buy,bc_util,chargeoff_within_12_mths,
                               delinq_amnt,mort_acc,mths_since_recent_bc, mo_sin_old_il_acct,num_tl_120dpd_2m,
                               percent_bc_gt_75,mths_since_recent_inq,hardship_flag))
#check the no. of col
ncol(lcdf) #57
#columns where there are missing values
colMeans(is.na(lcdf)) 
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]

#Remove the single value variables
lcdf <- subset(lcdf, select=-c(pymnt_plan,policy_code,application_type))
#check the no. of col
ncol(lcdf) #54
str(lcdf)

###################################4#########################################################
#Develop decision tree models to predict default.
#(a) Split the data into training and validation sets. What proportions do you consider, why?

library(rpart)
#summarize the lcdf
summary(lcdf)

#created attribute 
#$ annRet                 : num  2.61 2.78 1.98 10.13 7.24 ...
#$ actualTerm             : num  1.76 3.09 1 3.09 3.09 ...
#$ actualReturn           : num  0.0445 0.0271 0.0594 0.0985 0.0704 ...
#$ ratio_annualinc_loanamt: num  14.86 11.88 4.99 3.33 3.57 ...
#$ ratio_install_loanamt  : num  0.0305 0.0301 0.0314 0.0363 0.0339 ...

ncol(lcdf)

lcdfx <- subset(lcdf, select=-c(annRet,actualTerm,actualReturn,ratio_annualinc_loanamt,ratio_install_loanamt))
ncol(lcdfx) #49
str(lcdfx)
# lcdfx is the backup dataframe

lcdf <- lcdfx

#Ensure factor
str(lcdf)
lcdf= lcdf %>% mutate_if(is.character, as.factor)
str(lcdf)
dim(lcdf)

#Splitting Training Data set and Testing Data Set
nr=nrow(lcdf)
TRG_PCT=0.7
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE) 
lcdfTrn=lcdf[trnIndex,]   
lcdfTst = lcdf[-trnIndex,]

#Check Training and Testing size
nrow(lcdfTrn)
nrow(lcdfTst)

names(lcdfTrn)

#Model1 the decision tree (15 secs)
#rpModel1=rpart(loan_status ~ ., data=lcdfTrn, method="class")

#RPart minsplit = 30
lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", 
               parms = list(split = "information"), control = rpart.control(minsplit = 30))
print(lcDT1)
library(rpart.plot)
rpart.plot::prp(lcDT1, type=2, extra=1)

lcDT1$variable.importance
printcp(lcDT1)

#RPart minsplit = 50
lcDT2 <- rpart(loan_status ~., data=lcdfTrn, method="class", 
               parms = list(split = "information"), control = rpart.control(minsplit = 50))
print(lcDT2)
#plot the decision tree
library(rpart.plot)
rpart.plot::prp(lcDT2, type=2, extra=1)
summary(lcDT2)

lcDT3 <- rpart(loan_status ~., data=lcdfTrn, method="class", 
               parms = list(split = "information"), control = rpart.control(minsplit = 80))
print(lcDT3)


# Pruned tree
lcDT1p<- prune.rpart(lcDT1, cp=0.0003)  
print(lcDT1p)
library(rpart.plot)
rpart.plot::prp(lcDT1p, type=2, extra=1)

#Model2 > Select the best cp by observing the graph (1min)
#rpModel2 = rpart(loan_status ~ ., data=lcdfTrn, method="class", control = rpart.control(cp = 0.0))
#printcp(rpModel2)
#plotcp(rpModel2)

#Model2_pruned > Prune the tree ()
#rpModel2_pruned = prune(rpModel2, cp=0.0015)
#rpart.plot::prp(rpModel2_pruned, type=2, extra=1)

#Performance Evaluation Model1?
predTrn=predict(lcDT1,lcdfTrn, type='class')
table(pred = predTrn, true=lcdfTrn$loan_status)
mean(predTrn == lcdfTrn$loan_status)  #0.9856477

#str(lcdfTst)
#levels(droplevels(lcdfTst$title))

table(pred = predict(lcDT1,lcdfTst, type='class'), true=lcdfTst$loan_status)
mean(predict(lcDT1,lcdfTst, type='class') ==lcdfTst$loan_status)  # 0.9838784

###################################################
#Performance Evaluation with different classification threshold
#print(predProbTrn[, 'Charged Off'])

CTHRESH=0.60
predProbTrn=predict(lcDT1,lcdfTrn, type='prob')
predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
table(predTrnCT , true=lcdfTrn$loan_status)
# Or, to set the predTrnCT values as factors, and then get the confusion matrix
table(predictions=factor(predTrnCT, levels=c("Fully Paid", "Charged Off")), actuals=lcdfTrn$loan_status)

#precision_recall_curve()

library(lattice)
library(ggplot2)
library(caret)
confusionMatrix(predTrn, lcdfTrn$loan_status)
confusionMatrix(predTrn, lcdfTrn$loan_status, positive="Charged Off")

# Accuracy : 0.9885

#confusionMatrix(pred, lcdfTst$loan_status)

##############################################
# C5.0
library(C50)
str(lcdfTrn)

#costMat <- matrix(c(0,23,37,0), nrow=2)

C5_DT1 <- C5.0(loan_status ~., data=lcdfTrn, control=C5.0Control(),rules=TRUE)
print(C5_DT1)
C5imp(C5_DT1) %>% view()

predTrn = predict(C5_DT1,lcdfTrn)
table(predTrn, lcdfTrn$loan_status)
predTst = predict(C5_DT1,lcdfTst)
table(predTst, lcdfTrn$loan_status)

ncol(lcdfTrn)

c_tree <- C5.0(as.factor(lcdfTrn$loan_status) ~., data = lcdfTrn, 
               method = "class", trials = 3, control=C5.0Control(CF=0.45,earlyStopping =FALSE))
print(c_tree)

##################ROCR##################
library('ROCR')
score=predict(lcDT1,lcdfTst, type="prob")[,"Charged Off"]
pred=prediction(score, lcdfTst$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels   

#ROC curve
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)

#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values

#Lift curve
liftPerf <-performance(pred, "lift", "rpp")
plot(liftPerf)

################################################ Q 5 ###############################
##################### Random Forest###########################
#Random Forest
library(ranger)

# Number of trees = 300
rflcdf = ranger(loan_status ~ ., data = lcdfTrn, num.trees = 300, importance='permutation', probability = TRUE)
rflcdf
rflcdf

# Number of trees = 700
rflcdf700 = ranger(loan_status ~ ., data=lcdfTrn, num.trees = 700, importance='permutation', probability = TRUE)
rflcdf700

library(tidyverse)
#Variable Importance
importance(rflcdf) %>% view()
importance(rflcdf700) %>% view() 

# ROC Performance
library(ROCR)

pred_rflcdf=predict(rflcdf,lcdfTst)$predictions
perfROC_rflcdf=performance(prediction(predict(rflcdf,lcdfTst)$predictions[,2], lcdfTst$loan_status,label.ordering=c("Fully Paid", "Charged Off")), "tpr", "fpr")

pred_rflcdf700=predict(rflcdf700,lcdfTst)$predictions
perfROC_rflcdf700=performance(prediction(predict(rflcdf700,lcdfTst)$predictions[,2], lcdfTst$loan_status,label.ordering=c("Fully Paid", "Charged Off")), "tpr", "fpr")

plot(perfROC_rflcdf)
plot(perfROC_rflcdf700, add=TRUE, col="red")


#Lift Curve
liftPerfrflcdf<-performance(predrfl, "lift", "rpp")
plot(liftPerfrflcdf, add=TRUE, col="green")


#Confusion Matrix
pred300 = ifelse(pred_rflcdf[, "Fully Paid"] >= 0.5, "Fully Paid", "Charged Off")
pred300 <- factor(pred300, levels=c("Fully Paid", "Charged Off"))

ct300 = table(pred = pred300, true = lcdfTst$loan_status)

pred700 = ifelse(pred_rflcdf[, "Fully Paid"] >= 0.5, "Fully Paid", "Charged Off")
pred700 <- factor(pred700, levels=c("Fully Paid", "Charged Off"))
ct700 = table(pred = pre700, true = lcdfTst$loan_status)

ct300
ct700

confusionMatrix(pred300,lcdfTst$loan_status, positive="Charged Off")
confusionMatrix(pred600,lcdfTst$loan_status, positive="Charged Off")

#Compare Lift Curves of two best models
plot(liftPerfrflcdf)
plot(liftPerfrflcdf, add=TRUE, col="blue")


############################################ Q 6 ###################################################################
############################### 6 A ################################
#Confusion Matrix for Decision Tree and Random Forests
CMDT <- table(pred=pred300, true=lcdfTst$loan_status)
ct300

#Profit/Loss table
PROFITVAL <- 21
COSTVAL <- 37

PLtable <- matrix(c(PROFITVAL,21,COSTVAL,37),ncol=2,byrow=FALSE)
colnames(PLtable) <- c("Act Full Paid","Act Charged Off")
rownames(PLtable) <- c("Pred Fully Paid","Pred Charged Off")
PLtable <- as.table(PLtable)

#Calculate Profit
#Decision tree
sum(CMDT*PLtable)

#Random Forest
sum(ct700*PLtable)








