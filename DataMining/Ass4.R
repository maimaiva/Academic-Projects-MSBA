library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggpubr)

# the data file uses ';' as delimiter, and for this we use the read_csv2 function
resReviewsData <- read.csv2('/Users/vanisa/Dropbox/MSBA_Fall2020/IDS572_DataMining/Assignment_4/yelpResReviews_sample.csv',stringsAsFactors = FALSE)

################################################## Part A #########################################################################
# Data Exploration and Visualization 
# Objective: to get basic idea on dataset 

#number of reviews by start-rating
str(resReviewsData)
resReviewsData %>% group_by(stars) %>% count()
b1 <- resReviewsData %>% group_by(stars) %>% count()

p<-ggplot(data = b1, aes(x=stars, y=n)) + 
  geom_bar(stat="identity") 

p

ggplot(resReviewsData, aes(x= funny, y=stars)) + geom_point()

ggplot(resReviewsData, aes(x= cool, y=stars)) + geom_point()

ggplot(resReviewsData, aes(x= useful, y=stars)) + geom_point()

ggplot(resReviewsData, aes(x= cool, y=funny)) + geom_point()

names(resReviewsData)

#resReviewsData %>% select(stars,cool, funny, useful )
RR_New <- resReviewsData %>% filter(stars!=3) %>% select(stars,cool, funny, useful ) %>% mutate(PN = ifelse(stars <= 2, -1, 1))
view(RR_New)


##################################### Tokenize ########################################################################
# Tokenize 
# Objective: Extract the word out from reviews
# Plus, Cleansing

library(tidytext)
library(SnowballC)
library(textstem)

#tokenize the text of the reviews in the column named 'text'
rrTokens <- resReviewsData %>% unnest_tokens(word, text)
# this will retain all other attributes
#Or we can select just the review_id and the text column
rrTokens <- resReviewsData %>% select(review_id, stars, text ) %>% unnest_tokens(word, text)
head(rrTokens)

dim(rrTokens)
#How many tokens?
rrTokens %>% distinct(word) %>% dim()

#remove stopwords
rrTokens <- rrTokens %>% anti_join(stop_words)
#compare with earlier - what fraction of tokens were stopwords?
rrTokens %>% distinct(word) %>% dim()

#count the total occurrences of different words, & sort by most frequent
rrTokens %>% count(word, sort=TRUE) %>% top_n(10)
rrTokens %>% count(word, sort=TRUE) %>% top_n(20)

#Are there some words that occur in a large majority of reviews, or which are there in very few reviews?  
#Let's remove the words which are not present in at least 10 reviews
rareWords <-rrTokens %>% count(word, sort=TRUE) %>% filter(n<10)
rareWords

# using anti_join we remove rareWords
xx<-anti_join(rrTokens, rareWords)
tail(xx)

#check the words in xx .... 
xx %>% count(word, sort=TRUE) %>% view()
#you will see that among the least frequently occurring words are those starting with or including 
# numbers (as in 6oz, 1.15,...).  
# To remove these filter the data
xx2<- xx %>% filter(str_detect(word,"[0-9]")==FALSE)
# the variable xx, xx2 are for checking ....if this is what we want, set the rrTokens to the reduced set of words.  
# And you can remove xx, xx2 from the environment.
rrTokens<- xx2
#view(rrTokens)

# Analyze reviews by star ratings

#Check words by star rating of reviews
rrTokens %>% group_by(stars) %>% count(word, sort=TRUE)
#or...
rrTokens %>% group_by(stars) %>% count(word, sort=TRUE) %>% arrange(desc(stars)) %>% view()

#proportion of word occurrence by star ratings
ws <- rrTokens %>% group_by(stars) %>% count(word, sort=TRUE)
ws <- ws %>% group_by(stars) %>% mutate(prop=n/sum(n)) %>% arrange(prop)
ws

# experimenting group by by  words based on stars ratings
rrTokens %>% select(word, stars) %>% group_by(word)  %>% summarise(mean_s = mean(stars)) %>% arrange(mean_s)

#rrTokens %>% select(word, stars) %>% group_by(word)  %>% mutate(mean_s = mean(stars)) %>% summarise(mean_s) %>% 
# arrange(desc(mean_s)) 

#check the proportion of 'love' among reviews with 1,2,..5 stars 
ws %>% filter(word=='love')

#what are the most commonly used words by start rating
ws %>% group_by(stars) %>% arrange(stars, desc(prop)) %>% view()

#to see the top 20 words by star ratings
ws %>% group_by(stars) %>% arrange(stars, desc(prop)) %>% filter(row_number()<=20L) %>% view()

#To plot this
ws %>% group_by(stars) %>% arrange(stars, desc(prop)) %>% 
  filter(row_number()<=20L) %>% 
  ggplot(aes(word, prop))+geom_col()+coord_flip()+facet_wrap((~stars))

#ws %>% filter(stars==1)  %>%  ggplot(aes(word, n)) + geom_col()+coord_flip()

#Can we get a sense of which words are related to higher/lower star raings in general? 
#One approach is to calculate the average star rating associated with each word - can sum the star ratings 
#associated with reviews where each word occurs in.  
#Can consider the proportion of each word among reviews with a star rating.
xx<- ws %>% group_by(word) %>% summarise(totWS=sum(stars*prop))   #2*0.4 = 0.8       5*0.1 = 0.5 
#What are the 20 words with highest and lowerst star rating
xx %>% top_n(20)
xx %>% top_n(-20)
#Q - does this 'make sense'?

########################################### Part B ###############################################################################
# More Cleansing and manipulation 

############### Stemming and Lemmatization ####################
rrTokens_stem<-rrTokens %>%  mutate(word_stem = SnowballC::wordStem(word))
rrTokens_lemm<-rrTokens %>%  mutate(word_lemma = textstem::lemmatize_words(word))
#Check the original words, and their stemmed-words and word-lemmas

rrTokens_stem %>% distinct(word_stem)
rrTokens_lemm %>% distinct(word_lemma)

# Term-frequency, tf-idf
#tokenize, remove stopwords, and lemmatize (or you can use stemmed words instead of lemmatization)
rrTokens<-rrTokens %>%  mutate(word = textstem::lemmatize_words(word))
dim(rrTokens)

rr_bak1 <- rrTokens
rrTokens <- rr_bak1

# We may want to filter OUT words with less than 3 characters and those with more than 15 characters
rrTokens <- rrTokens %>% filter(str_length(word)>=3 & str_length(word)<=15)
dim(rrTokens)
view(head(rrTokens, 500))
#view(rrTokens)

rrTokens <- rrTokens %>% group_by(review_id, stars) %>% count(word)
head(rrTokens, 20)
dim(rrTokens)

#count total number of words by review, and add this in a column - WHY DID WE DO THIS??
totWords <- rrTokens  %>% group_by(review_id) %>%  count(word, sort=TRUE) %>% summarise(total=sum(n))
dim(totWords)
head(totWords, 20)
view(head(totWords, 500))

# when we use count(word) it counts the words that comes 2 times or more
view(rrTokens %>% filter(review_id == "--9vqlJ0xGKY2L1Uz-L9Eg"))

#totWordsXX <- rrTokens  %>% group_by(review_id) %>%  summarise(total=sum(n))
#view(head(totWordsXX, 500))

# We left join on review_id
xx <- left_join(rrTokens, totWords)
dim(xx)
head(xx)
view(head(xx, 100))
# now n/total gives the tf values
xx <- xx %>% mutate(tf=n/total)
head(xx,20)


################################################## Mapping to Dictionary ##################################################
#We can use the bind_tfidf function to calculate the tf, idf and tfidf values
# (https://www.rdocumentation.org/packages/tidytext/versions/0.2.2/topics/bind_tf_idf)
# Bind The Term Frequency And Inverse Document Frequency Of A Tidy Text Dataset To The Dataset
rrTokens <- rrTokens %>% bind_tf_idf(word, review_id, n)
head(rrTokens)
# Inverse Document Frequency: terms that occur across many documents are not useful for differentiating between documents
# If value of IDF is high that means the term is occurring fewer times

# Sentiment analysis using the 3 sentiment dictionaries available with tidytext (use library(textdata))
# AFINN http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010
# bing  https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html 
# nrc http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

library(textdata)

#take a look at the words in the sentimennt dictionaries
get_sentiments("bing") %>% view()
get_sentiments("nrc") %>% view()
get_sentiments("afinn") %>% view()

head(rrTokens)
dim(rrTokens)
#sentiment of words in rrTokens
rrSenti_bing <- rrTokens %>% left_join(get_sentiments("bing"), by="word")
head(rrSenti_bing,20)
dim(rrSenti_bing)

#if we want to retain only the words which match the sentiment dictionary, do an inner-join
rrSenti_bing_inner <- rrTokens %>% inner_join(get_sentiments("bing"), by="word")
dim(rrSenti_bing_inner)
head(rrSenti_bing_inner)

# Analyze Which words contribute to positive/negative sentiment - 
# we can count the occurrences of positive/negative sentiment words in the reviews
xx <- rrSenti_bing %>% group_by(word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))
#negate the counts for the negative sentiment words
view(head(xx, 100))
xx <- xx %>% mutate (totOcc=ifelse(sentiment=="positive", totOcc, -totOcc))
dim(xx)
head(xx,20)
view(tail(xx,200))
# checking the output
xx[579:650,] %>% view()

# the most positive and most negative words
# Running ungroup() will drop any grouping. This can be reinstated again with regroup().
# Earlier it was grouped by word and sentiment
xx <- ungroup(xx)    #WHY DID WE UNGROUP?? IT LOOKS SAME AFTER UNGROUP...
xx %>% top_n(25)
xx %>% top_n(-25)

#You can plot these
rbind(top_n(xx, 25), top_n(xx, -25)) %>% ggplot(aes(word, totOcc, fill=sentiment)) +geom_col()+coord_flip()

#or, with a better reordering of words
rbind(top_n(xx, 25), top_n(xx, -25)) %>% mutate(word=reorder(word,totOcc)) %>% 
  ggplot(aes(word, totOcc, fill=sentiment)) +geom_col()+coord_flip()

############## NRC dictionary
#with "nrc" dictionary
rrSenti_nrc <- rrTokens %>% inner_join(get_sentiments("nrc"), by="word") 
dim(rrSenti_nrc)
head(rrSenti_nrc)

xxnrc <- rrSenti_nrc %>% group_by (word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))


#How many words for the different sentiment categories
xxnrc %>% group_by(sentiment) %>% summarise(count=n(), sumn=sum(totOcc))
view(xxnrc)

# In 'nrc', the dictionary contains words defining different sentiments, like anger, 
# disgust, positive, negative, joy, trust,.....   you should check the words deonting these different sentiments
xxnrc %>% filter(sentiment=='anticipation') %>% view()
xxnrc %>% filter(sentiment=='fear') %>% view()

#Suppose you want   to consider  {anger, disgust, fear sadness, negative} to denote 'bad' reviews, and 
#{positive, joy, anticipation, trust} to denote 'good' reviews
xx <- xxnrc %>% 
  mutate(goodBad=ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), -totOcc,
                        ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust'), totOcc, 0))) %>% 
  arrange(goodBad)
dim(xx)
head(xx)

xx<-ungroup(xx)
top_n(xx, 10)
top_n(xx, -10)

rbind(top_n(xx, 25), top_n(xx, -25)) %>% mutate(word=reorder(word, goodBad)) %>% ggplot(aes(word, goodBad, fill=goodBad)) +
  geom_col()+coord_flip()


#AFINN carries a numeric value for positive/negative sentiment -- how would you use these

#with AFINN dictionary words....following similar steps as above, but noting that AFINN 
# assigns negative to positive sentiment value for words matching the dictionary
rrSenti_afinn<- rrTokens %>% inner_join(get_sentiments("afinn"), by="word")
head(rrSenti_afinn,10)

revSenti_afinnx <- rrSenti_afinn %>% group_by(review_id, stars) %>% summarise(nwords=n(), sentiSum =sum(value))
head(revSenti_afinn,10)

revSenti_afinnW <- rrSenti_afinn %>% group_by(word) %>% summarise(nwords=n(), sentiSum =sum(value)) %>% arrange(sentiSum)
head(revSenti_afinnW,10)

xx <- revSenti_afinnW 
head(xx,10)
xx<-ungroup(xx)
top_n(xx, 10)
top_n(xx, -10)

rbind(top_n(xx, 25), top_n(xx, -25)) %>% mutate(word=reorder(word, sentiSum)) %>% 
  ggplot(aes(word, sentiSum, fill=sentiSum)) +
  geom_col()+coord_flip()

revSenti_afinn %>% group_by(stars) %>% summarise(avgLen=mean(nwords), avgSenti=mean(sentiSum))




########################################### Part C #############################################################################

# Can we classify reviews on high/low stats based on aggregated sentiment of words in the reviews
#we can consider reviews with 1 to 2 stars as positive, and this with 4 to 5 stars as negative

#Compared pos-neg derived from 'stars' VS 3 'dictionary'

#Bing
revSenti_bing <- rrSenti_bing %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 )))
revSenti_bing <- revSenti_bing %>% mutate(pred_hiLo=ifelse(sentiment=="positive", 1, -1)) 
head(revSenti_bing)
revSenti_bing <- revSenti_bing %>% drop_na(pred_hiLo)
xx<-revSenti_bing %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo )


#NRC
revSenti_nrc <- rrTokens %>% left_join(get_sentiments("nrc"), by="word")
# Positive: "anticipation","joy","positive","trust", "surprise"         
# Negative: "anger", "disgust", "fear", "negative", "sadness"
# else NA: 0
revSenti_nrc <- revSenti_nrc %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 )))
revSenti_nrc <- revSenti_nrc %>% drop_na(sentiment)
revSenti_nrc <- revSenti_nrc %>% 
  mutate(pred_hiLo=ifelse(sentiment %in% c('anger', 'disgust', 
                                           'fear', 'sadness', 'negative'), -1,
                          ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust'), 1, 0)))

xx<-revSenti_nrc %>% filter(hiLo!=0)
xx<-xx %>% filter(pred_hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo)

#AFINN 
head(revSenti_afinn)
revSenti_afinn <- revSenti_afinn %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 )))
# pred_hiLo is mapping sentiSum as positive and negative
revSenti_afinn <- revSenti_afinn %>% mutate(pred_hiLo=ifelse(sentiSum >0, 1, -1)) 
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
xx<-revSenti_afinn %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$pred_hiLo )


####################################################################################################################################


#Can we learn a model to predict hiLo ratings, from words in reviews
#considering only those words which match a sentiment dictionary (for eg.  bing)
#use pivot_wider to convert to a dtm form where each row is for a review and columns correspond to words   
# (https://tidyr.tidyverse.org/reference/pivot_wider.html)
#revDTM_sentiBing <- rrSenti_bing %>%  pivot_wider(id_cols = review_id, names_from = word, values_from = tf_idf)
#Or, since we want to keep the stars column

dim(rrSenti_bing)
names(rrSenti_bing)
sum(is.na(rrSenti_bing$sentiment))

revDTM_sentiBing <- rrSenti_bing %>%  
  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf)  %>% ungroup()
#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and 
#this grouping is retained by default, and can cause problems in the later steps
dim(revDTM_sentiBing)
view(head(revDTM_sentiBing, 10))

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentiBing <- revDTM_sentiBing %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% select(-stars)
dim(revDTM_sentiBing)
head(revDTM_sentiBing)
#how many review with 1, -1  'class'
revDTM_sentiBing %>% group_by(hiLo) %>% tally()

######################################### Random Forest ################################################################ 
#develop a random forest model to predict hiLo from the words in the reviews
library(ranger)

#replace all the NAs with 0
revDTM_sentiBing<-revDTM_sentiBing %>% replace(., is.na(.), 0)

head(revDTM_sentiBing)
revDTM_sentiBing$hiLo <- as.factor(revDTM_sentiBing$hiLo)

library(dplyr)
#class(rrSenti_bing)
set.seed(1789)
dim(revDTM_sentiBing)
rrSenti_10k <- revDTM_sentiBing[sample(nrow(revDTM_sentiBing), 10000), ]
revDTM_sentiBing <- rrSenti_10k

library(rsample)
set.seed(1789)
revDTM_sentiBing_split<- initial_split(revDTM_sentiBing, 0.5)
revDTM_sentiBing_trn<- training(revDTM_sentiBing_split)
revDTM_sentiBing_tst<- testing(revDTM_sentiBing_split)

set.seed(1789)
rfModel1 <- ranger(dependent.variable.name = "hiLo", data=revDTM_sentiBing_trn %>% 
                   select(-review_id), num.trees = 500, importance='permutation', probability = TRUE)

rfModel1

#which variables are important
importance(rfModel1) %>% view()


#Obtain predictions, and calculate performance
revSentiBing_predTrn<- predict(rfModel1, revDTM_sentiBing_trn %>% select(-review_id))$predictions

revSentiBing_predTst<- predict(rfModel1, revDTM_sentiBing_tst %>% select(-review_id))$predictions

table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_predTrn[,2]>0.5)
table(actual=revDTM_sentiBing_tst$hiLo, preds=revSentiBing_predTst[,2]>0.5)
#Q - is 0.5 the best threshold to use here?  Can find the optimal threshold from the     ROC analyses


library(pROC)
rocTrn <- roc(revDTM_sentiBing_trn$hiLo, revSentiBing_predTrn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentiBing_tst$hiLo, revSentiBing_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
bThr
# table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_predTrn[,2]>bThr)



# Develop a model on broader set of terms (not just those matching a sentiment dictionary)

#if we want to remove the words which are there in too many or too few of the reviews
#First find out how many reviews each word occurs in
rWords<-rrTokens %>% group_by(word) %>% summarise(nr=n()) %>% arrange(desc(nr))

#How many words are there
length(rWords$word)

top_n(rWords, 20)
top_n(rWords, -20)

#Suppose we want to remove words which occur in > 90% of reviews, and those which are in, for example, less than 30 reviews
reduced_rWords<-rWords %>% filter(nr< 6000 & nr > 30)
length(reduced_rWords$word)

#reduce the rrTokens data to keep only the reduced set of words
reduced_rrTokens <- left_join(reduced_rWords, rrTokens)
dim(reduced_rrTokens)
#Now convert it to a DTM, where each row is for a review (document), and columns are the terms (words)
revDTM  <- reduced_rrTokens %>%  
  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf)  %>% ungroup()

#Check
dim(revDTM)
#do the number of colums match the words -- we should also have the stars column and the review_id

#create the dependent variable hiLo of good/bad reviews absed on stars, and remove the review with stars=3
revDTM <- revDTM %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% select(-stars)

#replace NAs with 0s
revDTM<-revDTM %>% replace(., is.na(.), 0)

revDTM$hiLo<-as.factor(revDTM$hiLo)

dim(revDTM)

library(dplyr)
#class(rrSenti_bing)
set.seed(1789)
dim(revDTM)
rrSenti2_10k <- revDTM[sample(nrow(revDTM), 10000), ]
revDTM <- rrSenti2_10k
dim(revDTM)

revDTM_split<- initial_split(revDTM, 0.5)
revDTM_trn<- training(revDTM_split)
revDTM_tst<- testing(revDTM_split)

#this can take some time...the importance ='permutation' takes time (we know why)
rfModel2<-ranger(dependent.variable.name = "hiLo", data=revDTM_trn %>% 
                   select(-review_id), num.trees = 500, importance='permutation', probability = TRUE)

rfModel2

head(revDTM_sentiBing$hiLo)
#lr_glm <- glm(hiLo ~., data=revDTM_sentiBing, family=binomial)


################################### cv.glmnet : alpha = 1 (Lasso) #######################################################
library(glmnet)
xD <- revDTM_sentiBing_trn %>% select(-hiLo) 
yD <- revDTM_sentiBing_trn$hiLo

xDTst <- revDTM_sentiBing_tst %>% select(-hiLo)
yDTst <- revDTM_sentiBing_tst$hiLo


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
glm_M1_train_pred <- factor(glm_M1_train_pred, levels=c(1,-1))
yD2 <- factor(yD, levels=c(1,-1))                         
confusionMatrix (glm_M1_train_pred,yD2, positive="1")                           

#confusion_matrix for Tst
glm_M1_test_pred <- predict(cvglmnet_1, data.matrix(xDTst),s=cvglmnet_1$lambda.1se,type="class")
glm_M1_test_pred <- factor(glm_M1_test_pred, levels=c(1,-1))
yDTst2 <- factor(yDTst, levels=c(1,-1))                         
confusionMatrix (glm_M1_test_pred,yDTst2, positive="1")

##############################################################################################################################
##############################################  Part D  ######################################################################
# Develop models using only the sentiment dictionary terms 
# We will use 3 dictionaries and combined dictionary
##############################################################################################################################
# 1) bing dictionary
# Preparing the Document Term Matrix

dim(rrSenti_bing)
dim(rrSenti_bing_inner)

revDTM_sentiBing_IJ <- rrSenti_bing_inner %>%  
  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf)  %>% ungroup()
dim(revDTM_sentiBing_IJ)
#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and 
#this grouping is retained by default, and can cause problems in the later steps
dim(revDTM_sentiBing_IJ)
view(head(revDTM_sentiBing_IJ, 10))

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentiBing_IJ <- revDTM_sentiBing_IJ %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% select(-stars)
dim(revDTM_sentiBing_IJ)
head(revDTM_sentiBing_IJ)
#how many review with 1, -1  'class'
revDTM_sentiBing_IJ %>% group_by(hiLo) %>% tally()


#replace all the NAs with 0
revDTM_sentiBing_IJ <- revDTM_sentiBing_IJ %>% replace(., is.na(.), 0)

dim(revDTM_sentiBing_IJ) #35711  1099
revDTM_sentiBing_IJ$hiLo <- as.factor(revDTM_sentiBing_IJ$hiLo)

library(dplyr)
#class(rrSenti_bing)
set.seed(1789)
dim(revDTM_sentiBing_IJ)  # 35711 rows  1099 columns
rrSenti_10k <- revDTM_sentiBing_IJ[sample(nrow(revDTM_sentiBing_IJ), 10000), ]
revDTM_sentiBing_IJ <- rrSenti_10k

library(rsample)
set.seed(1789)
revDTM_sentiBing_split<- initial_split(revDTM_sentiBing_IJ, 0.5)
revDTM_sentiBing_trn<- training(revDTM_sentiBing_split)
revDTM_sentiBing_tst<- testing(revDTM_sentiBing_split)

################################### 1) Bing: Naive Bayes ########################################################################

library(e1071)
set.seed(1789)
nbModel1<-naiveBayes(hiLo ~ ., data=revDTM_sentiBing_trn %>% select(-review_id))

revSentiBing_NBpredTrn<-predict(nbModel1, revDTM_sentiBing_trn, type = "raw")
revSentiBing_NBpredTst<-predict(nbModel1, revDTM_sentiBing_tst, type = "raw")

library(pROC)
auc(as.numeric(revDTM_sentiBing_trn$hiLo), revSentiBing_NBpredTrn[,2])
auc(as.numeric(revDTM_sentiBing_tst$hiLo), revSentiBing_NBpredTst[,2])

################################### 2) Bing: cv.glmnet : alpha = 1 (Lasso) #######################################################

library(glmnet)
xD <- revDTM_sentiBing_trn %>% select(-hiLo) 
yD <- revDTM_sentiBing_trn$hiLo

xDTst <- revDTM_sentiBing_tst %>% select(-hiLo)
yDTst <- revDTM_sentiBing_tst$hiLo


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
glm_M1_train_pred <- factor(glm_M1_train_pred, levels=c(1,-1))
yD2 <- factor(yD, levels=c(1,-1))                         
confusionMatrix (glm_M1_train_pred,yD2, positive="1")                           

#confusion_matrix for Tst
glm_M1_test_pred <- predict(cvglmnet_1, data.matrix(xDTst),s=cvglmnet_1$lambda.1se,type="class")
glm_M1_test_pred <- factor(glm_M1_test_pred, levels=c(1,-1))
yDTst2 <- factor(yDTst, levels=c(1,-1))                         
confusionMatrix (glm_M1_test_pred,yDTst2, positive="1")

################################### 3) Bing: Random Forest #######################################################

rf_bing<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiBing_trn %>% 
                   select(-review_id), num.trees = 100, importance='permutation', probability = TRUE)
rf_bing

#which variables are important
importance(rf_bing) %>% view()

#Obtain predictions, and calculate performance
pred_bing_Trn <- predict(rf_bing, revDTM_sentiBing_trn %>% select(-review_id))$predictions

pred_bing_Tst<- predict(rf_bing, revDTM_sentiBing_tst %>% select(-review_id))$predictions

table(actual=revDTM_sentiBing_trn$hiLo, preds=pred_bing_Trn[,2]>0.5)
table(actual=revDTM_sentiBing_tst$hiLo, preds=pred_bing_Tst[,2]>0.5)
#Q - is 0.5 the best threshold to use here?  Can find the optimal threshold from the     ROC analyses

library(pROC)
rocTrn <- roc(revDTM_sentiBing_trn$hiLo, pred_bing_Trn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentiBing_tst$hiLo, pred_bing_Tst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
bThr

###################################################################################################################

# 2) NRC dictionary 
# Preparing the Document Term Matrix

#dim(rrSenti_bing_inner)
dim(rrSenti_nrc)
head(rrSenti_nrc)

#compared to BING
head(rrSenti_bing_inner)
view(head(revDTM_sentiBing_IJ,10))


m <- rrSenti_nrc %>% select(-n,-tf,-idf,-sentiment) %>% distinct()

revDTM_Senti_nrc <- m %>%  
  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf)  %>% ungroup()
dim(revDTM_Senti_nrc)
#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and 
#this grouping is retained by default, and can cause problems in the later steps
dim(revDTM_Senti_nrc)
view(head(revDTM_Senti_nrc, 10))

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_Senti_nrc <- revDTM_Senti_nrc %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% select(-stars)
dim(revDTM_Senti_nrc)
head(revDTM_Senti_nrc)
#how many review with 1, -1  'class'
revDTM_Senti_nrc %>% group_by(hiLo) %>% tally()


#replace all the NAs with 0
revDTM_Senti_nrc <- revDTM_Senti_nrc %>% replace(., is.na(.), 0)

head(revDTM_Senti_nrc)
revDTM_Senti_nrc$hiLo <- as.factor(revDTM_Senti_nrc$hiLo)

library(dplyr)

set.seed(1789)
dim(revDTM_Senti_nrc)  # 35711 rows  1099 columns
rrSenti_10k <- revDTM_Senti_nrc[sample(nrow(revDTM_Senti_nrc), 10000), ]
revDTM_Senti_nrc <- rrSenti_10k

library(rsample)
set.seed(1789)
revDTM_sentiNRC_split<- initial_split(revDTM_Senti_nrc, 0.5)
revDTM_sentiNRC_trn<- training(revDTM_sentiNRC_split)
revDTM_sentiNRC_tst<- testing(revDTM_sentiNRC_split)

################################### 4) NRC: Naive Bayes ########################################################################

library(e1071)
set.seed(1789)
nbModel4<-naiveBayes(hiLo ~ ., data=revDTM_sentiBing_trn %>% select(-review_id))

revSentiNRC_NBpredTrn<-predict(nbModel4, revDTM_sentiNRC_trn, type = "raw")
revSentiNRC_NBpredTst<-predict(nbModel4, revDTM_sentiNRC_tst, type = "raw")

library(pROC)
auc(as.numeric(revDTM_sentiNRC_trn$hiLo), revSentiBing_NBpredTrn[,2])
auc(as.numeric(revDTM_sentiNRC_tst$hiLo), revSentiBing_NBpredTst[,2])


################################### 5) NRC: cv.glmnet : alpha = 1 (Lasso) #######################################################

library(glmnet)
xD <- revDTM_sentiNRC_trn %>% select(-hiLo,-review_id) 
yD <- revDTM_sentiNRC_trn$hiLo

xDTst <- revDTM_sentiNRC_tst %>% select(-hiLo,-review_id) 
yDTst <- revDTM_sentiNRC_tst$hiLo

set.seed(1789)
cvglmnet_5 <- cv.glmnet(data.matrix(xD), yD, 
                        family = "binomial",
                        nfolds = 5,
                        alpha = 1)
plot(cvglmnet_5)


########## variable importance glmnet
library(vip)
tb5 <- vi_model(cvglmnet_5)
arrange(tb5,desc(Importance),Variable)
sort(tb5$Importance, decreasing = TRUE) %>% view()

library(caret)
#confusion_matrix for Trn
glm_M5_train_pred <- predict(cvglmnet_5, data.matrix(xD),s=cvglmnet_5$lambda.1se,type="class")
glm_M5_train_pred <- factor(glm_M5_train_pred, levels=c(1,-1))
yD2 <- factor(yD, levels=c(1,-1))                         
confusionMatrix (glm_M5_train_pred,yD2, positive="1")                           

#confusion_matrix for Tst
glm_M5_test_pred <- predict(cvglmnet_5, data.matrix(xDTst),s=cvglmnet_5$lambda.1se,type="class")
glm_M5_test_pred <- factor(glm_M5_test_pred, levels=c(1,-1))
yDTst2 <- factor(yDTst, levels=c(1,-1))                         
confusionMatrix (glm_M5_test_pred,yDTst2, positive="1")

################################### 6) NRC: Random Forest #######################################################

rf_nrc <-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiNRC_trn %>% 
                  select(-review_id), num.trees = 100, importance='permutation', probability = TRUE)
rf_nrc

#which variables are important
importance(rf_nrc) #%>% view()

#Obtain predictions, and calculate performance
pred_nrc_Trn <- predict(rf_nrc, revDTM_sentiNRC_trn %>% select(-review_id))$predictions

pred_nrc_Tst<- predict(rf_nrc, revDTM_sentiNRC_tst %>% select(-review_id))$predictions

table(actual=revDTM_sentiNRC_trn$hiLo, preds=pred_nrc_Trn[,2]>0.5)
table(actual=revDTM_sentiNRC_tst$hiLo, preds=pred_nrc_Tst[,2]>0.5)

library(pROC)
rocTrn <- roc(revDTM_sentiNRC_trn$hiLo, pred_nrc_Trn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentiNRC_tst$hiLo, pred_nrc_Tst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
bThr


#################################
# 3) AFINN dictionary 
# Preparing the Document Term Matrix


dim(rrSenti_afinn)
head(rrSenti_afinn)

#head(rrSenti_bing_inner)
#view(head(revDTM_sentiBing_IJ,10))

revDTM_Senti_affin <- rrSenti_afinn %>%  
  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf)  %>% ungroup()
dim(revDTM_Senti_affin)

#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and 
#this grouping is retained by default, and can cause problems in the later steps
dim(revDTM_Senti_affin)
view(head(revDTM_Senti_affin, 10))

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_Senti_affin <- revDTM_Senti_affin %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% select(-stars)
dim(revDTM_Senti_affin)
head(revDTM_Senti_affin)
#how many review with 1, -1  'class'
revDTM_Senti_affin %>% group_by(hiLo) %>% tally()


#replace all the NAs with 0
revDTM_Senti_affin <- revDTM_Senti_affin %>% replace(., is.na(.), 0)

head(revDTM_Senti_affin)
revDTM_Senti_affin$hiLo <- as.factor(revDTM_Senti_affin$hiLo)

library(dplyr)

set.seed(1789)
dim(revDTM_Senti_affin)  # 34926 rows  593 columns
rrSenti_10k <- revDTM_Senti_affin[sample(nrow(revDTM_Senti_affin), 10000), ]
revDTM_Senti_affin <- rrSenti_10k

library(rsample)
set.seed(1789)
revDTM_sentiaffin_split<- initial_split(revDTM_Senti_affin, 0.5)
revDTM_sentiaffin_trn<- training(revDTM_sentiaffin_split)
revDTM_sentiaffin_tst<- testing(revDTM_sentiaffin_split)

################################### 7) AFINN: Naive Bayes ########################################################################

library(e1071)
set.seed(1789)
nbModel7<-naiveBayes(hiLo ~ ., data=revDTM_sentiaffin_trn %>% select(-review_id))

revSentiAFINN_NBpredTrn<-predict(nbModel7, revDTM_sentiaffin_trn, type = "raw")
revSentiAFINN_NBpredTst<-predict(nbModel7, revDTM_sentiaffin_tst, type = "raw")

library(pROC)
auc(as.numeric(revDTM_sentiaffin_trn$hiLo), revSentiAFINN_NBpredTrn[,2])
auc(as.numeric(revDTM_sentiaffin_tst$hiLo), revSentiAFINN_NBpredTst[,2])


################################### 8) AFINN: cv.glmnet : alpha = 1 (Lasso) #######################################################

library(glmnet)
xD <- revDTM_sentiaffin_trn %>% select(-hiLo,-review_id) 
yD <- revDTM_sentiaffin_trn$hiLo

xDTst <- revDTM_sentiaffin_tst %>% select(-hiLo,-review_id) 
yDTst <- revDTM_sentiaffin_tst$hiLo

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
glm_M1_train_pred <- factor(glm_M1_train_pred, levels=c(1,-1))
yD2 <- factor(yD, levels=c(1,-1))                         
confusionMatrix (glm_M1_train_pred,yD2, positive="1")                           

#confusion_matrix for Tst
glm_M1_test_pred <- predict(cvglmnet_1, data.matrix(xDTst),s=cvglmnet_1$lambda.1se,type="class")
glm_M1_test_pred <- factor(glm_M1_test_pred, levels=c(1,-1))
yDTst2 <- factor(yDTst, levels=c(1,-1))                         
confusionMatrix (glm_M1_test_pred,yDTst2, positive="1")

################################### 9) AFINN: Random Forest #######################################################

#revDTM_sentiaffin_trn<- training(revDTM_sentiaffin_split)
#revDTM_sentiaffin_tst<- testing(revDTM_sentiaffin_split)


rf_afn <-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiaffin_trn %>% 
                  select(-review_id), num.trees = 100, importance='permutation', probability = TRUE)
rf_afn

#which variables are important
importance(rf_afn) #%>% view()

#Obtain predictions, and calculate performance
pred_afn_Trn <- predict(rf_afn, revDTM_sentiaffin_trn %>% select(-review_id))$predictions

pred_afn_Tst<- predict(rf_afn, revDTM_sentiaffin_tst %>% select(-review_id))$predictions

table(actual=revDTM_sentiaffin_trn$hiLo, preds=pred_afn_Trn[,2]>0.5)
table(actual=revDTM_sentiaffin_tst$hiLo, preds=pred_afn_Tst[,2]>0.5)

library(pROC)
rocTrn <- roc(revDTM_sentiaffin_trn$hiLo, pred_afn_Trn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentiaffin_tst$hiLo, pred_afn_Tst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
bThr

##############################################################################################################################
# 4) Combined dictionary
# Preparing the Document Term Matrix

#combine; create rrTokens_com
rrTokens_com <- rrTokens %>% left_join(get_sentiments("bing"), by="word")
colnames(rrTokens_com)[8] <- "senti.bing"
rrTokens_com <- rrTokens_com %>% left_join(get_sentiments("nrc"), by="word")
colnames(rrTokens_com)[9] <- "senti.nrc"
rrTokens_com <- rrTokens_com %>% left_join(get_sentiments("afinn"), by="word")
colnames(rrTokens_com)[10] <- "senti.afinn"

#mutate hiLo
rrTokens_com <- rrTokens_com %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 ))) 
#mutate hiLo.bing
rrTokens_com <- rrTokens_com %>% mutate(hiLo.bing=ifelse(senti.bing=="positive", 1, -1)) 
#mutate hiLo.nrc
rrTokens_com <- rrTokens_com  %>% mutate(hiLo.nrc=ifelse(senti.nrc %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), -1,
                                                         ifelse(senti.nrc %in% c('positive', 'joy', 'anticipation', 'trust'), 1, 0)))
#mutate hiLo.afinn
rrTokens_com <- rrTokens_com %>% mutate(hiLo.afinn=ifelse(senti.afinn >0, 1, -1)) 
rrTokens_com <- rrTokens_com %>% select(-senti.bing, -senti.nrc,-senti.afinn)

#replace NA with 0 
rrTokens_com <- rrTokens_com %>% replace(., is.na(.), 0)
#combine 3 dictionaries
rrTokens_com <- rrTokens_com %>% mutate(hiLo.com = hiLo.bing+hiLo.nrc+hiLo.afinn)
#mutate comm
rrTokens_com <- rrTokens_com %>% mutate(hiLo.comm=ifelse(hiLo.com>0,1,ifelse(hiLo.com<0, -1, 0 ))) 
#filter out unmatch words
rrTokens_com <- rrTokens_com %>% filter(hiLo.comm != 0)

#for pivot
m <- rrTokens_com %>% 
  select(-n,-tf,-idf,-hiLo.bing,-hiLo.nrc,-hiLo.afinn,,-hiLo.com,-hiLo,-hiLo.com,-hiLo.comm) %>% distinct()
dim(m)
#pivot table
revDTM_com <- m %>%  
  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf) %>% ungroup()
dim(revDTM_com)

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_com <- revDTM_com %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% select(-stars)

#replace all the NAs with 0
revDTM_com <- revDTM_com %>% replace(., is.na(.), 0)

#change to factor
revDTM_com$hiLo <- as.factor(revDTM_com$hiLo)

library(dplyr)
#class(rrSenti_bing)
set.seed(1789)
dim(revDTM_com)  # 36955  rows 2041 cols

revDTM_com_10k <- revDTM_com[sample(nrow(revDTM_com), 10000), ]

library(rsample)
set.seed(1789)
revDTM_com_split<- initial_split(revDTM_com_10k, 0.5)
revDTM_com_trn<- training(revDTM_com_split)
revDTM_com_tst<- testing(revDTM_com_split)

########### Compare to stars ###############
xx<- rrTokens_com %>% filter(hiLo!=0)
table(actual=xx$hiLo, predicted=xx$hiLo.comm)




################################### 10) Com: Naive Bayes ########################################################################

library(e1071)
set.seed(1789)
nb_com <-naiveBayes(hiLo ~ ., data=revDTM_com_trn %>% select(-review_id))

com_NBpredTrn<-predict(nb_com, revDTM_com_trn, type = "raw")
com_NBpredTst<-predict(nb_com, revDTM_com_tst, type = "raw")

library(pROC)
auc(as.numeric(revDTM_com_trn$hiLo), com_NBpredTrn[,2])
auc(as.numeric(revDTM_com_tst$hiLo), com_NBpredTst[,2])

################################### 11) Com: cv.glmnet : alpha = 1 (Lasso) #######################################################

library(glmnet)
xD <- revDTM_com_trn %>% select(-hiLo,-review_id)  
yD <- revDTM_com_trn$hiLo

xDTst <- revDTM_com_tst %>% select(-hiLo,,-review_id) 
yDTst <- revDTM_com_tst$hiLo


set.seed(1789)
cvglmnet_com <- cv.glmnet(data.matrix(xD), yD, 
                        family = "binomial",
                        nfolds = 5,
                        alpha = 1)
plot(cvglmnet_com)


########## variable importance glmnet
library(vip)
tb1 <- vi_model(cvglmnet_com)
arrange(tb1,desc(Importance),Variable)
sort(tb1$Importance, decreasing = TRUE) %>% view()

library(caret)
#confusion_matrix for Trn
glm_com_train_pred <- predict(cvglmnet_com, data.matrix(xD),s=cvglmnet_com$lambda.1se,type="class")
glm_com_train_pred <- factor(glm_com_train_pred, levels=c(1,-1))
yD2 <- factor(yD, levels=c(1,-1))                         
confusionMatrix (glm_com_train_pred,yD2, positive="1")                           

#confusion_matrix for Tst
glm_com_test_pred <- predict(cvglmnet_com, data.matrix(xDTst),s=cvglmnet_com$lambda.1se,type="class")
glm_com_test_pred <- factor(glm_com_test_pred, levels=c(1,-1))
yDTst2 <- factor(yDTst, levels=c(1,-1))                         
confusionMatrix (glm_com_test_pred,yDTst2, positive="1")

################################### 12) Com: Random Forest #######################################################

rf_com<-ranger(dependent.variable.name = "hiLo", data=revDTM_com_trn %>% 
                  select(-review_id), num.trees = 100, importance='permutation', probability = TRUE)
rf_com

#which variables are important
importance(rf_bing) %>% view()

#Obtain predictions, and calculate performance
pred_com_Trn <- predict(rf_com, revDTM_com_trn %>% select(-review_id))$predictions

pred_com_Tst<- predict(rf_com, revDTM_com_tst %>% select(-review_id))$predictions

table(actual=revDTM_com_trn$hiLo, preds=pred_com_Trn[,2]>0.5)
table(actual=revDTM_com_tst$hiLo, preds=pred_com_Tst[,2]>0.5)
#Q - is 0.5 the best threshold to use here?  Can find the optimal threshold from the     ROC analyses

library(pROC)
rocTrn <- roc(revDTM_com_trn$hiLo, pred_com_Trn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_com_tst$hiLo, pred_com_Tst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
bThr

###########################################################################################################################
# Dimension 
dim(revDTM_sentiBing) #8396 cols
dim(revDTM_Senti_nrc) #1535 cols
dim(revDTM_Senti_affin) #593 cols
dim(revDTM_com) #2041 cols
