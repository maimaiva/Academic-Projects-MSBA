library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

#codelist<-read.xlsx("/Users/vanisa/Dropbox/MSBA_Fall2020/IDS572_DataMining/Assignment_3/BathSoap.xlsx",1) # for reference only

library(readxl)
soap_data <- read_excel("C:/Users/usmcc/OneDrive/Desktop/IDS572/Assignment3/Assgt3_BathSoap_Data.xls", sheet = "DM_Sheet")
head(soap_data)

#soap_data %>% rename(TV ownership = ...10)
names(soap_data)[names(soap_data) == '...10' ] <- 'TV_Own'  # renaming column
names(soap_data)

#better to change the colNameswhich contain punctuation, space
names(soap_data) <-gsub("[[:punct:]]|\\s", "_", names(soap_data))
names(soap_data)

soap_bak <- soap_data

class(soap_data)

summary(soap_data)

colnames(soap_data)
names(soap_data)

# checking total NAs in df
sum(is.na(soap_data))
colMeans(is.na(soap_data))>0         # If True that means at least one NA present
#any(is.na(soap_data[,3]))    # checking any Null value presence in column 3
sum(is.na(soap_data$FEH))

#Replacing NAs with 0. 0 means "not specified"
soap_data <- soap_data %>% replace_na(list(FEH = 0)) 
soap_data <- soap_data %>% replace_na(list(MT = 0, SEX = 0))
soap_data <- soap_data %>% replace_na(list(EDU = 0, MT = 0, TV_Own = 0))
soap_data <- soap_data %>% replace_na(list(CS = 0))

head(soap_data,50)
colMeans(is.na(soap_data))>0
str(soap_data)

# Plotting the features to get sense of number of NAs and other categories
# "0" means NAs/Not specified
p<-ggplot(data = soap_data, aes(x=SEC) ) + 
  geom_bar(fill = "#0073C2FF")+
  theme_pubclean()
p

p<-ggplot(data = soap_data, aes(x=FEH) ) + 
  geom_bar(fill = "#0073C2FF")+
  theme_pubclean() + scale_y_continuous(name = "Axis in 50s",breaks = seq(0, 600, by = 50))
p

# percentage bar plot
ggplot(data = soap_data) + 
  geom_bar(mapping = aes(x = FEH, y = ..prop.., group = 1), stat = "count") + 
  scale_y_continuous(labels = scales::percent_format())

p<-ggplot(data = soap_data, aes(x=MT) ) + 
  geom_bar(fill = "#0073C2FF")+
  theme_pubclean() + scale_x_continuous(name = "Axis X",breaks = seq(0, 20, by = 2))
p

p<-ggplot(data = soap_data, aes(x=SEX) ) + 
  geom_bar(fill = "#0073C2FF")+
  theme_pubclean() + scale_y_continuous(name = "Axis in 50s",breaks = seq(0, 600, by = 50))
p

p<-ggplot(data = soap_data, aes(x=EDU) ) + 
  geom_bar(fill = "#0073C2FF")+
  theme_pubclean() + scale_x_continuous(name = "Axis X",breaks = seq(0, 10, by = 1)) + 
scale_y_continuous(name = "Axis in 50s",breaks = seq(0, 600, by = 50)) 
p

p<-ggplot(data = soap_data, aes(x=CS) ) + 
  geom_bar(fill = "#0073C2FF")+
  theme_pubclean()  + 
  scale_y_continuous(name = "Axis in 50s",breaks = seq(0, 600, by = 50)) 
p

# checking total NAs in df
sum(is.na(soap_data))
colMeans(is.na(soap_data))[colMeans(is.na(soap_data))>0] 

# We still have to handle NAs in HS ie number of members in households
# lets replace NAs with average of HS
names(soap_data)
HS_mean <- round(mean(soap_data$HS, na.rm=TRUE))
soap_data <- soap_data %>% replace_na(list(HS = HS_mean))
colMeans(is.na(soap_data))[colMeans(is.na(soap_data))>0] 

######## Now we don't have any NAs in our dataset  #############
################################################################

################## Summarize Demographic data###################
names(soap_data)
library(broom)
library(clipr)
library(magrittr)

########################## group_by SEC #######################
soap_data %>% group_by(SEC) %>% tally() %>% tibble() %>% write_clip()
soap_data %>% group_by(SEC) %>% summarise_all(list(mean)) %>% tibble() %>% write_clip()
soap_data %>% group_by(SEC) %>% summarise(FEH = mean(FEH)) %>%
  ggplot(., aes(x = (SEC), y= (FEH)))+ geom_line(stat="identity")

soap_data %>% group_by(SEC) %>% summarise(EDU = mean(EDU))
  ggplot(., aes(x = (SEC), y= (EDU)))+ geom_line(stat="identity")

soap_data %>% group_by(SEC) %>% summarise(EDU = mean(EDU)) %>%
  ggplot(., aes(x = (SEC), y= (EDU)))+ geom_line(stat="identity")

soap_data %>% group_by(SEC) %>% summarise(CS = mean(CS)) %>%
  ggplot(., aes(x = (SEC), y= (CS)))+ geom_line(stat="identity")

soap_data %>% group_by(SEC) %>% summarise(Affluence_Index = mean(Affluence_Index)) %>%
  ggplot(., aes(x = (SEC), y= (Affluence_Index)))+ geom_line(stat="identity")

soap_data %>% group_by(SEC) %>% summarise(EDU = mean(EDU)) %>%
  ggplot(., aes(x = (SEC), y= (EDU)))+ geom_line(stat="identity")

########################## group_by FEH #######################
soap_data %>% group_by(FEH) %>% tally() %>% tibble() %>% write_clip()
soap_data %>% group_by(FEH) %>% summarise_all(list(mean)) %>% tibble() %>% write_clip()
soap_data %>% group_by(SEC) %>% summarise(FEH = mean(FEH)) %>%
  ggplot(., aes(x = (FEH), y= (SEC)))+ geom_line(stat="identity")

soap_data %>% group_by(FEH) %>% summarise(EDU = mean(EDU)) %>%
  ggplot(., aes(x = (FEH), y= (EDU)))+ geom_line(stat="identity")

soap_data %>% group_by(FEH) %>% summarise(CS = mean(CS)) %>%
  ggplot(., aes(x = (FEH), y= (CS)))+ geom_line(stat="identity")

soap_data %>% group_by(FEH) %>% summarise(Affluence_Index = mean(Affluence_Index)) %>%
  ggplot(., aes(x = (FEH), y= (Affluence_Index)))+ geom_line(stat="identity")

soap_data %>% group_by(FEH) %>% summarise(Affluence_Index = mean(Affluence_Index)) %>%
  ggplot(., aes(x = (FEH), y= (Affluence_Index)))+ geom_line(stat="identity")

########################## group_by EDU #######################
soap_data %>% group_by(EDU) %>% tally() %>% tibble() %>% write_clip()
soap_data %>% group_by(EDU) %>% summarise_all(list(mean)) %>% tibble() %>% write_clip()
soap_data %>% group_by(EDU) %>% summarise(FEH = mean(FEH)) %>%
  ggplot(., aes(x = (EDU), y= (FEH))) + geom_line(stat="identity")

soap_data %>% group_by(EDU) %>% summarise(SEC = mean(SEC)) %>%
  ggplot(., aes(x = (EDU), y= (SEC)))+ geom_line(stat="identity")

soap_data %>% group_by(EDU) %>% summarise(CS = mean(CS)) %>%
  ggplot(., aes(x = (EDU), y= (CS)))+ geom_line(stat="identity")

soap_data %>% group_by(EDU) %>% summarise(Affluence_Index = mean(Affluence_Index)) %>%
  ggplot(., aes(x = (EDU), y= (Affluence_Index)))+ geom_line(stat="identity")

########################## group_by CS #######################
soap_data %>% group_by(CS) %>% tally()
soap_data %>% group_by(CS) %>% summarise_all(list(mean)) %>% tibble() %>% write_clip()

soap_data %>% group_by(CS) %>% summarise(FEH = mean(FEH)) %>%
  ggplot(., aes(x = (CS), y= (FEH)))+ geom_line(stat="identity")

soap_data %>% group_by(CS) %>% summarise(SEC = mean(SEC)) %>%
  ggplot(., aes(x = (CS), y= (SEC)))+ geom_line(stat="identity")

soap_data %>% group_by(CS) %>% summarise(EDU = mean(EDU)) %>%
  ggplot(., aes(x = (CS), y= (EDU)))+ geom_line(stat="identity")

soap_data %>% group_by(CS) %>% summarise(Affluence_Index = mean(Affluence_Index)) %>%
  ggplot(., aes(x = (CS), y= (Affluence_Index)))+ geom_line(stat="identity")

########################## group_by Affluence Index #######################
soap_data %>% group_by(Affluence_Index) %>% tally()
soap_data %>% group_by(Affluence_Index) %>% summarise_all(list(mean)) %>% tibble() %>% write_clip()

soap_data %>% group_by(Affluence_Index) %>% summarise(FEH = mean(FEH)) %>%
  ggplot(., aes(x = (Affluence_Index), y= (FEH)))+ geom_line(stat="identity")

soap_data %>% group_by(Affluence_Index) %>% summarise(SEC = mean(SEC)) %>%
  ggplot(., aes(x = (Affluence_Index), y= (SEC)))+ geom_line(stat="identity")

soap_data %>% group_by(Affluence_Index) %>% summarise(CS = mean(CS)) %>%
  ggplot(., aes(x = (Affluence_Index), y= (CS)))+ geom_line(stat="identity")

soap_data %>% group_by(Affluence_Index) %>% summarise(EDU = mean(EDU)) %>%
  ggplot(., aes(x = (Affluence_Index), y= (EDU)))+ geom_line(stat="identity")

soap_data %>% group_by(Affluence_Index) %>% summarise(Brand_Runs = mean(Brand_Runs)) %>%
  ggplot(., aes(x = (Affluence_Index), y= (Brand_Runs)))+ geom_line(stat="identity")

soap_data %>% group_by(Affluence_Index) %>% summarise(Trans___Brand_Runs = mean(Trans___Brand_Runs)) %>%
  ggplot(., aes(x = (Affluence_Index), y= (Trans___Brand_Runs)))+ geom_line(stat="identity")

##########################################################################
head(soap_data$Affluence_Index)
max(soap_data$Affluence_Index)

#soap_d1 <- head(soap_data,500)
boxplot(soap_data$SEC, soap_data$FEH, soap_data$EDU, soap_data$CS, soap_data$Affluence_Index)
boxplot(soap_data$SEC, soap_data$FEH, soap_data$EDU, soap_data$MT, soap_data$SEX, soap_data$AGE, soap_data$EDU, soap_data$HS, soap_data$CHIILD, soap_data$CS, soap_data$Affluence_Index)
# boxplot(soap_data$No__of_Brands, soap_data$Brand_Runs, soap_data$No__of__Trans, soap_data$Trans___Brand_Runs, soap_data$Avg__Price)
# 
# boxplot(soap_data$SEC, soap_data$SEX)
# 
# plot(density(soap_data$SEC, soap_data$FEH, soap_data$EDU, soap_data$CS, soap_data$Affluence_Index))
# plot(density(soap_data$SEC))
# plot(density(soap_data$FEH))
# plot(density(soap_data$EDU))
# plot(density(soap_data$CS))
# plot(density(soap_data$Affluence_Index))
# plot(density(soap_data$No__of_Brands))
# plot(density(soap_data$Trans___Brand_Runs))
###########################################################################
#The data with '%' in values are read in as 'chr' type -change these to numeric
soap_data[20:46]<-lapply(soap_data[20:46], function(x) as.numeric(sub("%", "e-2", x)))
bsd<-soap_data

str(soap_data)
#for brLoyalty, suppose we calculate maxBras max of purchase by different major brand (exclothers)
bsd<-bsd%>% rowwise() %>% mutate(maxBr=max(Br__Cd__57__144, Br__Cd__55, Br__Cd__272, Br__Cd__286, Br__Cd__24, Br__Cd__481, Br__Cd__352, Br__Cd__5))

str(bsd)
bsd_bk <- bsd    # backup

#bsd <-  bsd_bk   # 
#########################
#for clustering on "purchase behavior" variables
PURCHASE_BEHAVIOR <-c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value', 
                      'Trans___Brand_Runs', 'Vol_Tran', 'Avg__Price', 'maxBr', 'Others_999')


x<-bsd

PURCHASE_BEHAVIOR

xpb <- x %>% select(PURCHASE_BEHAVIOR) %>% scale()  
# xpb is scaled data as input for determining number of k. See next line of code

library(NbClust)
library(factoextra)

fviz_nbclust(xpb, kmeans, method = "wss", print.summary = TRUE)
fviz_nbclust(xpb, kmeans, method = "silhouette", print.summary = TRUE)
fviz_nbclust(xpb2, kmeans, method = "gap_stat", print.summary = TRUE)
######### with k = 3
kmClus_pb<- x %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kmeans(centers=3, nstart=25)

kmClus_pb

library(factoextra)
#visualize the cluster -based on variables used for clustering
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))

### Describing the clusters
class(kmClus_pb$cluster)
class(x)

#kmClus1_list <- as.list(kmClus_pb$cluster)
#class(kmClus1_list)

x_des <- x
#df_c <- as.data.frame(do.call(rbind, kmClus_pb$cluster))
#x_des <- x %>% mutate(clusKM=kmClus_pb$cluster)  # did not run 

x_des$clusKM <- kmClus_pb$cluster #did run

names(x_des)

x_des %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 
                                              'maxBr', 'No__of_Brands', 'No__of__Trans', 
                                              'Brand_Runs', 'Total_Volume', 'Value', 
                                              'Trans___Brand_Runs'), mean, ) %>% view()


######### with k = 4
kmClus_pb<- x %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kmeans(centers=4, nstart=25)

kmClus_pb

library(factoextra)
#visualize the cluster -based on variables used for clustering
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))

######### with k = 6
kmClus_pb<- x %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kmeans(centers=6, nstart=25)

kmClus_pb

library(factoextra)
#visualize the cluster -based on variables used for clustering
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))


# clusters with 2 variables - brand loyalty and total volume
# k =3 
fviz_cluster(kmClus_pb, data=x %>% select(maxBr, Total_Volume))

##################################################################################
################### Clustering based on "basis of purchase"  #####################

toString(names(bsd_bk))

bsd2 <- bsd_bk 

BASIS_OF_PUR <- c( 'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__', 'Pur_Vol_Other_Promo__',  
'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4', 
'PropCat_5', 'PropCat_6', 'PropCat_7', 'PropCat_8', 
'PropCat_9', 'PropCat_10', 'PropCat_11', 'PropCat_12', 'PropCat_13', 'PropCat_14', 'PropCat_15')

#x2 <- bsd2

xpb2 <- bsd2 %>% select(BASIS_OF_PUR) %>% scale()  # xpb as input for determining number of k. See next lines of code

library(NbClust)
library(factoextra)

fviz_nbclust(xpb2, kmeans, method = "wss", print.summary = TRUE)
fviz_nbclust(xpb2, kmeans, method = "silhouette", print.summary = TRUE)
fviz_nbclust(xpb2, kmeans, method = "gap_stat", print.summary = TRUE)
######### with k = 3 based on Optimal number of clusters graph
kmClus_BoP<- bsd2 %>% select(BASIS_OF_PUR) %>% 
  scale() %>% kmeans(centers=3, nstart=25)

kmClus_BoP

library(factoextra)
#visualize the cluster -based on variables used for clustering
fviz_cluster(kmClus_BoP, data=bsd2 %>% select(BASIS_OF_PUR))

######### with k = 4 based on Optimal number of clusters graph
kmClus_BoP<- bsd2 %>% select(BASIS_OF_PUR) %>% 
  scale() %>% kmeans(centers=4, nstart=25)

kmClus_BoP

library(factoextra)
#visualize the cluster -based on variables used for clustering
fviz_cluster(kmClus_BoP, data=bsd2 %>% select(BASIS_OF_PUR))

###################################################################

# Clustering based on variables that describe both "purchase behavior" and "basis of purchase".

BOTH_SETS <- c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value', 
               'Trans___Brand_Runs', 'Vol_Tran', 'Avg__Price', 'maxBr', 'Others_999','Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__', 'Pur_Vol_Other_Promo__',  
               'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4', 
               'PropCat_5', 'PropCat_6', 'PropCat_7', 'PropCat_8', 
               'PropCat_9', 'PropCat_10', 'PropCat_11', 'PropCat_12', 'PropCat_13', 'PropCat_14', 'PropCat_15')

xpb_both <- bsd %>% select(BOTH_SETS) %>% scale()  # xpb as input for determining number of k. See next lines of code

fviz_nbclust(xpb_both, kmeans, method = "wss", print.summary = TRUE)
fviz_nbclust(xpb_both, kmeans, method = "silhouette", print.summary = TRUE)
fviz_nbclust(xpb2, kmeans, method = "gap_stat", print.summary = TRUE)
######### with k = 3 based on Optimal number of clusters graph
kmClus_BOTH<- bsd %>% select(BOTH_SETS) %>% 
  scale() %>% kmeans(centers=3, nstart=25)

kmClus_BOTH

library(factoextra)
#visualize the cluster -based on variables used for clustering
fviz_cluster(kmClus_BOTH, data=bsd %>% select(BOTH_SETS))

####################################################################################################

############## Clustering based on Purchase Behavior including some other set of variables
#Other variables include :
# 'Affluence_Index', 'FEH_3', 'FEH_2', 'FEH_1', 'SEC_4', 'SEC_3', 'SEC_2', 'TV_1', 'TV_2
str(bsd)
#str(soap_data)

#Examine the data even though values are recorded as numbers, can all attributes be considered as 'numeric' ?
summary(as.factor(bsd$FEH))

#convert this to dummies, since the values are not ordinal, and remove the '0' level dummy
bsd<-bsd%>% mutate(fehDummy=1) %>% pivot_wider(names_from= FEH, values_from= fehDummy, names_prefix= "FEH_", values_fill= list(fehDummy=0))
bsd<-bsd%>% select(-'FEH_0') # can append this to the last line of code if you want

head(bsd) %>% view()

#explore MT (language spoken -mother tongue)
summary(as.factor(bsd$MT)) #Many of the category levels have very few values..so
#keep levels 0, 4, 5, 10, 17 as dummies, with 0 in the dummies indicating 'other'
bsd<-bsd%>% mutate(MT=if_else(MT %in% c(0, 4, 5, 10, 17), MT, -1))
bsd<-bsd%>% mutate(mtDummy=1) %>% pivot_wider(names_from= MT, values_from= mtDummy, names_prefix= "MT_", values_fill= list(mtDummy=0))
bsd<-bsd%>% select(-`MT_-1`)

head(bsd) %>% view()  # NOTE: The view() shows at max 50 columns at a time. Use right arrow in view to see columns beyond 50

# convert SEC data to dummies
bsd<-bsd%>% mutate(secDummy=1) %>% pivot_wider(names_from= SEC, values_from= secDummy, names_prefix= "SEC_", values_fill= list(secDummy=0))
bsd<-bsd%>% select(-'SEC_1')

head(bsd) %>% view()

# convert TV_Own data into dummies
bsd<-bsd%>% mutate(TVDummy=1) %>% pivot_wider(names_from= CS, values_from= TVDummy, names_prefix= "TV_", values_fill= list(TVDummy=0))
bsd<-bsd%>% select(-'TV_0')

head(bsd) %>% view()

toString(names(bsd))

PURCHASE_BEHAVIOR_1 <-c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value', 
                      'Trans___Brand_Runs', 'Vol_Tran', 'Avg__Price', 'maxBr', 'Others_999','Affluence_Index',
                      'FEH_3', 'FEH_2', 'FEH_1', 'SEC_4', 'SEC_3', 'SEC_2', 'TV_1', 'TV_2')


#x<-bsd
xpb3 <- bsd %>% select(PURCHASE_BEHAVIOR_1) %>% scale()  # xpb as input for determining number of k. See next line of code

head(xpb3)

fviz_nbclust(xpb3, kmeans, method = "wss", print.summary = TRUE)
fviz_nbclust(xpb2, kmeans, method = "silhouette", print.summary = TRUE)
fviz_nbclust(xpb2, kmeans, method = "gap_stat", print.summary = TRUE)
######### with k = 5 based on the Optimal number of clusters graph
kmClus_pb1<- bsd %>% select(PURCHASE_BEHAVIOR_1) %>% 
  scale() %>% kmeans(centers=4, nstart=25)

kmClus_pb1

library(factoextra)
#visualize the cluster -based on variables used for clustering
fviz_cluster(kmClus_pb1, data=bsd %>% select(PURCHASE_BEHAVIOR_1))


### Describing the clusters
class(kmClus_pb1$cluster)
class(x)

#kmClus1_list <- as.list(kmClus_pb$cluster)
#class(kmClus1_list)

x_des1 <- x
#df_c <- as.data.frame(do.call(rbind, kmClus_pb$cluster))
#x_des <- x %>% mutate(clusKM=kmClus_pb$cluster)  # did not run 

x_des1$clusKM1 <- kmClus_pb1$cluster #did run

names(x_des1)

x_des1 %>% group_by(clusKM1) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 
                                              'maxBr', 'No__of_Brands', 'No__of__Trans', 
                                              'Brand_Runs', 'Total_Volume', 'Value', 
                                              'Trans___Brand_Runs'), mean, ) %>% view()

##########################################################################################
###########################################################################################

# Hierarchcial Clustering
library(cluster)

#xpb <- x %>% select(PURCHASE_BEHAVIOR) %>% scale()  
# xpb is scaled data

xdist <- dist(xpb, method = "euclidean")

#using hclust
hierC_pb <- hclust(xdist, method ="average" )
plot(hierC_pb, cex=0.3, hang=-3, main="hclust-average")

hierC_pb_c <- hclust(xdist, method = "complete" )
plot(hierC_pb_c, cex=0.3, hang=-3, main= 'hclust-complete')

hierC_pb_w<-hclust(xdist, method = "ward.D" )
plot(hierC_pb_w, cex=0.3, hang=-3, main= 'hclust-WardD')

#using agnes from the cluster package
hierC_pb_ag_a <- agnes(xdist, method = "average" )
#check the agglomerative coeffgiven by agnes
hierC_pb_ag_a$ac
plot(hierC_pb_ag_a,  which.plot = 2)
plot(hierC_pb_ag_a, which.plot = 2, main="agnes-average")

hierC_pb_ag_c <- agnes(xdist, method = "complete" )
#check the agglomerative coeffgiven by agnes
hierC_pb_ag_c$ac
#plot(hierC_pb_ag_c,  which.plot = 2)
plot(hierC_pb_ag_c, which.plot = 2, main="agnes-complete")

hierC_pb_ag_w<-agnes(xdist, method = "ward")
hierC_pb_ag_w$ac
plot(hierC_pb_ag_w, which.plot = 2, main="agnes-ward")

#check columns with full NA
#colMeans(is.na(soap_data))[colMeans(is.na(soap_data))==1] 
# there is no column with full NAs

# https://dplyr.tidyverse.org/reference/rowwise.html

#colMeans(is.na(soap_data))==1

############# DBCSAN clustering
#list of variables
#1 PURCHASE_BEHAVIOR
#2 BASIS_OF_PUR
#3 BOTH_SETS

# bsd
#Plot the points
data("bsd")
str(bsd)
#Just plot
bsd%>% ggplot(aes(x=Brand_Runs,y= Total_Volume, col=as.factor(SEX)))+geom_point()
bsd%>% ggplot(aes(x=Affluence_Index ,y= Total_Volume, col=as.factor(SEX)))+geom_point()

install.packages(ANN)
install.packages(dbscan)
library(ANN)
library(dbscan)
library(factoextra)
#try different esp = 0.5,0.4,0.3 etc
msDbscan<-dbscan(bsd[,1:26], eps = 0.5, minPts= 5)
fviz_cluster(msDbscan, data=bsd[,1:26], geom="point", ellipse = FALSE, main="dbscan", eps=0.5, minPts=5)

# Establish search parameters.
# k <- c(25, 50, 100, 200, 500, 1000)
# eps <- c(0.001, 0.01, 0.02, 0.05, 0.1, 0.2)
# # Perform grid search.
# grid <- expand.grid(k = k, eps = eps)
# results <- mapply(grid$k, grid$eps, FUN = function(k, eps) {
#   cluster <- dbscan(bsd[,1:26], minPts = k, eps = eps)$cluster
#   sum <- table(cluster)
#   cat(c("k =", k, "; eps =", eps, ";", sum, "\n"))
# })

msDbscan<-dbscan(bsd[,1:26], eps = 0.1, minPts= 5)
fviz_cluster(msDbscan, data=bsd[,1:26], geom="point", ellipse = FALSE, main="dbscan", eps=0.1, minPts=5)

msDbscan<-dbscan(bsd[,1:26], eps = 0.01, minPts= 5)
fviz_cluster(msDbscan, data=bsd[,1:26], geom="point", ellipse = FALSE, main="dbscan", eps=0.01, minPts=5)

msDbscan<-dbscan(bsd[,1:26], eps = 0.9, minPts= 20)
fviz_cluster(msDbscan, data=bsd[,1:26], geom="point", ellipse = FALSE, main="dbscan", eps=0.9, minPts=20)

msDbscan<-dbscan(bsd[,1:26], eps = 0.9, minPts= 50)
fviz_cluster(msDbscan, data=bsd[,1:26], geom="point", ellipse = FALSE, main="dbscan", eps=0.9, minPts=50)
############# Kernel k-means  ###############################################
#list of variables
#1 PURCHASE_BEHAVIOR
#2 BASIS_OF_PUR
#3 BOTH_SETS
library(kernlab)

# Kernel k-means = 2
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=2, nstart=25)

#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 2
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=2, nstart=25, kernel = 'rbfdot', kpar=list(sigma=0.1))
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 2
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=2, nstart=25, kernel = 'rbfdot', kpar=list(sigma=0.01))
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 2
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=2, nstart=25, kernel = 'rbfdot', kpar=list(sigma=0.001))
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 2
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=2, nstart=25, kernel = 'polydot')
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 2
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=2, kernel='vanilladot')
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 3
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=3, nstart=25)
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 3
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=3, nstart=25, kernel = 'rbfdot', kpar=list(sigma=0.001))
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 3
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=3, nstart=25, kernel = 'rbfdot', kpar=list(sigma=0.01))
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 4
kkm_PURCHASE_BEHAVIOR <- bsd %>% select(PURCHASE_BEHAVIOR) %>% 
  scale() %>% kkmeans(centers=4, nstart=25)
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_PURCHASE_BEHAVIOR@.Data), geom = "point",main="kkmeans")

### BASIS_OF_PUR ###

# Kernel k-means = 2
kkm_BASIS_OF_PUR <- bsd %>% select(BASIS_OF_PUR) %>% 
  scale() %>% kkmeans(centers=2, nstart=25)
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_BASIS_OF_PUR@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 3
kkm_BASIS_OF_PUR <- bsd %>% select(BASIS_OF_PUR) %>% 
  scale() %>% kkmeans(centers=3, nstart=25)
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_BASIS_OF_PUR@.Data), geom = "point",main="kkmeans")

### BOTH_SETS ###
# Kernel k-means = 2
kkm_BOTH<- bsd %>% select(BOTH_SETS) %>% 
  scale() %>% kkmeans(centers=2, nstart=25)
str(kkm_BOTH)
#the cluster plot
fviz_cluster(list(data=bsd, cluster=kkm_BOTH@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 3
kkm_BOTH<- bsd %>% select(BOTH_SETS) %>% 
  scale() %>% kkmeans(centers=3, nstart=25)
str(kkm_BOTH)
#the cluster assignments for examples is in kkc_pb@.Data
fviz_cluster(list(data=bsd, cluster=kkm_BOTH@.Data), geom = "point",main="kkmeans")

# Kernel k-means = 4
kkm_BOTH<- bsd %>% select(BOTH_SETS) %>% 
  scale() %>% kkmeans(centers=4, nstart=25)
str(kkm_BOTH)
#the cluster assignments for examples is in kkc_pb@.Data
fviz_cluster(list(data=bsd, cluster=kkm_BOTH@.Data), geom = "point",main="kkmeans")




