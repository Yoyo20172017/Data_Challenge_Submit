bank=read.csv("C:/Users/MU/Desktop/Apple Data Challenge/Data/bank-additional/bank-additional/bank-additional-full.csv",sep=";",header=TRUE)

list.of.packages <- c("ggplot2", "dplyr","MASS","xtable","dplyr","psych","stringr","car","faraway","popbio","gdata","reshape","randomForest","boot","caret","neuralnet","pscl","prediction","knitr","rpart","rpart.plot","rattle","prediction")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(MASS)
library(knitr)
library(xtable)
library(dplyr)
library(psych)
library(stringr)
library(faraway)
library(Rcpp)
library(ROCR)
library(Amelia)
library(popbio)
library(gdata)
library(reshape)
library(rpart)
library(randomForest)
library(boot)
library(ResourceSelection)
library(pscl)
library(prediction)
library(knitr)
library(rpart)
library(rpart.plot)
library(rattle)
library(prediction)


#####################################Missing Cell Check#############################################
newdata <- na.omit(bank)
nrow(newdata)==nrow(bank)

#################################box plot and hist plot#############################################
par(mfrow=c(2,4))
boxplot(bank$age~bank$y, main=" AGE",ylab="age of customers",xlab="Subscribed")
boxplot(bank$duration~bank$y, main="LAST DURATION",ylab="Last duration of contact",xlab="Subscribed")
boxplot(bank$campaign~bank$y, main="NUM CONTACTS",ylab="number of contacts",xlab="Subscribed")
boxplot(bank$pdays~bank$y, main=" Previous DAYS",ylab="Previous days of contact",xlab="Subscribed")
boxplot(bank$previous~bank$y, main=" Previous Contacts",ylab="Previous Contacts with customers",xlab="Subscribed")
boxplot(bank$emp.var.rate~bank$y, main=" Employment variation rate",ylab="Employment variation rate",xlab="Subscribed")
boxplot(bank$cons.price.idx~bank$y, main=" Consumer price index",ylab="Consumer price index",xlab="Subscribed")
boxplot(bank$euribor3m~bank$y, main=" Euribor 3 month rate",ylab="Euribor 3 month rate",xlab="Subscribed")

hist(bank$age, col = "orange", freq = FALSE)
hist(bank$duration, col = "orange", freq = FALSE)
hist(bank$campaign, col = "orange", freq = FALSE)
hist(bank$pdays, col = "orange", freq = FALSE)
hist(bank$previous, col = "orange", freq = FALSE)
hist(bank$emp.var.rate, col = "orange", freq = FALSE)
hist(bank$cons.price.idx, col = "orange", freq = FALSE)
hist(bank$euribor3m, col = "orange", freq = FALSE)

#############################################Data Clean##############################################

#write.csv(bank,"C:/Users/MU/Desktop/Apple Data Challenge/Data/bank-additional-full.csv")
##numeric variables
bank$age <- as.numeric(bank$age)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)
bank$emp.var.rate <- as.numeric(bank$emp.var.rate)
bank$cons.price.idx <- as.numeric(bank$cons.price.idx)
bank$cons.conf.idx <- as.numeric(bank$cons.conf.idx)
bank$euribor3m <- as.numeric(bank$euribor3m)
bank$nr.employed <- as.numeric(bank$nr.employed)


#update response variable to binary values of 0 and 1
#levels(bank$y)
levels(bank$y) <- c(0, 1)
bank$y <- as.numeric(levels(bank$y))[bank$y]
#str(bank)

#create dummy variables for job values
for(level in unique(bank$job)){
  bank[paste("job", level, sep = "_")] <- ifelse(bank$job == level, 1, 0)
}
#Delete original catagorical variable
bank$job <- NULL
#head(bank)

for(level in unique(bank$marital)){
  bank[paste("marital", level, sep = "_")] <- ifelse(bank$marital == level, 1, 0)
}
bank$marital <- NULL

##education
#education_None
bank$education_illiterate <- as.numeric(ifelse(bank$education == "illiterate", 1, 0))

#education_Unknown
bank$education_unknown <-as.numeric(ifelse(bank$education == "unknown", 1, 0))

#education_Primary
bank$education_primary <- as.numeric(ifelse(bank$education == "basic.4y" | bank$education == "basic.6y", 1, 0))

#education_Secondary
bank$education_secondary <- as.numeric(ifelse(bank$education == "basic.9y" | bank$education == "high.school", 1, 0))

#education_Tertiary
bank$education_tertiary <- as.numeric(ifelse(bank$education == "professional.course" | bank$education == "university.degree", 1, 0))

#Delete original catagorical variable
bank$education <- NULL

# default
for(level in unique(bank$default)){
  bank[paste("default", level, sep = "_")] <- ifelse(bank$default == level, 1, 0)
}
bank$default <- NULL

#housing
for(level in unique(bank$housing)){
  bank[paste("housing", level, sep = "_")] <- ifelse(bank$housing == level, 1, 0)
}
bank$housing <- NULL

#personal loans
for(level in unique(bank$loan)){
  bank[paste("loan", level, sep = "_")] <- ifelse(bank$loan == level, 1, 0)
}
bank$loan <- NULL

# contact
for(level in unique(bank$contact)){
  bank[paste("contact", level, sep = "_")] <- ifelse(bank$contact == level, 1, 0)
}
bank$contact <- NULL

#month
for(level in unique(bank$month)){
  bank[paste("month", level, sep = "_")] <- ifelse(bank$month == level, 1, 0)
}
bank$month <- NULL

#levels(bank$day_of_week)
for(level in unique(bank$day_of_week)){
  bank[paste("day_of_week", level, sep = "_")] <- ifelse(bank$day_of_week == level, 1, 0)
}
bank$day_of_week <- NULL

#dummy variable for pdays -previous contact yes or no ; 1 or 0  when 999
bank$previous_contact <- as.numeric(ifelse(bank$pdays == 999, 0, 1))

#levels(bank$poutcome)
for(level in unique(bank$poutcome)){
  bank[paste("poutcome", level, sep = "_")] <- ifelse(bank$poutcome == level, 1, 0)
}
bank$poutcome <- NULL


###########################Training and testing split##################################################
#training and testing data will have same ratio for target variable
library(caret)
#Rows selection for training data set
inTrain <- createDataPartition(y=bank$y ,p=0.75,list=FALSE) ## 75% is training data, 25% is testing data
bank_train<- bank[inTrain,]
bank_test <- bank[-inTrain,]

# We can see imbalancing has been taken care of 
table(bank_train$y); table(bank_test$y)

#######################################################################################################
bank_train_set<-bank_train
bank_test_set<-bank_test

# update "-" in train set
colnames(bank_train_set)[15]<-c("job_blue_collar")
colnames(bank_train_set)[20]<-c("job_self_employed")

# remove"-" from variable to avoid any issues in running the model in test
colnames(bank_test_set)[15]<-c("job_blue_collar")
colnames(bank_test_set)[20]<-c("job_self_employed")

#####################################outlier analysis###################################################
install.packages("reshape2")
library("reshape")

mdata<- select(bank_train_set,  age, previous, duration, campaign, emp.var.rate, cons.price.idx, euribor3m,nr.employed)
mdata2 <- melt(mdata)

# Output the boxplot
p <- ggplot(data = mdata2, aes(x=variable, y=value)) + 
  geom_boxplot() + ggtitle("Outliers Identification")
p + facet_wrap( ~ variable, scales="free", ncol=4)

bank_train_set <- bank_train_set %>%
  select(-y, everything())

fun1 <- function(a, y) cor(y, a)
x<-bank_train_set[,]
Correlation <- sapply(x, FUN = fun1, y=bank_train_set$y) 

Correlation <- sort(Correlation, decreasing = TRUE)
vars <- names(Correlation)

par(mfrow=c(2,4))
for (i in 2:ncol(bank_train_set)) {
  hist(bank_train_set[,vars[i]], main = vars[i], xlab = "")
}

#######################################Logistics Regression- Model#########################################

bank_train_set$y<-as.factor(bank_train_set$y)
modelLG <- glm(y ~.,family=binomial,data=na.omit(bank_train_set))
#summary(modelLG) #AIC 13778
#anova(modelLG, test="Chisq")

#modelLG_1<-glm(y ~.-duration-campaign-pdays-previous-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed-contact_telephone-contact_cellular-month_may-month_jun-month_jul-month_aug-month_oct-month_nov-month_dec-month_mar-month_apr-month_sep-day_of_week_mon-day_of_week_tue-day_of_week_wed-day_of_week_thu-day_of_week_fri-previous_contact-poutcome_nonexistent-poutcome_failure-poutcome_success,family=binomial,data=na.omit(bank_train_set))
##AIC 22252
#modelLG_2<-glm(y ~.-duration-campaign-pdays-previous-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed-contact_telephone-contact_cellular-month_may-month_jun-month_jul-month_aug-month_oct-month_nov-month_dec-month_mar-month_apr-month_sep-previous_contact-poutcome_nonexistent-poutcome_failure-poutcome_success,family=binomial,data=na.omit(bank_train_set))
#AIC 22228
#modelLG_3<-glm(y ~.-duration-campaign-pdays-previous-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed-contact_telephone-contact_cellular-previous_contact-poutcome_nonexistent-poutcome_failure-poutcome_success,family=binomial,data=na.omit(bank_train_set))
#AIC 21072
#modelLG_4<-glm(y ~.-duration-campaign-pdays-previous-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed-contact_telephone-contact_cellular-month_may-month_jun-month_jul-month_aug-month_nov-month_mar-month_apr-month_sep-day_of_week_mon-day_of_week_tue-day_of_week_wed-day_of_week_thu-day_of_week_fri-previous_contact-poutcome_nonexistent-poutcome_failure-poutcome_success,family=binomial,data=na.omit(bank_train_set))
#AIC 21955
#modelLG_5<-glm(y ~.-duration-campaign-pdays-previous-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed-contact_telephone-contact_cellular-education_illiterate-education_unknown-education_primary-education_secondary-education_tertiary-month_may-month_jun-month_jul-month_aug-month_oct-month_nov-month_dec-month_mar-month_apr-month_sep-day_of_week_mon-day_of_week_tue-day_of_week_wed-day_of_week_thu-day_of_week_fri-previous_contact-poutcome_nonexistent-poutcome_failure-poutcome_success,family=binomial,data=na.omit(bank_train_set))
#AIC 22282
#modelLG_6<-glm(y ~.-duration-campaign-pdays-previous-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed-contact_telephone-contact_cellular-education_illiterate-education_unknown-education_primary-education_secondary-education_tertiary-month_may-month_jun-month_jul-month_aug-month_oct-month_nov-month_dec-month_mar-month_apr-month_sep-marital_married-marital_single-marital_divorced-marital_unknown-previous_contact-poutcome_nonexistent-poutcome_failure-poutcome_success,family=binomial,data=na.omit(bank_train_set))
#AIC 22318
#modelLG_7<-glm(y ~.-duration-campaign-pdays-previous-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed-contact_telephone-contact_cellular-education_illiterate-education_unknown-education_primary-education_secondary-education_tertiary-month_may-month_jun-month_jul-month_aug-month_oct-month_nov-month_dec-month_mar-month_apr-month_sep-marital_married-marital_single-marital_divorced-marital_unknown-previous_contact-poutcome_nonexistent-poutcome_failure-poutcome_success,family=binomial,data=na.omit(bank_train_set))
#AIC 22320

modelLG_update<-glm(y ~.-duration-campaign-pdays-previous-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed-contact_telephone-contact_cellular-education_illiterate-education_unknown-education_primary-education_secondary-education_tertiary-month_may-month_jun-month_jul-month_aug-month_oct-month_nov-month_dec-month_mar-month_apr-month_sep-marital_married-marital_single-marital_divorced-marital_unknown-previous_contact-poutcome_nonexistent,family=binomial,data=na.omit(bank_train_set))
#AIC 19484

# exp(coef(modelLG_update))
# Cross validation of model for K=10

t1<-cv.glm(bank_train_set, modelLG_update, K = 10)$call
cv.glm(data = bank_train_set, glmfit = modelLG_update, K = 10)

#plot(modelLG_update)
##Using model for testing 
testing.prob<-predict.glm(modelLG_update,bank_test_set[,1:62],type="response")
Prediction<-cut(testing.prob,c(-Inf,0.5,Inf),labels=c(0,1))
#summary(Prediction)
##Confusion Matrix
confusion.matrix <-table(testing.prediction,bank_test_set$y)

######################################Decesion Tree Model#####################################################

modelDT <- rpart(y~., data=na.omit(bank_train_set), method = "class")
##updated decision tree model
modelDT_update <- prune(modelDT, cp = modelDT$cptable[which.min(modelDT$cptable[,"xerror"]),"CP"])
#printcp(modelDT_update) # display the results 

fancyRpartPlot(modelDT_update)

predictions <- predict(modelDT_update, bank_test_set, type = "class")
confusion.matrix <- table(predictions, bank_test_set$y)
#accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2] 

######################################random Forest model######################################################

modelRF <- randomForest(as.factor(y) ~ .,data=bank_train_set,importance=TRUE, ntree=50)
randForest_Tree_Size_50 <- randomForest(as.factor(y) ~ .,data=bank_train_set,importance=TRUE, ntree=50)
# print(modelRF) #view results 

modelRF100 <- randomForest(as.factor(y) ~ .,data=bank_train_set,importance=TRUE, ntree=100)
randForest_Tree_Size_100 <- randomForest(as.factor(y) ~ .,data=bank_train_set,importance=TRUE, ntree=100)

##plot importance variables
varImpPlot(modelRF)

par(mfrow=c(2,4))

#layout(matrix(c(1,2),nrow=1),
#width=c(1,1)) 
par(mar=c(1,1,2,0)) #No margin on the right side
plot(randForest_Tree_Size_50, log="y")
par(mar=c(3,3,1,1)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(randForest_Tree_Size_50$err.rate),col=1:3,cex=0.8,fill=1:4)

#layout(matrix(c(1,2),nrow=1),
#width=c(1,1)) 
par(mar=c(1,1,2,1)) #No margin on the right side
plot(randForest_Tree_Size_100, log="y")
par(mar=c(1,3,1,1)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(randForest_Tree_Size_100$err.rate),col=1:3,cex=0.8,fill=1:4)

predicted <- predict(modelRF, bank_test_set)
#table(predicted)
#confusionMatrix(predicted, bank_test_set$y)
confusion.matrix <- table(predicted, bank_test_set$y)

#Effect of increasing tree count 
#accuracy=c()
#for (i in seq(1,50, by=1)) {
#  modFit <- randomForest(y ~ ., data=bank_train_set, ntree=i)
#  accuracy <- c(accuracy, confusionMatrix(predict(modFit, bank_test_set, type="class"), bank_test_set$y)$overall[1])
#}
#par(mfrow=c(1,1))
#plot(x=seq(1,50, by=1), y=accuracy, type="l", col="black",
#     main="Accuracy VS Tree-Size", xlab="Tree Size", ylab="Accuracy")


#predicted <- predict(modelRF100, bank_test_set)
#table(predicted)
#confusionMatrix(predicted, bank_test_set$y)	 
#accuracy=c()
#for (i in seq(1,100, by=1)) {
#  modFit <- randomForest(y ~ ., data=bank_train_set, ntree=i)
#  accuracy <- c(accuracy, confusionMatrix(predict(modFit, bank_test_set, type="class"), bank_test_set$y)$overall[1])
#}
#par(mfrow=c(1,1))
#plot(x=seq(1,100, by=1), y=accuracy, type="l", col="black",
#     main="Accuracy VS Tree-Size", xlab="Tree Size", ylab="Accuracy")


##########################################k-NN Model####################################################
install.packages("kknn")
library(kknn)

#modelk_nn<-train.kknn(y~.,bank_train_set,kmax=15,distance=1.5,kernel=c("rectangular","triangular","epanechnikov","biweight","triweight","cos","inv","gaussian","optimal"),ykernal=NULL,scale=TRUE)
modelk_nn<-train.kknn(y~.,bank_train_set,kmax=15,distance=1.5,kernel=c("gaussian"),ykernal=NULL,scale=TRUE)

#plot(modelk_nn)

## Cross validation procedure to test prediction accuracy
##simulation(y~.,bank_train_set,runs=10,kernel="gaussian",k=10,train=TRUE)
## Any kernel can be used for beter modeling purposes

train.kknn=kknn(y~.,train=bank_train_set,test=bank_test_set,k=10,distance=1,kernel="gaussian",ykernel=NULL,scale=TRUE,kknn.dist(learn=train,valid=test,k=10,distance=1))
fit=fitted(train.kknn)
confusion.matrix <-table(predict(modelk_nn,bank_train_set),bank_train_set$y)

######################################Neural Network####################################################
library("neuralnet")

n <- names(bank_train_set)
f <- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + ")))
modelnn <- neuralnet(f,data=bank_train_set,hidden=c(5,3),linear.output=	F)
plot(modelnn)	
library(caret)
predicted <- predict(modelnn, bank_test_set)
table(predicted)
confusionMatrix(predicted, bank_test_set$y)
confusion.matrix <- table(predicted, bank_test_set$y)

#################################Evaluation function######################################################
Eval_Def<-function(x){
    TP<-x$Freq[x$metrics=="TRUE_1"]
    FP<-x$Freq[x$metrics=="FALSE_1"]
    TN<-x$Freq[x$metrics=="FALSE_0"]
    FN<-x$Freq[x$metrics=="TRUE_0"]
    Accuracy <-(TP+TN)/(TP+TN+FP+FN)
    Error_Rate<-(FP+FN)/(TP+TN+FP+FN)
    Precision<-TP/(TP+FP)
    sensitivity<-TP/(TP+FN)
    specificity<-TN/(TN+FP)
    F1_Score=2*Precision*sensitivity/(sensitivity+specificity)
    Eval_Def_result<-data.frame(Accuracy=c(0),Error_Rate=c(0),Precision=c(0),sensitivity=c(0),specificity=c(0),F1_Score=c(0))
    
    Eval_Def_result[1,1]<-Accuracy
    Eval_Def_result[1,2]<-Error_Rate
    Eval_Def_result[1,3]<-Precision
    Eval_Def_result[1,4]<-sensitivity
    Eval_Def_result[1,5]<-specificity
    Eval_Def_result[1,6]<-F1_Score
    Eval_Def_result
}

model_comparison<-data.frame(Accuracy=c(0),Error_Rate=c(0),Precision=c(0),sensitivity=c(0),specificity=c(0),F1_Score=c(0), AUC=c(0))

##########################################ROC Plot, AUC calculation#########################################
#the McFadden R^2 index can be used to assess the model fit.
#pR2(modelLG)

# Logistic regression model
bank_test_set$TARGET_FLAGLG<-predict(modelLG_update,newdata=na.omit(bank_test_set),type='response')
df_pre_train1<-as.data.frame(table(bank_test_set$TARGET_FLAGLG>0.5,bank_test_set$y))
df_pre_train1$metrics <- paste(df_pre_train1$Var1,df_pre_train1$Var2, sep = '_') 
model_comparison[1,]<-Eval_Def(df_pre_train1)
resultsLG<-ifelse(bank_test_set$TARGET_FLAGLG>0.5,1,0)
pr <- prediction(resultsLG, bank_test_set$y)
aucLG<- performance(pr,"auc")
model_comparison[1,c("AUC")]<-c(aucLG@y.values[1])

kable(model_comparison[1,],row.names = TRUE, caption = " Logistic Regression Model Eval_Defuation KPIs")

m <- modelLG_update<-glm(y ~.-duration-campaign-pdays-previous-emp.var.rate-cons.price.idx-cons.conf.idx-euribor3m-nr.employed-contact_telephone-contact_cellular-education_illiterate-education_unknown-education_primary-education_secondary-education_tertiary-month_may-month_jun-month_jul-month_aug-month_oct-month_nov-month_dec-month_mar-month_apr-month_sep-marital_married-marital_single-marital_divorced-marital_unknown-previous_contact-poutcome_nonexistent,family=binomial,data=na.omit(bank_train_set))
hoslem.test(modelLG_update$y, fitted(m))

### Decision tree model
bank_test_set$TARGET_FLAGDT<- predict(modelDT_update,newdata=bank_test_set)[,2]
df_pre_train1<-as.data.frame(table(bank_test_set$TARGET_FLAGDT>0.5,bank_test_set$y))
df_pre_train1$metrics <- paste(df_pre_train1$Var1,df_pre_train1$Var2, sep = '_') 
model_comparison[2,]<-Eval_Def(df_pre_train1)
resultsDT <- predict(modelDT_update,newdata=bank_test_set,type="prob")[,2]
pr <- prediction(resultsDT, bank_test_set$y)
aucDT<- performance(pr,"auc")
model_comparison[2,c("AUC")]<-c(aucDT@y.values[1])

kable(model_comparison[2,],row.names = TRUE, caption = " Decision Tree Model Evaluation KPIs")

# Random Forest model
bank_test_set$TARGET_FLAGRF<-as.numeric(predict(modelRF,newdata=bank_test_set,type='response'))
bank_test_set$TARGET_FLAGRF<-ifelse(bank_test_set$TARGET_FLAGRF==1,0,1)
df_pre_train1<-as.data.frame(table(bank_test_set$TARGET_FLAGRF,bank_test_set$y))
df_pre_train1$Var1<-as.character(df_pre_train1$Var1)
df_pre_train1$Var1[df_pre_train1$Var1==0]<-c("FALSE")
df_pre_train1$Var1[df_pre_train1$Var1==1]<-c("TRUE")
df_pre_train1$metrics <- paste(df_pre_train1$Var1,df_pre_train1$Var2, sep = '_') 
model_comparison[3,]<-Eval_Def(df_pre_train1)
resultsRF <- ifelse(as.numeric(predict(modelRF,newdata=na.omit(bank_test_set),type='response'))==1,0,1)
pr <- prediction(bank_test_set$TARGET_FLAGRF, bank_test_set$y)
aucRF<- performance(pr,"auc")
model_comparison[3,c("AUC")]<-c(aucRF@y.values[1])
kable(model_comparison[3,],row.names = TRUE, caption = " Random Forest Model Evaluation KPIs")

## K-NN Model
#bank_test_set$TARGET_FLAG5<- predict(modelk_nn,newdata=bank_test_set)
##bank_test_set$TARGET_FLAG5<-compute(modelk_nn, bank_test_set[,1:61])
#df_pre_train1<-as.data.frame(table(bank_test_set$TARGET_FLAG5,bank_test_set$y))
#df_pre_train1$Var1<-as.character(df_pre_train1$Var1)
#df_pre_train1$Var1[df_pre_train1$Var1==0]<-c("FALSE")
#df_pre_train1$Var1[df_pre_train1$Var1==1]<-c("TRUE")
#df_pre_train1$metrics <- paste(df_pre_train1$Var1,df_pre_train1$Var2, sep = '_') 
#model_comparison[4,]<-Eval_Def(df_pre_train1)
#pr <- prediction(bank_test_set$TARGET_FLAG5, bank_test_set$y)
#auc5<- performance(pr,"auc")
#model_comparison[4,c("AUC")]<-c(auc5@y.values[1])

#kable(model_comparison[4,],row.names = TRUE, caption = " K-NN Model Evaluation KPIs")

##########################################Compare three models#########################################

model_comparison$Model<-c("GLM","CRT","RF")
kable(model_comparison[1:3,c(8,1,2,3,4,5,6,7)],row.names = TRUE, caption = "Comparison of three Models")

#model_comparison$Model<-c("LG","CRT","RF","K-NN")
#kable(model_comparison[1:4,c(8,1,2,3,4,5,6,7)],row.names = TRUE, caption = "Comparison of four Models")

# Area under curve - logistic regression model
bank_test_set$y<-as.factor(bank_test_set$y)

myRoc_LG <- pROC::roc(bank_test_set$y,bank_test_set$TARGET_FLAGLG) 
bank_train_set$TARGET_FLAGLG<-predict(modelLG_update,newdata=na.omit(bank_train_set),type='response')
myRoc_LG_1<- pROC::roc(bank_train_set$y,bank_train_set$TARGET_FLAGLG) 

plot (c(1,0),c(0,1),type="n", xlab="False Positive Rate",ylab="True Positive Rate", xlim=rev(range(myRoc_LG$specificities)))
lines(x = myRoc_LG_1$specificities,y=myRoc_LG_1$sensitivities,lwd=2.5, lty=1)
lines(x = myRoc_LG$specificities,y=myRoc_LG$sensitivities,lwd=2.5,col="red", lty=2)
legend(x = "bottomright", c("Train","Test"), lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))


# Area under curve - decision tree model
myRoc_DT <- pROC::roc(bank_test_set$y,bank_test_set$TARGET_FLAGDT)
bank_train_set$TARGET_FLAGDT<- predict(modelDT_update,newdata=bank_train_set)[,2]
myRoc_DT_1<- pROC::roc(bank_train_set$y,bank_train_set$TARGET_FLAGLG) 

plot (c(1,0),c(0,1),type="n", xlab="False Positive Rate",ylab="True Positive Rate", xlim=rev(range(myRoc_DT$specificities)))
lines(x = myRoc_DT_1$specificities,y=myRoc_DT_1$sensitivities,lwd=2.5, lty=1)
lines(x = myRoc_DT$specificities,y=myRoc_DT$sensitivities,lwd=2.5,col="red",lty=2)
legend(x = "bottomright", c("Train","Test"), lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))


# Area under curve model - random forest model 
myRoc_RF<- pROC::roc(bank_test_set$y,bank_test_set$TARGET_FLAGRF) 
bank_train_set$TARGET_FLAGRF<-as.numeric(predict(modelRF,newdata=bank_train_set,type='response'))
myRoc_RF_1<- pROC::roc(bank_train_set$y,bank_train_set$TARGET_FLAGLG) 

plot (c(1,0),c(0,1),type="n", xlab="False Positive Rate",ylab="True Positive Rate", xlim=rev(range(myRoc_RF$specificities)))
lines(x = myRoc_RF_1$specificities,y=myRoc_RF_1$sensitivities,lwd=2.5, lty=1)
lines(x = myRoc_RF$specificities,y=myRoc_RF$sensitivities,lwd=2.5,col="red", lty=2)
legend(x = "bottomright", c("Train","Test"), lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))


# Area under curve model - knn model
myRoc5 <- pROC::roc(bank_test_set$y,bank_test_set$TARGET_FLAG5)
bank_train_set$TARGET_FLAG5<-predict(modelk_nn,newdata=na.omit(bank_train_set),type='response')
myRoc5_1<- pROC::roc(bank_train_set$y,bank_train_set$TARGET_FLAG5) 

plot (c(1,0),c(0,1),type="n", xlab="False Positive Rate",ylab="True Positive Rate", xlim=rev(range(myRoc5$specificities)))
lines(x = myRoc5_1$specificities,y=myRoc5_1$sensitivities,lwd=2.5, lty=1)
lines(x = myRoc5$specificities,y=myRoc5$sensitivities,lwd=2.5,col="red", lty=2)
legend(x = "bottomright", c("Train","Test"), lty=c(1,2), lwd=c(2.5,2.5),col=c("black","red"))


##ROC-Testing Set
plot (c(1,0),c(0,1),type="n",xlab="False Positive Rate",ylab="True Positive Rate", xlim=rev(range(myRoc_LG$specificities)))

lines(x = myRoc_LG$specificities,y=myRoc_LG$sensitivities,lwd=2.5,col="green", lty=1)
lines(x = myRoc_DT$specificities,y=myRoc_DT$sensitivities,lwd=2.5,col="red",lty=2)
lines(x = myRoc_RF$specificities,y=myRoc_RF$sensitivities,lwd=2.5,col="blue", lty=3)

legend(x = "bottomright", c("Logistic Regression Model","Classification Tree Model","Random Forest Model"), lty=c(1,2,3), lwd=c(2.5, 2.5 ,2.5),col=c("green","red","blue"))


