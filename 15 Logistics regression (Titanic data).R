#objective - Predict the chances of survival on titanic ship

#Pclass= passenger class
#sibsp= no of siblings/spouses abroad
#parch= no of parents/children abroad
#survival(0=no,1 =yes)
#fare= passenger fare(british pound)
#ticket=ticket no
#train_1=y variable data
#test= x variable data
#train= x and y variable data
#y = Survived

train<-read.csv(file.choose())
test<-read.csv(file.choose())
test_1<-read.csv(file.choose())

summary(train)

test = merge(test,test_1,by = "PassengerId") #merging 2 datasets
#here we are fixing test and then adding files from test_1 by vlookup and then assigning all to test
names(train)
str(train)
##Pclass from int to categorical variable
train$Pclass = as.factor(train$Pclass)
test$Pclass = as.factor(test$Pclass) #we are converting to factor in both test and train as they both need to have same data types
train$Survived <-as.factor(train$Survived)
test$Survived = as.factor(test$Survived)

str(train)
summary(train)
summary(train$Age)
sapply(train, function(x) sum(is.na(x))) #missing value check on train data
sapply(test, function(x) sum(is.na(x)))  #missing value check on test data
#fill in missing values for Age
train$Age[is.na(train$Age)] = mean(train$Age, na.rm=TRUE)#wherever there is missing value replace by mean
test$Age[is.na(test$Age)]=mean(test$Age, na.rm=TRUE)
names(train)
##'age' that are less than 1
train[which(train$Age<1),'Age'] #we are checking which age value is <1 in train data
test[which(test$Age<1), 'Age'] #we are checking which age value is <1 in test data
sapply(train, function(x) sum(is.na(x)))  #again checking missing value after missing value treatment in age
sapply(test, function(x) sum(is.na(x))) #same in test data
#train data- no missing values now
#test data- 1 missing value in fare column

#removing row which has missing value (fare column has 1 missing value)
test=test[!is.na(test$Fare),] #removing fare bcz it has only 1 missing value and it will not affect model if we remove it

##just taking a subset of column for modelling building
train1 = subset(train, select=c(2,3,5,6,7,8,10))
test1 = subset(test, select = c(2,4,5,6,7,9,11))

#treatment of outlier for Fare
boxplot(train1$Fare)
summary(train1$Fare)
upper<-31+1.5*IQR(train1$Fare); upper
train1$Fare[train1$Fare>upper]<-upper
boxplot(train1$Fare)
summary(train1$Fare)
#treatment of outlier for age and business logic
boxplot(train1$Age)
summary(train1$Age)
upper<-35+1.5*IQR(train1$Age);upper
train1$Age[train1$Age>upper]<- upper
lower<-22.00-1.5*IQR(train1$Age);lower
train1$Age[train1$Age<lower]<-lower
boxplot(train1$Age)
summary(train1$Age)


#model building
model=glm(Survived~., family='binomial', data=train1)
summary(model)  #HERE pclass1,female are taken as reference and added to intercept

###viariable significance selection
reg.model = step(glm(Survived~., family='binomial', data=train1), direction="both")
summary(reg.model) ##############what to do with those variable who are not impacting y variable (from summary)
anova(reg.model, test='Chisq')
#way of giving own reference
table(train1$Pclass)
table(train1$Sex)
#model building
reg.model1=step(glm(Survived~relevel(Pclass, ref=2) 
                    +relevel(Sex,ref='female')+Age+Fare+SibSp+Parch, family = 'binomial',data=train1),
                direction="both")
#ref=2 means pclass2 will be considered as reference(we are doing it of our own), it is not done by R software
#relevel is for manually doing relevelling after considering which one to be taken as reference
#+age+fare+sibsp+parch means they are numeric, so we cant relevel them

summary(reg.model1)
anova(reg.model1,test='Chisq')
Acc(reg.model1)
#training data accuracy is 85.83%


#to check multi collinearity
library(car)
vif(reg.model1)   ###gvif stands for global vif

#to get odds ratio
library(oddsratio)  
library(vcd)
exp(coef(reg.model1)) # 1.9% more chance of class 1 will survive as compared to class 2
# 0.36% higher chance of class 3 to survive as compared to class2
#0.06% higher chance for male to survive than female i.e 1-0.06= 0.04 % chances that female will be survived


##prediction
test1$probs<- predict(reg.model1,test1,type='response')
test1$Predict<-as.factor(ifelse(test1$probs>0.70,1,0))
table(test1$Survived, test1$Predict)
library(caret)
confusionMatrix(test1$Survived, test1$Predict)
#training data accuracy(85%) from acc(reg.model1)
#testing data accuracy (83.5%) from confusion matrix(test1$Survived, test1$Predict)
#so difference is only 2%, so model is good fit
#there is no overfit or underfit
#if difference would be more than 10-15% then there may be overfitting and underfitting

library(ROCR)
library(ggplot2)
#make predictions on test set
predictTrain=predict(reg.model1, test1, type="response") ###################interpretation
#prediction function
ROCRpred=prediction(predictTrain, test1$Survived) #################interpretation
#performance function
ROCRperf =performance(ROCRpred, "tpr", "fpr") ############interpretation
##plotting ROC curve
plot(ROCRperf)
#add colors
plot(ROCRperf, colorize=TRUE)

#AUC(area under curve)
pred=prediction(test1$probs, test1$Survived)
as.numeric(performance(pred, "auc")@y.values)
#we are getting 95% data auc means on 95% data model is working
