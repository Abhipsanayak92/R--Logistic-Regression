#objective= to predict which cust will default on their credit card data
#balance means remaining amount to pay to credit card after making half payment

Credit<-read.csv(file.choose())
View(Credit)
str(Credit)
names(Credit)
library(ggplot2)
library(gridExtra)
attach(Credit)
Credit$target_30<-as.factor(ifelse(Credit$DPD>30,1,0))
Credit$target_60<-as.factor(ifelse(Credit$DPD>60,1,0))
Credit$target_90<-as.factor(ifelse(Credit$DPD>90,1,0))

table(Credit$target_30)
4922/10000 #bad rate=no of bad cust /total population, range fr bad rate=3 to 30
#output =49% (not lying between 3 to 30). so we will not consider this for y value

table(Credit$target_60) #here we are getting output as 3.3%. and our benchmark should lie between 3 to 30. 
#so we will consider y=60
333/10000

table(Credit$target_90)
162/10000 # output is 1.6. it is not lying between 3 and 30. so we will not consider for y value

Credit<-Credit[-c(4,5,7)] #deleting 4th, 5th, 7th column
#y=target_60
#x=balance

#simple logistics regression

logit<-glm(target_60 ~ balance,data= Credit, family = 'binomial') #glm stands for generalized linear model
#y has 2 levels which are factor (categorical value). so we wrote binomial
#if it was numeric, then no need to write binomial
summary(logit)

anova(logit,test='Chisq')

help("chisq.test")
#prediction try with 2000
testing<-data.frame(balance=2000) #I have taken 1 sample from balance whose value will be 2000 and assigning it in testing
#we are predicting for the cust will be bad or good whose balance =2000

testing.probs<-predict(logit,testing, type= 'response') #response will give you probability
testing.probs   #58% chance that person with balance 2000 will be a bad cust and will default credit card

###different way (mthematically calculationg from formula)
#simple logistics regression
library(caret)  #to call function from package to console, library() is used everytime
Train<-createDataPartition(Credit$target_60, p=0.7, list=FALSE)
training<-Credit[Train,] #when there is any match, assign in training
testing<- Credit[-Train,] #when there is no match, assign in testing
logit<-glm(target_60 ~ balance, data=training, family = 'binomial') #~ means target_60 depends on balance
summary(logit) #y=1/(1+e ^-t)
#t= B0+B1X1+B2X2+B3X3 and so on
#here t=B0+B1X= -10.43+0.005364*2000
#then applying logistic regression func formula i.e. y=1/(1+e^-t), we will find probability will be 58%

Acc(logit) #Acc stands for accuracy
#first run acc in concordance file then run this line
#logit is model name
##we can see training data accuracy is 95%
##################here also data overfitting after partition

##prediction
#predit function is used to apply model on data
testing$probs<-predict(logit, testing, type = 'response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,1,0))


table(testing$Predict, testing$target_60)  #testing$Predict is fitted value and testing$target_60 is actual value
library(e1071)
confusionMatrix(testing$Predict, testing$target_60)
#test data accuracy is 97%
#difference between test and training data accuracy is only 2%. so it is good fit model

##ROC curve
library(ROCR)
#make prediction on training set
predictTrain=predict(logit, testing, type='response') #####################interpretation
#prediction function
ROCRpred=prediction(predictTrain, testing$target_60)   #################interpretation


#performance functionn
ROCRperf = performance(ROCRpred, "tpr", "fpr")#############interpretation
#plot ROC curve
plot(ROCRperf)
library(ROCR)

#calculating area under curve manually
pred=prediction(testing$probs, testing$target_60)
as.numeric(performance(pred, "auc")@y.values)

#create a new dummy variable for gender
Credit<-read.csv(file.choose())
Credit$target_60<-as.factor(ifelse(Credit$DPD>60,1,0))
Credit$Dummy<-as.factor(ifelse(Credit$Gender=='M',1,0))
Credit$DPD<-NULL #deleting DPD column
Credit$Gender<-NULL #deleting Gender column

###multiple logistics Regression

library(caret)
Train<-createDataPartition(Credit$target_60, p=0.7, list=FALSE)
training<-Credit[Train,]
testing<- Credit[-Train,]
logit<-glm(target_60~income+balance+Dummy, family='binomial', data=training)
summary(logit)

Acc(logit)
#training data accuracy = 94.8%

#model including all variable
logit2<- step(glm(target_60~ income +balance+ Dummy, family ='binomial', data=training), direction="both")
################in place of +. can we write target_60~.


summary(logit2)
y=b0+b1x1+b2x2+b3x3
y=-11.75+0.00585*balance+0.8809*dummy1 #dummy 1 stands for male, dummy 0 acts as reference and added to intercept only
#logistics regression has this one and only requirement that we need one reference and that reference will be added in to intercept
#in logistic regression there is no assumption other than the above mentioned requirements

Acc(logit2)  #first run acc in concordance and then only run this
###################final model training data accuracy is 94.8% which is same as befre doing variable selection method
#################### then what is the need of doing backward method


#higher the concordance, lower the discordance and tie pair, better the model
#concordance percentage >= 70 means good fit model

#mathematical calculation check
y=-11.46+0.005744*919.58853+0.8066*0 + 1000*0.0157#0 because female, 919 means that particular cust (919th cust), income = 1000 
a<- exp(-y)
b<-1+a
c<-1/b
c  ############################## 99% chance that 919th cust who is female and whose income is 1000 will be defaulter 
##gvif works when we have more than 1 independent variable

#prediction
testing$probs<-predict(logit, testing, type='response')  #predict() needs model(logit) and data(testing)
testing$Predict<- as.factor(ifelse(testing$probs>0.70,1,0))
table(testing$Predict, testing$target_60)
confusionMatrix(testing$target_60, testing$Predict)#confusion matrix used to chk accuracy
#we need to read only accuracy , sensitivity and specificity
#always try to get sensitivity to be higher side as compared to specificity for betterment of business
##accuracy of testing data = 97%.
## difference between accuracy of testing and training data is 97-94.8%= 2.2%
#so model is good fit

##ROC curve
library(ROCR)

#make predictions on training set
predictTrain=predict(logit2, testing, type="response") ####################interpretation

#prediction function used to generate tpr(specificity) and fpr(sensitivity)
ROCRpred = prediction(predictTrain, testing$target_60) ###################interpretation

#performance function to fetch 
ROCRperf=performance(ROCRpred, "tpr", "fpr") ########################interpretation
#tpr=specificity, fpr=sensitivity

#plot ROC curve
plot(ROCRperf)  #here we are getting more than 80% towards 1, but in real life scenario, we may not get this much area under curve towards 1.
#in real life scenario, if we get more than 50% or 60% then also it is ok

library(ROCR)
pred = prediction(testing$probs, testing$target_60)
as.numeric(performance(pred, "auc")@y.values)
