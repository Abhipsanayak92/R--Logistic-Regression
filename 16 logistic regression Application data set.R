#objective: to build application score cards which help us to identify whether we should give him laon or not
#cbs1- credit score 1
#cbinq- no of enquiries
#cbline- no of credit lines (no of loans)
#cbterm- no of termloans (durations >5yrs)
#cblineut- line utilization(0-100)
#cbtob- no of yrs in file 
#cbdpd- indicator od days past due(DPD) on bureau(yes, no)
#cbnew= no of new loans(less than 1 yr how much u took loan)
#pmt- type of payment(m= manual, A= Autopay, p=payroll(chq payment))
#dpd= level of delinquency(no. low, high) 
#home- home ownership indicator(yes, no)
#online= indicator of active online(5, No) means who are still paying emi
#period= factor that indicates year/month of the data


data<-read.csv(file.choose())
table(data$fgood)
names(data)
str(data)
#data conversion as per model requirement
data$cbs1<-as.numeric(data$cbs1)
data$cbinq<-as.numeric(data$cbinq)
data$cbline<-as.numeric(data$cbline)
data$cbterm<- as.numeric(data$cbterm)
data$cblineut<-as.numeric(data$cblineut)
data$cbtob<-as.numeric(data$cbtob)
#categorical variable
data$fgood<-as.factor(data$fgood)
data$pmt<-as.factor(data$pmt)
data$cbdpd<-as.factor(data$cbdpd)
data$home<-as.factor(data$home)
data$online<-as.factor(data$online)
data$cbnew<-as.factor(data$cbnew)
str(data)
#treatment for outlier for cblineut
boxplot(data$cblineut)
summary(data$cblineut)
upper<-54.089+1.5*IQR(data$cblineut);upper
data$cblineut[data$cblineut>upper]<- upper
lower<-37.734-1.5*IQR(data$cblineut); lower
data$cblineut[data$cblineut<lower]<-lower
boxplot(data$cblineut)
summary(data$cblineut)

#checking missing values
sapply(data, function(x) sum(is.na(x))) ### we found that cbs1 column is having 487 missing values

#cibil score not given means the cust is new
data$cbs1[is.na(data$cbs1)]<-1 #replacing missing values with 1 as per CIBIL
sapply(data, function(x) sum(is.na(x)))


#multiple logistic regression
library(caret)
Train<-createDataPartition(data$fgood, p=0.7, list = FALSE)
training<- data[Train,]
testing<-data[-Train,]
#fitting logistic regression to the training set
classifier=step(glm(fgood~. -period, family = 'binomial',
                   data=training), direction="both")  ####period is time variable in date format, so removing 
summary(classifier) ###classifier is the model name
Acc(classifier)
#accuracy of training data 78%

##odds ratios only
exp(coef(classifier))
##prediction
testing$probs<-predict(classifier, testing, type='response')
testing$Predict<-as.factor(ifelse(testing$probs>0.70,'1','0'))
table(testing$Predict, testing$fgood)
library(caret)
library(e1071)
confusionMatrix(testing$Predict, testing$fgood)
#we are getting train data accuracy=80.5% from acc(classifier)
#we are getting test data accuracy as 98% from confusion matrix
#difference is more than 15%, so data is overfitting
#table(fgood) gives output of 4920 good cust and 80 bad cust
#here bad rate = 80/5000=1.6(range is 3 to 30). means model is wrong/bad
# so data is biased towards good cust

library(ROCR)
#make prediction on test set
predictTrain = predict(classifier, testing, type="response")
#prediction functions
ROCRpred= prediction(predictTrain, testing$fgood)
#performance function
ROCRperf=performance(ROCRpred, "tpr", "fpr")

#plotting ROC curve
plot(ROCRperf)
#add colors

#we are doing undersampling here to tackle biasness of data
data2<-subset(data, fgood==1) #we are taking subset where there  are good cust(means fgood ==1)
data3<-subset(data,fgood==0) #we are taking subset where there are bad cust (means fgood==0)
data3<- data3[1:500,]# randomly we can take any numbers
data4<- rbind(data2, data3) 
###multiple logistic regression
library(caret)
Train<-createDataPartition(data4$fgood, p= 0.7, list=FALSE)
training<- data4[Train,]
testing<- data4[-Train,]
#fitting logistic regression to the training set
classifier= glm(fgood~.-period, family='binomial', data=training)
summary(classifier)
Acc(classifier)
#training data accuracy= 80%

###odds ratio  only
exp(coef(classifier))
##prediction
testing$probs<- predict(classifier, testing, type="response")
testing$Predict<-as.factor(ifelse(testing$probs>0.70, '1', '0'))
table(testing$Predict, testing$fgood)
library(caret)
library(e1071)
confusionMatrix(testing$Predict, testing$fgood)
#testing data accuracy= 72%
#difference is 8%. so good fit model

library(ROCR)
#make predictions on test set
predictTrain= predict(classifier,testing, type ="response")
#Prediction function
ROCRpred = prediction(predictTrain, testing$fgood)
#performance function
ROCRperf= performance(ROCRpred, "tpr", "fpr")
##plotting ROC curve
plot(ROCRperf)
#add colors
plot(ROCRperf, colorize=TRUE)
