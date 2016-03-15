# Titanic Challenge
library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

df<-read.csv("train.csv",header=TRUE)
df_test<-read.csv("test.csv",header=TRUE)

str(df)

str(df_test)

df$Survived<-as.factor(df$Survived)

#check the columns with NA values
sapply(df,function (x) any(is.na(x)))

#check the column with NA values in 
sapply(df_test,function (x) any(is.na(x)))


# imputing NA values
k<-which(is.na(df$Age))
df$Age[k]<--1    # replace with -1

k<-which(is.na(df_test$Age))
df_test$Age[k]<--1 #median(df_test$Age[-k])   #replace with -1

k<-which(is.na(df_test$Fare))
df_test$Fare[k]<-median(df_test$Fare[-k])


#Generating the cabin information

df$Cabin<-as.factor(substr(as.character(df$Cabin),1,1))
df_test$Cabin<-as.factor(substr(as.character(df_test$Cabin),1,1))


table(df$Cabin)

table(df_test$Cabin)

levels(df_test$Cabin)<-levels(df$Cabin)
levels(df_test$Embarked)<-levels(df$Embarked)



#Fitting a decision tree model
#Tuning parameters in rpart : 
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=df[,c(predictors,"Survived")], 
             method="class")
fancyRpartPlot(fit)

prediction <- predict(fit, df_test[,predictors], type = "class")
submission <- data.frame(PassengerId = df_test$PassengerId, Survived = prediction)
write.csv(submission, file="submission_decisiontree.csv", row.names = FALSE)



# Fitting a random forest model

predictors<-colnames(df)[c(3,5,6:8,10,12)]
rfmodel<-randomForest(x=df[,predictors],y=df[,2],replace=FALSE,importance=TRUE,ntree=500,mtry=4)
k<-predict(rfmodel,newdata=df_test[,predictors],type="class")

submission<-data.frame(PassengerId=df_test$PassengerId,Survived=k)

write.csv(file="submisssion_randomForest.csv",submission,row.names=FALSE)


# Dummy coding 
