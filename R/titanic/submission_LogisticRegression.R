# Titanic Challenge
# Logistic regression

df_train<-read.csv("train.csv",header=TRUE)
df_test<-read.csv("test.csv",header=TRUE)

df_test<-cbind(PassengerId=df_test$PassengerId,Survived=c(rep(0,nrow(df_test))),df_test[,-1])

df<-rbind(df_train,df_test)


df$Survived<-as.factor(df$Survived)

#check the columns with NA values
sapply(df,function (x) any(is.na(x)))


# imputing NA values

k<-which(is.na(df$Age))
df$Age[k]<--1


k<-which(is.na(df$Fare))
df$Fare[k]<-mean(df$Fare[-k])

#Generating the cabin information
df$Cabin<-as.factor(substr(as.character(df$Cabin),1,1))

for (col in colnames(df))
{
  if(class(df[,col])=="numeric")
  {
    df[,col]<-as.numeric(scale(df[,col],center=TRUE,scale=TRUE))
  }
}

predictors<-colnames(df)[c(2,3,5,6:8,10,12)]

trainIndex<-c(1:nrow(df_train))

# Fitting a Logistic regression model
glm.fit<-glm(Survived~.,data=df[trainIndex,predictors],family=binomial)

summary(glm.fit)

abs(coef(glm.fit))

glm.probs<-predict(glm.fit,df[-trainIndex,predictors],type="response")
glm.pred<-c(rep("0",nrow(df[-trainIndex,])))
glm.pred[glm.probs>0.5]="1"

submission<-data.frame(PassengerId=df$PassengerId[-trainIndex],Survived=glm.pred)

write.csv(submission,file="submisssion_LogisticRegression.csv",row.names=FALSE)


# Dummy coding 
