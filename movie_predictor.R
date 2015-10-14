movies_data<-read.csv("imdb.txt",sep=",",header=TRUE,stringsAsFactors=FALSE)

dim(movies_data)
#the first column in the movies_data dataframe are the row numbers . Not very useful.
#Now, we will remove some columns like Cast,Director,Genres,Movie from the movie as we have 
#used their contents in the other dummy variable columns
movies<-movies_data[,-c(1:4,6)]

#Now our dataframe has most of the columns which have dummy variables. Clearly, we dont 
# want to include columns which have very less number of 1's present. For instance, we wont 
# include an actor who has appeared in less than 5 movies for example.

library(caret)
inTrain<-createDataPartition(y=movies$IMDB.Rating,p=0.75,list=FALSE)
training<-movies[inTrain,]
testing<-movies[-inTrain,]

#Include only those columns which have more than 20 entries
rel_cols<-(colSums(training[,4:1480])>20)
train<-training[,4:1480][,rel_cols]
train<-cbind(training[,1:3],train)

pcacomp<-preProcess(train[,-1],method="pca",thresh=0.95)
#The dataframe has 29 variables and 26 of them are required to explain 95 % of the variance.
#So,significant dimensionality reduction wont be achieved. SO we wont use PCA here.

#preProc<-preProcess(train[,-1],method="pca",pcaComp=pcacomp)
#trainingPC<-predict(preProc,train[,-1])
#testingPC<-predict(preProc,testing[,-1])
test<-testing[,4:1480][,rel_cols]
test<-cbind(testing[,1:3],test)


#Creating the cross validation data set
inCross<-createDataPartition(y=train$IMDB.Rating,p=0.8,list=FALSE)
train_final<-train[inCross,]
cross_validating<-train[-inCross,]


#Now we are ready to perform the analyses.
library(Hmisc)
library(ggplot2)
library(plyr)
train_final$year_groups<-cut2(train_final$Year,g=5)
train_final$IMDB.Rating<-as.numeric(train_final$IMDB.Rating)
train_final<-na.omit(train_final)
avg_rat<-ddply(train_final,.(year_groups),summarise,mn=mean(IMDB.Rating))
#Using aggregate instead of ddply : aggregate(train_final$IMDB.Rating,by=list(train_final$year_groups),FUN=mean)

g<-ggplot(aes(year_groups,mn),data=avg_rat)
g<- g+ geom_point()
g
#Lesser the value of year_groups, older the movie is. So the abve plot tells us that older 
#tend to be rated higher than newer ones.
train_final$runtime_groups<-cut2(train_final$Runtime,g=5)
rat_runtime<-ddply(train_final,.(runtime_groups),summarise,mean_rating=mean(IMDB.Rating))
g<-ggplot(aes(runtime_groups,mean_rating),data=rat_runtime)
g<-g+geom_point()
g
#Movies having higher runtime tend to have in general higher ratings
train<-train_final[,-c(31,30)]
train$ratingclass=cut(train$IMDB.Rating,breaks=1:9)
train<-train[-1]

library(randomForest)
model<-randomForest(train$ratingclass~.,data=train,ntree=100000,do.trace=TRUE,mtry=4,importance=TRUE)
trpred<-predict(model,newdata=train[,1:28])
sum(trpred == train$ratingclass)/length(trpred)
cross_validating$IMDB.Rating<-as.numeric(cross_validating$IMDB.Rating)
cross_validating<-na.omit(cross_validating)
cross_validating$ratingclass=cut(cross_validating$IMDB.Rating,breaks=1:9)
cross_validating$predict<-predict(model,newdata=cross_validating[,2:29])
#The cross-validation accuracy
sum(cross_validating$predict == cross_validating$ratingclass)/dim(cross_validating)[1]
#odelFit1<-train(train_final[,1:34]$IMDB.Rating~.,method="rf",data=train_final[,1:34][,-1])
pred_crossval<-predict(modelFit1,newdata=cross_validating[,-1])
nas<-!is.na(as.numeric(cross_validating[,1]))
crossvalratings<-na.omit(as.numeric(cross_validating[,1]))
vec<-(pred_crossval[nas] - crossvalratings)