library(e1071)
library(Matrix)
library(SparseM)

## Load raw training data
##########################
rawdata<- read.csv("train_category.dat", sep="\t", nrow=100000, skip=100000, header=FALSE)

colnames(rawdata)<-c
################################
## Feature Engineering
#################################
rawdata$UserID <-as.factor(rawdata$UserID) # Convert UserId to factor
rawdata.userid <-data.frame(UserID= rawdata$UserID)
rawdata.userid <- as.factor(rawdata.userid$UserID)
rawdata.count <- rawdata$Count
###### Convert to a sparse matrix on events

#rawdata <- Matrix( rawdata, sparse = TRUE)
rawdata<-  sparse.model.matrix(~0+Event,data=rawdata)
rownames(rawdata)<- rawdata.userid
#rawdata<- as.matrix.csr(rawdata)
#multiply with count
rawdata <- rawdata*rawdata.count
#rawdata <- t(rawdata)
##rawdata.userid <- sparse.model.matrix(~0, rawdata.userid)
#rawdata<- cbind( UserID=rawdata.userid, rawdata)
#rawdata <-cbind( rawdata.userid, sparse.model.matrix(~0+Event+Count,data=rawdata))
#rawdata$UserID <-rawdata.userid

###### Multiply count into columns to get the actual sparse matrix
#col_num <- ncol(rawdata)
#rawdata <-data.frame( UserID=rawdata[,1], (rawdata[,c(-1,-col_num)]*rawdata[,col_num]))

###### Aggregate on UserId's
#rawdata <- aggregate(rawdata, by=list(rawdata[1], FUN=sum, data = rawdata)
#rawdata <- NEWaggregate.csr(x = as( rawdata, class("dgRMatrix")), fac = rawdata.userid)

rawdata<- combineByRow(m = rawdata)


#####Add labels from labeldata
labeldata<- read.csv("trainLabel.dat", sep="\t")
labeldata<- labeldata[3968:7877,]
#labeldata <- merge.with.order( rawdata.userid, labeldata, by="UserID", all.x = TRUE, keep_order = 1)
#rm(labeldata)# get rid of labeldata not needed now
gc()
labeldata<- labeldata$Label
labeldata<- as.factor(labeldata)

##############
# Train the model
##############

model = svm(rawdata,labeldata, kernel="linear", cost=0.3)
#########################
# Get the predictions
###########################
data.predictions <- predict(model, rawdata, type = "class", )
#weighteddata.test.observations <- weighteddata.test$Label
## show the confusion matrix
#confusion.matrix <- table(weighteddata.test.predictions, weighteddata.test.observations)
#confusion.matrix
## calculate the accuracy in testing set
#accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
#accuracy



#####Add weights to non-zero sparse factors
#col_num <- ncol(labeldata)
#weight <- rawdata[2:(col_num-1)]*5
#rawdata <- data.frame(UserID=rawdata$UserID, weight, Label=rawdata$Label) 
rawdata <- data.frame(weight, Label=rawdata$Label)
rawdata$Label <- as.factor(rawdata$Label)
rm(weight)

#####################################
# TODO: Work with demographic data
#####################################
#demodata <- read.csv("../u360_demodata.tsv", sep = "\t", header = FALSE)
###demodata.age <- (demodata$PreGenderProb>0.6)? demodata$PreGender: demodata$RegGender
###names(demodata)<- c("UserID", "RegCountry", "RegBirth", "RegGender", "PreGender", "PreGenderProb", "RegAgeGrp", "PreAgeGrp", "PreAgeGrpProb")
#weighteddata <- weighteddata[,-1]
# augmenteddata<- merge(weighteddata, demodata, by="UserID", all.x=TRUE)

##########################
## train the model
##########################
data.model <- naiveBayes(Label~., data = rawdata)

##########################
## Load raw test data
##########################
rawdata<- read.csv("test_category.dat", sep="\t" )

labeltestdata<- read.csv("testID.dat", sep="\t")

################################
## Feature Engineering on test data
#################################
rawdata$UserID <-as.factor(rawdata$UserID) # Convert UserId to factor

###### Convert to a sparse matrix on events
rawdata<-data.frame( rawdata$UserID, model.matrix(~0+Event+Count,data=rawdata))

###### Multiply count into columns to get the actual sparse matrix
col_num <- ncol(rawdata)
rawdata <-data.frame( UserID=rawdata[,1], rawdata[,c(-1,-col_num)]*rawdata[,col_num])

###### Aggregate on UserId's
rawdata <- aggregate(. ~ UserID, FUN=sum, rawdata)

#####Add weights to non-zero sparse factors
col_num <- ncol(rawdata)
weight <- rawdata[2:(col_num)]*5
#rawdata <- data.frame(UserID=rawdata$UserID, weight, Label=rawdata$Label) 
rawdata <- data.frame(weight)
rm(weight)

#########################
# Get the predictions
###########################
data.predictions <- predict(data.model, rawdata, type = "class")

#######################
# Create desired output
#######################
#####Add users  from test ID's 
labeldata<- read.csv("testID.dat", sep="\t")
labeldata <- data.frame(UserID=as.factor(labeldata$UserID))
rawdata<- data.frame(UserID=labeldata$UserID, Label=data.predictions)
#####Add users and labels from  from train_label data
labeldata<- read.csv("trainLabel.dat", sep="\t")
labeldata <- data.frame(UserID=as.factor(labeldata$UserID), Label=as.factor(labeldata$Label))
rawdata <- merge( rawdata, labeldata, by="UserID", all=FALSE)
rawdata<-rbind(rawdata, labeldata)
rm(labeldata)
rm(data.predictions)
gc()
write.csv(file = "auto_output.csv", x=rawdata)

#rawtestdata$UserID <-as.factor(rawtestdata$UserID)
#sparsetestdata<-data.frame( rawtestdata$UserID, model.matrix(~0+Event+Count,data=rawtestdata))
#col_num <- ncol(sparsetestdata)
#multipledtestdata <-data.frame( UserID=sparsetestdata[,1], sparsetestdata[,c(-1,-col_num)]*sparsetestdata[,col_num])
#aggregatetestdata <- aggregate(. ~ UserID, FUN=sum, multipledtestdata)
#labelledtestdata <- merge( aggregatetestdata, labeltedata, by="UserID", all=FALSE)
#col_num <- ncol(aggregatetestdata)
#weight <- aggregatetestdata[2:(col_num-1)]*5
#weightedtestdata <- data.frame(UserID=aggregatetestdata$UserID, weight)
#weighteddata$Label <- as.factor(weighteddata$Label)
#random.rows.train <- sample(1:nrow(weighteddata), 0.5*nrow(weighteddata), replace=F)
#weighteddata.train <- weighteddata[random.rows.train,]
#dim(weighteddata.train)
## select the other 1/2 left as the testing data
#random.rows.test <- setdiff(1:nrow(weighteddata),random.rows.train)
#weighteddata.test <- weighteddata[random.rows.test,]
#dim(weighteddata.test)
## fitting decision model on training set
#weighteddata.model <- naiveBayes(Label~., data = weighteddata)

## MODEL EVALUATION
## make prediction using decision model
#weighteddata.test.predictions <- predict(weighteddata.model, weightedtestdata, type = "class")
## extract out the observations in testing set
#weighteddata.test.observations <- weighteddata.test$Label
## show the confusion matrix
#confusion.matrix <- table(weighteddata.test.predictions, weighteddata.test.observations)
#confusion.matrix
## calculate the accuracy in testing set
#accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
#accuracy

#predictedata<- data.frame(UserID=weightedtestdata$UserID, Label=weighteddata.test.predictions)
#labeldata <- data.frame(UserID=as.factor(labeldata$UserID), Label=as.factor(labeldata$Label))
#x<-rbind(predictedata, labeldata)
