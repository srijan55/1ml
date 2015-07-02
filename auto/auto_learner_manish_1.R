library(e1071)
library(Matrix)
library(SparseM)


##########################
## Load raw training data
##########################
#rawdata<- read.csv("train_category.dat", sep="\t") 
rawdata <- read.csv("train_category.dat", sep="\t", nrows = 458756, skip = 800000, header = TRUE)
names(rawdata) <- c("UserID", "Event", "Count")
#rm(rawdata_test)
#gc()
################################
## Feature Engineering
#################################
rawdata$UserID <-as.factor(rawdata$UserID) # Convert UserId to factor
rawdata.userid <- rawdata$UserID
###### Convert to a sparse matrix on events
#s<- sparse.model.matrix(~0+Event,data=rawdata)
#multiply with count
#y <- s*rawdata$Count
#rawdata <-cbind( rawdata.userid, model.matrix(~0+Event+Count,data=rawdata))
gc()
rawdata <- model.matrix(~0+Event+Count,data=rawdata)

#rawdata$UserID <-rawdata.userid
#gc()
###### Multiply count into columns to get the actual sparse matrix
col_num <- ncol(rawdata)
#rawdata <-data.frame( UserID=rawdata[,1], (rawdata[,c(-1,-col_num)]*rawdata[,col_num]))
rawdata_Cnt <- rawdata[,col_num]
rawdata <- rawdata[,c(-1,-col_num)]
rawdata <-rawdata * rawdata_Cnt
rawdata <- data.frame(UserID=rawdata.userid, rawdata)
rm(rawdata_Cnt)
gc()
###### Aggregate on UserId's
rawdata <- aggregate(. ~ UserID, FUN=sum, data = rawdata)

#####Add labels from labeldata
labeldata<- read.csv("trainLabel.dat", sep="\t")
rawdata <- merge( rawdata, labeldata, by="UserID", all=FALSE)
rm(labeldata)# get rid of labeldata not needed now
#gc()

#####Add weights to non-zero sparse factors
#col_num <- ncol(rawdata)
#weight <- rawdata[2:(col_num-1)]*5
#rawdata <- data.frame(UserID=rawdata$UserID, weight, Label=rawdata$Label) 
#rawdata <- data.frame(weight, Label=rawdata$Label)
#rawdata$Label <- as.factor(rawdata$Label)
#rm(weight)
#gc()
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
data.model2 <- naiveBayes(Label~., data = rawdata)
#data.model<-data.model1
#save(data.model2, file = "AutoMOdel2.rda")
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
data.predictions <- predict(data.model1, rawdata, type = "class")
traceback(predict())
col_num_test <- ncol(rawdata)
data.observation <- rawdata[,col_num_test]
confusion.matrix <- table(data.predictions, data.observation)
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
