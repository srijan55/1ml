library(e1071)
library(Matrix)
library(SparseM)

############## function:
merge.with.order <- function(x,y, ..., sort = T, keep_order)
{
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  add.id.column.to.data <- function(DATA)
  {
    data.frame(DATA, id... = seq_len(nrow(DATA)))
  }
  # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
  order.by.id...and.remove.it <- function(DATA)
  {
    # gets in a data.frame with the "id..." column.  Orders by it and returns it
    if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
    
    ss_r <- order(DATA$id...)
    ss_c <- colnames(DATA) != "id..."
    DATA[ss_r, ss_c]
  }
  
  # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
  # tmp()
  
  if(!missing(keep_order))
  {
    if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
    if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
    # if you didn't get "return" by now - issue a warning.
    warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
  } else {return(merge(x=x,y=y,..., sort = sort))}
}

NEWaggregate.csr <- function(x,fac) {
  x.T <- as(as(x, "dgRMatrix"), "dgTMatrix")
  
  x.T@j <- as.integer(as.integer(fac[x.T@j+1])-1)
  
  y <- as(x.T, "matrix.csr")
  
  y@dimension <- as.integer(c(nrow(y),nlevels(fac)))
  y
}

combineByRow <- function(m) {
  m <- m[ order(rownames(m)), ]
  
  ## keep track of previous row name
  prev <- rownames(m)[1]
  i.start <- 1
  i.end <- 1
  
  ## cache the rownames -- profiling shows that it takes
  ## forever to look at them
  m.rownames <- rownames(m)
  stopifnot(all(!is.na(m.rownames)))
  
  
  ## go through matrix in a loop, as we need to combine some unknown
  ## set of rows
  for (i in 2:(1+nrow(m))) {
    
    curr <- m.rownames[i]
    
    ## if we found a new row name (or are at the end of the matrix),
    ## combine all rows and mark invalid rows
    if (prev != curr || is.na(curr)) {
      
      if (i.start < i.end) {
        m[i.start,] <- apply(m[i.start:i.end,], 2, max)
        m.rownames[(1+i.start):i.end] <- NA
      }
      
      prev <- curr
      i.start <- i
    } else {
      i.end <- i
    }
  }
  
  m[ which(!is.na(m.rownames)),]    
}
##########################
## Load raw training data
##########################
rawdata<- read.csv("train_category.dat", sep="\t")
cls <- colnames(rawdata)
#colnames(rawdata)<-c
################################
## Feature Engineering
#################################
count <- unique(rawdata$UserID)# nrow(rawdata)
rawdata<- rbind(rawdata, read.csv("test_category.dat", sep="\t"))
#tempraw <- read.csv("train_category.dat", sep="\t", nrow=100000, skip=100000, header = FALSE)
#colnames(tempraw)<-cls
#rawdata<- rbind(rawdata, tempraw)
#rm(tempraw)
gc()

rawdata.count <- rawdata$Count
rawdata$UserID <-as.factor(rawdata$UserID) # Convert UserId to factor
rawdata.userid <-data.frame(UserID= rawdata$UserID)
rawdata.userid <- as.factor(rawdata.userid$UserID)
#rawdatatest.count <- rawdatatest$Count
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
#labeldata<- labeldata[1:3967,]
#labeldata <- merge.with.order( rawdata.userid, labeldata, by="UserID", all.x = TRUE, keep_order = 1)
#rm(labeldata)# get rid of labeldata not needed now
gc()
labeldata<- labeldata$Label
labeldata<- as.factor(labeldata)

##############
# Train the model
##############

model = svm(rawdata[1:nrow(data.frame(count)),],labeldata, cost = 10, gamma = 0.1,  class.weights = c("0"=0.03, "1"=0.97))
#########################
# Get the predictions
###########################
data.predictions <- predict(model, rawdata[-(1:nrow(data.frame(count))),], type = "class" )
testlabeldata<- read.csv("testID.dat", sep="\t")
labeldata<- read.csv("trainLabel.dat", sep="\t")
answer <- data.frame("UserID"=testlabeldata, "Label"=data.predictions)
answer<- rbind(labeldata, answer)
write.csv(answer, file = "submission_consumer_electronics")
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
