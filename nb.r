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
rawdata$Event<- gsub(pattern="\\_[0-9A-Za-z]*", rawdata$Event, replacement="")
rawdata$Event<- gsub(pattern="\\.[0-9A-Za-z]*", rawdata$Event, replacement="")
rawdata$Event <-as.factor(rawdata$Event) # Convert UserId to factor

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
rawdata<-  model.matrix(~0+Event,data=rawdata)
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
rawdata <- data.frame("UserID" = rawdata.userid, rawdata)
rawdata <- aggregate(. ~ UserID, FUN=sum, data = rawdata)
#rawdata<- combineByRow(m = rawdata)


#####Add labels from labeldata
labeldata<- read.csv("trainLabel.dat", sep="\t")
#labeldata<- labeldata[1:3967,]
#labeldata <- merge.with.order( rawdata.userid, labeldata, by="UserID", all.x = TRUE, keep_order = 1)
#rm(labeldata)# get rid of labeldata not needed now
gc()
labeldata<- labeldata$Label
labeldata<- as.factor(labeldata)
nBdata <- data.frame(Label=labeldata, rawdata[1:579758,])
##############
# Train the model
##############
##########################
## train the model
##########################
data.model <- naiveBayes(Label~., data = nBdata)
#model = svm(rawdata[1:nrow(data.frame(count)),],labeldata, cost = 10, gamma = 0.1,  class.weights = c("0"=0.03, "1"=0.97))
#########################
# Get the predictions
###########################
data.predictions <- predict(data.model, rawdata[-(1:579758),], type = "class" )
testlabeldata<- read.csv("testID.dat", sep="\t")
labeldata<- read.csv("trainLabel.dat", sep="\t")
answer <- data.frame("UserID"=testlabeldata, "Label"=data.predictions)
answer<- rbind(labeldata, answer)
write.csv(answer, file = "submission_fin.csv")
#weighteddata.test.observations <- weighteddata.test$Label
## show the confusion matrix
#confusion.matrix <- table(weighteddata.test.predictions, weighteddata.test.observations)
#confusion.matrix
## calculate the accuracy in testing set
#accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
#accuracy

