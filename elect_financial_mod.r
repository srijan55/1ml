library(e1071)
library(Matrix)
library(SparseM)
library(slam)

# install.packages("slam")
# install.packages("Matrix")
# install.packages("SparseM")
############## function:

NEWaggregate.csr <- function(x,fac) {
  
  #x.T <- as(as(x, "dgRMatrix"), "dgTMatrix")
  #x.T = as(x,"dgTMatrix")
  x.T = x
  
  x.T@j <- as.integer(as.integer(fac[x.T@j+1])-1)
  
  y <- as(x.T, "matrix.csr")
  #y <- as(x.T, "dgCMatrix")
  
 y@dimension <- as.integer(c(nrow(y),nlevels(fac)))
  #y <- as(y, "dgCMatrix")
  y
}



as.sparseMatrix <- function(simple_triplet_matrix_sparse) {
  retval <-  sparseMatrix(i=as.numeric(simple_triplet_matrix_sparse$i),
                          j=as.numeric(simple_triplet_matrix_sparse$j),
                          x=as.numeric(as.character(simple_triplet_matrix_sparse$v)),
                          dims=c(simple_triplet_matrix_sparse$nrow, 
                                 simple_triplet_matrix_sparse$ncol),
                          dimnames = dimnames(simple_triplet_matrix_sparse),
                          giveCsparse=FALSE)
}


##########################
## Load raw training data
##########################
rawdata<- read.csv("train_category.dat", sep="\t")
#cls <- colnames(rawdata)
#ead(rawdata)

#str(count)
#colnames(rawdata)<-c
################################
## Feature Engineering
#################################
count <- unique(rawdata$UserID)# nrow(rawdata)
rawdata<- rbind(rawdata, read.csv("test_category.dat", sep="\t"))
# tempraw <- read.csv("train_category.dat", sep="\t", nrow=100000, skip=100000, header = FALSE)
# colnames(tempraw)<-cls
# rawdata<- rbind(rawdata, tempraw)
# rm(tempraw)
# gc()

rawdata.count <- rawdata$Count
rawdata$UserID <-as.factor(rawdata$UserID) # Convert UserId to factor
rawdata.userid <-data.frame(UserID= rawdata$UserID)
rawdata.userid <- as.factor(rawdata.userid$UserID)
#rawdatatest.count <- rawdatatest$Count
###### Convert to a sparse matrix on events

#rawdata$Event<- gsub(pattern="\\_[0-9A-Za-z]*", rawdata$Event, replacement="")

#rawdata <- Matrix( rawdata, sparse = TRUE)
rawdata<-  sparse.model.matrix(~0+Event,data=rawdata, transpose = TRUE)
#rawdata <-cbind( as.matrix(rawdata.userid), sparse.model.matrix(~0+Event+Count,data=rawdata))
#colnames(rawdata)<- rawdata.userid
#summary(rawdata)
rawdata <- rawdata*rawdata.count
#str(rawdata)
rawdata <- as.simple_triplet_matrix(rawdata)
rawdata<-as.sparseMatrix(rawdata)

#s2 <- as(s,"dgTMatrix")
#s1 = as(s,"dgTMatrix")
rawdata <- NEWaggregate.csr(x = rawdata,fac = rawdata.userid)
rawdata <- as(rawdata,"dgCMatrix")
rawdata = t(rawdata)
#rawdata <- as.spam.dgCMatrix(rawdata)

###### Multiply count into columns to get the actual sparse matrix
#####Add labels from labeldata
labeldata<- read.csv("trainLabel.dat", sep="\t")
#labeldata<- labeldata[1:3398,]
#labeldata <- merge.with.order( rawdata.userid, labeldata, by="UserID", all.x = TRUE, keep_order = 1)
#rm(labeldata)# get rid of labeldata not needed now
#gc()
#labeldata<- labeldata$Label
#labeldata<- as.factor(labeldata)

##############
# Train the model
##############
# set.seed(1)
labeldataSubset = labeldata[(labeldata$Label==1 | ( labeldata$Label==0 & as.integer(labeldata$UserID)%%30<=0)),]
rawdataSub = rawdata[1:nrow(data.frame(count)),]
#model = svm(rawdata[1:nrow(data.frame(count)),],labeldata, class.weights = c("0"=0.03, "1"=0.97))
model = svm(rawdataSub[(labeldata$Label==1 | ( labeldata$Label==0 & as.integer(labeldata$UserID)%%30<=0)),], as.factor(labeldataSubset$Label), class.weights = c("0"=0.3, "1"=0.7), scale = 2, gamma = 0.009, nu = 0.5, epsilon = 0.1, kernel = "polynomial" )
#########################
# Get the predictions
###########################
data.predictions <- predict(model, rawdata[-(1:nrow(data.frame(count))),], type = "class" )
testlabeldata<- read.csv("testID.dat", sep="\t")
labeldata<- read.csv("trainLabel.dat", sep="\t")
answer <- data.frame("UserID"=testlabeldata, "Label"=data.predictions)
answer<- rbind(labeldata, answer)
write.csv(answer, file = "submission_elec_new.csv")
