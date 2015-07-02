library(e1071)
library(Matrix)
library(SparseM)
library(slam)

# install.packages("slam")
# install.packages("Matrix")
# install.packages("SparseM")
############## function:
# merge.with.order <- function(x,y, ..., sort = T, keep_order)
# {
#   # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
#   add.id.column.to.data <- function(DATA)
#   {
#     data.frame(DATA, id... = seq_len(nrow(DATA)))
#   }
#   # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
#   order.by.id...and.remove.it <- function(DATA)
#   {
#     # gets in a data.frame with the "id..." column.  Orders by it and returns it
#     if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
#   
#     ss_r <- order(DATA$id...)
#     ss_c <- colnames(DATA) != "id..."
#     DATA[ss_r, ss_c]
#   }
#   
#   # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
#   # tmp()
#   
#   if(!missing(keep_order))
#   {
#     if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
#     if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
#     # if you didn't get "return" by now - issue a warning.
#     warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
#   } else {return(merge(x=x,y=y,..., sort = sort))}
# }

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


# combineByRow <- function(m) {
#   m <- m[ order(rownames(m)), ]
#   
#   ## keep track of previous row name
#   prev <- rownames(m)[1]
#   i.start <- 1
#   i.end <- 1
#   
#   ## cache the rownames -- profiling shows that it takes
#   ## forever to look at them
#   m.rownames <- rownames(m)
#   stopifnot(all(!is.na(m.rownames)))
#   
#   
#   ## go through matrix in a loop, as we need to combine some unknown
#   ## set of rows
#   for (i in 2:(1+nrow(m))) {
#     
#     curr <- m.rownames[i]
#     
#     ## if we found a new row name (or are at the end of the matrix),
#     ## combine all rows and mark invalid rows
#     if (prev != curr || is.na(curr)) {
#       
#       if (i.start < i.end) {
#         m[i.start,] <- apply(m[i.start:i.end,], 2, max)
#         m.rownames[(1+i.start):i.end] <- NA
#       }
#       
#       prev <- curr
#       i.start <- i
#     } else {
#       i.end <- i
#     }
#   }
#   
#   m[ which(!is.na(m.rownames)),]    
# }
##########################
## Load raw training data
##########################
rawdata<- read.csv("train_category.dat", sep="\t")
################################
## Feature Engineering
#################################
count <- unique(rawdata$UserID)# nrow(rawdata)
rawdata<- rbind(rawdata, read.csv("test_category.dat", sep="\t"))

rawdata.count <- rawdata$Count
rawdata$UserID <-as.factor(rawdata$UserID) # Convert UserId to factor
rawdata.userid <-data.frame(UserID= rawdata$UserID)
rawdata.userid <- as.factor(rawdata.userid$UserID)
#rawdatatest.count <- rawdatatest$Count
###### Convert to a sparse matrix on events

#rawdata$Event<- gsub(pattern="\\_[0-9A-Za-z]*", rawdata$Event, replacement="")

#rawdata <- Matrix( rawdata, sparse = TRUE)
rawdata<-  sparse.model.matrix(~0+Event,data=rawdata, transpose = TRUE)
rawdata <- rawdata*rawdata.count
#str(rawdata)
rawdata <- as.simple_triplet_matrix(rawdata)
rawdata<-as.sparseMatrix(rawdata)

#s2 <- as(s,"dgTMatrix")
#s1 = as(s,"dgTMatrix")
rawdata <- NEWaggregate.csr(x = rawdata,fac = rawdata.userid)
rawdata <- as(rawdata,"dgCMatrix")
rawdata = t(rawdata)

#####Add labels from labeldata
labeldata<- read.csv("trainLabel.dat", sep="\t")

##############
# Train the model
##############
labeldataSubset = labeldata[(labeldata$Label==1 | ( labeldata$Label==0 & as.integer(labeldata$UserID)%%15<=0)),]
rawdataSub = rawdata[1:nrow(data.frame(count)),]
#model = svm(rawdata[1:nrow(data.frame(count)),],labeldata, class.weights = c("0"=0.03, "1"=0.97))
model = svm(rawdataSub[(labeldata$Label==1 | ( labeldata$Label==0 & as.integer(labeldata$UserID)%%15<=0)),], as.factor(labeldataSubset$Label), class.weights = c("0"=0.4, "1"=0.6), scale = 5, gamma = 0.06  )
#########################
# Get the predictions
###########################
data.predictions <- predict(model, rawdata[-(1:nrow(data.frame(count))),], type = "class" )
testlabeldata<- read.csv("testID.dat", sep="\t")
labeldata<- read.csv("trainLabel.dat", sep="\t")
answer <- data.frame("UserID"=testlabeldata, "Label"=data.predictions)
answer<- rbind(labeldata, answer)
write.csv(answer, file = "submission_elec_services")

