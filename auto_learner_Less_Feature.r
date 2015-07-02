 library(e1071)
 library(Matrix)
 library(SparseM)
 rawdata<- read.csv("train_category.dat", sep="\t")
 
   
   cls <- colnames(rawdata)
   ################################
   ## Feature Engineering
   #################################
   count <- unique(rawdata$UserID)# nrow(rawdata)
   rawdata<- rbind(rawdata, read.csv("test_category.dat", sep="\t"))
   #Replace _ & . from event category name 
   rawdata$Event<- gsub(pattern="\\_[0-9A-Za-z]*", rawdata$Event, replacement="")
   rawdata$Event<- gsub(pattern="\\.[0-9A-Za-z]*", rawdata$Event, replacement="")
   unique(rawdata$Event)
   
   rawdata.count <- rawdata$Count
   rawdata$UserID <-as.factor(rawdata$UserID) # Convert UserId to factor
   rawdata.userid <-data.frame(UserID= rawdata$UserID)
   rawdata.userid <- as.factor(rawdata.userid$UserID)
   
   #rawdatatest.count <- rawdatatest$Count
   #rawdata_copy<-rawdata
   #rawdata<-  sparse.model.matrix(~0+Event,data=rawdata)

    rawdata <- model.matrix(~0+Event,data=rawdata)
    rawdata <- rawdata*rawdata.count
    colnames(rawdata)
    rawdata <- data.frame("UserID" = rawdata.userid, rawdata)
    
    rawdata <- aggregate(. ~ UserID, FUN=sum, data = rawdata)
    #####Add labels from labeldata
    labeldata<- read.csv("trainLabel.dat", sep="\t")
    #labeldata<- labeldata[1:3967,]
    #labeldata <- merge.with.order( rawdata.userid, labeldata, by="UserID", all.x = TRUE, keep_order = 1)
    #rm(labeldata)# get rid of labeldata not needed now
    gc()
    labeldata<- labeldata$Label
    labeldata<- as.factor(labeldata)
    
    #rawdata[1:nrow(data.frame(count)),]
    
    ##############
    # Train the model
    ##############
    
    model = svm(rawdata[1:nrow(data.frame(count)),2:15],labeldata, cost = 10, gamma = 0.1,  class.weights = c("0"=0.03, "1"=0.97))
    #########################
    # Get the predictions
    ###########################
    data.predictions <- predict(model, rawdata[-(1:nrow(data.frame(count))),2:15], type = "class" )
    #data.predictions <- predict(model, rawdata[104903:267848,2:15], type = "class" )
    summary(data.predictions)
    
    testlabeldata<- read.csv("testID.dat", sep="\t")
    labeldata<- read.csv("trainLabel.dat", sep="\t")
    answer <- data.frame("UserID"=testlabeldata, "Label"=data.predictions)
    answer<- rbind(labeldata, answer)
    write.csv(answer, file = "submission_elec_researcher.csv")
    
    #final_work<- read.csv("submission_auto_researcher.csv")
    #final_work$Label <- as.factor(final_work$Label)
    #labeldata$Label <- as.factor(labeldata$Label)
    #summary(labeldata$Label)
    