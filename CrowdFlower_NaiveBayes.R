###################################################################################
## This code is part of the Kaggle competition "Search Results Relevance"
## Copyright (C) 2015
##
## Objective: Cleandata/augment data/train/test
## Data source: Crowdflower data on search queries and their results
## install tm package if not already installed install.packages("tm")
## install package readr install.packages("readr")
## install package SnowballC install.packages("SnowballC")
## install package stringr install.packages("stringr")
## install pacakge e1071
###################################################################################

library(readr)
library(tm)
library(SnowballC)
library(stringr)
library(e1071)

####################################################################
## Functions
####################################################################
####################################################################
## Cleans up input dataset by -
## 1. Converting lowercase
## 2. Removing punctuation
## 3. Removing stop words
## 4. Removes white space
## 5. Stem the words
###################################################################
CleanTextData <- function(documents)
{
  # Create a corpus of documents
  corpus <- Corpus(VectorSource(documents))
  # Convert to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Remove English stop words
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # Stem the words
  corpus <- tm_map(corpus, stemDocument)

  corpus.dataframe<- data.frame(text=unlist(sapply(corpus, '[',"content")),stringsAsFactors=F)
  corpus.dataframe
}
#############
rm_garbage = function(string){
  garbage = c("<.*?>", "http", "www","img","border","style","px","margin","left", "right","font","solid","This translation tool is for your convenience only.*?Note: The accuracy and accessibility of the resulting translation is not guaranteed")
  for (i in 1:length(garbage)){
    string = gsub(garbage[i], "", string)
  }
  return (string)
}
####################################################################
## Fuction to take id, description and query_string and 
## find full match (both/none/id/description)
###################################################################
FindFullMatch <- function(query, title, desc)
{
  returnValue = "both";
  matchFoundTitle = pmatch(as.vector(query),as.vector(title), 0)
  matchFoundDesc = pmatch(as.vector(query),as.vector(desc), 0)
  if(matchFoundTitle != 0L && matchFoundDesc  == 0L)
    returnValue = "title"

  if(matchFoundTitle == 0L && matchFoundDesc  != 0L)
    returnValue = "desc"
  
  if(matchFoundTitle == 0L && matchFoundDesc  == 0L)
    returnValue = "none"
  
  return(returnValue)
}

FindFullMatch_Mod <- function(title, desc, querytoken)
{
  
  returnValue = "none";
  if(title == querytoken && desc  != querytoken)
    returnValue = "title"
  
  if(title != querytoken && desc  == querytoken)
    returnValue = "desc"
  
  if(title == querytoken && desc  == querytoken)
    returnValue = "both"
  
  return(returnValue)
  
  
}

####################################################################
## Fuction to take query_string and other dimensions and return the 
## number of words matched in query_string and the dimension
###################################################################
CompareTwoVectors <- function(query, document)
{
  train.extendquery <- unique(unlist(strsplit(query," ")))
  train.extendeddoc <- unique(unlist(strsplit(document," ")))
  docmatch <- intersect(train.extendquery,train.extendeddoc)
  as.vector(length(docmatch))
}


####################################################################
## GET THE DATA
####################################################################
## below path may not be at the right location. user setwd("..path") 
## to set it so that it matches the below path
## load the relevance training and test sets in R
####################################################################
relevance.train <- read_csv("data files/train.csv/train.csv")
relevance.test <- read_csv("data files/test.csv/test.csv")

###########################
## explore the data set
##########################
# head(relevance.train,1)
# head(relevance.test,1)
# str(relevance.train)
# str(relevance.test)
# summary(relevance.train)
# summary(relevance.test)
############################

## Remove any NA rows for median_relevance and relevance_variance
relevance.train <- relevance.train[complete.cases(relevance.train$median_relevance),]
relevance.train <- relevance.train[complete.cases(relevance.train$relevance_variance),]

## TODO: Replace the product descriptions with empty string with <blank>
## Replace the product descriptions with empty strings to <blank>
## Get a count first
## sum(relevance.train$product_desc == "")


## CLEAN THE DATA SET
train.query <- CleanTextData(relevance.train$query)
train.producttitle <- CleanTextData(relevance.train$product_title)
train.productdesc <- CleanTextData(relevance.train$product_description)
train.productdesc = lapply(train.productdesc,rm_garbage)
train.producttitle = lapply(train.producttitle,rm_garbage)
#gsub(pattern = "\\", replacement = "", train.query = train.query, ignore.case = T)

#generate the required cleaned data frame (change the column names and remove the first column )
train <- data.frame(query=train.query$text, title=train.producttitle$text, description=train.productdesc$text, median_relevance=relevance.train$median_relevance, stringsAsFactors = FALSE)

# Find number of query matches in title
TitleMatch <- mapply(CompareTwoVectors, train$query, train$title)
# Find number of query matches in description
DescMatch <- mapply(CompareTwoVectors, train$query, train$description)
# Find if there is a full match of query in title, description or both
#ExactMatch <- as.factor(mapply(FindFullMatch, train$query, train$title, train$description ))

# Find the number of query string words to add as dimension
NumQueryTokens <- mapply(function(x)length(unlist(strsplit(x," "))), train$query)


## ADD Required dimensions to data
AugmentedData <- data.frame(train, TitleMatch=TitleMatch, DescMatch=DescMatch, NumQueryTokens=NumQueryTokens, stringsAsFactors = FALSE)
ExactMatch <- as.factor(mapply(FindFullMatch_Mod, AugmentedData$TitleMatch, AugmentedData$DescMatch, AugmentedData$NumQueryTokens))
AugmentedData$ExactMatch <- ExactMatch
AugmentedData$median_relevance <- as.factor(AugmentedData$median_relevance)
###############################
## BUILD MODEL
###############################
## randomly choose 1/2 of the data set as training data
#set.seed(777)
#random.rows.train <- sample(1:nrow(AugmentedData), 0.5*nrow(AugmentedData), replace=F)
#AugmentedData.train <- AugmentedData[random.rows.train,]
#dim(AugmentedData.train)
## select the other 1/2 left as the testing data
#random.rows.test <- setdiff(1:nrow(AugmentedData),random.rows.train)
#AugmentedData.test <- AugmentedData[random.rows.test,]
#dim(AugmentedData.test)
## fitting decision model on training set
AugmentedData.model <- naiveBayes(median_relevance ~., data = AugmentedData)

## MODEL EVALUATION
## make prediction using decision model
AugmentedData.test.predictions <- predict(AugmentedData.model, AugmentedData.test, type = "class")
## extract out the observations in testing set
AugmentedData.test.observations <- AugmentedData.test$median_relevance
## show the confusion matrix
confusion.matrix <- table(AugmentedData.test.predictions, AugmentedData.test.observations)
confusion.matrix
## calculate the accuracy in testing set
accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
accuracy
