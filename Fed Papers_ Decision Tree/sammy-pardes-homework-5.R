#Sammy Pardes
#IST707
#Homework 5
#2/10/2020

#load required packages
#library(tm)
#library(rpart)
#library(rattle)
#library(rpart.plot)
#library(RColorBrewer)
#library(Cairo)
#library(CORElearn)
#library(caret)

#load in data
setwd("C:/Users/samantha.pardes/Desktop/graduate/IST707/week5/homework-5")

#try with only Hamilton and Madison papers
HMpapers <- Corpus(DirSource("fed-papers-hm"))
HMpapers

head(summary(HMpapers))

####################################

#cleanse data
(minFreq <- length(HMpapers)*.60) #freq of words with 60% frequency or less
(maxFreq <- length(HMpapers)*.98)#freq of words with 98% frequency or more

(wordsToRemove <- c("alexander", "hamilton", "james", "madison", "john", "jay"))

hmDTM <- DocumentTermMatrix(HMpapers, control=list(
  remove_separators=TRUE, #remove separators
  tolower=TRUE, #convert to lower case
  stopwords = wordsToRemove,
  removeWords=(wordsToRemove),
  stemming=TRUE, #gets roots of words, source: http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html
  bounds=list(global=c(minFreq, maxFreq)) #remove infrequent and very frequent words
  ))

hmDTM

#normalize
hmDTM <- weightTfIdf(hmDTM, normalize = TRUE)

max(hmDTM) 
min(hmDTM)
mean(hmDTM)

#convert to matrix, then to data frame
hmDF <- as.data.frame(as.matrix(hmDTM))

head(hmDF[,1:10])
rownames(hmDF)
dim(hmDF)

#create column of document names
(docNames <- rownames(hmDF))

docNames[1:51] <- "Hamilton"
docNames[52:66] <- "Madison"

(author <- docNames)

(hmDF$author <- author)

#create training and test data sets
(hmRows <- nrow(hmDF)) #get # of rows
(cutPoint <- floor((hmRows*2)/3)) #how many rows is 2/3 of federalist papers data frame
hmRandom <- sample(1:hmRows) #randomize indexes

hmTrain <- hmDF[hmRandom[1:cutPoint],] #create training data set using randomized indexes w/ 2/3 of documents
hmTest <- hmDF[hmRandom[(cutPoint+1):hmRows],] #create test data set with remaiing rows

dim(hmTrain)
dim(hmTest)

####################################

#build decision tree
fit1 <- rpart(author ~ ., data = hmTrain, method="class")
summary(fit1)

(predicted= predict(fit1,hmTest, type="class"))

fancyRpartPlot(fit1)

fit2 <- rpart(author ~ ., data=hmTrain, method="class", control=rpart.control(minsplit=2, cp=0))

(predicted2= predict(fit2,hmTest, type="class"))

fancyRpartPlot(fit2)

####################################

#compare predicted vs. actual results
#hmCompare <- data.frame(hmTrain$author[1:22], hmTest$author)

#confustion matrix
#hmCompare$fedTrain.author <- as.factor(hmCompare$hmTrain.author)
#hmCompare$fedTest.author <- as.factor(hmCompare$hmTest.author)

#hmConfusionMatrix <- confusionMatrix(hmCompare$hmTrain.author, hmCompare$hmTest.author)
#hmConfusionMatrix
#(hmConfusionMatrix$table)

######################################
##retry with corpus of all documents##
######################################

#load in data
fedPapers <- Corpus(DirSource("fed-papers"))
fedPapers

#cleanse data
fedDTM <- DocumentTermMatrix(fedPapers, control=list(
  remove_separators=TRUE,
  tolower=TRUE,
  stopwords = wordsToRemove,
  removeWords=(wordsToRemove),
  stemming=TRUE,
  bounds=list(global=c(minFreq, maxFreq))
))

fedDTM

#normalize
fedDTM <- weightTfIdf(fedDTM, normalize = TRUE)

max(fedDTM) 
min(fedDTM)
mean(fedDTM)

#convert to matrix, then to data frame
fedDF <- as.data.frame(as.matrix(fedDTM))

head(fedDF[,1:10])
rownames(fedDF)
dim(fedDF)

#create column of document names
(docNames <- rownames(fedDF))

docNames[1:11] <- "Disputed"
docNames[12:62] <- "Hamilton"
docNames[63:65] <- "H-M"
docNames[66:70] <- "Jay"
docNames[71:85] <- "Madison"

(author <- docNames)

(fedDF$author <- author)

#create training and test data sets
(fedRows <- nrow(fedDF)) #get # of rows

#get every 3rd row to ensure train/test represent all groups
#source: https://stackoverflow.com/questions/23279550/select-every-nth-row-from-dataframe
fedDF1 <- fedDF[seq(1, fedRows, 3),]

fedDF2 <- fedDF[seq(2, fedRows, 3),]
fedDF3 <- fedDF[seq(3, fedRows, 3),]

dim(fedDF1) #29 rows
dim(fedDF2) #28 rows
dim(fedDF3) #28 rows

(sum(nrow(fedDF1) + nrow(fedDF2) + nrow(fedDF3))) #85

fedDF2 <- rbind(fedDF2, fedDF3)

fedTrain <- fedDF1
fedTest <- fedDF2

dim(fedTrain) #29 rows
dim(fedTest) #56 rows

####################################

#build decision tree
fedFit <- rpart(fedTrain$author ~ ., data=fedTrain, method="class")
summary(fedFit)
(predicted= predict(fedFit,fedTest, type="class"))

fancyRpartPlot(fedFit)

fedFit2 <- rpart(author ~ .,data=fedTrain,method="class",control=rpart.control(minsplit=2, cp=0))

(predicted= predict(fedFit2,fedTest, type="class"))

fancyRpartPlot(fedFit2)

#compare predicted vs. actual results

#fedCompare <- data.frame(fedTrain$author, fedTest$author[1:29])

#confustion matrix
#fedCompare$fedTrain.author <- as.factor(fedCompare$fedTrain.author)
#fedCompare$fedTest.author <- as.factor(fedCompare$fedTest.author)

#fedConfusionMatrix <- confusionMatrix(fedCompare$fedTrain.author, fedCompare$fedTest.author)
#fedConfusionMatrix
#(fedConfusionMatrix$table)
