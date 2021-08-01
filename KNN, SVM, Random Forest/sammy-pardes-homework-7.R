#Sammy Pardes
#IST707
#Homework 7
#3/2/2020

#############
##load data##
#############

#library(class)
#library(caret) 
#library(e1071)
#library(randomForest)
#library(ggplot2)
#library(gridExtra)

digitsTrain <- read.csv("C:/Users/samantha.pardes/Desktop/graduate/IST707/week7/homework-7/Kaggle-digit-train-sample-small-1400.csv")
digitsTest <- read.csv("C:/Users/samantha.pardes/Desktop/graduate/IST707/week7/homework-7/Kaggle-digit-test-sample1000.csv")

str(digitsTrain)
any(is.na(digitsTrain)) #no missing information
any(is.na(digitsTest))

dim(digitsTrain) #1400 rows, 785 columns
dim(digitsTest) #1000 rows, 785 columns

################
##cleanse data##
################

digitsTrain$label <- as.factor(digitsTrain$label) #convert label variable to factor

#create training subset for comparing accuracy
(digitsTrainRows <- nrow(digitsTrain)) 

(cutPoint <- floor((digitsTrainRows*2)/3)) #2/3 of training set is 933 rows

digitsRandom <- sample(1:digitsTrainRows) #randomize indexes

digitsTrainCopy <- digitsTrain

digitsTrain <- digitsTrainCopy[digitsRandom[1:cutPoint],] #set training set to only include 2/3 data
digitsTrainSubset <- digitsTrainCopy[digitsRandom[(cutPoint+1):digitsTrainRows],] #training subset includes the remaining 1/3

any((is.na(digitsTrain[digitsRandom[(cutPoint+1):digitsTrainRows],])))

dim(digitsTrain) #933 rows, 785 columns
dim(digitsTrainSubset) #467 rows, 785 columns

#head(digitsTrain)
#head(digitsTrainSubset)

#remove labels from training subset for testing
digitsTrainLabeled <- digitsTrain #keep copy with labels

digitsTrainLabels <- digitsTrain$label
digitsTrain <- digitsTrain[,-1]

digitsTrainSubsetLabels <- digitsTrainSubset$label
digitsTrainSubset <- digitsTrainSubset[,-1]

digitsTestLabels <- digitsTest$label
digitsTest <- digitsTest[,-1]

#str(digitsTrain)
#str(digitsTrainSubset)
#str(digitsTest)

dim(digitsTrain)
dim(digitsTrainSubset)
dim(digitsTest)

#################
##data analysis##
#################

#######
##kNN##
#######
knnDigits <- knn(train=digitsTrain, test=digitsTrainSubset, cl=digitsTrainLabels[1:nrow(digitsTrain)], k=10, prob=TRUE)
print(knnDigits)

table(knnDigits)

#check accuracy
(knnTable <- table(knnDigits, digitsTrainSubsetLabels[1:length(knnDigits)]))
confusionMatrix(knnTable) 

#k=10, Accuracy : 0.8672 
#k=50, Accuracy : 0.7752
#k=100, Accuracy : 0.6681 

#misclassified %
(1 - sum(diag(knnTable))/sum(knnTable))

#test w/ unknown data
knnDigitsTestPred <- knn(train=digitsTrain, test=digitsTest[1:nrow(digitsTrain),], cl=digitsTrainLabels, k=10, prob=TRUE)
print(knnDigitsTestPred)

(knnDigitsTable <- table(knnDigitsTestPred))

#######
##svm##
#######

#use training set w/ labels 
svmDigits <- svm(label ~., data=digitsTrainLabeled, kernel="polynomial", scale=FALSE)
print(svmDigits)

#prediction
svmDigitsPred <- predict(svmDigits, digitsTrainSubset, type="class")

#check accuracy
(svmTable <- table(svmDigitsPred, digitsTrainSubsetLabels))
confusionMatrix(svmTable) 

#Accuracy : 0.9058 

#misclassified %
(1 - sum(diag(svmTable))/sum(svmTable))

#test w/ unknown data
svmDigitsTestPred <- predict(svmDigits, digitsTest, type="class")
print(svmDigitsTestPred)

(svmDigitsTable <- table(svmDigitsTestPred))

#################
##random forest##
#################

rfDigits <- randomForest(label~., data=digitsTrainLabeled)
print(rfDigits)
#estimate of  error rate: 10.4%

#prediction
rfDigitsPred <- predict(rfDigits, digitsTrainSubset)

rfTable <- table(rfDigitsPred, digitsTrainSubsetLabels)

confusionMatrix(rfTable)
#Accuracy : 0.9165

#test w/ unknown data
rfDigitsTestPred <- predict(rfDigits, digitsTest)

(rfDigitsTable <- table(rfDigitsTestPred))

##################
##visualizations##
##################
table(knnDigitsTestPred)
table(svmDigitsTestPred)
table(rfDigitsTestPred)

#knn

#known
knnDF <- data.frame(predicted=knnDigits, actual=digitsTrainSubsetLabels[1:length(knnDigits)])

knnPlot <- ggplot(knnDF, aes(x=actual, y=predicted, color=actual)) + geom_jitter() + ggtitle("KNN Predicted vs. Actual Digits")
knnPlot <- knnPlot + theme(legend.position="none")
knnPlot

#unknown
knnDFTest <- as.data.frame(knnDigitsTable)

knnPlotTest <- ggplot(knnDFTest, aes(x=knnDFTest$knnDigitsTestPred, y=knnDFTest$Freq, fill=knnDigitsTestPred)) + geom_histogram(stat="identity")
knnPlotTest <- knnPlotTest + ggtitle("KNN Predicted Digits") + xlab("Predicted") + ylab("Frequency")
knnPlotTest <- knnPlotTest + theme(legend.position="none")
knnPlotTest

#svm 

#known
svmDF <- data.frame(predicted=svmDigitsPred, actual=digitsTrainSubsetLabels)

svmPlot <- ggplot(svmDF, aes(x=actual, y=predicted, color=actual)) + geom_jitter() + ggtitle("SVM Predicted vs. Actual Digits")
svmPlot <- svmPlot + theme(legend.position="none")
svmPlot 

#unknown
svmDFTest <- as.data.frame(svmDigitsTable)

svmPlotTest <- ggplot(svmDFTest, aes(x=svmDFTest$svmDigitsTestPred, y=svmDFTest$Freq, fill=svmDigitsTestPred)) + geom_histogram(stat="identity")
svmPlotTest <- svmPlotTest + ggtitle("SVM Predicted Digits") + xlab("Predicted") + ylab("Frequency")
svmPlotTest <- svmPlotTest + theme(legend.position="none")
svmPlotTest

#random forest

#known
rfDF <- data.frame(predicted=rfDigitsPred, actual=digitsTrainSubsetLabels)

rfPlot <- ggplot(rfDF, aes(x=actual, y=predicted, color=actual)) + geom_jitter() + ggtitle("Random Forest Predicted vs. Actual Digits")
rfPlot <- rfPlot + theme(legend.position="none")
rfPlot

#unknown
rfDFTest <- as.data.frame(rfDigitsTable)

rfPlotTest <- ggplot(rfDFTest, aes(x=rfDFTest$rfDigitsTestPred, y=rfDFTest$Freq, fill=rfDigitsTestPred)) + geom_histogram(stat="identity")
rfPlotTest <- rfPlotTest + ggtitle("Random Forest Predicted Digits") + xlab("Predicted") + ylab("Frequency")
rfPlotTest <- rfPlotTest + theme(legend.position="none")
rfPlotTest

#grid extra plots

#train
grid.arrange(knnPlot, svmPlot, rfPlot, nrow=1)

#test
grid.arrange(knnPlotTest, svmPlotTest, rfPlotTest, nrow=1)