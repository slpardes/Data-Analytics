#Sammy Pardes
#IST707
#Homework 4
#2/7/20

#load libraries
#library(stringr)
#library(wordcloud)
#library(arules)
#library(proxy)
#library(cluster)
#library(stringi)
#library(Matrix)
#library(tidytext)
#library(plyr)
#library(ggplot2)
#library(factoextra)
#library(mclust)
#library(slam)
#library(quanteda)
#library(SnowballC)
#library(tm)
#library(fpc)

#load in data
setwd("C:/Users/samantha.pardes/Desktop/graduate/IST707/week4/homework-4/")

fedCorpus <- Corpus(DirSource("fed-papers"))
(fedCorpus) #85 documents is correct

#removed Jay and co-authored papers from corpus
#fedCorpus <- Corpus(DirSource("fed-papers-clean"))
#(fedCorpus) #77 documents is correct

head(summary(fedCorpus))

(meta(fedCorpus[[1]]))

##########################################

#cleanse data
#convert corpus to document term matrix

#(minFreq <- length(fedCorpus)*.04) #freq of words with 4% frequency or less
#(maxFreq <- length(fedCorpus)*.98)#freq of words with 98% frequency or more

#(minFreq <- length(fedCorpus)*.02) #freq of words with 2% frequency or less
#(maxFreq <- length(fedCorpus)*.98)#freq of words with 98% frequency or more

(minFreq <- length(fedCorpus)*.60) #freq of words with 60% frequency or less
(maxFreq <- length(fedCorpus)*.98)#freq of words with 98% frequency or more

fedDTM <- DocumentTermMatrix(fedCorpus, control=list(
          #stopwords=TRUE, #remove stopwords
          #removePunctuation=TRUE, #remove punctuation
          #removeNumbers=TRUE, #remove numbers
          tolower=TRUE, #convert to lower case
          stemming=TRUE, #gets roots of words, source: http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html 
          #remove_separators=TRUE, #remove separators
          #wordLengths=c(6, 100)
          bounds=list(global=c(minFreq, maxFreq)) #remove infrequent and very frequent words
        ))

fedDTM #85 or 77 documents
(inspect(fedDTM))

max(fedDTM)
min(fedDTM)

#convert from DTM to matrix
fedMatrix <- as.matrix(fedDTM)
fedMatrix[1:10,1:10]

#normalize data

####################
#test function on smaller data set
#matrixSmall <- matrix(1:10, 5, 5)
#matrixSmall
#matrixSmall_norm <- t(apply(matrixSmall, 1, function(x) round(x/sum(x),3)))
#matrixSmall_norm

#matrixSmall_check <- c(matrixSmall[1,1]/sum(matrixSmall[1,]), matrixSmall[1,2]/sum(matrixSmall[1,]), matrixSmall[1,3]/sum(matrixSmall[1,]), matrixSmall[1,4]/sum(matrixSmall[1,]), matrixSmall[1,5]/sum(matrixSmall[1,]))
  
#(matrixSmall_check)
#(matrixSmall_norm[1,])
#same values, function works as expected
####################

fedMatrixNorm <- t(apply(fedMatrix, 1, function(x) round(100*(x/sum(x)),3))) #multiply to get %s, round to 3 decimal places, and transform
head(fedMatrixNorm[,1:10])

max(fedMatrixNorm)
min(fedMatrixNorm)

dim(fedMatrixNorm)
rowSums(fedMatrixNorm) #sums ~100% for all rows

head(fedMatrixNorm[,1:10])

#data is cleansed and normalized

##########################################

#data analysis 

#distance measures
#Euclidean distance (L2)
distMatrixEuc <- dist(fedMatrixNorm, method="euclidean")
head(distMatrixEuc)

max(distMatrixEuc)
min(distMatrixEuc)

#eucDist <- get_dist(fedMatrixNorm, method="euclidean")
#head(eucDist)
#head(distMatrixEuc) #same

#Manhattan distance (L1)
distMatrixManh <- dist(fedMatrixNorm, method="manhattan")
head(distMatrixManh)

max(distMatrixManh)
min(distMatrixManh)

#Cosine Similarity distance
distMatrixCos <- dist(fedMatrixNorm, method="cosine")
head(distMatrixCos) #closest to frequency matrix scale

max(distMatrixCos)
min(distMatrixCos)

##########################################

#clustering
#hierarchical 
#Euclidean
eucClust <- hclust(distMatrixEuc, method="ward.D")

#par(mar=c(1,1,1,1)) #reset margins for plotting

plot(eucClust, cex=0.9, hang=-1) #plot and set font size and height
rect.hclust(eucClust, k=5) 
#3 clusters (Hamilton, Madison, Disputed)
#5 clusters (Hamilton, Madison, HM, Jay, Disputed)

#Cosine Similarity
cosClust <- hclust(distMatrixCos, method="ward.D")

plot(cosClust, cex=0.9, hang=-1)
rect.hclust(cosClust, k=5)

#Manhattan distance
manhClust <- hclust(distMatrixManh, method="ward.D")

plot(manhClust, cex=0.9, hang=-1)
rect.hclust(manhClust, k=5)

############ 

#fviz
#Euclidean

eucViz <- fviz_dist(distMatrixEuc, gradient=list(low="purple", mid="white", high="green"))
eucViz

#Manhattan
manhViz <- fviz_dist(distMatrixManh, gradient=list(low="purple", mid="white", high="green"))
manhViz

#Cosine Similarity
cosViz <- fviz_dist(distMatrixCos, gradient=list(low="purple", mid="white", high="green"))
cosViz

#kmeans
#transpose so docs are the columns, words are the rows
t_fedMatrixNorm <- t(fedMatrixNorm)

dim(t_fedMatrixNorm)

kmeansFed <- kmeans(t_fedMatrixNorm,centers=5)

(summary(kmeansFed))
(kmeansFed$cluster)
(kmeansFed$centers)
(kmeansFed$size)

fviz_cluster(kmeansFed, t_fedMatrixNorm)

#####################
#library(fpc)
#source: https://stats.stackexchange.com/questions/31083/how-to-produce-a-pretty-plot-of-the-results-of-k-means-cluster-analysis

#K means cluster 
fedClus <- kmeans(t_fedMatrixNorm, centers=5)

plotcluster(t_fedMatrixNorm, fedClus$cluster)

############
#try with non-transposed matrix
kmeansFed2 <- kmeans(fedMatrixNorm,centers=5)

(summary(kmeansFed2))
(kmeansFed2$cluster)
(kmeansFed2$centers)
(kmeansFed2$size)

fviz_cluster(kmeansFed2, fedMatrixNorm)

###############################################
#test code with smaller data set

#load in data
#setwd("C:/Users/samantha.pardes/Desktop/graduate/IST707/week4/homework-4/")

#testCorpus <- Corpus(DirSource("testCorpus"))
#(testCorpus) #6 documents is correct
#head(summary(testCorpus))

#cleanse data
#convert corpus to document term matrix
#testDTM <- DocumentTermMatrix(testCorpus, control=list(
  #removePunctuation=TRUE, #remove punctuation
  #tolower=TRUE, #convert to lower case
  #remove_separators=TRUE, #remove separators
  #bounds=list(global=c(0, 1000000)) #remove infrequent and very frequent words
#))

#testDTM #6 docs, 32 terms
#(inspect(testDTM))

#max(testDTM)
#min(testDTM)

#convert from DTM to matrix
#testMatrix <- as.matrix(testDTM)
#testMatrix

#normalize data
#testMatrix_norm <- t(apply(testMatrix, 1, function(x) round(x/sum(x),3)))
#testMatrix_norm

#testMatrix_norm <- t(apply(testMatrix, 1, function(x) round(100*(x/sum(x)),3))) #multiply to get %s, round to 3 decimal places, and transform
#head(testMatrix_norm[,1:10])

#max(testMatrix_norm)
#min(testMatrix_norm)

#dim(testMatrix_norm) #85 rows, 1,851 columns
#rowSums(testMatrix_norm) #sums ~100% for all rows

#head(testMatrix_norm)

#data is cleansed and normalized

#data analysis 

#distance measures
#Euclidean distance (L2)
#TESTdistMatrixEuc <- dist(testMatrix_norm, method="euclidean")
#head(TESTdistMatrixEuc)

#max(TESTdistMatrixEuc)
#min(TESTdistMatrixEuc)

#Manhattan distance (L1)
#TESTdistMatrixManh <- dist(testMatrix_norm, method="manhattan")
#head(TESTdistMatrixManh)

#max(TESTdistMatrixManh)
#min(TESTdistMatrixManh)

#Cosine Similarity distance
#TESTdistMatrixCos <- dist(testMatrix_norm, method="cosine")
#head(TESTdistMatrixCos) #closest to frequency matrix scale

#max(TESTdistMatrixCos)
#min(TESTdistMatrixCos)

#clustering
#hierarchical 
#Euclidean
#TESTeucClust <- hclust(TESTdistMatrixEuc, method="ward.D")
#plot(TESTeucClust, cex=0.9, hang=-1) #plot and set font size and height
#rect.hclust(TESTeucClust, k=2) #3 clusters (Hamilton, Madison, Jay)

#Cosine Similarity
#TESTcosClust <- hclust(TESTdistMatrixCos, method="ward.D")
#plot(TESTcosClust, cex=0.9, hang=-1)
#rect.hclust(TESTcosClust, k=2)

#Manhattan distance
#TESTmanhClust <- hclust(TESTdistMatrixManh, method="ward.D")
#plot(TESTmanhClust, cex=0.9, hang=-1)
#rect.hclust(TESTmanhClust, k=2)

#k-means
#Euclidean
#TESTeucViz <- fviz_dist(TESTdistMatrixEuc, gradient=list(low="purple", mid="white", high="green"))
#TESTeucViz

#Manhattan
#TESTmanhViz <- fviz_dist(TESTdistMatrixManh, gradient=list(low="purple", mid="white", high="green"))
#TESTmanhViz

#Cosine Similarity
#TESTcosViz <- fviz_dist(TESTdistMatrixCos, gradient=list(low="purple", mid="white", high="green"))
#TESTcosViz

#transpose so docs are the columns, words are the rows
#TESTt_MatrixNorm <- t(testMatrix_norm)

#dim(TESTt_MatrixNorm)

#kmeansTEST <- kmeans(TESTt_MatrixNorm,centers=2)

#(summary(kmeansTEST))
#(kmeansTEST$cluster)
#(kmeansTEST$centers)
#(kmeansTEST$size)

#fviz_cluster(kmeansTEST, TESTt_MatrixNorm)

#try with non-transposed matrix
#kmeansTEST2 <- kmeans(testMatrix_norm,centers=2)

#(summary(kmeansTEST2))
#(kmeansTEST2$cluster)
#(kmeansTEST2$centers)
#(kmeansTEST2$size)

#fviz_cluster(kmeansTEST2, testMatrix_norm)
#looks good!