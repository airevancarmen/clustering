#LIBRARIES
library(clusterGeneration)
library(ggplot2)

# DATA
#RFM is a method used for segmenting customers and analyze their value
data_rfm = read.csv("C:/Users/Aire van Carmen/Desktop/Bootcamp/material_segmentation/material_segmentation/data_rfm.csv",sep=";")
names(data_rfm) = c("recency","frequency", "monetary")


#PROCEDURE: THREE STEPS CLUSTERING
# _______________________________________________________________


#STEP ONE: NON HIERARCHICAL CLUSTERING: K-MEANS

nclust = 1000
#Forgy algorithm: each object in the data set is randomly assigned to one of k clusters
step1 = kmeans(data_rfm[,c("recency","frequency", "monetary")], 
               1000, iter.max = 100, nstart = 1, algorithm = "Forgy")
centroids = aggregate(data_rfm[,c("recency","frequency", "monetary")],by=list(step1$cluster),FUN="mean")
centroids = centroids[,c(2,3, 4)] #you could also do centroids[c,(-1)]

#attach cluster to the data:
data_rfm$c = as.character(step1$cluster)

# _______________________________________________________________
#STEP TWO: HIERARCHICAL CLUSTERING

#MODEL
dendrogram = hclust(dist(centroids[,c("recency","frequency", "monetary")]),method="ward.D")
plot(dendrogram)

#New column selecting 5 clusters 
step2 = cutree(dendrogram, 5)

#centroids
centroids = aggregate(centroids[,c("frequency","monetary","recency")],by=list(step2),FUN="mean")
centroids = centroids[,c(2:4)]

# _______________________________________________________________
#STEP THREE: NON HIERARCHICAL CLUSTERING: K-MEANS

step3 = kmeans(data_rfm[,c("frequency","monetary","recency")], centroids, iter.max = 100, nstart = 1, algorithm = "Forgy")
#attach cluster to the data:
data_rfm$c3step = as.character(step3$cluster)

# How many customers are in each cluster? 
table(data_rfm$c3step)

# Cluster  1   2   3   4   5 
# Customers 722 756 792 797 796 

#Let´s compare with the result obtained from the hierarchical procedure
# Cluster     1   2   3   4   5 
# Customers 696 476 894 978 819 

#make a plot of the clustering

qplot(data_rfm$frequency, data_rfm$recency, log='xy', data= data_rfm, color = step3$cluster)
qplot(data_rfm$monetary, data_rfm$recency, log= 'xy', data= data_rfm, color = step3$cluster)
qplot(data_rfm$monetary, data_rfm$frequency, log= 'xy',  data= data_rfm, color = step3$cluster)

# Box plots (show the shape of the distribution)
boxplot(recency~c3step,data=data_rfm, main="Recency", xlab="Cluster number", ylab="Recency")
boxplot(monetary~c3step,data=data_rfm, main="Monetary", xlab="Cluster number", ylab="Monetary",ylim=c(0,10))
boxplot(frequency~c3step,data=data_rfm, main="Frequency", xlab="Cluster number", ylab="Frequency",ylim=c(0,200))


#Statistics
aggregate(data_rfm[, 1:3], list(data_rfm$c3step), mean)
aggregate(data_rfm[, 1:3], list(data_rfm$c3step), median)

 
#The variance between cluster frequencies is very small (7% between score 1 and 5)
# Monetary variance is small (17% between score 1 and 5)
# Cluster recency variances are the most relevant, with a difference of 25% between score 1 and 5
#Traditional segment value etiquettes are used according to the score

#Group.1  recency frequency monetary    score  segment value 
#1       1 13328.51  89.47099 4.564917  1 3 5  At risk, spend the most
#2       2 15488.17  91.02247 3.951311  3 5 2  Loyal, responsive to promotions 
#3       3 14390.07  85.37931 4.184350  2 1 3  Hibernating 
#4       4 16601.89  86.80503 3.838994  4 2 1  Promising, price conscious
#5       5 17726.47  89.92142 4.233207  5 4 4  Champions, persuadable to cross-selling 

#Because the variance frequency is so small, we can do the experiment of giving all segments
#a high frequency score, then:

#Group.  score  segment value 
#1        1 5 5  At risk, spend the most
#2        3 5 2  Loyal, responsive to promotions 
#3        2 5 3  Can´t lose them  
#4        4 5 1  Loyal, price conscious
#5        5 5 4  Champions, persuadable to cross-selling 



#Let´s compare the result with the hierarchical clustering method

#Group     recency frequency monetary   Scores Segment value
#1       1 13308.02  89.90948 4.612069  1 3 5  At risk, spend the most
#2       2 14156.17  95.28992 3.970588  2 5 2  Can't lose them, need to reactive them, minority
#3       3 15123.60  87.62304 4.045861  3 2 3  Need attention & brand awarening 
#4       4 16430.67  83.34765 3.947853  4 1 1  Promising, price conscious, majority
#5       5 17704.83  90.64225 4.199023  5 4 4  Champions, persuadable to cross-selling

