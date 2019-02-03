#HIERARCHICAL CLUSTERING:  
# Determine the number of clusters
# Carry out the clustering

#LIBRARIES
library(clusterGeneration)
library(ggplot2)

# DATA
#RFM is a method used for segmenting customers and analyzing their value
# read the RFM data
data_rfm = read.csv("C:/Users/Aire van Carmen/Desktop/Bootcamp/material_segmentation/material_segmentation/data_rfm.csv",sep=";")
names(data_rfm) = c("recency","frequency", "monetary")


#PROCEDURE: THREE STEPS CLUSTERING
# _______________________________________________________________


#STEP ONE: NON HIERARCHICAL CLUSTERING: K-MEANS

nclust = 1000
#Forgy algorithm: each object in the data set is randomly assigned to one of k clusters
step1 = kmeans(data_rfm[,c("recency","frequency", "monetary")], 
               1000, iter.max = 100, nstart = 1, algorithm = "Forgy")
centroids = aggregate(data_rfm[,c("recency","frequency", "monetary")],by=list(step1),FUN="mean")
#centroids = centroids[,c(2,3, 4)] #you could also do centroids[c,(-1)]
data_rfm$c = as.character(step1$cluster)

qplot(data_rfm$frequency, data_rfm$recency, log='xy', data= data_rfm, color = step1$cluster)
qplot(data_rfm$monetary, data_rfm$recency, log= 'xy', data= data_rfm, color = step1$cluster)
qplot(data_rfm$monetary, data_rfm$frequency, log= 'xy',  data= data_rfm, color = step1$cluster)


# _______________________________________________________________
#STEP TWO: HIERARCHICAL CLUSTERING

#MODEL
dendrogram = hclust(dist(centroids[,c("recency","frequency", "monetary")]),method="ward.D")
plot(dendrogram)

#Choose number of clusters and obtain clustering
step2 = cutree(dendrogram, 5)
centroids = aggregate(centroids[,c("frequency.prep","monetary.prep","recency.prep")],by=list(step2),FUN="mean")
centroids = centroids[,c(2:4)]

centroids = aggregate(data_rfm[,c("recency","frequency", "monetary")],by=list(step2),FUN="mean")
centroids = centroids[,c(2,3)]

# QUESTION 1: How many customers are in each cluster? 

#Solution 1: rows
#New column selecting 5 clusters 
data_rfm$c = as.character(cutree(dendrogram, 5))
# table gives us the number of customers per cluster
table(data_rfm$c)

# Cluster     1   2   3   4   5 
# Customers 696 476 894 978 819 

#Cluster 2 is the smaller, 4 is the bigger 

#Solution 2: columns 
library(data.table)
DT <- data.table(data_rfm)
DT[, .N, by = c]

# QUESTION 2: What is the mean/median RFM per cluster?

# Aggregate function splits the data into subsets, computing summary statistics for each

#Determine place of the columns  
colnames (data_rfm) 

#Results all together
aggregate(data_rfm[, 1:3], list(data_rfm$c), mean)
aggregate(data_rfm[, 1:3], list(data_rfm$c), median)

#Result Mean
#Group     recency frequency monetary   Scores Segment value
#1       1 13308.02  89.90948 4.612069  5 3 5  Champions, persuadable to cross-selling 
#2       2 14156.17  95.28992 3.970588  4 5 2  Loyal, responsive to promotions, minority
#3       3 15123.60  87.62304 4.045861  3 2 3  Need attention & brand awarening 
#4       4 16430.67  83.34765 3.947853  2 1 1  Ocassional, price-conscious, majority
#5       5 17704.83  90.64225 4.199023  1 4 4  Can't lose them, need to reactive them

#Result Median 
#Group     recency frequency monetary
#1       1 13307.5      54.5        2
#2       2 14151.0      57.5        2
#3       3 15129.5      47.0        2
#4       4 16428.0      50.0        2
#5       5 17697.0      45.0        2

#Results one by one, mean
aggregate(data_rfm$recency, list(data_rfm$c), mean)
aggregate(data_rfm$frequency, list(data_rfm$c), mean)
aggregate(data_rfm$monetary, list(data_rfm$c), mean)

#PLOTING
#Plot of the clustering (rec-freq, rec-mon, freq-mon)
qplot(data_rfm$frequency, data_rfm$recency, data= data_rfm, color = data_rfm$c)
qplot(data_rfm$monetary, data_rfm$recency, data= data_rfm, color = data_rfm$c, xlim=c(0,20))
qplot(data_rfm$monetary, data_rfm$frequency, data= data_rfm, color = data_rfm$c)


# Box plots (show the shape of the distribution)

boxplot(recency~c,data=data_rfm, main="Recency", xlab="Cluster number", ylab="Recency")
boxplot(monetary~c,data=data_rfm, main="Monetary", xlab="Cluster number", ylab="Monetary",ylim=c(0,10))
boxplot(frequency~c,data=data_rfm, main="Frequency", xlab="Cluster number", ylab="Frequency",ylim=c(0,400))
  




# _______________________________________________________________
#STEP THREE: NON HIERARCHICAL CLUSTERING: K-MEANS

#number of clusters
nclust = 5
#kmeans clustering. Forgy=random initialization
c = kmeans(data_rfm[,c("recency","frequency", "monetary")], nclust, iter.max = 100, nstart = 1, 
           algorithm = "Forgy")
#attach cluster to the data:
data_rfm$c = as.character(c$cluster)
#make a plot of the clustering
qplot(data_rfm$frequency, data_rfm$recency, data = data_rfm, color=data$c)
