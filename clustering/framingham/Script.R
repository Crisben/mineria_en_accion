setwd("/home/mario/mineria_en_accion/clustering/framingham")

# Read in the dataset
framingham = read.csv("framingham.csv")

# Look at structure
str(framingham)

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

train <- na.omit(train)

# Define the number of clusters
k <- 4

# cluster de data
cl <- kmeans(train[,1:15],k)

#Let's check the clusters
table(train$TenYearCHD)
table(train$TenYearCHD,cl$cluster)

#Function to return the closest
closest.cluster <- function(x) {
  cluster.dist <- apply(cl$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}

# Cluster the test data
test.cluster <- apply(test[,1:15], 1, closest.cluster)
table(test$TenYearCHD,test.cluster)

# load library randomForest
library(randomForest)

for(cluster in 1:k){
  
  # select the data in the current cluster
  cluster.train <- train[cl$cluster==cluster,]
  cluster.test <- test[test.cluster==cluster,]
  
  # Convert outcome to factor
  cluster.train$TenYearCHD = as.factor(cluster.train$TenYearCHD)
  cluster.test$TenYearCHD = as.factor(cluster.test$TenYearCHD)
  
  # train random forest
  clusterForest = randomForest(TenYearCHD ~ ., data = cluster.train, ntree=100, nodesize=3 )
  
  # Make predictions
  PredictForest = predict(clusterForest, newdata = cluster.test)
  print(table(cluster.test$TenYearCHD, PredictForest))
}

# Accuracy
(165+3+406+2+388+1+104+8)/(1484)
