setwd("/home/mario/mineria_en_accion/clustering/galaxy")

#Load the data
solutions = read.csv("solutions.csv", header = FALSE)
data = NULL
for(i in 1:10*100){
  current = read.csv(paste("out-",sprintf("%06d",i),"-100.csv",sep=""), header = FALSE)
  data <- rbind(data,current)
}

data <- data[order(data$V1),]
solutions <- solutions[order(solutions$V1),]

summary(data)

cor(data[,2:99])

set.seed(3000)

# Define the number of clusters
k <- 5

# cluster the data
cl <- kmeans(data[,2:99],k)

cl$centers

table(cl$cluster)

cluster.table <- aggregate(solutions[,2:38], by=list(cl$cluster),FUN=mean)

summary(cluster.table)

sd(cluster.table[,2])
sapply(cluster.table,1, FUN=sd)

# Let's pca the data
pca <- princomp(data[,2:99])
pca

# Rotate the data
pca.data <- predict(pca)

plot(pca.data[,1],pca.data[,2],col=as.factor(cl$cluster))
plot(pca.data[,2],pca.data[,3],col=as.factor(cl$cluster))
plot(pca.data[,2],pca.data[,4],col=as.factor(cl$cluster))


