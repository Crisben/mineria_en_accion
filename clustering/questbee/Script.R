setwd("/home/mario/mineria_en_accion/clustering/questbee")

#Load the data
data <- read.delim2("potential.csv",header=FALSE,stringsAsFactors=FALSE)
option.variables <- c("V4","V5","V7","V8","V9","V10","V11","V12","V13","V15","V16","V17","V18")
checkbox.variables <- data.frame(name=c("V6","V14"),min=c(0,0),max=c(2,2))

#Split data between brackets
for(pos in 1:nrow(checkbox.variables)){
  name <- as.character(checkbox.variables$name[pos])
  col <- strsplit(gsub("\\]","",gsub("\\[","",data[,name])),", ")
  for(m in checkbox.variables$min[pos]:checkbox.variables$max[pos]){
    data[paste(name,as.character(m),sep=".")]<-as.array(unlist(lapply(col,function(x){any(as.numeric(x)==m)})))
  }
}

#Create dummy variables for checkbox options
checkbox.variables$size <- checkbox.variables$max - checkbox.variables$min + 1
checkbox.new.variables <- NULL
checkbox.weigth.variables <- NULL
for(pos in 1:nrow(checkbox.variables)){
  for(m in checkbox.variables$min[pos]:checkbox.variables$max[pos]){
    checkbox.new.variables <- c(checkbox.new.variables,paste(as.character(checkbox.variables$name[pos]),as.character(m),sep="."))
    checkbox.weigth.variables <- c(checkbox.weigth.variables,1/checkbox.variables$size[pos])
  }
}

# Compute distance between individuals
data.rows <- nrow(data)
from <- NULL
to <- NULL
distance <- NULL
for(i in 1:data.rows){
  for(j in 1:data.rows){
    from <- c(from, data$V3[i])
    to <- c(to, data$V3[j])
    d <- (sum(as.numeric(data[i,option.variables]==data[j,option.variables]),na.rm=TRUE)+
            sum(as.numeric(data[i,checkbox.new.variables]==data[j,checkbox.new.variables])*checkbox.weigth.variables,na.rm=TRUE))/
      (length(option.variables)+nrow(checkbox.variables))
    distance <- c(distance,d)
  }
}

# Create distance matrix
distance.matrix <- matrix(distance,nrow=7,ncol=7)
colnames(distance.matrix)<-data$V3
rownames(distance.matrix)<-data$V3
d <- dist(distance.matrix)

fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(fit, k=3, border="red") 
