setwd("/home/mario/mineria_en_accion/clustering/questbee")

questbee.download <- function(questionnaire,file,key,password){
  url <- paste("http://www.questbee.com/questionnaire/download/",questionnaire,"?key=",key,"&password=",password,sep="")
  download.file(url, file, "auto")
}

#key y password generados en la pantalla de edición de la secuencia
key <- "8a7483674b7f31c0014be1c06d7e0139"
password <- "6ZH1HHOHD6SM4UVPPC0IXBZ0W4WS1EMXN2RZINMTXJ0XO9NI93"

#usar el id del questionario, ejemplo: http://questbee.com/questionnaire/fill/8a74836747ee55840147f3e66d800001
#Download the data
questbee.download("8a74836747ee55840147f3e66d800001","potential.csv",key,password)

#Load the data
data <- read.delim2("potential.csv",header=TRUE,stringsAsFactors=FALSE)
data<-data[complete.cases(data),]
option.variables <- c("q1","q2","q4","q5","q6","q7","q8","q9","q10","q12","q13","q14","q15")
checkbox.variables <- data.frame(name=c("q3","q11"),min=c(0,0),max=c(2,2))

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
    from <- c(from, data$user[i])
    to <- c(to, data$user[j])
    d <- (sum(as.numeric(data[i,option.variables]==data[j,option.variables]),na.rm=TRUE)+
            sum(as.numeric(data[i,checkbox.new.variables]==data[j,checkbox.new.variables])*checkbox.weigth.variables,na.rm=TRUE))/
      (length(option.variables)+nrow(checkbox.variables))
    distance <- c(distance,d)
  }
}

# Create distance matrix
distance.matrix <- matrix(distance,nrow=nrow(data),ncol=nrow(data))
colnames(distance.matrix)<-data$user
rownames(distance.matrix)<-data$user
d <- dist(distance.matrix)

# Generate and plot clusters
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(fit, k=3, border="red") 
