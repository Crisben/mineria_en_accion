setwd("/home/mario/mineria_en_accion/clustering/galaxy")

solutions = read.csv("solutions.csv", header = FALSE)
data = NULL
for(i in 1:10*100){
  current = read.csv(paste("out-",sprintf("%06d",i),"-100.csv",sep=""), header = FALSE)
  data <- rbind(data,current)
}
