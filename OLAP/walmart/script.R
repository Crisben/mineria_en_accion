## Establecemos el directorio de trabajo
setwd("/home/mario/mineria_en_accion/OLAP/walmart")

## Cargamos la data disponible
sales <- read.csv("data.csv")
features <- read.csv("features.csv")
stores <- read.csv("stores.csv")

## Describir la data
summary(sales)
install.packages("lubridate")
library(lubridate)
sales$Date<-ymd(sales[,"Date"])
summary(sales)
max(sales$Date) - min(sales$Date) + 6
str(sales)
head(sales)
tail(sales)

features$Date<-ymd(features[,"Date"])

pairs <- unique(sales[,c("Store","Dept")])
table(pairs$Store)
count.departments <- aggregate(cbind(Departments=pairs$Dept),by=list(Store=pairs$Store),FUN=length)

merge(stores,count.departments)

#roll-up
store.sales <- aggregate(Weekly_Sales ~ Store,FUN=sum,na.rm=TRUE,data=sales)
colnames(store.sales)[4] <- "Sales"
store.sales <- merge(stores,store.sales)
store.sales <- merge(store.sales,count.departments)
store.features <- aggregate(cbind(Temperature=features$Temperature,Fuel_Price=features$Fuel_Price,CPI=features$CPI,Unemployment=features$Unemployment),by=list(Store=features$Store),FUN=mean,na.rm=TRUE)
store.sales <- merge(store.sales,store.features)
str(store.sales)

#Histograma de las ventas
hist(store.sales$Sales)
summary(store.sales)

#install.packages("ggplot2")
library(ggplot2)
pairs(store.sales[,3:9],col=store.sales$Type)

#drill down
store.department.sales <- aggregate(Weekly_Sales ~ Store + Dept, FUN=sum, na.rm=TRUE, data=sales)
colnames(store.department.sales)[3] <- "Sales"

hist(subset(store.department.sales,Dept==1)$Sales)
#Qué tiendas vendieron menos de un millon de dolares?
subset(store.department.sales, Sales < 1000000 & Dept==1)

boxplot(Sales~Dept,data=store.department.sales)
aggregate(store.department.sales$Sales,by=list(store.department.sales$Dept),FUN=mean)

#slice/dice
dept92.sales <- subset(store.department.sales, Dept == 92, select = c(Store,Sales))
dept92.sales[order(-dept92.sales$Sales),]
dept92 <- subset(sales, Dept == 92, select = c(Date,Store,Weekly_Sales))

qplot(Date,Weekly_Sales,geom="line",color=as.factor(Store),data=dept92)
qplot(yday(Date),Weekly_Sales,geom="line",color=as.factor(year(Date)),data=subset(dept92, Store==14))
qplot(yday(Date),Weekly_Sales,geom="line",color=as.factor(year(Date)),data=subset(sales, Store==2 & Dept==92))
qplot(yday(Date),Weekly_Sales,geom="line",color=as.factor(year(Date)),data=subset(sales, Store==20 & Dept==92))

qplot(yday(Date),Weekly_Sales,geom="line",color=as.factor(year(Date)),data=subset(sales, Store==14 & Dept==1))
qplot(yday(Date),Weekly_Sales,geom="line",color=as.factor(year(Date)),data=subset(sales, Store==2 & Dept==1))
qplot(yday(Date),Weekly_Sales,geom="line",color=as.factor(year(Date)),data=subset(sales, Store==20 & Dept==1))

qplot(yday(Date),Weekly_Sales,geom="line",color=as.factor(year(Date)),data=subset(sales, Store==14 & Dept==2))
qplot(yday(Date),Weekly_Sales,geom="line",color=as.factor(year(Date)),data=subset(sales, Store==2 & Dept==2))
qplot(yday(Date),Weekly_Sales,geom="line",color=as.factor(year(Date)),data=subset(sales, Store==20 & Dept==2))

