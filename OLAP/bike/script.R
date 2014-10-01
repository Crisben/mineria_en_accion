## Establecemos el directorio de trabajo
setwd("/home/mario/mineria_en_accion/OLAP/bike")

## Cargamos la data disponible

rentals <- read.csv("hour.csv")
str(rentals)
#install.packages("lubridate")
library(lubridate)
rentals$dteday<-ymd(rentals[,"dteday"])
rentals$temp <- rentals$temp*41
rentals$atemp <- rentals$atemp*50
str(rentals)
plot(rentals$dteday,rentals$cnt)
rentals$datetime <- ymd(rentals$dteday)
hour(rentals$datetime) <- rentals$hr
summary(rentals)
plot(rentals$datetime,rentals$cnt)
#install.packages("ggplot2")
library(ggplot2)
qplot(datetime,cnt,data=rentals,geom="line",color=season)

which(is.na(rentals$cnt))

#Cuboide al dia
daily.count <- aggregate(cnt ~ dteday, FUN=sum, data=rentals)
qplot(dteday,cnt,data=daily.count,geom="line")

#Cuboide al mes
aggregate(rentals$cnt, by=list(rentals$mnth), FUN=mean)
monthly.count <- aggregate(cbind(cnt=rentals$cnt), by=list(mnth=rentals$mnth), FUN=mean)
qplot(mnth,cnt,data=monthly.count,geom="line")
monthly.count <- aggregate(cbind(cnt=rentals$cnt), by=list(yr=rentals$yr,mnth=rentals$mnth), FUN=mean)
qplot(mnth,cnt,data=monthly.count,geom="line",color=yr)
monthly.count$year <- as.factor(monthly.count$yr+2011)
qplot(mnth,cnt,data=monthly.count,geom="line",color=year,xlab="Month",ylab="Total Rentals", main="Monthly Bike Rentals")

#Cuboide estacion
aggregate(rentals$cnt, by=list(workingday=rentals$workingday,weekday=rentals$weekday), FUN=length)
daily.count <- aggregate(cnt ~ dteday+season, FUN=sum, data=rentals)
aggregate(daily.count$cnt, by=list(day=weekdays(daily.count$dteday)), FUN=mean)
daily.count[daily.count$season==1,"season"] <- "winter"
daily.count[daily.count$season==2,"season"] <- "spring"
daily.count[daily.count$season==3,"season"] <- "summer"
daily.count[daily.count$season==4,"season"] <- "fall"
daily.count$season <- as.factor(daily.count$season)
boxplot(cnt~season,data=daily.count)

# Cuboide día de la semana
weekday.season.mean <- aggregate(cbind(cnt=daily.count$cnt), by=list(day=wday(daily.count$dteday),season=daily.count$season), FUN=mean)
qplot(day,cnt,geom="line",color=season,data=weekday.season.mean)+geom_hline(aes(yintercept=mean(daily.count$cnt)), colour="blue", linetype = "longdash")

# Cuboide hora
hour.season <- aggregate(cbind(cnt=rentals$cnt),by=list(hr=rentals$hr,season=rentals$season), FUN=mean, data=rentals)
qplot(hr,cnt,data=hour.season,geom="line",color=as.factor(season))

#Correlacion con temperatura
temp.weather <- aggregate(cbind(temp=rentals$temp),by=list(hr=rentals$hr,weathersit=rentals$weathersit), FUN=mean, data=rentals)
qplot(hr,temp,data=temp.weather,geom="line",color=as.factor(weathersit))
temp.season <- aggregate(cbind(temp=rentals$temp),by=list(hr=rentals$hr,season=rentals$season), FUN=mean, data=rentals)
qplot(hr,temp,data=temp.season,geom="line",color=as.factor(season))


# Cuboide clima
hour.weather <- aggregate(cbind(cnt=rentals$cnt),by=list(hr=rentals$hr,weathersit=rentals$weathersit), FUN=mean, data=rentals)
qplot(hr,cnt,data=hour.weather,geom="line",color=as.factor(weathersit))
subset(hour.weather, weathersit==4)








