#Set working directory
setwd("C:/Users/Sagar/OneDrive/Documents/Learning/Bikesharing")

#Check for and load required packages
if(!require(plyr)) install.packages("plyr")
library(plyr)
if(!require(graphics)) install.packages("graphics")
library(graphics)
if(!require(data.table)) install.packages("data.table")
library(data.table)
if(!require(grDevices)) install.packages("grDevices")
library(grDevices)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if(!require(nnet)) install.packages("nnet")
library(nnet)

#Read all data
alldata<-read.csv("hour.csv")

#Convert date to proper date format
alldata$dteday<-as.POSIXct(alldata$dteday)

#Visualize data
#Plot count as a function of date
ggplot(alldata, aes(x=dteday,color=type))+
  geom_line(aes(y=cnt,col="total"))+
  geom_line(aes(y=registered,col="registered"))+
  geom_line(aes(y=casual,col="casual"))+
  ggtitle("Number of users over time")
#Plot monthly trends
aggmonth<-aggregate(cbind(casual, registered, cnt)~mnth,data=alldata,FUN=mean)
ggplot(aggmonth, aes(x=mnth,color=type))+
  geom_line(aes(y=cnt,col="total"))+
  geom_line(aes(y=registered,col="registered"))+
  geom_line(aes(y=casual,col="casual"))+
  ggtitle("Mean users by month")
#Plot monthly trends for each year
aggmonthyr<-aggregate(cbind(casual, registered, cnt)~yr+mnth,data=alldata,FUN=mean)
ggplot(aggmonthyr, aes(x=mnth,color=type,size=factor(yr)))+
  geom_line(aes(y=cnt,col="total"))+
  geom_line(aes(y=registered,col="registered"))+
  geom_line(aes(y=casual,col="casual"))+
  ggtitle("Mean users by month and year")
#Plot hourly trends
agghr<-aggregate(cbind(casual, registered, cnt)~hr,data=alldata,FUN=mean)
ggplot(agghr, aes(x=hr,color=type))+
  geom_line(aes(y=cnt,col="total"))+
  geom_line(aes(y=registered,col="registered"))+
  geom_line(aes(y=casual,col="casual"))+
  ggtitle("Mean users by hour")
#Plot trends for day of the week
aggdy<-aggregate(cbind(casual, registered, cnt)~weekday,data=alldata,FUN=mean)
ggplot(aggdy, aes(x=weekday,color=type))+
  geom_line(aes(y=cnt,col="total"))+
  geom_line(aes(y=registered,col="registered"))+
  geom_line(aes(y=casual,col="casual"))+
  ggtitle("Mean users by day of the week")
#Plot trends by business days and holidays
aggwkdy<-aggregate(cbind(casual, registered, cnt)~workingday,data=alldata,FUN=mean)
ggplot(aggwkdy, aes(x=workingday,color=type))+
  geom_line(aes(y=cnt,col="total"))+
  geom_line(aes(y=registered,col="registered"))+
  geom_line(aes(y=casual,col="casual"))+
  ggtitle("Mean users by business days and holidays")
#Plot trends by weather
aggwthr<-aggregate(cbind(casual, registered, cnt)~weathersit,data=alldata,FUN=mean)
ggplot(aggwthr, aes(x=weathersit,color=type))+
  geom_line(aes(y=cnt,col="total"))+
  geom_line(aes(y=registered,col="registered"))+
  geom_line(aes(y=casual,col="casual"))+
  ggtitle("Mean users by weather")
#Plot trends by temperature
aggtemp<-aggregate(cbind(casual, registered, cnt)~temp,data=alldata,FUN=mean)
ggplot(aggtemp, aes(x=temp,color=type))+
  geom_line(aes(y=cnt,col="total"))+
  geom_line(aes(y=registered,col="registered"))+
  geom_line(aes(y=casual,col="casual"))+
  ggtitle("Mean users by temperature")

#Create X
X=alldata[,c(4,5,8,6,3,7,9,10,11,12,13,14)]
#Create dummy variables for day of week
X$Monday<-as.numeric(X$weekday==1)
X$Tuesday<-as.numeric(X$weekday==2)
X$Wednesday<-as.numeric(X$weekday==3)
X$Thursday<-as.numeric(X$weekday==4)
X$Friday<-as.numeric(X$weekday==5)
X$Saturday<-as.numeric(X$weekday==6)
#Create dummy variable for month of year
X$January<-as.numeric(X$mnth==1)
X$February<-as.numeric(X$mnth==2)
X$March<-as.numeric(X$mnth==3)
X$April<-as.numeric(X$mnth==4)
X$May<-as.numeric(X$mnth==5)
X$June<-as.numeric(X$mnth==6)
X$July<-as.numeric(X$mnth==7)
X$August<-as.numeric(X$mnth==8)
X$September<-as.numeric(X$mnth==9)
X$October<-as.numeric(X$mnth==10)
X$November<-as.numeric(X$mnth==11)
#Create dummy variable for hour of day
X$am12<-as.numeric(X$hr==0)
X$am1<-as.numeric(X$hr==1)
X$am2<-as.numeric(X$hr==2)
X$am3<-as.numeric(X$hr==3)
X$am4<-as.numeric(X$hr==4)
X$am5<-as.numeric(X$hr==5)
X$am6<-as.numeric(X$hr==6)
X$am7<-as.numeric(X$hr==7)
X$am8<-as.numeric(X$hr==8)
X$am9<-as.numeric(X$hr==9)
X$am10<-as.numeric(X$hr==10)
X$am11<-as.numeric(X$hr==11)
X$pm12<-as.numeric(X$hr==12)
X$pm1<-as.numeric(X$hr==13)
X$pm2<-as.numeric(X$hr==14)
X$pm3<-as.numeric(X$hr==15)
X$pm4<-as.numeric(X$hr==16)
X$pm5<-as.numeric(X$hr==17)
X$pm6<-as.numeric(X$hr==18)
X$pm7<-as.numeric(X$hr==19)
X$pm8<-as.numeric(X$hr==20)
X$pm9<-as.numeric(X$hr==21)
X$pm10<-as.numeric(X$hr==22)
X$pm11<-as.numeric(X$hr==23)
#Create dummy variable for weather
X$Sunny<-as.numeric(X$weathersit==1)
X$Misty<-as.numeric(X$weathersit==2)
X$LightPrec<-as.numeric(X$weathersit==3)
X$HeavyPrec<-as.numeric(X$weathersit==4)
#Reorder columns in X
tempx<-X[,c(1,19:29,13:18,30:53,6:7,9:12,54:57)]
X<-tempx
X<-data.matrix(X,rownames.force=1)
Xwb<-cbind(matrix(1,nrow(X),1),X)

#Create ys
y.reg<-data.matrix(alldata$registered, rownames.force=1)
y.cas<-data.matrix(alldata$casual, rownames.force=1)
y.tot<-data.matrix(alldata$cnt, rownames.force=1)

#Create train, CV and test sets
#Calculate size of sets
m.train=ceiling(0.6*(nrow(Xwb)))
m.cv=ceiling((nrow(Xwb)-m.train)/2)
m.test=nrow(Xwb)-m.train-m.cv
#Find random indices for training set
train.in<-sample(1:nrow(Xwb),m.train,replace=FALSE)
#Create training set
Xwb.train<-Xwb[train.in,]
X.train<-X[train.in,]
y.reg.train<-y.reg[train.in,,drop=FALSE]
y.cas.train<-y.cas[train.in,,drop=FALSE]
y.tot.train<-y.tot[train.in,,drop=FALSE]
#Create not-training set
Xwb.nottrain<-Xwb[-c(train.in),]
X.nottrain<-X[-c(train.in),]
y.reg.nottrain<-y.reg[-c(train.in),,drop=FALSE]
y.cas.nottrain<-y.cas[-c(train.in),,drop=FALSE]
y.tot.nottrain<-y.tot[-c(train.in),,drop=FALSE]

#Define neural network
insize=ncol(Xwb.train)-1
hidsize=25
outsize=1

#Initialize thetas
source("RandInitTheta.R")
thetainit1vec<-RandInitTheta(insize,hidsize)
thetainit2vec<-RandInitTheta(hidsize,outsize)
inittheta<-c(thetainit1vec,thetainit2vec)

#Optimize
optim.theta<-optim(inittheta,fn=ComputeCostNN3,gr=ComputeGradNN3,method='BFGS',control=list(maxit=100),insize=insize,hidsize=hidsize,outsize=outsize,X=Xwb.train,y=y.reg.train,lambda=0)



