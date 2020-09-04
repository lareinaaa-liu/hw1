#R basics and HW1 

#load data 

COVID19Data=read.csv('ExampleData/COVID_track_daily.csv')#read csv as data frame
class(COVID19Data)
head(COVID19Data)

TradeData=read.csv("ExampleData/Effects-of-COVID-19-on-trade-1-February-29-July-2020-provisional.csv")
head(TradeData)

#plot number of cases of COVID-19 overtime 
COVID19Data$date=as.Date(as.character(COVID19Data$date),tryFormats ="%Y%m%d")

plot(COVID19Data$positiveIncrease~COVID19Data$date,xlab="Date",ylab="New positive")

TradeData$Date=as.Date(as.character(TradeData$Date),tryFormats ="%d/%m/%Y",origin="1970-01-01")

tail(TradeData)
head(TradeData)

unique(TradeData$Direction)

VecPlot=TradeData[(TradeData$Date>="2020-01-01")&(TradeData$Direction=="Exports"),]
plot(VecPlot$Value~VecPlot$Date,ylab="trade value",xlab="Date",type="s",main="2020 Exports")


VecPlot=TradeData[(TradeData$Year=="2019")&(TradeData$Direction=="Exports"),]
plot(VecPlot$Value~VecPlot$Date,ylab="trade value",xlab="Date",type="s",main="2019 Exports")



#Correlation between trade and number of new cases 
TradeData=TradeData[TradeData$Direction=="Exports",]
AllDates=intersect(COVID19Data$date,TradeData$Date)

VecPlot=data.frame(Date=AllDates,
                   COVID=COVID19Data$positiveIncrease[match(AllDates,COVID19Data$date)],
                   Export=TradeData$Value[match(AllDates,TradeData$Date)])

VecPlot$Date=as.Date(VecPlot$Date,origin="1970-01-01")
tail(VecPlot)

plot(VecPlot$Export~VecPlot$COVID,ylab="Exports",xlab="COVID")

cor(VecPlot$Export,VecPlot$COVID,method = "pearson")
cor.test(VecPlot$Export,VecPlot$COVID,method = "pearson")

cor(VecPlot$Export,VecPlot$COVID,method = "spearman")
cor.test(VecPlot$Export,VecPlot$COVID,method = "spearman")


plot(VecPlot$Export~VecPlot$COVID,ylab="Exports",xlab="COVID")
LM=lm(VecPlot$Export~VecPlot$COVID)
abline(LM,col='blue',lty=2,lwd=2)
