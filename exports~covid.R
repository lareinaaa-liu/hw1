library(readr)
trade = read_csv('~/Desktop/Effects-of-COVID-19-on-trade-1-February-29-July-2020-provisional.csv')
class(trade)
head(trade)

COVID19 = read_csv('~/Desktop/COVID_track_daily.csv')
head(COVID19)

COVID19$date = as.Date(as.character(COVID19$date), tryFormats = '%Y%m%d')
head(COVID19)

plot(COVID19$positiveIncrease~COVID19$date, xlab = 'Date', ylab = 'Positive Increasing')

trade$Date = as.Date(as.character(trade$Date),tryFormats ="%d/%m/%Y",origin="1970-01-01")
head(trade)

unique(trade$Direction)
Vecplot=trade[(trade$Date>="2020-01-01")&(trade$Direction=="Exports"),]
plot(Vecplot$Value~Vecplot$Date, xlab='Date', ylab='Value', main='2020 Exports', type='s')

Vecplot=trade[(trade$Year=='2019')&(trade$Direction=='Exports'),]
plot(Vecplot$Value~Vecplot$Date, xlab='Date', ylab='Value', main='2019 Exports', type='s')

trade=trade[trade$Direction=='Exports',]
Date=intersect(trade$Date, COVID19$date)
head(Date)

VecPlot=data.frame(Date=Date,
                   COVID=COVID19$positiveIncrease[match(Date,COVID19$date)],
                   Export=trade$Value[match(Date,trade$Date)])

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
