library(readr)

COVID_track_daily$date=as.Date(as.character(COVID_track_daily$date), tryFormats = '%Y%m%d')
head(COVID_track_daily)

Amazon$Date=as.Date(Amazon$Date)
vecplot=Amazon[(Amazon$Date>='2020-01-02')&(Amazon$Adj.Close),]
head(vecplot)
plot(vecplot$Adj.Close~vecplot$Date, xlab='Date', ylab = 'Adj Close', main='2020 Amazon Stock')

AllDates=intersect(Amazon$Date, COVID_track_daily$date)

VecPlot=data.frame(Date=AllDates,
                   COVID=COVID_track_daily$positiveIncrease[match(AllDates,COVID_track_daily$date)],
                   AmazonStock=Amazon$Adj.Close[match(AllDates,Amazon$Date)])

plot(VecPlot$AmazonStock~VecPlot$COVID, xlab='COVID', ylab = 'Amazon Stock')

cor(VecPlot$AmazonStock,VecPlot$COVID,method = 'pearson')
cor.test(VecPlot$AmazonStock,VecPlot$COVID,method = 'pearson')

cor(VecPlot$AmazonStock,VecPlot$COVID,method = 'spearman')
cor.test(VecPlot$AmazonStock,VecPlot$COVID,method = 'pearson')
