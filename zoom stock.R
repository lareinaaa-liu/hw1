library(readxl)
head(Zoom_3)

Zoom_3$Date=as.Date(Zoom_3$Date)
head(Zoom_3)

Vecplot = Zoom_3[(Zoom_3$Date>='2020-01-01')&(Zoom_3$`Adj Close`),]
head(Vecplot)
plot(Vecplot$`Adj Close`~Vecplot$Date, xlab='Date', ylab='Adj Close', main='2020 Zoom stock', type='s')

Vecplot = Zoom_3[(Zoom_3$Date<='2020-01-01')&(Zoom_3$`Adj Close`),]
tail(Vecplot)
plot(Vecplot$`Adj Close`~Vecplot$Date, xlab='Date', ylab='Adj Close', main='2019 Zoom stock', type='s')

head(COVID19)
head(Zoom_3)
Alldates=intersect(Zoom_3$Date,COVID19$date)

COVID=data.frame(COVID19$positiveIncrease[match(Alldates, COVID19$date)])

ZoomStock=data.frame(Zoom_3$`Adj Close`[match(Alldates,Zoom_3$Date)])

VecPlot=data.frame(Date=Alldates,
                   COVID=COVID19$positiveIncrease[match(Alldates,COVID19$date)],
                   ZoomStock=Zoom_3$`Adj Close`[match(Alldates,Zoom_3$Date)])

tail(VecPlot)
plot(VecPlot$ZoomStock~VecPlot$COVID, xlab='COVID', ylab = 'Zoom Stock', main='Zoom Stock')
LM=lm(VecPlot$ZoomStock~VecPlot$COVID)
abline(LM, col='blue', lty=2, lwd=1)

cor(VecPlot$ZoomStock, VecPlot$COVID, method = 'pearson')
cor.test(VecPlot$ZoomStock, VecPlot$COVID, method = 'pearson')

cor(VecPlot$ZoomStock, VecPlot$COVID, method = 'spearman')


