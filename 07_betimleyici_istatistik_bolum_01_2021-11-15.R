################################
options(digits=4, width=70)
library(corrplot)
library(PerformanceAnalytics)
library(tseries)
library(methods)
library(zoo)
library(xts)

## library(IntroCompFinR)
## install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")

Sys.setenv(TZ="UTC")

################################
## January 2, 1998 -- May 31, 2012

start.date = "1998-01-02"
end.date   = "2012-05-31"

# finance.yahoo.com

# msftDailyPrices = get.hist.quote(instrument="msft", start="1980-01-01", end="2021-11-15", quote = c("Open", "High", "Low", "Close", "Adjusted", "Volume"), provider="yahoo", origin="1970-01-01", compression="d", retclass="zoo")
# 
# sp500DailyPrices = get.hist.quote(instrument="^gspc", start="1980-01-01", end="2021-11-15",  quote = c("Open", "High", "Low", "Close", "Adjusted", "Volume"), provider="yahoo", origin="1970-01-01", compression="d", retclass="zoo")

msftDailyPrices = get.hist.quote(instrument="msft", start = start.date, end = end.date, quote = c("Adjusted"), provider="yahoo", origin="1970-01-01", compression="d", retclass="zoo")


sp500DailyPrices = get.hist.quote(instrument="^gspc",  start = start.date, end = end.date, quote = c("Adjusted"), provider="yahoo", origin="1970-01-01", compression="d", retclass="zoo")


msftDailyPrices = xts(msftDailyPrices)
colnames(msftDailyPrices) = "msft.prices"

sp500DailyPrices = xts(sp500DailyPrices)
colnames(sp500DailyPrices) = "sp500.prices"

# ay sonu veri seti olusturmak
msftMonthlyPrices = to.monthly(msftDailyPrices, OHLC=FALSE)
colnames(msftMonthlyPrices) = "msft.prices"

sp500MonthlyPrices = to.monthly(sp500DailyPrices, OHLC=FALSE)
colnames(sp500MonthlyPrices) = "sp500.prices"


## verileri birlestirmek 
msftSp500DailyPrices = merge.xts(msftDailyPrices, sp500DailyPrices)
msftSp500MonthlyPrices = merge(msftMonthlyPrices, sp500MonthlyPrices)

# verilerin baslangici
head(msftSp500DailyPrices, n=3)
head(msftSp500MonthlyPrices, n=3)
################################

## gunluk ve aylik getiri serilerini hesaplamak

msftMonthlyRetS                = na.omit(Return.calculate(msftMonthlyPrices, method="simple"))
colnames(msftMonthlyRetS)      = "msft.ret"

msftDailyRetS                  = na.omit(Return.calculate(msftDailyPrices, method="simple"))
colnames(msftDailyRetS)        = "msft.ret"

sp500MonthlyRetS               = na.omit(Return.calculate(sp500MonthlyPrices, method="simple"))
colnames(sp500MonthlyRetS)     = "sp500.ret"

sp500DailyRetS                 = na.omit(Return.calculate(sp500DailyPrices, method="simple"))
colnames(sp500DailyRetS)       = "sp500.ret"

msftSp500MonthlyRetS           = na.omit(Return.calculate(msftSp500MonthlyPrices, method="simple"))
colnames(msftSp500MonthlyRetS) = c("msft.ret", "sp500.ret")

msftSp500DailyRetS             = na.omit(Return.calculate(msftSp500DailyPrices, method="simple"))
colnames(msftSp500DailyRetS)   = c("msft.ret", "sp500.ret")

# verilerin baslangici
head(msftSp500DailyRetS, n=3)
head(msftSp500MonthlyRetS, n=3)

################################
# NA degerleri temizlemek

# # Not: na.omit() yerine alttakileri de kullanabilirdik. na.omit() tum NA degerlerini temizler. alttakiler sadece ilk satirdakileri siler

# msftRetS = msftRetS[-1]
# msftDailyRetS = msftDailyRetS[-1]
# sp500RetS = sp500RetS[-1]
# sp500DailyRetS = sp500DailyRetS[-1]
# msftSp500RetS = msftSp500RetS[-1]
# msftSp500DailyRetS = msftSp500DailyRetS[-1]

################################
## surekli bilesik faiz hesaplamak - Compute continously compounded returns

msftMonthlyRetC = log(1 + msftMonthlyRetS)

sp500MonthlyRetC = log(1 + sp500MonthlyRetS)


msftSp500MonthlyRetC = merge.xts(msftMonthlyRetC, sp500MonthlyRetC)

# show data
head(msftSp500MonthlyRetC, n=3)

# stop()

################################

## aylik fiyat ve getiri serilerini iki farkli panelde cizdirmek

plot(msftSp500MonthlyPrices, multi.panel=TRUE, 
                             yaxis.same=FALSE, main="", lwd=2, col="blue")

plot(msftSp500MonthlyRetS, multi.panel=TRUE, 
                           yaxis.same=FALSE, main="", lwd=2, col="blue")
       
plot(msftSp500MonthlyRetS, main="", multi.panel=FALSE, lwd=2, 
                           col=c("red", "blue"), lty=c("dashed", "solid"), 
                           legend.loc = "topright")


################################

## basit ve surekli bilesik faizleri kiyaslamak

retDiff = msftMonthlyRetS - msftMonthlyRetC

dataToPlot = merge(msftMonthlyRetS, msftMonthlyRetC, retDiff)

colnames(dataToPlot) = c("Simple", "cc", "Diff")

plot(dataToPlot, multi.panel=TRUE, yaxis.same=FALSE, main="", 
                 lwd=2, col=c("black", "blue", "red"))
         
################################

## gunluk getirileri cizdirmek
         
plot(msftSp500DailyRetS, multi.panel=TRUE, yaxis.same=FALSE,
                         main="", col="blue") 

     
################################

## gunluk ve aylik getirilerin cubuk grafigi (histogram) - Microsoft ve S&P5500
     
        
par(mfrow=c(2,2))
hist(msftMonthlyRetS, main="", col="cornflowerblue")
hist(msftDailyRetS, main="", col="cornflowerblue")
hist(sp500MonthlyRetS, main="", col="cornflowerblue")
hist(sp500DailyRetS, main="", col="cornflowerblue")


## verileri kiyaslanabilir yapmak icin ayni sayida cubuk (bin) kullanalim 
msftHist = hist(msftMonthlyRetS, plot=FALSE, breaks=15)
par(mfrow=c(2,2))
hist(msftMonthlyRetS, main="", col="cornflowerblue")
hist(msftDailyRetS, main="", col="cornflowerblue",
     breaks=msftHist$breaks)
hist(sp500MonthlyRetS, main="", col="cornflowerblue", 
     breaks=msftHist$breaks)
hist(sp500DailyRetS, main="", col="cornflowerblue",
     breaks=msftHist$breaks)
     
     
################################     

## Microsoft getirileri normal dagilima mi sahip? ilk bakis

set.seed(123)
gwnDaily = rnorm(length(msftDailyRetS), mean=mean(msftDailyRetS), sd=sd(msftDailyRetS))
gwnDaily = xts(gwnDaily, index(msftDailyRetS))

gwnMonthly = rnorm(length(msftMonthlyRetS), mean=mean(msftMonthlyRetS), sd=sd(msftMonthlyRetS))
gwnMonthly = xts(gwnMonthly, index(msftMonthlyRetS))


## simule edilmis ve gerceklesen getirileri cizdirelim

msftDailyHist = hist(msftDailyRetS, plot=FALSE, breaks=15)
par(mfrow=c(2,2))
plot.zoo(msftDailyRetS, main="Monthly Returns on MSFT", 
     lwd=2, col="blue", ylim=c(-0.15, 0.15), ylab="Returns")
abline(h=0)

plot.zoo(gwnDaily, main="Simulated Normal Returns",  
     lwd=2, col="blue", ylim=c(-0.15, 0.15), ylab="Returns")
abline(h=0)

hist(msftDailyRetS, main="", col="cornflowerblue", 
     xlab="returns")
     
hist(gwnDaily, main="", col="cornflowerblue", 
     xlab="returns", breaks=msftDailyHist$breaks)
     


################################     

##  aylik Microsoft getirilerinin duzeltilmis cubuk grafigi (smoothed histogram)

par(mfrow=c(1,1))
MSFT.density = density(msftMonthlyRetS)
hist(msftMonthlyRetS, main="", xlab="Microsoft Monthly Returns",
     col="cornflowerblue", probability=TRUE, ylim=c(0,5.5))
points(MSFT.density,type="l", col="orange", lwd=2)

################################     

##  aylik Microsoft getirilerinin ampirik birikimli dağılım fonksiyonu (empirical CDF)
Fhat.0 = sum(msftMonthlyRetS <= 0)/nrow(msftMonthlyRetS) 

plot(ecdf(coredata(msftMonthlyRetS)), main="", col="blue")

################################    

##  aylik getirilerin ampirik yuzdelik dilimleri (empirical quantiles) - Microsoft ve S&P 500

apply(msftSp500MonthlyRetS, 2, quantile)

apply(msftSp500MonthlyRetS, 2, quantile, probs=c(0.01,0.05))



################################     

med = apply(msftSp500MonthlyRetS, 2, median)
iqr = apply(msftSp500MonthlyRetS, 2, IQR)
rbind(med, iqr)

################################    


######################### BOLUM 2 ############################

