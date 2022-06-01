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



################################
## surekli bilesik faiz hesaplamak - Compute continously compounded returns

msftMonthlyRetC = log(1 + msftMonthlyRetS)

sp500MonthlyRetC = log(1 + sp500MonthlyRetS)


msftSp500MonthlyRetC = merge.xts(msftMonthlyRetC, sp500MonthlyRetC)



################################

## basit ve surekli bilesik faizleri kiyaslamak

retDiff = msftMonthlyRetS - msftMonthlyRetC

dataToPlot = merge(msftMonthlyRetS, msftMonthlyRetC, retDiff)

colnames(dataToPlot) = c("Simple", "cc", "Diff")

################################


## Microsoft getirileri normal dagilima mi sahip? ilk bakis
# 
set.seed(123)
gwnDaily = rnorm(length(msftDailyRetS), mean=mean(msftDailyRetS), sd=sd(msftDailyRetS))
gwnDaily = xts(gwnDaily, index(msftDailyRetS))

gwnMonthly = rnorm(length(msftMonthlyRetS), mean=mean(msftMonthlyRetS), sd=sd(msftMonthlyRetS))
gwnMonthly = xts(gwnMonthly, index(msftMonthlyRetS))

################################     


################################    
# stop()

######################### BOLUM 2 ############################

################################ 
## orneklem otokovaryansi ve otokorrelasyonu (sample autocovariances and autocorrelations)

## aylik veri
acf(coredata(msftMonthlyRetS), lag.max=5, plot=FALSE)

## gunluk veri
acf(coredata(msftDailyRetS), lag.max=5, plot=FALSE)

## otokorrelasyonlari cizdirmek

par(mfrow=c(2,2))
acf(coredata(msftMonthlyRetS), main="msftMonthlyRetS", lwd=2)
acf(coredata(sp500MonthlyRetS), main="sp500MonthlyRetS", lwd=2)
acf(coredata(msftDailyRetS), main="msftDailyRetS", lwd=2)
acf(coredata(sp500DailyRetS), main="sp500DailyRetS", lwd=2)

################################ 
## iki değişkenli betimleyici istatistikler


## dagilim grafigi (scatterplot)

par(mfrow=c(1,2)) 
plot(coredata(sp500MonthlyRetS),coredata(msftMonthlyRetS),
     main="Monthly returns", xlab="S&P500", ylab="MSFT", lwd=2,
     pch=16, cex=1.25, col="blue") 
abline(v=mean(sp500MonthlyRetS))   
abline(h=mean(msftMonthlyRetS)) 
plot(coredata(sp500DailyRetS),coredata(msftDailyRetS),           
     main="Daily returns", xlab="S&P500", ylab="MSFT", lwd=2, 
     pch=16, cex=1.25, col="blue") 
abline(v=mean(sp500DailyRetS))   
abline(h=mean(msftDailyRetS))

## ikiserli cizimler
dataToPlot = merge(gwnMonthly,msftMonthlyRetS,sp500MonthlyRetS)
pairs(coredata(dataToPlot), col="blue", pch=16, cex=1.25, cex.axis=1.25)


## orneklem kovaryansi ve korelasyonu (microsoft ve s&p 500 getirileri)

## aylik veri
cov(sp500MonthlyRetS, msftMonthlyRetS)

cor(sp500MonthlyRetS, msftMonthlyRetS)

## gunluk veri
cov(sp500DailyRetS, msftDailyRetS) 

cor(sp500DailyRetS, msftDailyRetS) 

## matris kullanimi

cov(msftSp500MonthlyRetS)

cor(msftSp500MonthlyRetS)

## orneklem kovaryans matrisini korelasyon matrisine donusturmek

cov2cor(cov(msftSp500MonthlyRetS))


## korelasyon matrislerini gorsellestirmek

dataToPlot = merge(gwnMonthly, msftMonthlyRetS, sp500MonthlyRetS)
cor.mat = cor(dataToPlot)
corrplot.mixed(cor.mat, lower="number", upper="ellipse")


## capraz gecikmeli kovaryans ve otokorrelasyon 

Ghat = acf(coredata(msftSp500MonthlyRetS), type="covariance",
           lag.max=5, plot=FALSE) 
Chat = acf(coredata(msftSp500MonthlyRetS), type="correlation", 
           lag.max=5, plot=FALSE) 
names(Ghat)


# Chat0 
Chat$acf[1,,] 

# Chat1 
Chat$acf[2,,]

## tumunu listelemek
Chat

plot(Chat, lwd=2)


## kayan betimleyici istatistikler

args(zoo:::rollapply.zoo)

## kayan ortalamalari hesaplamak

roll.data = merge(msftMonthlyRetS,sp500MonthlyRetS,gwnMonthly)
colnames(roll.data) = c("MSFT", "SP500", "GWN")
roll.muhat = rollapply(roll.data, width=24, by=1, by.column=TRUE,                       
                       FUN=mean, align="right") 
class(roll.muhat) 

# baslangic degerleri listelemek
head(roll.muhat, n=3)
# NA degerlerini silerek  baslangic degerleri listelemek
head(na.omit(roll.muhat), n=3)
# NA degerlerini silerek  bitis degerleri listelemek
tail(na.omit(roll.muhat), n=3)

plot(roll.muhat, main="", multi.panel=FALSE, lwd=2, 
     col=c("black", "red", "green"), lty=c("solid", "solid", "solid"), 
     major.ticks="years", grid.ticks.on="years",
     legend.loc = "topright")
     
## degerlerin araligini listelemek

ans = apply(na.omit(roll.muhat),2,range)
rownames(ans) = c("Min", "Max")

ans

## kayan standard sapmalari  hesaplamak
roll.sigmahat = rollapply(roll.data,width=24, by=1, by.column=TRUE,
                          FUN=sd, align="right")

                          
plot(roll.sigmahat, main="", multi.panel=FALSE, lwd=2, 
     col=c("black", "red", "green"), lty=c("solid", "solid", "solid"), 
     major.ticks="years", grid.ticks.on="years",
     legend.loc = "topright")                          
                          
## degerlerin araligini listelemek

ans = apply(na.omit(roll.sigmahat),2,range)
rownames(ans) = c("Min", "Max")

ans                          

## kayan korelasyonlari hesaplamak

rhohat = function(x) {   
   corhat = cor(x)   
   corhat.vals = corhat[lower.tri(corhat)]   
   names(corhat.vals) = c("MSFT.SP500", "MSFT.GWN", "SP500.GWN")   
   corhat.vals 
}
## isimleri kontrol edelim
cov.matrisi = cov(roll.data)
lower.tri = lower.tri(cov.matrisi)
cov(roll.data)[lower.tri]



roll.rhohat = rollapply(roll.data, width=24, FUN=rhohat,                          
   by=1, by.column=FALSE, align="right") 
head(na.omit(roll.rhohat),n=3)


plot(roll.rhohat, main="", multi.panel=FALSE, lwd=2, 
     col=c("black", "red", "green"), lty=c("solid", "solid", "solid"), 
     major.ticks="years", grid.ticks.on="years",
     legend.loc = "bottomleft")            
     
## degerlerin araligini listelemek
ans = apply(na.omit(roll.rhohat),2,range)
rownames(ans) = c("Min", "Max")

ans     
     
     
