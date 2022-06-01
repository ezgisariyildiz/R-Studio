library(corrplot)
library(PerformanceAnalytics)
library(tseries)
library(methods)
library(zoo)
library(xts)
library(readr)
library(data.table)

month <- month[-1,]
week <- week[-1,]

## Veri çekme
start.date = "2001-01-02"
end.date   = "2022-04-01"

nasdaq = get.hist.quote(instrument="^NDX", start = start.date, end = end.date, quote = c("Adjusted"), provider="yahoo", origin="1970-01-01", compression="d", retclass="zoo")
apple = get.hist.quote(instrument="AAPL",  start = start.date, end = end.date, quote = c("Adjusted"), provider="yahoo", origin="1970-01-01", compression="d", retclass="zoo")

nasdaq = xts(nasdaq)
colnames(nasdaq) = "nasdaq"
apple = xts(apple)
colnames(apple) = "apple"

day = merge.xts(nasdaq, apple)
day = na.omit(Return.calculate(day, method="simple"))
colnames(day)   = c("nasdaq", "apple")


## 5 Soru


## gunluk, haftalýk ve aylik getirilerin cubuk grafigi (histogram)
## verileri kiyaslanabilir yapmak icin ayni sayida cubuk (bin) kullanalim 
## Nasdaq
msftHist = hist(month[,2], plot=FALSE, breaks=20)
par(mfrow=c(1,3))
hist(month[,2], main="Nasdaq Aylýk", col="cornflowerblue")
hist(week[,2], main="Nasdaq Haftalýk", col="cornflowerblue",
     breaks=msftHist$breaks)
hist(day[,1], main="Nasdaq Günlük", col="cornflowerblue", 
     breaks=msftHist$breaks)

## Apple
msftHist = hist(month[,3], plot=FALSE, breaks=22)
par(mfrow=c(1,3))
hist(month[,3], main="Apple Aylýk", col="cornflowerblue")
hist(week[,3], main="Apple Haftalýk", col="cornflowerblue",
     breaks=msftHist$breaks)
hist(day[,2], main="Apple Günlük", col="cornflowerblue", 
     breaks=msftHist$breaks)



## 7. Soru
# Günlük nasdaq
obs = length(day[,1])
mean <- mean(day[,1])
variance <- var(day[,1])
shata <- sqrt(variance)
skew <- skewness(day[,1])
kurt <- kurtosis(day[,1])

expect1 = obs*0.68
m_s = mean - shata
mps = mean + shata
obs1 = 0
for(i in 1:length(day[,1])){
  if (m_s <= day[i,1] & day[i,1]<= mps){
    obs1 = obs1 + 1
  }
}

expect2 = obs*0.95
m_s2 = mean - (2*shata)
mps2 = mean + (2*shata)
obs2 = 0
for(i in 1:length(day[,1])){
  if (m_s2 <= day[i,1] & day[i,1]<= mps2){
    obs2 = obs2 + 1
  }
}

expect3 = obs*0.99
m_s3 = mean - (3*shata)
mps3 = mean + (3*shata)
obs3 = 0
for(i in 1:length(day[,1])){
  if (m_s3 <= day[i,1] & day[i,1]<= mps3){
    obs3 = obs3 + 1
  }
}

# Haftalýk - nasdaq

obs_week = length(week[,2])
mean_week <- mean(week[,2])
variance_week <- var(week[,2])
shata_week <- sqrt(variance)
skew_week <- skewness(week[,2])
kurt_week <- kurtosis(week[,2])

expect1_week = obs_week*0.68
m_s_week = mean_week - shata_week
mps_week = mean_week + shata_week
obs1_week = 0
for(i in 1:length(week[,2])){
  if (m_s_week <= week[i,2] & week[i,2]<= mps_week){
    obs1_week = obs1_week + 1
  }
}

expect2_week = obs*0.95
m_s2_week = mean_week - (2*shata_week)
mps2_week = mean_week + (2*shata_week)
obs2_week = 0
for(i in 1:length(week[,2])){
  if (m_s2 <= week[i,2] & week[i,2]<= mps2_week){
    obs2_week = obs2_week + 1
  }
}

expect3_week = obs_week*0.99
m_s3_week = mean_week - (3*shata_week)
mps3_week = mean_week + (3*shata_week)
obs3_week = 0
for(i in 1:length(week[,2])){
  if (m_s3_week <= week[i,2] & week[i,2]<= mps3_week){
    obs3_week = obs3_week + 1
  }
}

# Aylýk - nasdaq

obs_month = length(month[,2])
mean_month <- mean(month[,2])
variance_month <- var(month[,2])
shata_month <- sqrt(variance)
skew_month <- skewness(month[,2])
kurt_month <- kurtosis(month[,2])

expect1_month = obs_month*0.68
m_s_month = mean_month - shata_month
mps_month = mean_month + shata_month
obs1_month = 0
for(i in 1:length(month[,2])){
  if (m_s_month <= month[i,2] & month[i,2]<= mps_month){
    obs1_month = obs1_month + 1
  }
}

expect2_month = obs*0.95
m_s2_month = mean_month - (2*shata_month)
mps2_month = mean_month + (2*shata_month)
obs2_month = 0
for(i in 1:length(month[,2])){
  if (m_s2 <= month[i,2] & month[i,2]<= mps2_month){
    obs2_month = obs2_month + 1
  }
}

expect3_month = obs_month*0.99
m_s3_month = mean_month - (3*shata_month)
mps3_month = mean_month + (3*shata_month)
obs3_month = 0
for(i in 1:length(month[,2])){
  if (m_s3_month <= month[i,2] & month[i,2]<= mps3_month){
    obs3_month = obs3_month + 1
  }
}
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

nasdaqsigma <- matrix(c(obs_month,mean_month, variance_month, shata_month, skew_month, kurt_month,paste0("[",specify_decimal(m_s_month,3),",",specify_decimal(mps_month,3),"]"),obs_month,expect1_month,
                        paste0("[",specify_decimal(m_s2_month,3),",",specify_decimal(mps2_month,3),"]"),obs2_month,expect2_month,paste0("[",specify_decimal(m_s3_month,3),",",specify_decimal(mps3_month,3),"]"),obs3_month,expect3_month,
                        obs_week,mean_week, variance_week, shata_week, skew_week, kurt_week,paste0("[",specify_decimal(m_s_week,3),",",specify_decimal(mps_week,3),"]"),obs_week,expect1_week,
                        paste0("[",specify_decimal(m_s2_week,3),",",specify_decimal(mps2_week,3),"]"),obs2_week,expect2_week,paste0("[",specify_decimal(m_s3_week,3),",",specify_decimal(mps3_week,3),"]"),obs3_week,expect3_week,
                        obs,mean, variance, shata, skew, kurt,paste0("[",specify_decimal(m_s,3),",",specify_decimal(mps,3),"]"),obs,expect1,
                        paste0("[",specify_decimal(m_s2,3),",",specify_decimal(mps2,3),"]"),obs2,expect2,paste0("[",specify_decimal(m_s3,3),",",specify_decimal(mps3,3),"]"),obs3,expect3), ncol = 3, byrow = FALSE)
View(nasdaqsigma)
colnames(nasdaqsigma) <- c('Aylýk','Haftalýk',"Günlük")
rownames(nasdaqsigma) <- c('Gözlem Sayýsý','Aritmetik Ortalama','varyans',"standart sapma","çarpýklýk","basýklýk","[r-s,r+s]","gözlemlenen","beklenen","[r-2s,r+2s]","gözlemlenen","beklenen","[r-3s,r+3s]","gözlemlenen","beklenen")


## Apple - sigma tablosu

## günlük
obs = length(day[,2])
mean <- mean(day[,2])
variance <- var(day[,2])
shata <- sqrt(variance)
skew <- skewness(day[,2])
kurt <- kurtosis(day[,2])

expect1 = obs*0.68
m_s = mean - shata
mps = mean + shata
obs1 = 0
for(i in 1:length(day[,2])){
  if (m_s <= day[i,2] & day[i,2]<= mps){
    obs1 = obs1 + 1
  }
}

expect2 = obs*0.95
m_s2 = mean - (2*shata)
mps2 = mean + (2*shata)
obs2 = 0
for(i in 1:length(day[,2])){
  if (m_s2 <= day[i,2] & day[i,2]<= mps2){
    obs2 = obs2 + 1
  }
}

expect3 = obs*0.99
m_s3 = mean - (3*shata)
mps3 = mean + (3*shata)
obs3 = 0
for(i in 1:length(day[,2])){
  if (m_s3 <= day[i,2] & day[i,2]<= mps3){
    obs3 = obs3 + 1
  }
}

# Haftalýk - apple

obs_week = length(week[,3])
mean_week <- mean(week[,3])
variance_week <- var(week[,3])
shata_week <- sqrt(variance)
skew_week <- skewness(week[,3])
kurt_week <- kurtosis(week[,3])

expect1_week = obs_week*0.68
m_s_week = mean_week - shata_week
mps_week = mean_week + shata_week
obs1_week = 0
for(i in 1:length(week[,3])){
  if (m_s_week <= week[i,3] & week[i,3]<= mps_week){
    obs1_week = obs1_week + 1
  }
}

expect2_week = obs*0.95
m_s2_week = mean_week - (2*shata_week)
mps2_week = mean_week + (2*shata_week)
obs2_week = 0
for(i in 1:length(week[,3])){
  if (m_s2 <= week[i,3] & week[i,3]<= mps2_week){
    obs2_week = obs2_week + 1
  }
}

expect3_week = obs_week*0.99
m_s3_week = mean_week - (3*shata_week)
mps3_week = mean_week + (3*shata_week)
obs3_week = 0
for(i in 1:length(week[,3])){
  if (m_s3_week <= week[i,3] & week[i,3]<= mps3_week){
    obs3_week = obs3_week + 1
  }
}

# Aylýk - apple

obs_month = length(month[,3])
mean_month <- mean(month[,3])
variance_month <- var(month[,3])
shata_month <- sqrt(variance)
skew_month <- skewness(month[,3])
kurt_month <- kurtosis(month[,3])

expect1_month = obs_month*0.68
m_s_month = mean_month - shata_month
mps_month = mean_month + shata_month
obs1_month = 0
for(i in 1:length(month[,3])){
  if (m_s_month <= month[i,3] & month[i,3]<= mps_month){
    obs1_month = obs1_month + 1
  }
}

expect2_month = obs*0.95
m_s2_month = mean_month - (2*shata_month)
mps2_month = mean_month + (2*shata_month)
obs2_month = 0
for(i in 1:length(month[,3])){
  if (m_s2 <= month[i,3] & month[i,3]<= mps2_month){
    obs2_month = obs2_month + 1
  }
}

expect3_month = obs_month*0.99
m_s3_month = mean_month - (3*shata_month)
mps3_month = mean_month + (3*shata_month)
obs3_month = 0
for(i in 1:length(month[,3])){
  if (m_s3_month <= month[i,3] & month[i,3]<= mps3_month){
    obs3_month = obs3_month + 1
  }
}

applesigma <- matrix(c(obs_month,mean_month, variance_month, shata_month, skew_month, kurt_month,paste0("[",specify_decimal(m_s_month,3),",",specify_decimal(mps_month,3),"]"),obs_month,expect1_month,
                       paste0("[",specify_decimal(m_s2_month,3),",",specify_decimal(mps2_month,3),"]"),obs2_month,expect2_month,paste0("[",specify_decimal(m_s3_month,3),",",specify_decimal(mps3_month,3),"]"),obs3_month,expect3_month,
                       obs_week,mean_week, variance_week, shata_week, skew_week, kurt_week,paste0("[",specify_decimal(m_s_week,3),",",specify_decimal(mps_week,3),"]"),obs_week,expect1_week,
                       paste0("[",specify_decimal(m_s2_week,3),",",specify_decimal(mps2_week,3),"]"),obs2_week,expect2_week,paste0("[",specify_decimal(m_s3_week,3),",",specify_decimal(mps3_week,3),"]"),obs3_week,expect3_week,
                       obs,mean, variance, shata, skew, kurt,paste0("[",specify_decimal(m_s,3),",",specify_decimal(mps,3),"]"),obs,expect1,
                       paste0("[",specify_decimal(m_s2,3),",",specify_decimal(mps2,3),"]"),obs2,expect2,paste0("[",specify_decimal(m_s3,3),",",specify_decimal(mps3,3),"]"),obs3,expect3), ncol = 3, byrow = FALSE)
View(applesigma)
colnames(applesigma) <- c('Aylýk','Haftalýk',"Günlük")
rownames(applesigma) <- c('Gözlem Sayýsý','Aritmetik Ortalama','varyans',"standart sapma","çarpýklýk","basýklýk","[r-s,r+s] ","gözlemlenen","beklenen","[r-2s,r+2s]","gözlemlenen","beklenen","[r-3s,r+3s]","gözlemlenen","beklenen")


for (i in 1:3) {
  for (a in 2:6)  {
    applesigma[a,i] <- round(as.numeric(applesigma[a,i]),digits = 5)
    nasdaqsigma[a,i] <- round(as.numeric(nasdaqsigma[a,i]),digits = 5)
  }
  
}

library(gridExtra)
png("nasdaqsigma.png", height = 25*nrow(nasdaqsigma), width = 150*ncol(nasdaqsigma))
grid.table(nasdaqsigma)
dev.off()

png("applesigma.png", height = 25*nrow(applesigma), width = 150*ncol(applesigma))
grid.table(applesigma)
dev.off()


# 8. Soru- hepsi deðil

library("psych")
DataSon <- day*100

Table <- describe(DataSon)
Tablo <- t(Table) # tablonun transpozunu aldým

summary(DataSon) #1. çeyrek, 3. çeyrek özet tablosu

obs = length(day[,1])
obs
mean <- mean(day[,1]*100)
mean
variance <- var(day[,1]*100)
variance
shata <- sqrt(variance)
shata
skew <- skewness(day[,1]*100)
skew
kurt <- kurtosis(day[,1]*100)
kurt

obs = length(day[,2])
obs
mean <- mean(day[,2]*100)
mean
variance <- var(day[,2]*100)
variance
shata <- sqrt(variance)
shata
skew <- skewness(day[,2]*100)
skew
kurt <- kurtosis(day[,2]*100)
kurt

# 9. Soru
model <- lm(day[,1] ~ day[,2], data=day)
summary(model)

# 10. Soru
par(mfrow=c(1,1))
acf(day[,1], type = "correlation", lag.max=30, plot=TRUE , main = "Nasdaq ACF") 
acf(day[,2], type = "correlation", lag.max=30, plot=TRUE , main = "Apple ACF") 


# 11. Soru
AR <- arima(day[,1], order = c(1,0,1))
print(AR)
acf(AR$residuals, type = "correlation", lag.max=20, plot=TRUE , main = "nasdaq 100 ACF")

AR1 <- arima(day[,2], order = c(1,0,1))
print(AR1)
acf(AR1$residuals, type = "correlation", lag.max=20, plot=TRUE , main = "Apple ACF")


# 12.Soru
par(mfrow=c(1,1))
acf(day[,1]^2, type = "correlation", lag.max=30, plot=TRUE , main = "Nasdaq ACF") 
acf(day[,2]^2, type = "correlation", lag.max=30, plot=TRUE , main = "Apple ACF") 


#13.Soru

# Load package
library(vars)

x <- VAR(y = day)
x
plot(irf(x,n.ahead = 2))


# 14. Soru

library("rugarch")
spec = ugarchspec() #the empty function specifies the default model. 
print(spec)

def.fit = ugarchfit(spec = spec, data = day[,1])
print(def.fit)


x = garch(day[,1])
x

