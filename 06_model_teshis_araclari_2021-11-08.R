###############################
## Teshis Yontemleri (Diagnostic Tools)
###############################

## y = alpha + beta*x + epsilon : G-M teoremi epsilon ~ i.i.d. N(0,sigma^2)

## y^(sapka) = alpha^(sapka)+ beta^(sapka)*x
## y  = y^(sapka) + e^(sapka)

############################
## Kalintilar ve uydurulmus degerler (Residuals vs Fitted)
############################

## ornek: ikinci dereceden bir denklem
x = 1:20
x
y = x^2
y
plot(lm(y~x))

## Cars veri seti

?cars

layout(1)
plot(lm(dist~speed, data = cars))

## Boston evleri
library(mlbench)
data("BostonHousing")
?BostonHousing
plot(lm(medv~crim + rm + tax + lstat, data = BostonHousing))


############################
## Q-Q grafigi (Quantile-to-Quantile (QQ) plot)
############################

model <-lm(dist~speed, data=cars)
plot(model)

####
## Diger normal dagilim kontrolleri
####

####
## Dagilim grafigi
####
d<- density(model[['residuals']])
?density
plot(d, main="kalinti KDE grafigi", xlab="kalinti degerleri")

####
## Ampirik birikimli dagilim fonksiyonu (ECDF)
## (Empirical Cumulative Distribution Function)
####

plot(ecdf(model[['residuals']]), main='Ampirik birikimli dagilim grafigi', xlab=' kalinti degerleri')

####
## Shapiro Test
####

shapiro.test(model[['residuals']])
?shapiro.test


##Soru: Eger normal dagilim ihlal ediliyorsa ne yapabiliriz?

####
## Veriyi donustur (transforming data)
####

model <-lm(log(dist)~speed, data=cars)
plot(model)

d<- density(model[['residuals']])

plot(d, main="kalinti KDE grafigi", xlab="kalinti degerleri")

shapiro.test(model[['residuals']])

?lm
?glm

############################
## Olcek konum grafigi (Scale location plot)
############################

model <-lm(dist~speed, data=cars)
plot(model)

####
## Breusch-Pagan testi
####

library(lmtest)
model <-lm(dist~speed, data=cars)
bptest(model)
?bptest

library(mlbench)
data("BostonHousing")
plot(lm(medv~crim + rm + tax + lstat, data = BostonHousing))

model<-lm(medv~crim + rm + tax + lstat, data = BostonHousing)
bptest(model)

############################
## Kalinti - Kaldirac grafigi (Residuals vs Leverages plot)
############################

x = c(1,2,3,4,5,20)
y = c(1,2,3,4,5,20)

plot(x,y)
abline(lm(y~x))

y = c(1,2,7,4,5,20)
abline(lm(y~x), col = "green")

y = c(1,2,3,4,5,24)
abline(lm(y~x), col = "red")

model <-lm(dist~speed, data=cars)
plot(model)

