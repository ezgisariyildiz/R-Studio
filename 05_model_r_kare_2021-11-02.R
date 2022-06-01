############################
## Model secimi
############################


## R-kare (R-squared)
## ongoru (forecasting)

## Ornek: y = 2sin(x) + 1 verisetini simule ediyoruz.
## R-karedeki degisimlerin anlamlilik duzeyi uzerine etkilerini inceleyelim.

#x-degerlerini olusturmak
x <- seq(0, 2*pi, by = 0.01)
x

# y-degerlerini olusturmak
y<- 2*sin(x) + 1
y

# y: dusuk seviye gurultu 
y.dusuk <- y + rnorm(n = length(y), mean = 0, sd = 0.1)

# y: orta seviye gurultu 
y.orta <- y + rnorm(n = length(y), mean = 0, sd = 1)

# y: yuksek seviye gurultu 
y.yuksek <- y + rnorm(n = length(y), mean = 0, sd = 10)

# cizdirelim
layout(t(1:3))
plot(y.dusuk ~ x,  main = "Dusuk Gurultu")
plot(y.orta ~ x,   main = "Orta Gurultu")
plot(y.yuksek ~ x, main = "Yuksek Gurultu")

# dusuk seviye gurultu
summary(lm(y.dusuk~sin(x)))

# orta seviye gurultu
summary(lm(y.orta~sin(x)))

# yuksek seviye gurultu
summary(lm(y.yuksek~sin(x)))
