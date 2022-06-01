# ~: tilde isareti
# #: number sign - diyez isareti 
# https://learnr4free.com - Cok sayida Turkce kaynak
# Guzel bir ornek kitap ve uygulamalar: Ahmet Akgul: https://rpydaneogrendim.github.io/rEkonometri/

#################################################################
##Lineer regresyon
#################################################################
#R'de lineer regresyon lm() fonksiyonu ile tahmin edilir
#################################################################
#ornek: ustel buyume modeli y(t) = y_0* e^(k*t) simule etmek ve parametrelerini tashmin etmek

#zaman ekseni
t <- seq(0,10, by = 0.01)
t

#k = 0.33 ve baslangic degeri y_0 = 1000 olacak sekilde y degerlerini simüle et
y <- 1000*exp(0.33*t)
y
plot(y~t, main="Nufusun buyumesi - gurultusuz")

# gurultu eklemek

y <- y * rnorm(n = length(y), mean = 1, sd = 0.1)
y
#cizdirelim
plot(y~t, main="Nufusun buyumesi - gurultulu")


## ln alalim. z = ln(y_t) =ln(y_0) + k*t = alpha + beta*t
## alpha = ln(y_0)
## beta = k

##y degiskenini donusturelim
z <- log(y)
z

#model kuralim
#z: bagimli degisken
#t: bagimsiz degisken
mod <- lm(z~t)
summary(mod)

# katsayilari ayirmak (extract the coefficients)
mod.c <- coefficients(mod)
mod.c

# alpha'yi ayirmak
alpha <- mod.c[1]
alpha

# beta'yi ayirmak
beta <- mod.c[2]
beta

#y_0'i hesaplamak
#y_0 = e^alpha
y0 <- exp(alpha)
y0

#k'yi hesaplamak
#k = beta
k <- beta
k

#tahmin edilen parametreleri yazdirmak
sprintf("y0 = %s; k = %s", y0, k)

#gercek degerlerin: y0 = 1000 ve k = 0.33 oldugunu biliyoruz
#tahminlerimizin guven araligini olusturalim

# tahmin edilen parametrelerimizin guven araligi
mod.i <- confint(mod, level = 0.95)
mod.i


##y0 icin guven araligi

alt <- exp(mod.i[1,1])
alt
ust <- exp(mod.i[1,2])
ust

#yazdiralim
sprintf("guven araligi y0: %s- %s", round(alt,1), round(ust,1))


#################################################################
## Model Secimi
#################################################################

y
t
#veriyi cizdirmek
plot(y~t, main = "veriye ilk bakis")


plot(log(y)~t, main = "log(y)~t")

# veri cercevesi (data.frame) olusturmak

veri <- data.frame(log.y = log(y), t1 = t, t2 = t^2, t3 = t^3)
veri

# modeli tahmin etmek
mod <- lm(log.y ~ t1 + t2 + t3, data = veri)

#ozet sonuclar

summary(mod)

## yeni model: 

mod2 <- lm(log.y ~ t1, data = veri)
mod2
#ozet sonuclar

summary(mod2)


## katsayıları ayiklamak

mod.c <- coef(mod2)
mod.c

## y0

y0 <- exp(mod.c[1])
y0

## k

k<- mod.c[2]

## sonuclari yazdiralim
sprintf("y0 = %s; k = %s", y0,k)
