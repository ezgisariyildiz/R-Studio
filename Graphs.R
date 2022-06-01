
library(car)
library(rgl)

?Prestige 
####################################################################

## scatterplot per group: grup başına dagilim grafigi
scatterplot(prestige~income|type, boxplots = FALSE, data = Prestige)
## prof: profesyonel, bc: blue collar (mavi yaka), wc: white collar (beyaz yaka)

####################################################################

## scatterplot in matrix form: matris formunda dagilim grafigi.
## ciftler halinde cizilmis dagilim grafikleri
# prestige + income + education icin olasi kombinasyonlar alttadir
# 
# prestige vs prestige
# prestige ~ income
# prestige ~ education
# 
# income vs income
# income ~ prestige
# income ~ education
# 
# education vs education
# education ~ prestige
# education ~ income
scatterplotMatrix(~ prestige + income + education, data = Prestige)
####################################################################

## 3 boyutlu grafik, scatter3d() --car paketinden, ayri bir pencerede acilacaktir
scatter3d(prestige ~ income + education, data=Duncan)

####################################################################

