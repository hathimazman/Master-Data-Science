install.packages("rmarkdown")

# Pembersihan Data

## 1. Kenalpasti corak data-data lenyap
library(mice)

MData = read.csv(file.choose(), sep = ";")
head(MData, 10)

md.pattern(MData) # Kenal pasti lokasi data lenyap
str(MData)
summary(MData)


## 2. Keluarkan cerapan yang mengandungi data lenyap
MData2 = MData[complete.cases(MData),]
MData2

### 2.1 Lihat cerapan yang mempunyai data lenyap
MData[!complete.cases(MData),]

## 3. Lengkapkan data lenyap secara manual

MData$crim
MData$indus
indus.fix = edit(MData$indu) # Tak tukar ori data
indus.fix

## 4. Gunakan sukatan memusat sebagai anggaran terhadap data lenyap
attach(MData)

par(mfrow =c(1,3))
hist(crim) #tak simetri
hist(indus) #tak simteri
hist(medv) # tak simetri
hist(rm) # simetri

### 4.2 Untuk data taburan bersifat pincang/bukan simetri: median boleh digunakan.
#### Kenal pasti median data

#### crim
median.crim = median(crim, na.rm=T)
median.crim
crim.fix = ifelse(is.na(crim), median.crim, crim)

par(mfrow = c(1,2))
hist(crim, main="Bentuk taburan data asal")
hist(crim.fix, main="Bentuk taburan data dengan anggaran median")

#### indus
median.indus = median(indus, na.rm=T)
median.crim
indus.fix = ifelse(is.na(indus), median.indus, indus)

par(mfrow = c(1,2))
hist(indus, main="Bentuk taburan data asal")
hist(indus.fix, main="Bentuk taburan data dengan anggaran median")

#### medv
median.medv = median(medv, na.rm=T)
median.medv
medv.fix = ifelse(is.na(medv), median.medv, medv)

par(mfrow = c(1,2))
hist(medv, main="Bentuk taburan data asal")
hist(medv.fix, main="Bentuk taburan data dengan anggaran median")


### 4.3 Untuk data taburan normal/simetri dengan nilai berangka: nilai min boleh digunakan.

#### rm
mean.rm = mean(rm, na.rm=T)
mean.rm
rm.fix = ifelse(is.na(rm), mean.rm, rm)

par(mfrow = c(1,2))
hist(rm, main="Bentuk taburan data asal")
hist(rm.fix, main="Bentuk taburan data dengan anggaran median")

#### Bentukkan set data lengkap
MData.lengkap = MData
MData.lengkap$crim = crim.fix
MData.lengkap$medv = medv.fix
MData.lengkap$indus = indus.fix
MData.lengkap$rm = rm.fix
md.pattern(MData.lengkap)
MData.lengkap = MData.lengkap[-1]
MData.lengkap

## 5. Gunakan maklumat k-jiran terdekat sebagai anggaran terhadap data lenyap
iris.mis1 = read.csv(file.choose())
iris.mis1 = iris.mis1[-1]

library(multiUS)
install.packages("multiUS")

iris.mis1 = KNNimp(data=iris.mis1, k=10)
iris.mis1

## 6. Anggaran data lenyap menerusi pelbagai kaedah imputasi statistik: (pakej mice)
### 6.1 Data dengan p/ubah nilai berangka
### model = predictive mean matching
airquality = read.table(file.choose(), header=T)
airquality
par(mfrow = c(1,1))
md.pattern(airquality)

ImpData = mice(airquality, m=5, meth='pmm')
completeData = complete(ImpData)
completeData

## 6. Anggaran data lenyap menerusi pelbagai kaedah imputasi statistik:
### 6.2 Data dengan p/ubah nilai berbeza
### model = Logistic Regression

data2 = read.csv(file.choose())
data2 = data2[-1]
md.pattern(data2)
str(data2)

library(dplyr)
dat = data2%>%
            mutate(Smoking = as.factor(Smoking)) %>%
            mutate(Education = factor(Education, levels = c("Low","Medium","High"), ordered=T)) %>%
            mutate(Gender = as.factor(Gender))

### imputasi data
init = mice(dat, maxit=0)
meth = init$method
predM = init$predictorMatrix

### setkan kaedah imputasi yang digunakan
### Setiap p/ubah akan mengambil kaedah yang berbeza mengikut jenis data

str(dat)

meth[c('Age')] = "pmm"
meth[c('Cholesterol')] = "pmm"
meth[c('SystolicBP')] = "pmm"
meth[c('BMI')] = "pmm"
meth[c('Gender')] = "logreg"
meth[c('Smoking')] = "logreg"
meth[c('Education')] = "polyreg"

ImputedData = mice(dat, method=meth, predictorMatrix = predM)

CompletedData = complete(ImputedData)
CompletedData
md.pattern(CompletedData)

# Mengurus Data Pencil

## 1. Pendekatan Univariat (satu p/ubah)
ozone3 = read.csv(file.choose(), header=T)
ozone3 = ozone3[,-1]
attach(ozone3)
names(ozone3)
str(ozone3)

pressure_height

par(mfrow = c(2,7))
for (col in names(ozone3)) {
  boxplot(ozone3[[col]], main = col)
}

### 1.1 Mengesan data pencil
#### Ozone Reading
outlier_ozone = boxplot.stats(ozone_reading)$out
outlier_ozone
out_ind = which(ozone_reading%in%outlier_ozone)
out_ind

ozone3[c(out_ind),]

#### Pressure height
outlier_PH = boxplot.stats(pressure_height)$out
outlier_PH
out_ind = which(pressure_height%in%c(outlier_PH))
out_ind

ozone3[c(out_ind),]

#### Wind Speed
outlier_WS = boxplot.stats(Wind_speed)$out
outlier_WS
out_ind = which(Wind_speed%in%outlier_WS)
out_ind

ozone3[c(out_ind),]

#### Visibility
outlier_vis = boxplot.stats(Visibility)$out
outlier_vis
out_ind = which(Visibility%in%outlier_vis)
out_ind

ozone3[c(out_ind),]

## 2. Pendekatan Bivariat (2 p/ubah (X dan Y)):
### 2.1 X ialah kategori dan y berangka
par(mfrow=c(1,2))
boxplot(ozone3$ozone_reading~ozone3$Month, main="Plot Kotak Bacaan Ozone Bulanan")
boxplot(ozone3$ozone_reading~ozone3$Day_of_week, main="Plot Kotak Bacaan Ozone Harian")

#### Kesan data pencil dari set bulanan
outBiv = boxplot(ozone3$ozone_reading~ozone3$Month)$out
outBiv
out_D = which(ozone3$ozone_reading%in%outBiv)
out_D
ozone3[c(out_D),]

#### Kesan data pencil dari set harian
outBiv = boxplot(ozone3$ozone_reading~ozone3$Day_of_week)$out
outBiv
out_H = which(ozone3$ozone_reading%in%outBiv)
out_H
ozone3[c(out_H),]

### 2.2 X ialah berangka dan y berangka
head(ozone3,5)
boxplot(ozone3$ozone_reading~ozone3$pressure_height, main = "Plot Kotak ozone_reading vs pressure_height") # tak sesuai
plot(ozone3$pressure_height, ozone3$ozone_reading, main = "Plot Serakan ozone_reading vs pressure_height")

#### Kenal pasti data pencil, setkan nilai ambang (threshold) bersesuaian
#### nilai ambang bawah (low threshold)
x_min = 5400
y_min = 5

abline(v=x_min, col='blue', lty=2)
abline(h=y_min, col='blue', lty=2)

#### nilai ambang atas (high threshold)
x_max = 5900
y_max = 35

abline(v=x_max, col='red', lty=2)
abline(h=y_max, col='red', lty=2)

#### kesan data pencil dari data asal
outlier_MinT = ozone3[ozone3$pressure_height < x_min & ozone3$ozone_reading < y_min,]
outlier_MinT

outlier_MaxT = ozone3[ozone3$pressure_height > x_max & ozone3$ozone_reading > y_max,]
outlier_MaxT

## 3. Pendekatan Multivariat
### 3.1 kes terselia

#### y = ozone_reading
#### x = lain pemboleh ubah

model.Reg = lm(ozone_reading~.,data = ozone3)
summary(model.Reg)

#### jarak-Cook
cooksd = cooks.distance(model.Reg)
cooksd

#### kenal pasti data pencil
plot(cooksd, main="Data Pencil Berdasarkan Jarak Cook")
min_cook = 4*mean(cooksd)
abline(h=(min_cook), col='red', lty=2)

text(x=1:length(cooksd), y=cooksd,
     labels = ifelse(cooksd>min_cook, names(cooksd), ""), col='blue')

#### ekstrak data outlier
outlier_cook = as.numeric(names(cooksd)[cooksd>min_cook])
outlier_cook
ozone3[outlier_cook,]

### 3.2 kes tak terselia
dataMUS  = read.csv(file.choose(), header=T)
dataMUS = dataMUS[-1]
dataMUS

### hitung jarak Mahalanobis
M_dist = mahalanobis(dataMUS, center = colMeans(dataMUS), cov = cov(dataMUS))
M_dist

#### setkan nilai ambang untuk kesan data pencil
#### 97.5 persentil untuk taburan khi-kuasa dua

ambang = qchisq(0.975, df=ncol(dataMUS))
ambang

outlier_MD = which(M_dist>ambang)
outlier_MD

dataMUS[outlier_MD,]

#### pengvisualan 3d
library(scatterplot3d)
install.packages("scatterplot3d")

s3d = scatterplot3d(dataMUS, main="Pengecaman Data Pencil mengikut Jarak Mahalanobis")
s3d$points(dataMUS[outlier_MD,], col='red', pch = 16, cex = 1.5)
