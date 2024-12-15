# Penjelmaan Data

## 1. Penormalan Data

### 1.1 Penjelmaan skala data

#### 1.1.1 Penormalan Min-Max:
##### Skalakan semua p/ubah kepada unit yang sama
###### selang (0,1), tiada unit

dataAP3 = read.csv(file.choose(), header=T)
head(dataAP3)
dataAP3 = dataAP3[-1]

attach(dataAP3)
names(dataAP3)

###### Ozone
min.03 = min(ozone_ppm)
min.03
max.03 = max(ozone_ppm)
max.03

new_max.03 = 1
new_min.03 = 0

new.03 = ((ozone_ppm - min.03)*(new_max.03 - new_min.03) / (max.03-min.03)) + new_min.03
new.03

#### ulang untuk semua data p/ubah
#### analises perlombongan data akan dijalankan terhadap data yang diskalakan
#### selepas analisis perlombongan data, keputusan perlu dijelmakan semula kepada unit data asal

min.03 = 0
min.03
max.03 = 1
max.03

new_max.03 = max(ozone_ppm)
new_min.03 = min(ozone_ppm)

data.asal.03 = ((new.03 - min.03)*(new_max.03 - new_min.03) / (max.03-min.03)) + new_min.03
head(data.asal.03)
head(ozone_ppm)

#### 1.1.2 Penormalan skor z
mean.hPA = mean(pressure_height.hPA)
sd.hPA = sd(pressure_height.hPA)

z.score.hPA = (pressure_height.hPA - mean.hPA)/sd.hPA
z.score.hPA

#### ulang untuk semua data p/ubah
#### analises perlombongan data akan dijalankan terhadap data yang diskalakan
#### selepas analisis perlombongan data, keputusan perlu dijelmakan semula kepada unit data asal

data.asal.hPA = (z.score.hPA*sd.hPA) + mean.hPA

head(data.asal.hPA)
head(pressure_height.hPA)

#### 1.1.2 Penormalan berdasarkan penskalaan perpuluhan
pHnew = pressure_height.hPA/10000
vpAnew = Visibility_pAerosol/1000
