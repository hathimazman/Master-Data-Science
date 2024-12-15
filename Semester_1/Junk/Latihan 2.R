library(dplyr)
library(plyr)
library(openxlsx)

#1. Gabungkan data dari file custdata2i dan custdata3i menerusi entiti pengecaman terhadap atribut “customer id” yang sama. Abaikan cerapan yang yang tidak mengandungi maklumat atribut yang lengkap.

custdata2i = custdata2
custdata3i = read.csv(file.choose())
custdata3i = custdata3i[,-1]

str(custdata2i)
str(custdata3i)

# Merge data
str(data1)
data1 = merge(custdata2i, custdata3i, by.x='custid', by.y='custid')
data1 = data1[,1:16]
colnames(data1) =  gsub("\\.x$", "", colnames(data1))

data1 # final merge

head(custdata2i)

#Rbind columns

data2 = rbind(custdata2i,custdata3i)
str(data2)

duplikasi_ID = data2[duplicated(data2$custid) | duplicated(data2$custid, fromLast=T),]
duplikasi_ID

data2cleaned = data2%>%distinct(data2$custid, .keep_all=T)
str(data2cleaned)
data2 = data2cleaned[,1:16] #final rbind
data2

duplikasi_ID2 = data2[duplicated(data2$custid) | duplicated(data2$custid, fromLast=T),]
duplikasi_ID2

#2. Bentukkan data set baru bagi pelanggan lelaki yang mempunyai gaji melebihi 7000 dollar dan juga mengandungi maklumat bagi atribut-atribut berikut:
  # - state.of.res , custid, marital.stat, health.ins, housing.type , num.vehicles , sex, income
col_list = c('state.of.res' , 'custid', 'marital.stat', 'health.ins', 'housing.type' , 'num.vehicles' , 'sex', 'income')
#colnames(cust_data) <- gsub("\\.x$", "", colnames(cust_data))
#colnames(cust_data) <- gsub("\\.y$", "", colnames(cust_data))

data1_filtered = subset(data1, data1$income > 7000 & data1$sex == 'M')
data1_filtered = data1_filtered[col_list]
data1_filtered

data2_filtered = subset(data2, data2$income > 7000 & data2$sex == 'M')
data2_filtered = data2_filtered[col_list]
data2_filtered



#3. Tunjukkan data bagi setiap pelanggan dalam bentuk susunan gaji yang semakin tinggi.

data1_sorted = data1[order(data1$income),]
data1_sorted

data2_sorted = data2[order(data2$income),]
data2_sorted

#4. Misalkan diketahui maklumat baru seperti berikut:
#- state.of.res: alabama, Louisiana, new york
#- ID customer: 567891, 33421, 21134
#- marital.stat: Married, Never Married, bercerai
#- Ins.health: TRUE, FALSE, TRUE
#- Home Status: Sewa, Not Available, loan
#- num.vehicles: 2, 1, 2
#- sex: M, Male, lelaki
#- is.employed: TRUE, FALSE, TRUE
#- income: 99200, Not Available, 150341 

#5. Tambahkan maklumat cerapan baru tersebut dalam data set anda.

new_data1 = data.frame(
  custid = c(567891, 33421, 21134),
  state.of.res = c('Alabama', 'Louisiana', 'New York'),
  sex = c('M','M','M'),
  is.employed = c('employed', 'not employed', 'employed'),
  income = c(99200, NA, 150341),
  marital.stat = c('Married', 'Never Married', 'Divorced'),
  health.ins = c(TRUE, FALSE, TRUE),
  housing.type = c('Rented', NA, 'Homeowner with mortgage/loan'),
  recent.move = c(NA,NA,NA),
  num.vehicles = c(2,1,2),
  age = c(NA,NA,NA),
  is.employed.fix1 = c(NA,NA,NA),
  Median.Income = c(NA,NA,NA),
  gp = c(NA,NA,NA),
  income.lt.30K = c(NA,NA,NA),
  age.range = c(NA,NA,NA)
)

data_total1 = rbind(data1, new_data1)
data_total1

new_data2 = data.frame(
  state.of.res = c('Alabama', 'Louisiana', 'New York'),
  custid = c(567891, 33421, 21134),
  sex = c('M','M','M'),
  is.employed = c('employed', 'not employed', 'employed'),
  income = c(99200, NA, 150341),
  marital.stat = c('Married', 'Never Married', 'Divorced'),
  health.ins = c(TRUE, FALSE, TRUE),
  housing.type = c('Rented', NA, 'Homeowner with mortgage/loan'),
  recent.move = c(NA,NA,NA),
  num.vehicles = c(2,1,2),
  age = c(NA,NA,NA),
  is.employed.fix1 = c(NA,NA,NA),
  Median.Income = c(NA,NA,NA),
  gp = c(NA,NA,NA),
  income.lt.30K = c(NA,NA,NA),
  age.range = c(NA,NA,NA)
)

data_total2 = rbind(data2, new_data2)
data_total2
