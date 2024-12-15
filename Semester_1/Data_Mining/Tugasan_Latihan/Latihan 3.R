library(mice)
library(multiUS)
library(dplyr)
library(scatterplot3d)

getwd()
data = read.csv("D:/MSc DSc/Sem 1/Data Mining/custdata5.csv", header=T)
head(data,5)

# i) Terangkan berkenaan latar belakang data dan statistik ringkas data.
str(data)
unique(data$state.of.res)

data$num.vehicles = ifelse(is.na(data$num.vehicles), 1, data$num.vehicles)
data$is.employed.fix1 = ifelse(data$is.employed.fix1 == 'missing', NA, data$is.employed.fix1)

data = data%>%
              mutate(sex = as.factor(sex)) %>%
              mutate(marital.stat = as.factor(marital.stat)) %>%
              mutate(health.ins = as.factor(health.ins)) %>%
              mutate(housing.type = as.factor(housing.type)) %>%
              mutate(recent.move = as.factor(recent.move)) %>%
              mutate(is.employed.fix1 = as.factor(is.employed.fix1)) %>%
              mutate(age.range = as.factor(age.range)) %>%
              mutate(state.of.res = as.factor(state.of.res)) %>%
              mutate(num.vehicles = as.factor(num.vehicles))

data = data[,-c(2,4)]
summary(data)

# ii) Terangkan berkenaan corak dan sifat-sifat data lenyap.

par(mfrow=c(1,1))
md.pattern(data)

# is.employed.fix ~ mode
# Yearly Income - median
# age ~ pmm

# iii) Gunakan teknik-teknik yang sesuai untuk menganggar data lenyap bagi setiap atribut.
# iv) Berikan alasan yang wajar mengapa teknikteknik tersebut digunakan untuk anggaran data lenyap.

## 1) Yearly Income - median
hist(data$Yearly.Income)

median.YI = median(data$Yearly.Income, na.rm=T)
median.YI
YI.fix = ifelse(is.na(data$Yearly.Income), median.YI, data$Yearly.Income)

par(mfrow = c(1,2))
hist(data$Yearly.Income, main="Bentuk taburan data asal")
hist(YI.fix, main="Bentuk taburan data dengan anggaran median")

data$Yearly.Income = ifelse(is.na(data$Yearly.Income), median.YI, data$Yearly.Income)

## 2) is.employed.fix1 ~ mode

data$is.employed.fix1 = ifelse(is.na(data$is.employed.fix1), "employed", data$is.employed.fix1)
data$is.employed.fix1 = factor(data$is.employed.fix1, labels=c('employed','unemployed','employed'))

summary(data)

## 4) num.vehicles ~ mode
# as above

## 3) age ~ pmm

init = mice(data, maxit=0)
meth = init$method
predM = init$predictorMatrix

#meth[c('is.employed.fix1')] = "logreg"
meth[c('age')] = "pmm"

ImputedData = mice(data, method=meth, predictorMatrix = predM)

CompletedData = complete(ImputedData)
CompletedData

md.pattern(CompletedData)

#### ==============================================

init = mice(data, maxit=0)
meth = init$method
predM = init$predictorMatrix

#meth[c('state.of.res')] = "polyreg"
#meth[c('sex')] = "logreg"
#meth[c('marital.stat')] = "polyreg"
#meth[c('health.ins')] = "logreg"
#meth[c('housing.type')] = "polyreg"
#meth[c('recent.move')] = "logreg"
#meth[c('age')] = "pmm"
#meth[c('age.range')] = "polyreg"
#meth[c('num.vehicles')] = "polyreg"

ImputedData = mice(data, method=meth, predictorMatrix = predM)

CompletedData = complete(ImputedData)
CompletedData

md.pattern(CompletedData)

## ========================

hist(data$age)

median.age = median(data$age, na.rm=T)
median.age
age.fix = ifelse(is.na(data$age), median.age, data$age)

par(mfrow = c(1,2))
hist(data$age, main="Bentuk taburan data asal")
hist(age.fix, main="Bentuk taburan data dengan anggaran median")

data$age = ifelse(is.na(data$age), median.age, data$age)