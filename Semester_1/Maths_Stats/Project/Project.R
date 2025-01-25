# Load Libraries
library(tidyverse)
library(mice)
library(forecast)
library(corrplot)
library(colorspace)

# Load Dataset
data = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Maths_Stats/Project/insurance.csv")
str(data)
pal = choose_palette()

# Data Cleaning
data = data %>%
  mutate(sex = factor(sex),
         smoker = factor(smoker),
         region = factor(region))
str(data)
NumCols = c('age', 'bmi', 'charges', 'children')
CatCols = c('sex','smoker','region')


# Finding Null Values
md.pattern(data)


# Handling Outliers
## Charges
Q1_charges = quantile(data$charges, 0.25)
Q3_charges = quantile(data$charges, 0.75)
IQR_charges = Q3_charges - Q1_charges

## Age
Q1_age = quantile(data$age, 0.25)
Q3_age = quantile(data$age, 0.75)
IQR_age = Q3_age - Q1_age

## BMI
Q1_bmi = quantile(data$bmi, 0.25)
Q3_bmi = quantile(data$bmi, 0.75)
IQR_bmi = Q3_bmi - Q1_bmi

data = data %>%
  filter(charges > (Q1_charges - 1.5 * IQR_charges) & charges < (Q3_charges + 1.5 * IQR_charges)) %>%
  filter(age > (Q1_age - 1.5 * IQR_age) & age < (Q3_age + 1.5 * IQR_age)) %>%
  filter(bmi > (Q1_bmi - 1.5 * IQR_bmi) & bmi < (Q3_bmi + 1.5 * IQR_bmi))

#Summary of data
summary(data)
sd(data$charges)


  # Visualizing Data
## Histogram
par(mfrow = c(1,1))
hist(data$age, main= "Histogram of Age", col = pal(10), xlab='')
hist(data$bmi, main= "Histogram of BMI", col = pal(10), xlab='')
hist(log(data$charges), main= "Histogram of log of Insurance Charges", col = pal(10), xlab='')

pie(table(data$sex), col = c('pink', 'skyblue'), main='Pie Chart of Gender')
pie(table(data$smoker), col = c('palegreen', 'grey'), main='Pie Chart of Smoker')
pie(table(data$region), col = pal(5), main='Pie Chart of Regions')


# Model Building
model = lm(charges ~ age + sex + bmi + children+ smoker + region -1, data = data)
summary(model)


# Checking Assumptions
## Linearity
pairs(data[NumCols], main="Pair Plot of Numerical Variables")

cor(data[NumCols])

cor(data[NumCols][-4]) %>%
  corrplot(order = 'hclust')

## Independence
chi1 = chisq.test(data$sex, data$smoker)
chi1


## Normality and Equal Variance
plot(model)


#Model Optimization
MASS::boxcox(model)

str(data)
data2 = data %>%
  mutate(charges = log(charges))

model2 = lm(log(charges) ~ age + sex + bmi + children+ smoker + region -1, data = data)

# Recheck New Model
summary(model2)
plot(model2)
