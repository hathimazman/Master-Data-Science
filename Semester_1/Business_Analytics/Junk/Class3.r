adverts = read.csv("C:/Users/PC03/Desktop/P153146/Business Analytics/Ch4_marketing.csv")
head(adverts, 10)

str(adverts)
summary(adverts)
?lm
# scatter plot
layout(matrix(2:1, ncol=2))
pairs(adverts)
plot(adverts$marketing_total, adverts$revenues, ylab="Revenues",xlab="Marketing Total", main="Revenues and Marketing Total")
# fit the model <- m1
m1 = lm(revenues ~ marketing_total, data = adverts)
m1

# str(m1)
str(m1)

# find a way to call yhat,beta0,beta1, and e
yhat_model = m1$fitted.values
beta0_model = m1$coefficients[1]
beta1_model = m1$coefficients[2]
Res_model = m1$residuals

# compute yhat and e manually
yhat_manual = beta0_model + (beta1_model*adverts$marketing_total)
Res_manual = adverts$revenues - yhat_manual

# construct DF (yhat_model, yhat_manual, Res_model, Res_manual)
compiled = data.frame(
  "yhat_model" = yhat_model,
  "yhat_manual" = yhat_manual,
  "Diff_yhat" = sum(round(yhat_manual,8)) - sum(round(yhat_model,8)),
  "Res_model" = Res_model,
  "Res_manual" = Res_manual
)

head(compiled,10)
str(compiled)
sum(round(yhat_manual,8)) - sum(round(yhat_model,8))
sum(Res_manual) - sum(Res_model)

## Assumptions
# Normality

par(mfrow = c(3,1))
hist(m1$residuals, xlab = "Residuals", col = 'grey', main = "Histogram of Residuals")
qqnorm(m1$residuals, main = "QQPlot of Residuals")
qqline(m1$residuals)

# Equal Variance

plot(m1$fitted.values, m1$residuals, ylab = "Residuals", xlab = 'Fitted Values', main = "Residual Distribution")
abline(h = 0,lwd=3); abline(h = c(-5,5), lwd=3,lty=3)
# ?abline

summary(m1)

newdata = data.frame(marketing_total = 460)
predict.lm(m1, newdata, interval = 'predict')
?predict.lm

predict.lm(m1,newdata, level = 0.99, interval = 'predict')

newdata = data.frame(marketing_total = c(450,460,470))
predict.lm(m1, newdata, interval = 'predict')
predict.lm(m1, newdata, interval = 'confidence')

?confint
library(dplyr)
market_sample = sample_frac(adverts, 0.3, replace = FALSE)
samp_model  


--------------------
print(Transforming_data)
x0 = c(1,2,3,4,5,6,7,8,9,10)
y0 = c(1.00,1.41,1.73,2.00,2.24,2.45,2.65,2.83,3.00,3.16)

fit0=lm(y0~x0)

par(mfrow = c(3,1))
plot(x0,y0, pch = 19, main="Linearity?"); abline(fit0)
hist(fit0$residuals,main="Normality?", col="gray")
plot(fit0$fitted.values, fit0$residuals,
     main="Equal Variance?", pch=19); abline(h=0)

y0_t = y0^2
fit0_t = lm(y0_t ~ x0)
par(mfrow = c(3,1))
plot(x0,y0_t, pch = 19, main="Linearity?"); abline(fit0_t)
hist(fit0_t$residuals,main="Normality?", col="gray")
plot(fit0_t$fitted.values, fit0_t$residuals,
     main="Equal Variance?", pch=19); abline(h=0)

library(MASS)
boxcox(fit0)


x1 = c(1,5,15,30,60,120,240,480,720,1440,2880,5760,10080)
y1 = c(0.84,0.71,0.61,0.56,0.54,0.47,0.45,0.38,0.36,0.26,0.2,0.16,0.08)
fit1 = lm(y1~x1)

par(mfrow = c(3,1))
plot(x1,y1, pch=19, main="Linearity?"); abline(fit1)
hist(fit1$residuals,main="Normality?", col="gray")
plot(fit1$fitted.values,fit1$residuals,
     main="Equal Variance?", pch=19);abline(h=0)

x1_t = log(x1)
fit1_t = lm(y1~x1_t)
par(mfrow = c(3,1))
plot(x1_t,y1, pch=19, main="Linearity?"); abline(fit1_t)
hist(fit1_t$residuals,main="Normality?", col="gray")
plot(fit1_t$fitted.values,fit1_t$residuals,
     main="Equal Variance?", pch=19);abline(h=0)

plot(fit1_1)
pairplot()

adverts = read.csv(file.choose())
adverts

adverts$twitter_t = log(adverts$twitter)

model2 = lm(formula = revenues ~ google_adwords + facebook + twitter_t, data = adverts)
summary(model2)

pairs(adverts)
