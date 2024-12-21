library(tidyverse)
library(TSstudio)
library(mice)
library(forecast)

url = "C:/Users/Hathim/Downloads/Combined Data.xlsx"

data = readxl::read_excel(url)
str(data)

md.pattern(data)

Admission = data[, c(1,2)]

Admission.ts = ts(data$Admission, start = c(2010, 1), end = c(2019,12), frequency = 12)
ts.plot(Admission.ts, col = "blue", lty = 1, main = "Time Series of Admission", xlab = "Time")
Admission.ts.decompose = decompose(Admission.ts)
plot(Admission.ts.decompose)

str(Admission)

Admission$year = as.numeric(format(Admission$Date, "%Y"))
Admission$month = as.numeric(format(Admission$Date, "%m"))

boxplot(Admission ~ month, data = Admission,
        main = "Monthly Pattern Over Years",
        xlab = "Month",
        ylab = "Values",
        col = "lightblue",
        las = 2)  # Rotate x-axis labels

# Month plot
monthplot(Admission.ts, 
          main = "Month Plot: Average Monthly Pattern",
          xlab = "Month",
          ylab = "Values",
          col = "blue")

# Seasonal plot
seasonplot(Admission.ts, 
           col = rainbow(10),  # Different colors for each year
           year.labels = TRUE,
           main = "Seasonal Plot: Monthly Patterns (2010-2019)",
           ylab = "Values",
           xlab = "Month")


AGE = ts(data$AGE_Confirmed_cases, start = c(2010, 1), end = c(2019,12), frequency = 12)
ts.plot(AGE, col = "blue", lty = 1, main = "Time Series of AGE", xlab = "Time")
AGE.decompose = decompose(AGE)
plot(AGE.decompose)

model = auto.arima(Admission)
model

f.value = forecast(model, h = 12)
resid = f.value$residuals

acf(resid)
hist(resid)
plot.ts(resid)

fore = predict(model, n.ahead = 12)
fore

UL = fore$pred + 0.69*fore$se
LL = fore$pred - 0.69*fore$se

ts.plot(Admission, fore$pred, UL, LL, 
        main = "Time Series of Admission",
        col = c(1,2,4,4), lty = c(1,1,2,2));legend("topright", 
                               c("Observed Data", "Forecast", "Confidence Interval 50%"),
                               col=c(1,2,4), lty=c(1,1,2))
