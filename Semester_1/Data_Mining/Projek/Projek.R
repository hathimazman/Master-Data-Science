# Upload DataSet

library(tidyverse)
library(mice)
library(imputeTS)
library(arrow)
library(TSstudio)

data = read_parquet(file.choose())
str(data)

max(temp$date)
min(temp$date)
# convert to correct types
data$date = as.Date(data$date)
data$hospital = as.factor(data$hospital)
data$state = as.factor(data$state)

names(data)[4:9] # numerical cols
temp = data

Agg_temp <- temp %>%
  group_by(state, date) %>%
  summarize(
    Beds_nonICU = mean(util_nonicu),
    Beds_ICU = mean(util_icu),
    Ventilators = mean(util_vent)
  )
print(Agg_temp)

# impute missing dates

Johor = Agg_temp %>% filter(state == "Johor")
Johor = Johor[-1]
str(Johor)

unique(Agg_temp$state)

Johor = Johor %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(min(date), max(date), by = "day"))

mice_result = mice(Johor, m=5, method='pmm', maxit=0, seed=123)
imputed_data = complete(mice_result)
Johor = imputed_data

md.pattern(Johor)

# Specify the start date and frequency for the time series
ts_data <- ts(Johor[, c("Beds_ICU", "Ventilators")], 
              start = c(2023, 9, 7),
              frequency = 365)     # Daily data

str(ts_data)

# Plot the time series
ts.plot(ts_data, 
        col = c("blue", "red"), 
        lty = 1, 
        main = "Time Series of Beds and Ventilators",
        xlab = "Time", 
        ylab = "Percentage of utilization");legend("bottomright", 
       legend = c("Beds_ICU", "Ventilators"), 
       col = c("blue", "red"), 
       lty = 1, cex = 0.8)

ts_data.decompose = decompose(ts_data)
plot(ts_data.decompose)
