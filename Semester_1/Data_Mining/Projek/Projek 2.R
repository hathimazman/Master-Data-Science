library(tidyverse)
library(arrow)
library(arules)
library(arulesViz)

# URL of the Parquet file
url <- "G:/My Drive/Master-Data-Science/Semester_1/Data_Mining/Projek/DataSet/covid_deaths_linelist.parquet"

# Read the Parquet file directly from the URL
covid_deaths <- read_parquet(url)

# View the first few rows
str(covid_deaths[, c(-2,-3,-4,-5,-6)])
data = covid_deaths[, c(-2,-3,-4,-5,-6)]

# Convert the date column to a Date object
data$date <- as.Date(data$date)

# Convert the state column to a factor
data$state <- as.factor(data$state)
data[,c(-1,-6)] = lapply(data[,c(-1,-6)], as.factor)
str(data)

Aturan = apriori(data, parameter = list(support = 0.1, confidence = 0.5))
                 
inspect(Aturan)  

Aturan1 = apriori(data,
                    parameter=list(supp=0.1, conf=0.05),
                    appearance = list(lhs=c('Class=1st', 'Class=2nd','Class=3rd','Age=Child','Age=Adult'),rhs='Survived=Yes'))
