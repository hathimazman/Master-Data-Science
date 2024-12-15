#Upload data into R Studio
getwd()
setwd(dir = "C:/Users/PC03/Documents")
datch1= read.csv(file.choose())
head(datch1,10)
str(datch1)

# Transforming data
library(dplyr)

# Using logical expression
extracted_rows = filter(datch1, registered == 0,
				season == 1 | season == 2)
dim(extracted_rows)									#gives info on no. of rows and cols

using_membership = filter(datch1, registered == 0, season %in% c(1,2))
identical(extracted_rows, using_membership)

# Adding calculated column
add_revenue = mutate(extracted_rows, revenue = casual*5)

head(add_revenue,10)
head(extracted_rows,10)

# Aggregate Data
grouped = group_by(add_revenue,season)
head(grouped,10)
grouped
report = summarise(grouped, Casual = sum(casual), Revenue = sum(revenue))
report
# Export data
write.csv(report, "revenue_report.csv", row.names = FALSE)
write.table(report, "revenue_report.txt", row.names = FALSE)

# -----------------------------------------------------------------------

bike = read.csv(file.choose())
head(bike,10)
str(bike)

table(is.na(bike))

bad_data = str_subset(bike$humidity, '[a-z A-Z]')
bad_data
location = str_detect(bike$humidity, bad_data)
bike[,location]

bike$humidity = str_replace_all(bike$humidity, bad_data, "61")
table(is.na(bike))
str(bike)
bike$humidity = as.numeric(bike$humidity)
library(lubridate)

bike$holiday = factor(bike$holiday, levels = c(0,1),
                        labels = c("no","yes"))
bike$workingday = factor(bike$workingday, levels = c(0,1),
                        labels = c("no","yes"))
bike$weather = factor(bike$weather, levels = c(1,2,3,4),
                        labels = c("clr_part_cloud",
                                    "mist_cloudy",
                                    "lt_rain_snow",
                                    "hcy_rain_snow"),
                        ordered = TRUE)
bike$season = factor(bike$season, levels = c(1,2,3,4),
                        labels = c("spring","summer",
                                    "fall","winter"),
                        ordered = TRUE)
bike$datetime = mdy_hm(bike$datetime)

unique(bike$sources)
bike$sources = tolower(bike$sources)
bike$sources = str_trim(bike$sources)
library(stringr)
na_loc = is.na(bike$sources)
na_loc
bike$sources[na_loc] = "unknown"

library(DataCombine)
install.packages("DataCombine")
web_sites = "www.[\w]*.[\w]*((.[a-z]*)*)?"
current = unique(str_subset(bike$sources, web_sites))
current
replace = rep("web", length(current))
replace
replacements = data.frame(from = current, to = replace)
replacements
bike = FindReplace(data = bike, Var = "sources", replacements, from = "from", to = "to", exact = FALSE)
unique(bike$sources)
bike$sources = as.factor(bike$sources)
str(bike$sources)
