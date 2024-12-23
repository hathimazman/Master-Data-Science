library(tidyverse)
library(stringr)
library(mice)
library(TraMineR)
library(zoo)
library(arulesViz)

ge12 = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Data_Mining/Projek/DataSet/candidates_ge12.csv")
ge13 = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Data_Mining/Projek/DataSet/candidates_ge13.csv")
ge14 = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Data_Mining/Projek/DataSet/candidates_ge14.csv")
ge15 = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Data_Mining/Projek/DataSet/candidates_ge15.csv")

str(ge12)
str(ge13)
str(ge14)
str(ge15)

ge12 = ge12[-c(3,11,9)]
ge13 = ge13[-c(3,11,9)]
ge14 = ge14[-c(3,11,9)]
ge15 = ge15[-c(3,4,10,12,13)]

colnames(ge15) = colnames(ge12)

ls()
rm(list=ls())

# Aturan Jujukan Parti

AJ_ge12 = ge12[c(3,5,6,7)]
AJ_ge13 = ge13[c(3,5,6,7)]
AJ_ge14 = ge14[c(3,5,6,7)]
AJ_ge15 = ge15[c(3,5,6,7)]

main = rbind(AJ_ge12[c(1,2,3)], AJ_ge13[c(1,2,3)], AJ_ge14[c(1,2,3)], AJ_ge15[c(1,2,3)])
main = main[!duplicated(main$name),]

sum(duplicated(main$name))

dataset = merge(main, AJ_ge12[c(1,4)], by = "name", all = TRUE, suffixes = c("", "_ge12"))
dataset = merge(dataset, AJ_ge13[c(1,4)], by = "name", all = TRUE, suffixes = c("", "_ge13"))
dataset = merge(dataset, AJ_ge14[c(1,4)], by = "name", all = TRUE, suffixes = c("", "_ge14"))
dataset = merge(dataset, AJ_ge15[c(1,4)], by = "name", all = TRUE, suffixes = c("", "_ge15"))

str(dataset)

colnames(dataset) = c("name","sex","ethnicity", "party_ge12", "party_ge13", "party_ge14", "party_ge15")

dataset3 = dataset

# Use forward fill and backward fill to impute missing data
temp = t(apply(dataset[4:7], 1, function(row) zoo::na.locf(row, na.rm = FALSE)))
temp2 = t(apply(temp, 1, function(row) zoo::na.locf(row, na.rm = FALSE, fromLast = TRUE)))

dataset3[4:7] = temp2

#dataset3$party_ge12 = ifelse(is.na(dataset3$party_ge12), "No Data/Not Contending (NO)", dataset$party_ge12)
#dataset3$party_ge13 = ifelse(is.na(dataset3$party_ge13), "No Data/Not Contending (NO)", dataset$party_ge13)
#dataset3$party_ge14 = ifelse(is.na(dataset3$party_ge14), "No Data/Not Contending (NO)", dataset$party_ge14)
#dataset3$party_ge15 = ifelse(is.na(dataset3$party_ge15), "No Data/Not Contending (NO)", dataset$party_ge15)

dataset3 = drop_na(dataset3)

dataset3$party_ge12 = as.factor(dataset3$party_ge12)
dataset3$party_ge13 = as.factor(dataset3$party_ge13)
dataset3$party_ge14 = as.factor(dataset3$party_ge14)
dataset3$party_ge15 = as.factor(dataset3$party_ge15)

md.pattern(dataset3)

str(dataset3)

# GE12 mapping
data.labels12 = unique(dataset3$party_ge12)
data.scode12 = c()
for (i in seq(length(unique(dataset3$party_ge12)))) {
  code = str_extract_all(data.labels12[i], "\\(([^)]+)\\)")
  letter = str_remove_all(code, "[()]")
  data.scode12[i] = letter
}
map12 = data.frame(
  alphabet = data.labels12,
  label = data.scode12,
  long_label = data.labels12
)

# GE13 mapping
data.labels13 = unique(dataset3$party_ge13)
data.scode13 = c()
for (i in seq(length(unique(dataset3$party_ge13)))) {
  code = str_extract_all(data.labels13[i], "\\(([^)]+)\\)")
  letter = str_remove_all(code, "[()]")
  data.scode13[i] = letter
}
map13 = data.frame(
  alphabet = data.labels13,
  label = data.scode13,
  long_label = data.labels13
)

# GE14 mapping
data.labels14 = unique(dataset3$party_ge14)
data.scode14 = c()
for (i in seq(length(unique(dataset3$party_ge14)))) {
  code = str_extract_all(data.labels14[i], "\\(([^)]+)\\)")
  letter = str_remove_all(code, "[()]")
  data.scode14[i] = letter
}
map14 = data.frame(
  alphabet = data.labels14,
  label = data.scode14,
  long_label = data.labels14
)

# GE15 mapping
data.labels15 = unique(dataset3$party_ge15)
data.scode15 = c()
for (i in seq(length(unique(dataset3$party_ge15)))) {
  code = str_extract_all(data.labels15[i], "\\(([^)]+)\\)")
  letter = str_remove_all(code, "[()]")
  data.scode15[i] = letter
}
map15 = data.frame(
  alphabet = data.labels15,
  label = data.scode15,
  long_label = data.labels15
)

# Combine mapping
combine = rbind(map12, map13, map14, map15)
clean = combine%>%distinct(combine$alphabet, .keep_all = TRUE)
clean = clean[-4]

clean$label = as.character(clean$label)
clean$alphabet = as.character(clean$alphabet)
clean$long_label = as.character(clean$long_label)

sum(duplicated(combine))
sum(duplicated(clean))

str(clean)


data.seq = seqdef(dataset3, 
                  4:7, 
                  states = clean$label, 
                  labels = clean$label,
                  alphabet = clean$long_label,
                  )
head(data.seq)
head(dataset3)
class(data.seq)

seqmeant(data.seq)

seqmtplot(data.seq, group = dataset3$sex, main='Sex')

head(seqtransn(data.seq))
hist(seqtransn(data.seq), main='bilangan transisi')

data.trate = seqtrate(data.seq)
print(data.trate)

idxs = 1:20
selected_seq = data.seq[idxs,]
relevent_seq = unique(selected_seqs)

seqiplot(data.seq, 
         border = NA, 
         idxs=idxs,
         with.legend=FALSE);legend("topright", legend=clean$label, cex=0.6, col=seq(length(clean$label)),pch=15)

seqfplot(data.seq, idxs=idxs, with.legend=T, cex.legend=0.5)

seqdplot(data.seq, border=T, cex.legen=0.5)

seqHtplot(data.seq, main='Entropi Rentas Lintang')
#====================
# Aturan Sekutuan

test = as(AS_ge12, 'transactions')

itemFrequencyPlot(test, topN=10)

#GE12
str(ge12)
AS_ge12 = ge12

for (col in seq(length(colnames(AS_ge12)))) {
  AS_ge12[,col] = as.factor(AS_ge12[,col])
}

str(AS_ge12)

Aturan.12 = apriori(AS_ge12,
                    parameter = list(supp=0.1, conf=0.5),
                    appearance = list(rhs='result=1', default="lhs"),)

head(inspect(Aturan.12))
plot(Aturan.12, method="paracoord", main='Aturan Jujukan GE12 \n (Supp = 0.1, Conf = 0.5)')

#GE13
AS_ge13 = ge13

for (col in seq(length(colnames(AS_ge13)))) {
  AS_ge13[,col] = as.factor(AS_ge13[,col])
}

str(AS_ge13)

Aturan.13 = apriori(AS_ge13,
                    parameter = list(supp=0.1, conf=0.5),
                    appearance = list(rhs='result=1', default="lhs"),)

head(inspect(Aturan.13))
plot(Aturan.13, method="paracoord", main='Aturan Jujukan GE13 \n (Supp = 0.1, Conf = 0.5)')

#GE14
AS_ge14 = ge14

for (col in seq(length(colnames(AS_ge14)))) {
  AS_ge14[,col] = as.factor(AS_ge14[,col])
}

str(AS_ge14)

Aturan.14 = apriori(AS_ge14,
                    parameter = list(supp=0.1, conf=0.5),
                    appearance = list(rhs='result=1', default="lhs"),)

head(inspect(Aturan.14))
plot(Aturan.14, method="paracoord", main='Aturan Jujukan GE14, \n (Supp = 0.1, Conf = 0.5)')

#GE15
AS_ge15 = ge15

for (col in seq(length(colnames(AS_ge15)))) {
  AS_ge15[,col] = as.factor(AS_ge15[,col])
}

str(AS_ge15)

Aturan.15 = apriori(AS_ge15,
                    parameter = list(supp=0.05, conf=0.1),
                    appearance = list(rhs=c('result=1'), default='lhs'),)
temp = sort(Aturan.15, by="lift", decreasing=TRUE)

head(inspect(Aturan.15))
plot(Aturan.15, method="paracoord", main='Aturan Jujukan GE15 \n (Supp = 0.05, Conf = 0.1)')
