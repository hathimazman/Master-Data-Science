# Intrgrasi Data
# 1. Import data ke dalam R
# Rdata

data(iris)
data("precip")

rm(iris)
rm(precip) #remove data

# drag data format R ke konsol
custdata2
pop(iris)

# 1.2 Excel Data
# jenis .xlsx
install.packages('openxlsx')
library(openxlsx)

data.ex = read.xlsx(xlsxFile = file.choose(), 
                    sheet =1, 
                    startRow = 1)
data.ex

# 1.3 jenis .csv
Data3 = read.csv(file.choose(),
                 header = TRUE)
Data3

# 1.4 jenis .txt
Data4 = read.table(file.choose(), 
                   header=TRUE)
Data4

#=============================================================================
#2. Teknik integrasi Data dari sumber/format berbeza

#2.1 Integrasi data yang berlainan attribut
mydata1 = read.table(file.choose(), header=TRUE)
mydata2 = read.csv(file.choose(), header=TRUE)

mydata1
mydata2

mydata3 = cbind(mydata1, mydata2)
mydata3new = mydata3[, -c(7,8)]
mydata3new

#2.2 Integrasi data nama attribut yang tak konsisten
mydata5 = read.csv(file.choose(), header=T)
mydata4
mydata5

mydata6 = merge(mydata4,mydata5, by.x="ID", by.y="IDPerson")
mydata6

#2.3 Integrasi data nama saiz tak sama (INNER_JOIN)
mydata7 = mydata5[1:10,]
mydata8 = merge(mydata4, mydata7, 
                by.x="ID", by.y="IDPerson")
mydata7       # semua data yang tak sepadan akan dikeluarkan

# jika nak kekalkan data yang tak sepadan
# data yang tak sepadan akan ditaarof sebagai NA (FULL_JOIN)
mydata9 = merge(mydata4, mydata7, 
                by.x="ID", by.y="IDPerson",
                all=T)
mydata9

# menamakan semula atribut
library(plyr)
mydata10 <- rename(mydata9,c("ID"="Nombor ID","house"="Number of House")) 
mydata10

mydata9

#4 ubah suai nilai data yang tak konsisten
#4.1 ubah suai secara manual
mydata11 = edit(mydata10)

#4.2 ubah suai data tak konsisten (huruf besar dan kecil)
dataM1 = read.csv(file.choose(), header=T)
dataM1

library(dplyr)

# Taarifkan nama atribut yang nak diubah suaikan

city_name = function(city) {
  city = tolower(city) #tukarkan semua ayat ke huruf
  city = trimws(city) #buang semua spacing
  city = gsub("+", "", city) #ganti dengan hanya 1 spacing
  city = tools::toTitleCase(city) #format ke Title case
  
  return(city)
}

dataM1$City = sapply(dataM1$City, city_name)
dataM1

#4.3 ubah suai data tak konsisten (ejaan singkat dan ejaan penuh)

dataM2 = read.csv(file.choose(), header=T)
dataM2

#Petakan singkat ke ejaan penuh

city_mapping = list('NY'='New York',
                    'LA'='Los Angeles',
                    'CHI'='Chicago')

#bina fungsi untuk ubah suai nama singkat ke penuh
standard_city_name = function(city){
  if(city%in%names(city_mapping)){
    return(city_mapping[[city]])}
  else{
    return(city)
  }
}

#gunakan fungsi terhadap data
dataM2$City = sapply(dataM2$City, standard_city_name)
dataM2

#5. Buang data yang berulang (redundant)
dataM3 = read.csv(file.choose(), header=T, sep = ';')
dataM3

dataM3_NoDup = dataM3%>%distinct(dataM3$id, .keep_all=T)
dataM3_NoDup

#6 eksport data (save data)
#6.1 save file R

getwd()
save(dataM3_NoDup, file="dataM3_NoDup.RData")

#6.2 save file.csv
write.csv(mydata11, file='mydata11.csv')

#6. save file txt
write.table(Data4, file='mydata11.txt', sep='\t')

#7. Kesan duplikasi data dengan id yang sama
dataM4 = read.csv(file.choose(), header=T)
dataM4

duplikasi_ID = dataM4[duplicated(dataM4$id) | duplicated(dataM4$id, fromLast=T),]
duplikasi_ID
