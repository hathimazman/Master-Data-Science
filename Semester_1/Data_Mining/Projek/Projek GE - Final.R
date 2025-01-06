library(tidyverse)
library(mice)
library(arulesViz)

ge12 = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Data_Mining/Projek/DataSet/candidates_ge12.csv")
ge13 = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Data_Mining/Projek/DataSet/candidates_ge13.csv")
ge14 = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Data_Mining/Projek/DataSet/candidates_ge14.csv")
ge15 = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Data_Mining/Projek/DataSet/candidates_ge15.csv")

str(ge12)
str(ge13)
str(ge14)
str(ge15)

ge12 = ge12[-c(2,3,4,11,9)]
ge13 = ge13[-c(2,3,4,11,9)]
ge14 = ge14[-c(2,3,4,11,9)]
ge15 = ge15[-c(2,3,4,5,10,12,13)]

colnames(ge15) = colnames(ge12)

#===================================
# Data Cleaning
# Remove null values
md.pattern(ge12)
md.pattern(ge13)
md.pattern(ge14)
md.pattern(ge15)

ge15[is.na(ge15$age),] # Semua missing value adalah daripada yang kalah, jadi boleh buang null values.

ge15 = drop_na(ge15)

# grup semula umur mengikut bin

bin = c(0, 20, 30, 40, 50, 60, Inf)
label = c("0-20", "21-30","31-40", "41-50","51-60", "61+")

ge12 <- ge12 %>%
  mutate(AgeBin = cut(
    age,
    breaks = bin,
    labels = label,
    right = T
  ))

ge13 <- ge13 %>%
  mutate(AgeBin = cut(
    age,
    breaks = bin,
    labels = label,
    right = T
  ))

ge14 <- ge14 %>%
  mutate(AgeBin = cut(
    age,
    breaks = bin,
    labels = label,
    right = T
  ))

ge15 <- ge15 %>%
  mutate(AgeBin = cut(
    age,
    breaks = bin,
    labels = label,
    right = T
  ))

ge12 = ge12[,-2]
ge13 = ge13[,-2]
ge14 = ge14[,-2]
ge15 = ge15[,-2]

#===================================
# Aturan Sekutuan

#GE12
AS_ge12 = ge12

for (col in seq(length(colnames(AS_ge12)))) {
  AS_ge12[,col] = as.factor(AS_ge12[,col])
}

Aturan.12 = apriori(AS_ge12,
                    parameter = list(supp=0.1, conf=0.5),
                    appearance = list(default='lhs', rhs="result=1"),)

head(inspect(Aturan.12))
plot(Aturan.12,
     method = "graph",
     control = list(
       edges = ggraph::geom_edge_link(
         end_cap = ggraph::circle(4, "mm"),
         start_cap = ggraph::circle(4, "mm"),
         color = "black",
         arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
         alpha = .6
       ),
       nodes = ggraph::geom_node_point(aes(size = support, color = confidence)),
       nodetext = ggraph::geom_node_label(aes(label = label), alpha = .8, repel = T)
     ),
     limit = 10
) +
  ggtitle("General Election 12") +
  scale_color_gradient(low = "yellow", high = "red") +
  scale_size(range = c(2, 20))

#GE13
AS_ge13 = ge13

for (col in seq(length(colnames(AS_ge13)))) {
  AS_ge13[,col] = as.factor(AS_ge13[,col])
}

Aturan.13 = apriori(AS_ge13,
                    parameter = list(supp=0.1, conf=0.5),
                    appearance = list(rhs='result=1', default="lhs"),)

head(inspect(Aturan.13))
plot(Aturan.13,
     method = "graph",
     control = list(
       edges = ggraph::geom_edge_link(
         end_cap = ggraph::circle(4, "mm"),
         start_cap = ggraph::circle(4, "mm"),
         color = "black",
         arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
         alpha = .6
       ),
       nodes = ggraph::geom_node_point(aes(size = support, color = confidence)),
       nodetext = ggraph::geom_node_label(aes(label = label), alpha = .8, repel = T)
     ),
     limit = 10
) +
  ggtitle("General Election 13") +
  scale_color_gradient(low = "yellow", high = "red") +
  scale_size(range = c(2, 20))


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
plot(Aturan.14,
     method = "graph",
     control = list(
       edges = ggraph::geom_edge_link(
         end_cap = ggraph::circle(4, "mm"),
         start_cap = ggraph::circle(4, "mm"),
         color = "black",
         arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
         alpha = .6
       ),
       nodes = ggraph::geom_node_point(aes(size = support, color = confidence)),
       nodetext = ggraph::geom_node_label(aes(label = label), alpha = .8, repel = T)
     ),
     limit = 10
) +
  ggtitle("General Election 14") +
  scale_color_gradient(low = "yellow", high = "red") +
  scale_size(range = c(2, 20))

#GE15
AS_ge15 = ge15

for (col in seq(length(colnames(AS_ge15)))) {
  AS_ge15[,col] = as.factor(AS_ge15[,col])
}

str(AS_ge15)

Aturan.15 = apriori(AS_ge15,
                    parameter = list(supp=0.08, conf=0.1),
                    appearance = list(rhs=c('result=1'), default='lhs'),)
temp = sort(Aturan.15, by="lift", decreasing=TRUE)

head(inspect(Aturan.15))
plot(Aturan.15,
     method = "graph",
     control = list(
       edges = ggraph::geom_edge_link(
         end_cap = ggraph::circle(4, "mm"),
         start_cap = ggraph::circle(4, "mm"),
         color = "black",
         arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
         alpha = .6
       ),
       nodes = ggraph::geom_node_point(aes(size = support, color = confidence)),
       nodetext = ggraph::geom_node_label(aes(label = label), alpha = .8, repel = T)
     ),
     limit = 10
) +
  ggtitle("General Election 15") +
  scale_color_gradient(low = "yellow", high = "red") +
  scale_size(range = c(2, 20))

#======================

selected_state = c('Kedah','Kelantan','W.P. Kuala Lumpur', 'Johor')

for (state in selected_state) {
  AS_ge15_state = ge15[ge15$state == state,]
  str(AS_ge15_state)
  
  for (col in seq(length(colnames(AS_ge15_state)))) {
    AS_ge15_state[,col] = as.factor(AS_ge15_state[,col])
  }
  
  Aturan.15.state = apriori(AS_ge15_state,
                            parameter = list(supp=0.1, conf=0.5),
                            appearance = list(rhs='result=1', default='lhs'),)
  head(inspect(Aturan.15.state))
  plot(Aturan.15.state, method="paracoord", main=paste('Aturan Jujukan GE15', state, '\n (Supp = 0.1, Conf = 0.5)'))
}

selected_state2 = c('Sabah')

for (state in selected_state2) {
  AS_ge15_state = ge15[ge15$state == state,]
  str(AS_ge15_state)
  
  for (col in seq(length(colnames(AS_ge15_state)))) {
    AS_ge15_state[,col] = as.factor(AS_ge15_state[,col])
  }
  
  Aturan.15.state = apriori(AS_ge15_state,
                            parameter = list(supp=0.05, conf=0.5),
                            appearance = list(rhs='result=1', default='lhs'),)
  sorted = sort(Aturan.15.state, by='confidence',decreasing=T)
  head(inspect(sorted))
  plot(sorted, method="paracoord", main=paste('Aturan Jujukan GE15', state, '\n (Supp = 0.05, Conf = 0.5)'))
}

selected_state3 = c('Sarawak')

for (state in selected_state3) {
  AS_ge15_state = ge15[ge15$state == state,]
  str(AS_ge15_state)
  
  for (col in seq(length(colnames(AS_ge15_state)))) {
    AS_ge15_state[,col] = as.factor(AS_ge15_state[,col])
  }
  
  Aturan.15.state = apriori(AS_ge15_state,
                            parameter = list(supp=0.1, conf=0.5),
                            appearance = list(rhs='result=1', default='lhs'),)
  sorted = sort(Aturan.15.state, by='confidence',decreasing=T)
  head(inspect(sorted))
  plot(sorted, method="paracoord", main=paste('Aturan Jujukan GE15', state, '\n (Supp = 0.1, Conf = 0.5)'))
}
