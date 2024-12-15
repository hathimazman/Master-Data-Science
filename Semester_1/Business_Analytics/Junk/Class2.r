marketing = read.csv("C:/Users/PC03/Desktop/P153146/Business Analytics/Ch3_marketing.csv", stringsAsFactors = TRUE)
str(marketing)

marketing$pop_density = factor(marketing$pop_density,
                                ordered = TRUE,
                                levels = c('Low','Medium','High'))

summary(marketing$pop_density)
summary(marketing$google_adwords)
sd(marketing$google_adwords)
var(marketing$google_adwords)

summary2 = function(x) {
    results = c(summary(x),'StdDev.' = sd(x),'Var.' = var(x), 'IQR' = IQR(x))
    return(results)
}
summary2(marketing$google_adwords)
quantile(marketing$google_adwords, 0.25)

summary3 = function(x) {
    results = c('Min' = min(x), 
                'Q1' = quantile(x, 0.25), 
                'Median' = median(x), 
                'Mean' = mean(x), 
                'Q3' = quantile(x, 0.75), 
                'Max' = max(x), 
                'StdDev' = sd(x), 
                'Var' = var(x),
                'IQR' = IQR(x))
    results
}
summary3(marketing$google_adwords)
summary2((marketing$facebook))

summary(marketing$pop_density)
layout(matrix(1:4,ncol = 2)) # or par(mfrow = c(2,2))
boxplot(marketing$google_adwords, ylab = 'Expenditures', main = 'Google')
hist(marketing$google_adwords, main = 'Google', xlab = NULL)
boxplot(marketing$twitter, ylab = 'Expenditures', col = 'blue', main = 'Twitter')
hist(marketing$twitter, col = 'blue', main = 'Twitter', xlab = NULL)
# plot(marketing$pop_density)
summary(marketing)
marketing$emp_factor = cut(marketing$employees , 2)
marketing$emp_factor
# marketing$emp_factor = as.factor(marketing$emp_factor, labels = c('Low Employee', 'High Employee'))
levels(marketing$emp_factor) = c('Low Employee', 'High Employee')
table1 = table(marketing$pop_density,marketing$emp_factor)

layout(matrix(3:1, ncol = 1))
mosaicplot(table1, 
            col=c('gray','black'), 
            main = 'Factor / Factor')
boxplot(marketing$marketing_total ~ marketing$pop_density, 
        main = 'Factor / Numeric')
plot(marketing$revenues, marketing$google_adwords,
    main = 'Numeric / Numeric')

cor(marketing$google_adwords,marketing$revenues)
cor(marketing$google_adwords, marketing$facebook)

cor.test(marketing$google_adwords, marketing$revenues)

cor_test = function(x,y) {
    results = cor.test(x,y)
    output = c(#'Data' = results$data.name, 
                'Correlation Coefficient' = results$estimate, 
                'p-value' = results$p.value)
    output
}

a = cor_test(marketing$google_adwords, marketing$revenues)
str(a)
colnames(marketing)
for (item in colnames(marketing)) {
    for(item2 in colnames(marketing)) {
        if(is.numeric(marketing[[item]]) & is.numeric(marketing[[item2]])) {
            result = cor_test(marketing[[item]],marketing[[item2]])
            print(result)
        }
    }
}
# is.numeric(marketing$google_adwords)
for(item in colnames(marketing)) {
    for(item2 in colnames(marketing)) {
        print(marketing[[item]])
        print(item2)
    }
}

cor(marketing[,1:6])
library(psych)
#install.packages('psych')
corr.test(marketing[,1:6])
pairs(marketing)

# Correlogram
#install.packages('corrgram')
library(corrgram)
layout(matrix(1:2, ncol=2))
corrgram(marketing,order=TRUE,
         main = "Correlogram of Marketing Data Ordered",
         lower.panel = panel.shade,
         upper.panel = panel.ellipse,
         diag.panel = panel.minmax,
         text.panel = panel.txt)

corrgram(marketing,order=FALSE,
         main = "Correlogram of Marketing Data Unordered",
         lower.panel = panel.conf,
         upper.panel = panel.shade,
         diag.panel = panel.minmax,
         text.panel = panel.txt)
