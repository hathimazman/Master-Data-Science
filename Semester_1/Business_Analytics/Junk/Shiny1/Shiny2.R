library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)  # The package where the data comes from

# Loading data ----
data <- read.csv("G:/My Drive/Master-Data-Science/Semester_1/Business_Analytics/Junk/Shiny1/Ch8_marketing.csv", header=T)

head(data)
# model ---
model = lm(revenues ~ marketing_total, data=data)

# ui.R ----
ui <- fluidPage(
  titlePanel("Revenue Prediction from Marketing Expenditure"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = 'spend',
                  label = 'Expenditure Level in $K:',
                  min = 54, max = 481, value = 250)
    ),
    mainPanel(
      plotOutput('plot')
    )
  )
)

# server.R ----
server <- function(input, output) {
  output$plot = renderPlot({
    
    plot(data$marketing_total, data$revenues, xlab = 'Marketing Expenditure ($K)', ylab = 'Revenues ($K)')
    
    abline(model, col='blue')
    
    points(input$spend, predict(model, data.frame(marketing_total = input$spend)), col='blue', cex=2, pch=19)
    
    abline(h = predict(model, data.frame(marketing_total = input$spend)), col='red', lty=2)
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

#========
plot(data$marketing_total, data$revenues, xlab = 'Marketing Expenditure ($K)', ylab = 'Revenues ($K)')
abline(model, col='blue')
points(200,  col='red')

data.frame(marketing_total = 200)

?points

summary(model)
plot(model)
abline(model)
