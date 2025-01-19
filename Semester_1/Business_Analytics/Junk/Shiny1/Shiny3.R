# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(agridat)
library(devtools)
library(scales)
library(dendextend)
library(RColorBrewer)
library(DT)

# Loading data ----
market = read.csv("G:/My Drive/Master-Data-Science/Semester_1/Business_Analytics/Data/Ch8_global_market_data.csv")

# ui.R ----
ui <- 
  fluidPage(
    
    fluidRow(
      class='vertical-align',
      column(3,
             p(strong("Hello"), ", this applicaiton allows you to specify different clusters of customers based on age and income to determine more targeted marketing segments. Simply move the slider at the right for more or less clusters and choose a clustering method."),
             p("After choosing a clustering scheme you can filter and download customer data based on those clusters to run a campaign.")
             ),
      
      column(3,
             sliderInput("cluster_count",
                         label = "How many Clusters?",
                         min = 2, max = 10,
                         value = 6, step = 1)
             ),
      column(6,
             radioButtons("cluster_method",
                          label = "Clustering Method",
                          choices = c('K-means', 'Hierarchical'),
                          select = 'K-means', inline=F)
             )
    ),
    
    fluidRow(
      column(7,
             plotOutput('cluster_viz', height = '500px')
             ),
      column(5,
             h3("Cluster Summary Table"),
             DT::dataTableOutput("campaign_summary_table", width='100%')
             )
    ),
    
    fluidRow(
      column(12,
             downloadButton('downloadDatafromTable', "Download Table Data"))
    ),
    
    fluidRow(
      column(12,
             DT::dataTableOutput('campaign_table', width='100%'))
    )
  )

# server.R ----
server <- function(input, output) {
  clustered_dataset = reactive ({
    # append the cluster_id to a result dataframe
    # remember to leave the market data.frame alone
    # since it is shared between other Shiny user sessions
    result_dat = market
    
    # rebuild the model with the user specified cluster count
    # because this code is inside a "reactive" function
    # it will always re-execute whenever the user
    # changes input$cluster_count or input$cluster_method
    if (input$cluster_method == 'K-means') {
      kmeans_model = kmeans(x = market[, c('age_scale','inc_scale')],
                            centers = input$cluster_count)
      result_dat$cluster_id = as.factor(kmeans_model$cluster)
    }
    else {
      hierarchical_model = hclust(dist(market[,c('age_scale','inc_scale')]),
                                  method = 'ward.D2')
      result_dat$cluster_id = as.factor(cutree(hierarchical_model,
                                               input$cluster_count))
    }
    return(result_dat)
  })
  
  # create the cluster summary dataset
  cluster_summary_dataset = reactive({
    
    # this will always re-evaluate whenver 
    # clustered_dataset changes. This is
    # how reactive changes "bubble" through
    # the application
    summary = clustered_dataset %>%
      group_by(cluster_id) %>%
      summarise(min_age = min(age), median_age = median(age),
                max_age = max(age), median_inc = median(income),
                min_inc = min(income), max_inc = max(income)) %>%
      arrange(median_inc) %>%
      select("Median Age" = median_age, "Median Income" = median_inc,
             "Cluster ID" = cluster_id,
             "Min. Age" = min_age, "Max. Age" = max_age,
             "Min. Income" = min_inc, "Max. Income" = max_inc
             )
    return(summary)
  })
  
  # Create all aspects of the table of data, it is recommended to use DT:: notation so shiny is not confused with shiny::renderDataTable
  output$campaign_summary_table = DT::renderDataTable({
    
    # Create a datatable with a specific set of configuration options
    d = DT::datatable(cluster_summary_dataset(),
                      options = list(
                        deferRender = F,
                        # center all of the columns using the dt-center class
                        # defined in our app-styleing.css file
                        columnDefs = list(list(className = 'dt-center',
                                               targets = '_all')),
                        autowidth = F,
                        lengthChange = F,
                        searching = F,
                        paginate = F,
                        info = F,
                        ordering = F
                      ),
                      filter = 'none',
                      selection = 'none',
                      class = 'cell-border strips',
                      rownames = F,
                      escape = F)
    
    # Add formats to the columns for ease of interpretation
    d = d %>%
      formatRound("Median Age", digits = 0) %>%
      formatCurrency(c("Median Income", 'Min. Income',' Max. Income')) %>%
      formatStyle("Median Income", fontWeight = 'bold',
                  background = styleColorBar(data = c(0,125000),
                                             color = '#d5fdd5'),
                  backgroundSize = "100% 70%",
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
    
    # Return the datatable object to be rendered
    return(d)
  })
  
  # Create a visual representation of the clusters
  output$cluster_viz = renderPlot({
    
    # Calculate cluster centers so we can show a label at each center to denote each cluster
    centers = clustered_dataset() %>%
      group_by(cluster_id) %>%
      summarize(median_age = median(age),
                median_income = median(income)) %>%
      ungroup() %>%
      mutate(cluster_id = as.numeric(as.character(cluster_id)),
             label = paste("Cluster", cluster_id, "\nMedian Age:",
                           median_age, "\nMedian Income:", dollar(round(median_income, 2)))) %>%
      arrange(cluster_id) %>%
      as.data.frame
    
    # Calculate a total count of clusters to display at the plot title
    cluster_count = nrow(centers)
    
    # The plotting will change based on the clustering method
    if (input$cluster_method == 'Hierarchical') {
      
      # recompute the clusters based on the user specified cluster count
      cent = NULL
      for (k in 1:cluster_count) {
        cent = rbind(cent, colMeans(clustered_dataset()[clustered_dataset()$cluster_id == k,
                                                        c('age_scale','inc_scale'),
                                                        drop = F]))
      }
      cut_tree = hclust(dist(cent) ^ 2, method = 'cen',
                        members = table(clustered_dataset()$cluster_id)) 5trrrrrrrrrrrrrrrrdccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
