library(shiny)
library(dplyr)
library(ggplot2)
library(fmsb)

df <- read.csv("data/clean.csv")
radar_df <- group_by(df, Borough)
radar_df <- summarize(radar_df, felony = max(felony), misdemeanor = max(misdemeanor), violation = max(violation))

ui <- fluidPage(
  titlePanel("Radar Chart"), 
  sidebarLayout(
    sidebarPanel(
      
      h2("Control Panel"), 
      selectInput(
        inputId = "borough_name", 
        label = "Select a borough", 
        choices = unique(df$Borough)
      )
    ), 
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", h3("Rates for different crimes in five boroughs"), plotOutput(outputId = "radar")) 
      )
    )
  )
)

server <- function(input, output){
  
  make_radar_tb <- function(name) {
    data_pt <- filter(radar_df, Borough == name)
    data_pt <- select(data_pt, c(felony, misdemeanor, violation))
    
    max_pt <- summarise_all(radar_df, max)
    max_pt <- select(max_pt, c(felony, misdemeanor, violation))
    
    min_pt <- summarise_all(radar_df, min)
    min_pt <- select(min_pt, c(felony, misdemeanor, violation))
    
    do.call("rbind", list(max_pt, min_pt, data_pt))
  }
  
  output$radar <- renderPlot({
    tb <- make_radar_tb(input$borough_name)
    radarchart(tb)
  })
}

shinyApp(ui = ui, server = server)

