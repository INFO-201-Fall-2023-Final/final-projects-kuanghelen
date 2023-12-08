
library(shiny)
library(ggplot2)

source("data_cleaning.R")

df <- read.csv("data/clean.csv")

 ui <- fluidPage(
   titlePanel("Crime Data Analysis"),
   sidebarLayout(
     sidebarPanel(
       # Sidebar content (input elements, filters, etc.)
      radioButtons(
        inputId = "radio",
        label = h3("Select A Borough:"),
        choices = list("Bronx" , "Brooklyn", "Manhattan", "Queens", "Staten Island")
      )
    ),
    mainPanel(
#       # Main content (output elements, plots, tables, etc.)
      plotOutput("crimeBarPlot")
     )
   )
 )

 server <- function(input, output) {
   output$crimeBarPlot <- renderPlot({
  
    color_palette <- c("BRONX" = "pink", "BROOKLYN" = "blue", "MANHATTAN" = "green", "QUEENS" = "orange", "STATEN ISLAND" = "purple")
    borough_choice <- toupper(input$radio)
    race <- filter(borough_df, Borough == borough_choice)
 
    race_in_data <- df$vic_race_unknown +
      df$vic_race_white_hispanic +
      df$vic_race_white +
      df$vic_race_black +
      df$vic_race_american_indian_alaskan_native +
      df$vic_race_asian_pacific_islander
 
    number_of_crimes <- df$felony + df$misdemeanor + df$violation 
 
    crime_data <- data.frame(race = c("Unknown", "White Hispanic", "White", "Black", "American Indian/Alaskan Native", "Asian Pacific Islander"),
                          number_of_crimes = c(race$vic_race_unknown, race$vic_race_white_hispanic, race$vic_race_white, race$vic_race_black, race$vic_race_american_indian_alaskan_native, race$vic_race_asian_pacific_islander))
 
    crime_bar_plot <- ggplot(data = crime_data, aes(x = race, y = number_of_crimes)) +
        geom_bar(stat = "identity", fill = color_palette[borough_choice]) +
        labs(x = "Race", y = "Number of Crimes", 
            title = "Victim Race of Crimes Committed in New York City",
    )
    plot(crime_bar_plot)
#     
#     
#     
   })
# 
 }
# 
 shinyApp(ui = ui, server = server)



