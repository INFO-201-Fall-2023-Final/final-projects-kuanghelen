# Server Code
######################

library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(shiny)
library(plotly)
library(fmsb)
library(bslib)
library(shinycssloaders)

df <- read.csv("data/clean.csv")

map_df <- filter(df, Longitude != "" & Latitude != "")

borough_df <- group_by(df, Borough)
borough_df <- summarize(borough_df, 
  felony = max(felony), 
  misdemeanor = max(misdemeanor), 
  violation = max(violation), 
  vic_race_unknown = max(vic_race_unknown), 
  vic_race_white_hispanic = max(vic_race_white_hispanic), 
  vic_race_white = max(vic_race_white), 
  vic_race_black = max(vic_race_black), 
  vic_race_american_indian_alaskan_native = max(vic_race_american_indian_alaskan_native), 
  vic_race_asian_pacific_islander = max(vic_race_asian_pacific_islander)
)

make_map <- function(change, min, max) {
  if (change == "Assessed value") {
    mk_map_df <- filter(map_df, assessed_change >= min & assessed_change <= max)
  } else if (change == "Market value") {
    mk_map_df <- filter(map_df, market_change >= min & market_change <= max)
  } else {
    mk_map_df <- filter(map_df, taxable_change >= min & taxable_change <= max)
  }
  
  # https://stackoverflow.com/questions/41533583/r-leaflet-adding-colors-for-character-variables
  pal <- colorFactor(
    palette = c("#F62DAE", "#811BE6", "#63F775", "#26F0F1", "#FF7D26"),
    domain = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
  )
  
  # https://learn.r-journalism.com/en/mapping/leaflet_maps/leaflet/
  map_plot <- leaflet(mk_map_df) %>% 
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
    setView(lng = -73.935242, lat = 40.730610, zoom = 10) %>% 
    addCircles(~Longitude, ~Latitude, weight = 1, radius=40, 
               color=~pal(Borough), stroke = TRUE, fillOpacity = 0.8) %>% 
    addLegend("bottomright", pal = pal, values = ~Borough, 
              title = "Borough", opacity = 1)
  return(map_plot)
}

make_map_tb <- function(change, min, max) {
  if (change == "Assessed value") {
    map_df <- filter(map_df, assessed_change >= min & assessed_change <= max)
  } else if (change == "Market value") {
    map_df <- filter(map_df, market_change >= min & market_change <= max)
  } else {
    map_df <- filter(map_df, taxable_change >= min & taxable_change <= max)
  }
  tb_map_df <- group_by(map_df, Borough)
  tb_map_df <- summarize(tb_map_df, count = n())
}

make_radar_tb <- function(name) {
  data_pt <- filter(borough_df, Borough == toupper(name))
  data_pt <- select(data_pt, c(felony, misdemeanor, violation))
  
  max_pt <- summarise_all(borough_df, max)
  max_pt <- select(max_pt, c(felony, misdemeanor, violation))
  
  min_pt <- summarise_all(borough_df, min)
  min_pt <- select(min_pt, c(felony, misdemeanor, violation))
  
  do.call("rbind", list(max_pt, min_pt, data_pt))
}

make_bar_chart <- function(borough) {
  color_palette <- c("BRONX" = "pink", "BROOKLYN" = "#ABA5E8", "MANHATTAN" = "#AADCB0", "QUEENS" = "#8CC8F0", "STATEN ISLAND" = "#F4C0AC")
  borough_choice <- toupper(borough)
  race <- filter(borough_df, Borough == borough_choice)
  
  crime_data <- data.frame(race = c("Unknown", "White Hispanic", "White", "Black", "American Indian/\nAlaskan Native", "Asian Pacific\nIslander"),
                           number_of_crimes = c(race$vic_race_unknown, race$vic_race_white_hispanic, race$vic_race_white, race$vic_race_black, 
                                                race$vic_race_american_indian_alaskan_native, race$vic_race_asian_pacific_islander))
  
  crime_bar_plot <- ggplot(data = crime_data, aes(x = race, y = number_of_crimes, text = number_of_crimes)) +
    geom_bar(stat = "identity", fill = color_palette[borough_choice]) +
    labs(x = "Race", y = "Number of Crimes", 
         title = paste0("Victim Race of Crimes Committed in ",  borough,", New York"))
  return(crime_bar_plot)
}

server <- function(input, output) {
  output$map <- renderLeaflet({
    return(make_map(input$radio_change, input$map_min, input$map_max))
  })
  
  output$map_table <- renderTable({
    return(make_map_tb(input$radio_change, input$map_min, input$map_max))
  })
  
  output$radar <- renderPlot({
    tb <- make_radar_tb(input$borough_name)
    return(radarchart(tb))
  })
  
  output$table <- renderTable({
    return(make_radar_tb(input$borough_name))
  }) 
  
  output$crimeBarPlot <- renderPlotly({
    p <- make_bar_chart(input$radio)
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
}


