# Server Code
######################

library(dplyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(shiny)
library(plotly)
library(bslib)
library(shinycssloaders)

df <- read.csv("data/clean.csv")

make_map <- function(slider) {
  map_df <- filter(df, Longitude != "" & Latitude != "")
  map_df <- filter(map_df, assessed_change >= slider[1] & assessed_change <= slider[2])
  
  # https://stackoverflow.com/questions/41533583/r-leaflet-adding-colors-for-character-variables
  pal <- colorFactor(
    palette = c("#811BE6", "#F62DAE", "#FDE74C", "#26F0F1", "#FF7D26"),
    domain = df$Borough
  )
  
  # https://learn.r-journalism.com/en/mapping/leaflet_maps/leaflet/
  map_plot <- leaflet(map_df) %>% 
    addProviderTiles(providers$CartoDB.DarkMatter) %>% 
    setView(lng = -73.935242, lat = 40.730610, zoom = 10) %>% 
    addCircles(~Longitude, ~Latitude, weight = 1, radius=40, 
               color=~pal(Borough), stroke = TRUE, fillOpacity = 0.8) %>% 
    addLegend("bottomright", pal = pal, values = ~Borough, 
              title = "Borough", opacity = 1)
  return(map_plot)
}

server <- function(input, output) {
  output$map <- renderLeaflet({
    s <- input$slider
    return(make_map(s))
  })
}


