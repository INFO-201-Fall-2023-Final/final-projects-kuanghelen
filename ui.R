# UI Code
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

intro_view <- fluidPage(
  h2("NYC Revised Property Values in Relation to NYPD Crimes"),
  p("Helen Kuang, Binta Soffie Manneh, Tina Song"),
  img(src = "nyc.jpg", height="50%", width="50%", style="display: block; margin-left: auto; margin-right: auto;"),
  tags$div(style = "text-align: center;",
    tags$p(style = "display: inline; margin: 0px;", "Source: "),
    tags$a(href = "https://www.britannica.com/video/179448/Overview-New-York-City",
      "https://www.britannica.com/video/179448/Overview-New-York-City",
      style = "display: inline; margin: 0px;")
  ),
  h3("Introduction"),
  p("New York City is a highly populated city with over 8 million people. With this large population
    comes a great prevalence of crime. For example, this includes theft, grand larceny, violence, and rape,
    among many others. High crime rates raise many questions in regard to the safety and security among
    New York residents. Safety is a factor that many people consider when choosing a place to live. People
    may want to avoid living in areas where crime rates are high. Similarly, crime rates may have an impact
    on housing prices."),
  p("Our project aims to analyze New York Police Department data on crimes that occurred in 2015,
    as well as the City of New York Finance Department data on the revised property value of homes in 2016.
    New York City has five boroughs: Brooklyn, the Bronx, Manhattan, Queens, and Staten Island. We want
    to examine how the frequency of crimes that occurred in each borough in 2015, correlates with the
    percentage change in the revised property value of homes in those boroughs in 2016. Is there a correlation
    between crime and property value in New York City’s five boroughs? Does the frequency of specific
    crimes have a stronger correlation with changes in property value? This is an interesting area to explore
    because this is an issue that affects millions of people living in the city of New York. In fact, this is an
    issue that examines several fundamental aspects of human life: housing, safety/security, and finances. We
    hope that through data analysis, we can visualize a story about the financial impacts of housing in regard
    to the safety and security of New York residents."),
  p("Notably, our project focuses on New York City's five boroughs and the specific relationship
    between crime and property value within this urban area. This is different than many other studies, as
    previous studies such as Richard Thaler's work, have examined broader national trends. Overall, Thaler
    found that there seems to be a negative correlation between these two factors at the national level. We are
    especially interested in New York—the largest city in the United States—as it is a densely populated
    region. That is, our focus is on crime and housing within a highly populated urban context. We hope that
    studying New York City, in particular, will enable us to draw stronger relationships and tell a broader
    story about crime and housing in highly populated regions in the United States.")
)

plot1_view <- fluidPage(
  titlePanel("Map Plot"),
  sidebarLayout(
    sidebarPanel(
      h3("Examine "),
      sliderInput(inputId = "slider", label = "select a range", min = -38000000, max = 85000000, value = c(-38000000, 0), round = FALSE,
                  ticks = TRUE, animate = FALSE,
                  width = NULL, sep = ",", pre = NULL, post = NULL)
      # numericInput(
      #   inputId = "map_min",
      #   label = "Select the minimum property value to change",
      #   value = -169983900,
      #   min = -169983900,
      #   max = 129260696
      # ),
      # numericInput(
      #   inputId = "map_max",
      #   label = "Select the maximum property value to change",
      #   value = 129260696,
      #   min = -169983900,
      #   max = 129260696
      # )
    ),
    mainPanel(
      h2("Main panel"),
      withSpinner(leafletOutput(outputId = "map"), type = 8, color = "Black")
    )
  )
)

plot2_view <- fluidPage(
  titlePanel("Plot 2"),
  sidebarLayout(
    sidebarPanel(
      h2("Control Panel")
    ),
    mainPanel(
      h2("Main panel"),
    )
  )
)

plot3_view <- fluidPage(
  titlePanel("Plot 3"),
  sidebarLayout(
    sidebarPanel(
      h2("Control Panel")
    ),
    mainPanel(
      h2("Main panel"),
    )
  )
)

summary_view <- fluidPage(
  titlePanel("Summary")
)

ui <- navbarPage(
  title = "NYC Property Value and Crime",
  # https://algotech.netlify.app/blog/advancing-your-shinyapp/
  theme = bs_theme(bg = "white",
                   fg = "black",
                   primary = "maroon",
                   base_font = font_google("Montserrat")
  ),
  tabPanel("Introduction", intro_view),
  tabPanel("Map Plot", plot1_view),
  tabPanel("Plot 2", plot2_view),
  tabPanel("Plot 3", plot3_view),
  tabPanel("Summary", summary_view),
  tags$style(HTML("* {color: #000000;}")),
  tags$style(HTML("p {color: #666565;}")),
  inverse = T
)

