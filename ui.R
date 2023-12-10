# UI Code
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

intro_view <- fluidPage(
  h2("NYC Revised Property Values in Relation to NYPD Crimes"),
  p("Helen Kuang, Binta Soffie Manneh, Tina Song"),
  img(src = "NYC.jpg", height="50%", width="50%", style="display: block; margin-left: auto; margin-right: auto;"),
  tags$div(style = "text-align: center;",
           tags$p(style = "display: inline; margin: 0px;", "Source: "),
           tags$a(href = "https://www.britannica.com/video/179448/Overview-New-York-City",
                  "https://www.britannica.com/video/179448/Overview-New-York-City",
                  style = "display: inline; margin: 0px; text-decoration:none")
  ),
  h3("Introduction"),
  p("New York City is a highly populated city with over ", style = "display: inline;"), 
  tags$a(href="https://datacommons.org/place/geoId/3651000/?utm_medium=explore&mprop=count&popt=Person&hl=en", 
         "8 million", style = "display: inline; text-decoration:none"), 
  p("people. With this large population
    comes a great prevalence of crime. For example, this includes theft, grand larceny, violence, and rape,
    among many others. High crime rates raise many questions in regard to the safety and security among
    New York residents. Safety is a factor that many people consider when choosing a place to live. People
    may want to avoid living in areas where crime rates are high. Similarly, crime rates may have an impact
    on housing prices.", style = "margin: 0px;display: inline;"),
  br(),
  br(),
  p("Our project aims to analyze New York Police Department ", style = "display: inline;"), 
  tags$a(href="https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i/explore/query/SELECT%0A%20%20%60cmplnt_num%60%2C%0A%20%20%60cmplnt_fr_dt%60%2C%0A%20%20%60rpt_dt%60%2C%0A%20%20%60ofns_desc%60%2C%0A%20%20%60law_cat_cd%60%2C%0A%20%20%60boro_nm%60%2C%0A%20%20%60latitude%60%2C%0A%20%20%60longitude%60%2C%0A%20%20%60vic_age_group%60%2C%0A%20%20%60vic_sex%60%2C%0A%20%20%60%3A%40computed_region_efsh_h5xi%60%2C%0A%20%20%60%3A%40computed_region_yeji_bk3q%60%0AWHERE%0A%20%20%60cmplnt_fr_dt%60%0A%20%20%20%20BETWEEN%20%222015-01-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp%0A%20%20%20%20AND%20%222015-12-31T11%3A59%3A00%22%20%3A%3A%20floating_timestamp%0AORDER%20BY%20%60rpt_dt%60%20DESC%20NULL%20FIRST/page/filter", 
    "data", style = "display: inline; text-decoration:none;"), 
  p("on crimes that occurred in 2015, as well as the City of New York Finance Department ", style = "display: inline;"), 
  tags$a(href="https://catalog.data.gov/dataset/revised-notice-of-property-value-rnopv","data",
         style = "display: inline; text-decoration:none;"), 
  p("on the revised property value of homes in 2016.
    New York City has five boroughs: Brooklyn, the Bronx, Manhattan, Queens, and Staten Island. We want
    to examine how the frequency of crimes that occurred in each borough in 2015, correlates with the 
    change in the revised property value of homes in those boroughs in 2016. Is there a correlation
    between crime and property value in New York City’s five boroughs? Does the frequency of specific
    crimes have a stronger correlation with changes in property value? This is an interesting area to explore
    because this is an issue that affects millions of people living in the city of New York. In fact, this is an
    issue that examines several fundamental aspects of human life: housing, safety/security, and finances. We
    hope that through data analysis, we can visualize a story about the financial impacts of housing in regard
    to the safety and security of New York residents.", style = "display: inline;"),
  br(),
  br(),
  p("Notably, our project focuses on New York City's five boroughs and the specific relationship
    between crime and property value within this urban area. This is different than many other studies, as
    previous studies such as Richard Thaler's ", style = "display:inline;"), 
  tags$a(href="https://www0.gsb.columbia.edu/faculty/jrockoff/aer.98.3.pdf", "work",
    style = "display: inline; text-decoration:none;"), 
  p(", have examined broader national trends. Overall, Thaler
    found that there seems to be a negative correlation between these two factors at the national level. We are
    especially interested in New York—the largest city in the United States—as it is a densely populated
    region. That is, our focus is on crime and housing within a highly populated urban context. We hope that
    studying New York City, in particular, will enable us to draw stronger relationships and tell a broader
    story about crime and housing in highly populated regions in the United States.", style = "display:inline;"),
  br(),
  br(),
  h3("About the Data"),
  p("Our first ", style = "display: inline;"), 
  tags$a(href="https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i/explore/query/SELECT%0A%20%20%60cmplnt_num%60%2C%0A%20%20%60cmplnt_fr_dt%60%2C%0A%20%20%60rpt_dt%60%2C%0A%20%20%60ofns_desc%60%2C%0A%20%20%60law_cat_cd%60%2C%0A%20%20%60boro_nm%60%2C%0A%20%20%60latitude%60%2C%0A%20%20%60longitude%60%2C%0A%20%20%60vic_age_group%60%2C%0A%20%20%60vic_sex%60%2C%0A%20%20%60%3A%40computed_region_efsh_h5xi%60%2C%0A%20%20%60%3A%40computed_region_yeji_bk3q%60%0AWHERE%0A%20%20%60cmplnt_fr_dt%60%0A%20%20%20%20BETWEEN%20%222015-01-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp%0A%20%20%20%20AND%20%222015-12-31T11%3A59%3A00%22%20%3A%3A%20floating_timestamp%0AORDER%20BY%20%60rpt_dt%60%20DESC%20NULL%20FIRST/page/filter", 
         "dataset", style = "display: inline; text-decoration:none;"), 
  p("is New York Police Department data on crimes that occurred in 2015. The data in
    this dataset is provided by the New York Police Department and is generated by NYC OpenData. NYC
    OpenData is managed by a team in the NYC Office of Technology and Innovation (OTI). This dataset
    includes 478,596 rows. Each row indicates a crime incident, and the columns include information 
    such as the date, offense description, level of offense, location (borough number, latitude/longitude, 
    zip code), and victim information. In our project, we aggregate this information to better communicate 
    overall trends.", style = "display: inline;"),
  br(),
  br(),
  p("Our second ", style = "display: inline;"), 
  tags$a(href="https://catalog.data.gov/dataset/revised-notice-of-property-value-rnopv",
         "dataset", style = "display: inline; text-decoration:none;"),
  p(" is the City of New York Finance Department data on the revised property
    value of homes in 2016. This dataset is provided by the New York Finance Department and hosted on
    Data.gov. Each year, property owners are notified about changes in their property value. This dataset
    includes 27,312 rows. Each row indicates a piece of property, and the columns include information such 
    as original/revised market value, assessed value, and taxable value, as well as location (borough number, 
    latitude/longitude, zip code, etc.)", style = "display: inline;"),
  br(),
  br(),
  p("To join the above datasets, we aggregated crime data by borough locations 
    and joined with the revised property value dataset by borough location. By 
    connecting the datasets by borough location, we can see how varying crimes 
    in different boroughs correlate with the property values in their respective regions."),
  br()
)

p1_text <- fluidPage(
  h3("Analysis"),
  p("The radar chart displays and compares the relative occurrences of three distinct 
    crime categories—felonies, misdemeanors, and violations—across the five boroughs 
    within New York City. Users can select a borough from the dropdown list in the 
    sidebar. The accompanying table shows the specific crime rates for each category in the selected 
    borough. Within this visualization, Brooklyn emerges as a borough with the highest 
    occurrences across these crime categories, while Staten Island, in contrast, 
    shows the lowest occurrences among the five boroughs.")
)

plot1_view <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "borough_name", 
        label = h4("Select a borough"), 
        choices = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
      )
    ),
    mainPanel(
      h2("Radar Chart"),
      tabsetPanel(
        tabPanel("Plot", 
                 br(),
                 h5("Frequency of Different Crimes in the Five Boroughs"), 
                 plotOutput(outputId = "radar"),
                 p1_text
        ), 
        tabPanel("Table",
                 br(),
                 h5("Maximum, Minimum, and Borough Crime Count"),
                 br(),
                 tableOutput(outputId = "table"),
                 br(),
                 tags$div(
                   tags$p(style="margin: 6px; color: black", "Here is what each row represents:"),
                   tags$ul(
                     tags$li(style="color: #666565;", "Row 1 indicates the MAXIMUM count of specific crimes out of all boroughs."),
                     tags$li(style="color: #666565;", "Row 2 indicates the MINIMUM count of specific crimes out of all boroughs."),
                     tags$li(style="color: #666565;", "Row 3 indicates the count of specific crimes for the SELECTED borough."),
                   )
                 ),
                 br(),
                 br(),
                 br(),
                 p1_text
        )
      )
    )
  )
)

plot2_view <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "radio",
        label = h4("Select a borough"),
        choices = list("Bronx" , "Brooklyn", "Manhattan", "Queens", "Staten Island")
      )
    ),
    mainPanel(
      h2("Bar Chart"),
      plotlyOutput("crimeBarPlot"),
      br(),
      h3("Analysis"),
      p("Analyzing the victim race of crimes committed in New York City across different 
        boroughs reveals distinct patterns and potential disparities suggesting that 
        certain races may be disproportionately affected in specific areas. The five 
        New York City boroughs that can be selected are the Bronx, Brooklyn , Manhattan, 
        Queens as well as Staten Island. The races that were observed in this data 
        were American Indian/Alaskan Native, Asian/Pacific Islander, Black, White, 
        White Hispanic and lastly, unknown. Users can interact with this data by selecting 
        which borough out of the five they desire to observe."),
      p("This analysis displays the highest amount of crimes, sectioned by race, 
        to be in the borough of Brooklyn, New York. Compared to the other four boroughs, 
        Brooklyn, New York had significantly higher rates with victims of a Black 
        racial background being the highest category of crimes at about 48,000 crimes. 
        Second to that we see that the unknown category has about 42,000 crimes. Then, 
        White victims came at the third highest crime rate with just above 26,000 
        crimes. White Hispanic victims had about 16,000 crimes and American Indian/Alaskan 
        Native as well as Asian/Pacific Islander victims crimes were under 10000."),
      p("The bar plot overlooking Staten Island illustrates that the borough with 
        the least amount of crimes, sectioned by race, is Staten Island, New York. 
        The category of victims with the highest number of crimes committed in this 
        borough were White victims which had about 8,600 crimes. Next, we see that 
        victims that were categorized as unknown had the second highest number of 
        crimes at about 5,900. Then, Black victims had the third highest number 
        of crimes of about 3,900 crimes. At fourth, White Hispanic Victims had a 
        total of about 2,800 crimes. Pacific Islander victims had about 500 crimes. 
        And lastly, American Indian/Alaskan Native victims had the least number 
        of crimes at about 100 crimes. ")
    )
  )
)

plot3_view <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "radio_change",
        label = h4("Select a property value category"),
        choices = list("Assessed value", "Market value", "Taxable value")
      ),
      br(),
      numericInput(
        inputId = "map_min",
        label = h5("Minimum value to display"),
        value = -38000000,
        min = -38000000,
        max = 85000000
      ),
      p("Min default: -38000000"),
      numericInput(
        inputId = "map_max",
        label = h5("Maximum value to display"),
        value = 85000000,
        min = -38000000,
        max = 85000000
      ),
      p("Max default: 85000000"),
      br(),
      h6("Tip: A lot of the data is centered around 0. Try adjusting the min to 1 or max to -1.")
    ),
    mainPanel(
      h2("Map Plot"),
      withSpinner(leafletOutput(outputId = "map"), type = 8, color = "Black"),
      br(),
      h3("Table of Counts"),
      tableOutput(outputId = "map_table"),
      br(),
      h3("Analysis"),
      p("The map plot displays the location of over 22,000 New York City properties.
        Each data point is colored to represent its corresponding borough 
        region. As the input values are adjusted, properties will appear or 
        disappear based on how the property value has changed from 2015 to 2016. 
        The map can show three different categories of property values: assessed value, 
        market value, and taxable value. Negative values indicate a decrease in property 
        value, while positive values indicate an increase in property value."),
      p("Among properties that increased in value (based on all three categories), 
        most properties were located in Brooklyn, followed by Manhattan, and then Queens. 
        In all value categories, the Bronx had the least properties that increased in value."),
      p("Property value decreases were less uniform among the five boroughs. For 
        assessed value decreases, Brooklyn held the most properties, followed by 
        Manhattan and Staten Island. For market value decreases, Brooklyn 
        held the most properties, followed by a close three-way tie where Staten Island 
        had the most properties, followed by Manhattan and Queens. Taxable value 
        changes showed the most unique distribution. Manhattan had the most taxable 
        property value decreases, with a staggering count of over 8,400 properties.
        This is nearly double the total decreased taxable properties in Queens and 
        Brooklyn combined, which were the two boroughs that followed Manhattan. 
        In all value categories, the Bronx had the least properties 
        that decreased in value."),
      p("In addition to these trends, it is also important to note that most NYC 
        properties had no change in assessed value and market value from 2015 to 2016.")
    )
  )
)

summary_view <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Data Nutrition Label"),
      img(src = "nutrition_label.png", height="100%", width="100%", style="display: block; margin-left: auto; margin-right: auto;")
    ),
    mainPanel(
      h2("Summary"),
      p("The radar chart provides visual and numerical perspectives on crime rates 
        across New York City's boroughs. We found that the frequency of crime is 
        highest in Brooklyn, followed by Manhattan, the Bronx, Queens, and Staten Island."),
      p("In terms of the bar plot, comparing the boroughs with the highest amount 
        of crimes, Brooklyn, New York, and the least amount of crimes, Staten Island, 
        New York, provides a contrast in crime distribution across racial categories. 
        Brooklyn is the borough with the highest crime, notably driven by a substantial 
        number of crimes against Black victims, surpassing 48,000 incidents along 
        with the unknown category and crimes against White victims contributing 
        to Brooklyn’s higher crime rate. On the other hand, Staten Island is 
        highlighted as the borough with the fewest crimes showcasing a different 
        pattern in victim race distribution. The prevalence of crimes against 
        victims categorized as White victims is notable in Staten Island equating 
        to about 8,600 incidents with unknown victims having the second highest 
        category with about 5,900 following  Black victims with the third highest 
        category with about 3,900. This goes to show a distinctive racial crime 
        distribution compared to Brooklyn."),
      p("The map plot and its accompanying table reveal insight into the NYC property 
        value changes from 2015 to 2016. The top boroughs where property value 
        increased were Brooklyn, Manhattan, and Queens. Though Brooklyn also has 
        the most properties that decrease in assessed value and market value, 
        Manhattan tops the chart with the most decreased taxable properties. In 
        fact, it has more decreased taxable properties than the rest of the boroughs 
        combined."),
      h2("Main Takeaway"),
      p("Overall, when we examine all of this information together, we find that 
        crimes are most frequent in Brooklyn, where crime victims are predominantly 
        Black, and crimes are least frequent in Staten Island, where crime victims 
        are predominantly White. When we connect this with revised property values, 
        we find that Brooklyn tends to have the most increased property values, and 
        Staten Island is the borough with the least decreased property values."),
      p("In regards to New York's boroughs and our data, there appears to be a weak 
        correlation between crime and property value. There are many other factors 
        such as population size, available housing per capita, and cost of living, 
        which may contribute to changing property values.")
    )
  )
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
  tabPanel("Radar Chart", plot1_view),
  tabPanel("Bar Chart", plot2_view),
  tabPanel("Map Plot", plot3_view),
  tabPanel("Summary", summary_view),
  tags$style(HTML("* {color: #000000;}")),
  tags$style(HTML("p {color: #666565;}")),
  inverse = T
)

