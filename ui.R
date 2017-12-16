
# load("clim_data_PR_TAS.Rda")
# load("clim_series.Rda")
# load("country.Rda")
# load("clim_data_PR_TAS_COUNTRY.Rda")


## importing required libraries

library(dplyr)
library("tidyr")
library("ggplot2")
# install.packages("rworldmap")
library(rworldmap)

#install.packages("shinythemes")
library(shinythemes)

# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")
## if (!require(devtools))
##   install.packages("devtools")
## devtools::install_github("jcheng5/googleCharts")
library(devtools)
library(googleCharts)

# Use global max/min for axes so the view window stays
# constant as the user moves between years
xlim <- list(
  min = min(clim.data.PR.TAS$PR.MAX, na.rm = TRUE) - 500,
  max = max(clim.data.PR.TAS$PR.MAX, na.rm = TRUE) + 500
)
ylim <- list(
  min = min(clim.data.PR.TAS$TAS.MAX, na.rm = TRUE),
  max = max(clim.data.PR.TAS$TAS.MAX, na.rm = TRUE) + 5
)




shinyUI(fluidPage(title = "Climate Data Analysis",
                
                theme = shinythemes::shinytheme("cosmo"),           
                
                tags$head(tags$style(
                  HTML('
                       #sidebar {
                       background-color: #a9dff4;
                       }
                       #main {
                       background-color: #1dace3;
                       }
                       
                       body, label, input, button, select { 
                       font-family: "Arial";
                       }')
  )),
  
  tabsetPanel(
    
    ## TAB 1
    tabPanel(title = "Relationship and Trend Analysis",
             
             
             tags$h1("Climate Data Analysis"),
             tags$img(height= 30, width = 100, src = "http://www.worldbank.org/content/dam/wbr/logo/logo-wb-header-en.svg"),
             tags$code("Climate Data was extracted from world bank data sources."), tags$br(),
             tags$a(href = "https://data.worldbank.org/data-catalog/climate-change","world Bank Climate Change Data"),tags$br(),
             tags$a(href = "http://sdwebx.worldbank.org/climateportal/index.cfm?page=downscaled_data_download&menu=historical" , "world Bank Climate Change Knowledge Portal"),
             
             fluidPage(  
               fluidRow(
                 #column(8,
                 sidebarLayout(
                   sidebarPanel( id="sidebar",
                                 
                                 selectInput(
                                   'y0', 'Choose dependent variable', choices = c("Temperature","precipitation"),
                                   selectize = FALSE
                                 ),            
                                 
                                 selectizeInput(
                                   'x0', 'Choose independent variable', 
                                   choices = (clim.series %>% 
                                                filter(Series.code %in% names(clim.data.PR.TAS[4:17])) %>% 
                                                select(Series.name)
                                   )
                                 ),
                                 
                                 selectizeInput(inputId='e0', label = 'Country-select', 
                                                choices = Country$Country.name, multiple = TRUE
                                 ),
                                 
                                 sliderInput(inputId = "sYear", label = "Range : Years", 
                                             value = c(min(clim.data.PR.TAS$Year),max(clim.data.PR.TAS$Year)),
                                             min = min(clim.data.PR.TAS$Year), 
                                             max = max(clim.data.PR.TAS$Year), sep = NULL
                                 )
                                 #)
                   ),
                   mainPanel( 
                     column( actionButton(inputId = "Vari_Upd", label = "Update Data")),
                     column( plotOutput("Year_Trend")) 
                   )
                 )
               ),
               
               
               tags$h4(textOutput("head22")),
               
               fluidRow(
                 shiny::column(4, offset = 0, plotOutput("Min_Relation") ),
                 shiny::column(4, offset = 0, plotOutput("Mean_Relation") ),
                 shiny::column(4, offset = 0, plotOutput("Max_Relation") )
               )
             ) 
             
    ),
    
    ## TAB 2
    tabPanel(title = "Google Analysis",
             
             # This line loads the Google Charts JS library
             googleChartsInit(),
             
             # Use the Google webfont "Source Sans Pro"
             tags$link(
               href=paste0("http://fonts.googleapis.com/css?",
                           "family=Source+Sans+Pro:300,600,300italic"),
               rel="stylesheet", type="text/css"),
             tags$style(type="text/css",
                        "body {font-family: 'Source Sans Pro'}"
             ),
             
             h2("Region wise Precipitation vs Temperature trend"),
             
             googleBubbleChart("chart",
                               width="100%", height = "475px",
                               # Set the default options for this chart; they can be
                               # overridden in server.R on a per-update basis. See
                               # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                               # for option documentation.
                               options = list(
                                 fontName = "Source Sans Pro",
                                 fontSize = 13,
                                 # Set axis labels and ranges
                                 hAxis = list(
                                   title = "Maximum Temperature ("~degree~"C)",
                                   viewWindow = xlim
                                 ),
                                 vAxis = list(
                                   title = "Maximum Precipitation (mm)",
                                   viewWindow = ylim
                                 ),
                                 # The default padding is a little too spaced out
                                 chartArea = list(
                                   top = 50, left = 75,
                                   height = "75%", width = "75%"
                                 ),
                                 # Allow pan/zoom
                                 explorer = list(),
                                 # Set bubble visual props
                                 bubble = list(
                                   opacity = 0.4, stroke = "none",
                                   # Hide bubble label
                                   textStyle = list(
                                     color = "none"
                                   )
                                 ),
                                 # Set fonts
                                 titleTextStyle = list(
                                   fontSize = 16
                                 ),
                                 tooltip = list(
                                   textStyle = list(
                                     fontSize = 12
                                   )
                                 )
                               )
             ),
             fluidRow(
               shiny::column(4, offset = 4,
                             sliderInput("year", "Year",
                                         min = min(clim.data.PR.TAS %>% filter(Year != 1990) %>% select(Year)), max = max(clim.data.PR.TAS$Year),
                                         value = min(clim.data.PR.TAS %>% filter(Year != 1990) %>% select(Year)), animate = TRUE)
               )
             )
             
             
             
             
             
    ),
    
    
    ## TAB 3
    tabPanel(title = "Explanatory variable Analysis",
             
             tags$h4(textOutput("head31")),
             
             fluidRow(
               plotOutput("mapdata")
             ),
             
             hr(),
             
             
             fluidRow( 
               shiny::column(4, offset = 4,
                             selectizeInput(
                               'x1', 'Choose Explanatory variable', 
                               choices = (clim.series %>% 
                                            filter(Series.code %in% names(clim.data.PR.TAS[4:17])) %>% 
                                            select(Series.name)
                               )
                             )
               )
               
             )
             
             
             
    )
    
    
    
    
    
    
    
    
    
    
    
  )
  
                  )

)