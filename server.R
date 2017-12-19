
load("clim_data_PR_TAS.Rda")
load("clim_series.Rda")
load("country.Rda")
load("clim_data_PR_TAS_COUNTRY.Rda")


## importing required libraries

library(dplyr)
library("tidyr")
library("ggplot2")
# install.packages("rworldmap")
library(rworldmap)

#install.packages("shinythemes")
library("shinythemes")

# More info on google charts:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")
## if (!require(devtools))
##   install.packages("devtools")
## devtools::install_github("jcheng5/googleCharts")
library(devtools)
library(googleCharts)

library(DT)
library(mgcv)





shinyServer(function(input , output, session) {
  
  clim.data.viz <- reactiveValues( 
    X = clim.data.PR.TAS['SP.POP.TOTL'],
    Y1 = clim.data.PR.TAS['TAS.MIN'],
    Y2 = clim.data.PR.TAS['TAS.MEAN'],
    Y3 = clim.data.PR.TAS['TAS.MAX'] ,
    Y4 = clim.data.PR.TAS['TAS.MAX'] ,
    # Y4 = clim.data.PR.TAS['PR.MIN'],
    #  Y5 = clim.data.PR.TAS['PR.MEAN'],
    #  Y6 = clim.data.PR.TAS['PR.MAX'],
    TXT1 =  "Min, Mean and Max Temperature vs Population",
    TXT2 =  "Country wise Population distribution"
  )
  
  
  
  
  
  ### TAB1 plots
  
  observeEvent(input$Vari_Upd, {
    
    
    temp_ind_var <- (clim.series %>% filter(Series.name==input$x0) %>% select(Series.code))[[1]]
    
    
    clim.data.viz$TXT1 <- ( paste("Min, Mean and Max ", input$y0 , " vs ", input$x0) 
    )
    
    
    if(input$y0 == "precipitation") {
      clim.data.viz$Y4 = clim.data.PR.TAS['PR.MAX']
    } else {
      clim.data.viz$Y4 = clim.data.PR.TAS['TAS.MAX']
    }
    
    
    
    if(!is.null(input$e0)){  
      
      
      if(input$y0 == "precipitation")   {
        
        
        clim.data.viz$X <- clim.data.PR.TAS %>% 
          filter(Country.name %in% input$e0) %>%
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select(temp_ind_var)
        clim.data.viz$Y1 <- clim.data.PR.TAS %>% 
          filter(Country.name %in% input$e0) %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('PR.MIN')
        clim.data.viz$Y2 <- clim.data.PR.TAS %>% 
          filter(Country.name %in% input$e0) %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('PR.MEAN')
        clim.data.viz$Y3 <- clim.data.PR.TAS %>% 
          filter(Country.name %in% input$e0) %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('PR.MAX')
        
      } else {
        
        clim.data.viz$X <- clim.data.PR.TAS %>% 
          filter(Country.name %in% input$e0) %>%
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select(temp_ind_var)
        clim.data.viz$Y1 <- clim.data.PR.TAS %>% 
          filter(Country.name %in% input$e0) %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('TAS.MIN')
        clim.data.viz$Y2 <- clim.data.PR.TAS %>% 
          filter(Country.name %in% input$e0) %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('TAS.MEAN')
        clim.data.viz$Y3 <- clim.data.PR.TAS %>% 
          filter(Country.name %in% input$e0) %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('TAS.MAX')
        
      }
      
      
    } 
    
    else {
      
      if(input$y0 == "precipitation")   {
        
        
        clim.data.viz$X <- clim.data.PR.TAS %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select(temp_ind_var)
        clim.data.viz$Y1 <- clim.data.PR.TAS %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('PR.MIN')
        clim.data.viz$Y2 <- clim.data.PR.TAS %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('PR.MEAN')
        clim.data.viz$Y3 <- clim.data.PR.TAS %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('PR.MAX')
        
      } else {
        
        clim.data.viz$X <- clim.data.PR.TAS %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select(temp_ind_var)
        clim.data.viz$Y1 <- clim.data.PR.TAS %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('TAS.MIN')
        clim.data.viz$Y2 <- clim.data.PR.TAS %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('TAS.MEAN')
        clim.data.viz$Y3 <- clim.data.PR.TAS %>% 
          filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
          select('TAS.MAX')
        
      }
      
      
    }
    
    
  })
  
  # observeEvent(input$Vari_Upd, {
  #   print(input$y0)
  # })
  # 
  
  
  
  output$Min_Relation <- renderPlot({
    ggplot(mapping = aes( clim.data.viz$X, clim.data.viz$Y1) ) +
      geom_point() + geom_smooth(method ="auto") +
      scale_x_log10(paste(input$x0," (log10 scale)")) +
      scale_y_continuous( paste("Minimum ", input$y0) )
  })
  
  
  output$Mean_Relation <- renderPlot({
    ggplot(mapping = aes( clim.data.viz$X, clim.data.viz$Y2) ) +
      geom_point() + geom_smooth(method ="auto") +
      scale_x_log10(paste(input$x0," (log10 scale)")) +
      scale_y_continuous( paste("Mean ", input$y0) )
  })
  
  
  
  output$Max_Relation <- renderPlot({
    ggplot(mapping = aes( clim.data.viz$X, clim.data.viz$Y3) ) +
      geom_point() + geom_smooth(method ="auto") +
      scale_x_log10(paste(input$x0," (log10 scale)")) +
      scale_y_continuous( paste("Maximum ", input$y0) )
  })
  
  
  
  
  clim.mod1 <- glm(TAS.MAX ~ .-PR.MIN-PR.MAX-PR.MEAN-TAS.MIN-TAS.MAX-TAS.MEAN-Country.code-Country.name 
                   , data = clim.data.PR.TAS)
  
  output$TAS_MAX_mod <- renderPrint({ 
    summary(clim.mod1)
  })
  
  
  
  clim.mod2 <- glm(PR.MAX ~ .-PR.MIN-PR.MAX-PR.MEAN-TAS.MIN-TAS.MAX-TAS.MEAN-Country.code-Country.name 
                   , data = clim.data.PR.TAS)
  
  output$PR_MAX_mod <- renderPrint({ 
    summary(clim.mod2)
  })
  
  
  output$Year_Trend <- renderPlot({
    ggplot(clim.data.PR.TAS, aes(Year, clim.data.viz$Y4) ) +
      stat_summary(fun.y = mean, colour="red", geom="point",size = 3, pch=9 ) + 
      theme_light()+
      geom_smooth(method="loess", colour='dark red', se=FALSE,linetype="dashed") +
      scale_y_continuous( paste("Maximum ", input$y0) ) +
      
      theme(
        panel.background = element_rect(fill = "lightblue",
                                        colour = "dark blue",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.1, linetype = "dotdash",
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.1, linetype = "dotdash",
                                        colour = "white")
      )
    
    
  })
  
  
  
  
  output$head22 <- renderText({clim.data.viz$TXT1})
  
  
  
  
  
  #############
  #tab2
  ############
  
  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    #names = levels(data$Region)
    names = c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", "Middle East & North Africa", "North America",     "South Asia", "Sub-Saharan Africa")
  )
  
  yearData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- clim.data.PR.TAS.COUNTRY %>%
      filter(Year == input$year , Region !="Aggregates", Year != 1990) %>%
      select(Country.name, PR.MAX, TAS.MAX,
             Region, SP.POP.TOTL) %>%
      arrange(Region)
  })
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "Maximum Temperature vs Maximum Precipitation",
          input$year),
        series = series
      )
    )
  })
  
  
  
  
  #############
  #tab3
  ############
  
  
  observe( {( clim.data.viz$TXT2 <- paste("Country wise ", input$x1 , " distribution ")) } )
  
  output$head31 <- renderText({clim.data.viz$TXT2})
  
  temp_exp_var <- reactive({ (clim.series %>% filter(Series.name==input$x1) %>% select(Series.code))[[1]]})
  
  
  output$mapdata <- renderPlot({
    
    
    mapped_data2 <- joinCountryData2Map(clim.data.PR.TAS, joinCode = "ISO3", 
                                        nameJoinColumn = "Country.code")
    par(mai=c(0,0,1,0),xaxs="i",yaxs="i")
    mapCountryData(mapped_data2, nameColumnToPlot = temp_exp_var(), colourPalette = "topo") 
    
  })  
  
  #############
  #tab4
  ############  
  
  
  #clim_ser_ref <- renderPrint( { kable(clim.series, caption = "Explanatory Variable Reference Data") })
  
  output$clim_ser_ref = DT::renderDataTable({clim.series})
  
  
}


# clim.data.x <- reactive({
#              clim.data.PR.TAS %>% 
#              filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
#              select(SP.URB.GROW)
#  })
# 
# clim.data.y <- reactive({
#   clim.data.PR.TAS %>% 
#     filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>%
#     select(PR.MIN)
# })




# observe( if(!is.null(input$Vari_Upd)){
#                           clim.data.viz <- clim.data.PR.TAS %>% 
#                                         filter(Country.name %in% input$e0)
#         }else{
#           clim.data.viz <- clim.data.PR.TAS
#         }
#          
#       )

# clim.data.viz <- eventReactive(input$Vari_Upd,if(!is.null(input$Vari_Upd)) {clim.data.PR.TAS %>% 
#                               filter(Country.name %in% input$e0) %>%
#                               filter(Year >= input$sYear[1] & Year <= input$sYear[2])
#                               } else {
#                                 clim.data.PR.TAS
#                               })




# observe({
#     clim.data.viz$X <- clim.data.PR.TAS %>% filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>% select('SP.URB.GROW')
#     clim.data.viz$Y <- clim.data.PR.TAS %>% filter(Year >= input$sYear[1] & Year <= input$sYear[2]) %>% select('SP.URB.GROW')
# 
# })



#  output$hist <- renderPlot({
#    hist(rnorm(input$snum), main = paste(input$snum , " random normal values"))
# })

#  output$stats <- renderPrint({
#    summary(rnorm(input$snum))
#  })

)
#shinyApp(ui = ui, server = server)