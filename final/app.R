#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(haven)
library(tidyverse)
library(gt)
library(devtools)
library(tibble)
library(foreign)
library(scales)
library(shinythemes)
library(plotly)
library(leaflet)
library(broom)

# packages for mapping
library(leaflet)
library(spdplyr)
library(rmapshaper)
library(sf)


data1800s <- readRDS("my_data.rds")


# Input

ui <- fluidPage(
    
    # flatly is a nice theme to make the tabs look put together
    
    # Application title
    
    navbarPage("Agriculture and Education in the U.S.", theme = shinytheme("flatly"),
               
               # remember that tab panel is for what shows up in the tab, and title panel is 
               # for what shows up at the top of the page within the tab                  
               
               tabPanel("Tab 1",
                        
                        # Give the page a title
                        titlePanel("Relationship Between Farm Value and Education, by County"),
                        
                        # Generate a row with a sidebar
                        sidebarLayout(      
                            
                            # Define the sidebar with one input
                            # insteaad of region, I want year ######
                            sidebarPanel(
                                selectInput("year", "Year:", 
                                            choices=c("1870" = "1870", "1890" = "1890", "1910" = "1910"), 
                                            multiple = TRUE)),
                            helpText("From Great Plains Population and Environment Survey Agricultural and Social & Demographic Data.")
                        ),
                        
                        # Create a spot for the plot
                        mainPanel(
                            tabsetPanel(id = "tabsMain",
                                        tabPanel("Plot",
                                                 
                                                 # this tells the UI what plot to put 
                                                 
                                                 plotOutput("agPlot")  
                                        )))),
               tabPanel("Models",
                        
                        # Give the page a title
                        titlePanel("Relationship Between Farm Value and Education, by County"),
                        p("Make some models! (See notes)"),
                        gt_output("model1")
               ),
               tabPanel("Maps",
                
                # Give the page a title
                titlePanel("Title Relating to Maps"),
                p("Add a slider for different years/variables & figure out how to get two maps side by side"),
                p("Maps may take up to 30 seconds to load."),
                # Generate a row with a sidebar
                sidebarLayout(      
                    
                    # Define the sidebar with one input
                    # insteaad of region, I want year ######
                    sidebarPanel(
                        selectInput("YEAR", "Year:", 
                                    choices=c("1870" = "1870", "1880" = "1880", "1890" = "1890", "1900" = "1900", 
                                              "1910" = "1910", "1920" = "1920", "1930" = "1930", "1940" = "1940",
                                              "1950" = "1950", "1960" = "1960"), 
                                    multiple = FALSE)),
                    helpText("From Great Plains Population and Environment Survey Agricultural and Social & Demographic Data.")
                ),
                
                # Create a spot for the plot
                mainPanel(
                    tabsetPanel(id = "tabsMain",
                                tabPanel("Plot",
                                         
                                         # this tells the UI what plot to put 
                                         
                                         leafletOutput("map1"),
               p()),
               
               tabPanel("About",
                        
                        # Give the page a title
                        titlePanel("About this project"),
                        p("My name is Julia Englebert. I am a senior at Harvard College studying Near Eastern Languages and Civilizations with a secondary field in Ethnicity, Migration, and Rights. Some of my academic interests include the history and future of Midwestern agriculture, and the role of data science in shaping social policy."),
                        br(),
                        p("This project uses historical census and American Community Survey (ACS) data to explore correlations between agriculture and education in the Midwestern United States. By joining the two datasets, the former containing agricultural data and the later containing demographic data, I was able to look at the impact of farming, land value, crop value, and more on education in Midwestern communities over the course of a century. 
                        I chose this particular time range in part because of the data that was available to me, and in part due to the economic, industrial, and education policy changes that ocurred from 1870 to 1960. 
                          My data is sourced from the IPUMS National Historical Geographic Information System (NHGIS) at nhgis.org. IPUMS provides census data from around the world, but my project focuses specifically on the United States from 1870 to 1960. All datasets and gis files used for this project can be downloaded from the NHGIS Data Finder:"),
                        p(a(href="https://data2.nhgis.org/main", "NHGIS Data Finder")),
                        br(),
                        p("To see more of my work, please visit my Github account:"),
                        p(a(href="https://github.com/julia-englebert", "Julia Englebert")),
                        br(),
                        titlePanel("Citations"),
                        p("Steven Manson, Jonathan Schroeder, David Van Riper, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 14.0 [Database]. Minneapolis, MN: IPUMS. 2019. http://doi.org/10.18128/D050.V14.0"),
                        p("The College of William and Mary and the Minnesota Population Center. School Attendance Boundary Information System (SABINS): Version 1.0. Minneapolis, MN: University of Minnesota 2011.")
               )
    ))



# use plotly to allow hover text to appear on plots, giving more info about the point

server <- function(input, output) {
    
    # Fill in the spot we created for a plot
    output$agPlot <- renderPlot({
        
        # Render a plot
        b_1870_1910 %>% 
            filter(year %in% input$year) %>% 
            group_by(year) %>%
            ggplot(aes(x = value_farms, y = perc_school, color = year)) + 
            geom_point(alpha = 0.5) + 
            ylim(0, 100) + 
            geom_smooth(method = "lm") + 
            labs(title = "Relationship Between Farm Value and Education, by County", 
                 subtitle = "Great Plains Region, United States", 
                 x = "Total Dollar Value of Farms", 
                 y = "Percent Students Ages 5-24 Enrolled in School") +
            scale_x_log10(labels = comma) +
            theme_classic()
    })
    
    output$map1 <- renderLeaflet({
        year <- input$YEAR
        myvar <- "Average Farm Value"
        
        datayear <- mwData %>%
            # use get() to use the stored variable
            # idk why, but it doesn't work without this
            # the variable must be converted to a double in the original mwData dataset
            mutate(myvariable = (get(myvar))) %>%
            filter(YEAR == year, myvariable > 0) %>%
            select(GISJOIN, myvariable)
        
        # Import Census Tract Shapefile into R as SpatialPolygonsDataFrameFormat (SP Dataframe)
        # dsn is location of folder which contains shapefiles, (.proj, .shp etc.)
        # layer is the filename of the .shp file inside the
        # folder dsn points to. 
        
        dsnyear <- shapefiles %>% 
            filter(Year == year) %>% 
            pull(Dsn)
        layeryear <- shapefiles %>% 
            filter(Year == year) %>% 
            pull(Layer)
        
        countyyear <- sf::st_read(dsn = dsnyear,
                                  layer = layeryear)
        
        countyyear <-
            countyyear %>%
            merge(datayear, "GISJOIN")
        
        
        # Set projection of tracts dataset to `projection` required by leaflet
        
        countyyear <- sf::st_transform(countyyear, crs="+init=epsg:4326")
        
        # Condense size of data for faster processing
        
        countyyear <- rmapshaper::ms_simplify(countyyear)
        
        # Set palette color
        
        pal <- colorNumeric("viridis", NULL)
        
        #  Plot the data
        
        # use shiny to add a title, which will be be equivalent to myvar
        
        leaflet(countyyear) %>%
            addTiles() %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                        fillColor = ~pal(myvariable)) %>%
            addLegend(pal = pal, values = ~myvariable, opacity = 1.0, title = myvar)
    })
    
    output$model1 <- render_gt({
        
        # experimental model
        
        model1
        
    })
}



# Run the application 

shinyApp(ui = ui, server = server)
