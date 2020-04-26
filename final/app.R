#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# LOAD THE REQUIRED PACKAGES

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
library(patchwork)
library(magick)
library(giphyr)


# packages for mapping
library(leaflet)
library(spdplyr)
library(rmapshaper)
library(sf)


# READ IN THE DATA

# Read in the shapefiles

shapefiles <- readRDS(file = "shapefiles.rds")


# Read in the dataset made specifically for mapping

x <- readRDS("mwData.rds")

# Now subset by year

# I tried saving these as Rds files as well, but I kept getting a weird error
# Might be a bug, according to SO
Census_1870 <- x %>% filter(YEAR == "1870") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)

Census_1880 <- x %>% filter(YEAR == "1880") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)

Census_1890 <- x %>% filter(YEAR == "1890") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)

Census_1900 <- x %>% filter(YEAR == "1900") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)

Census_1910 <- x %>% filter(YEAR == "1910") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)

Census_1920 <- x %>% filter(YEAR == "1920") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)

Census_1930 <- x %>% filter(YEAR == "1930") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)

Census_1940 <- x %>% filter(YEAR == "1940") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)

Census_1950 <- x %>% filter(YEAR == "1950") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)

Census_1960 <- x %>% filter(YEAR == "1960") %>%
    select_if(~ !all(is.na(.))) %>%
    select(-STATEA:-GISJOIN & -COUNTY)


# If I put "Census_1870" = "1870", would only the year show up in dropdown menu?
data_sets <- c("1870" = "Census_1870", "1880" = "Census_1880", "1890" = "Census_1890", "1900" = "Census_1900", "1910" = "Census_1910", "1920" = "Census_1920",  "1930" ="Census_1930",  "1940" ="Census_1940",  "1950" ="Census_1950",  "1960" ="Census_1960")


# Read in the dataset for maps and models

allData <- readRDS("my_data.rds")


# START THE APP

# Input

ui <- fluidPage(
    
    # flatly is a nice theme to make the tabs look put together
    
    # Application title
    
    navbarPage("Agriculture and Education in the U.S.", theme = shinytheme("flatly"),
               
               # remember that tab panel is for what shows up in the tab, and title panel is 
               # for what shows up at the top of the page within the tab                  
               
               tabPanel("Introduction",
                        titlePanel("A Brief Overview of Midwest Agriculture"),
                        
                        
                        # Create a spot for the plot
                        mainPanel(
                            tabsetPanel(id = "tabsMain",
                                        tabPanel("Farms & Acreage",
                                                 # Generate a row with a sidebar
                                                 mainPanel(
                                                     imageOutput("acre_gif")
                                                 )),
                                        tabPanel("Urbanization",
                                                 # Generate a row with a sidebar
                                                 mainPanel(
                                                     imageOutput("pop_gif")
                                                 )),
                                        tabPanel("Mechanization",
                                                 # Generate a row with a sidebar
                                                 mainPanel(
                                                     imageOutput("plot2")
                                                 ))
                                        ))),
               
               
               tabPanel("Models",
                        
                        # Give the page a title
                        titlePanel("Agriculture and Education, by County"),
                        
                         
                        # Create a spot for the plot
                        mainPanel(
                            tabsetPanel(id = "tabsMain",
                                        
                                        tabPanel("Implements",
                                                 # Generate a row with a sidebar
                                                 sidebarLayout(      
                                                     
                                                     # Define the sidebar with one input
                                                     # comprehensive options:
                                                     # c("1870" = "1870", "1880" = "1880", "1890" = "1890", "1900" = "1900","1910" = "1910", "1920" = "1920", "1930" = "1930", "1940" = "1940", "1950" = "1950", "1960" = "1960")
                                                     sidebarPanel(
                                                         selectInput("Year", "Year:", 
                                                                     choices=c("1870" = "1870", "1890" = "1890", "1910" = "1910"), 
                                                                     multiple = TRUE)),
                                                     helpText("*Enter help text*")
                                                 )
                                        ),
                                        
                                        tabPanel("Cattle",
                                                 # Generate a row with a sidebar
                                                 sidebarLayout(      
                                                     
                                                     # Define the sidebar with one input
                                                     # comprehensive options:
                                                     # c("1870" = "1870", "1880" = "1880", "1890" = "1890", "1900" = "1900","1910" = "1910", "1920" = "1920", "1930" = "1930", "1940" = "1940", "1950" = "1950", "1960" = "1960")
                                                     sidebarPanel(
                                                         selectInput("cowYear", "Year:", 
                                                                     choices=c("1870" = "1870", "1890" = "1890", "1910" = "1910"), 
                                                                     multiple = TRUE)),
                                                     helpText("*Enter help text*")
                                                 ),
                                                 
                                                 # this tells the UI what plot to put 
                                                 
                                                 plotOutput("agPlot"),
                                                 p("Number of cows might be seen as a stand in for farm size. Is it?")
                                        ),
                                        
                                        tabPanel("Gender",
                                                 # Generate a row with a sidebar
                                                 sidebarLayout(      
                                                     
                                                     # Define the sidebar with one input
                                                     # comprehensive options:
                                                     # c("1870" = "1870", "1880" = "1880", "1890" = "1890", "1900" = "1900","1910" = "1910", "1920" = "1920", "1930" = "1930", "1940" = "1940", "1950" = "1950", "1960" = "1960")
                                                     sidebarPanel(
                                                         selectInput("Year", "Year:", 
                                                                     choices=c("1870" = "1870", "1890" = "1890", "1910" = "1910"), 
                                                                     multiple = TRUE)),
                                                     helpText("*Enter help text*")
                                                 )
                                        )
                                        )
                            )
                        ),
               
               
               tabPanel("Maps",
                        titlePanel("Midwest Map Explorer"),
                        p("For best results, view in fullscreen mode."),
                        p("Maps may take up to 30 seconds to load...but they're worth it!"),
                        sidebarLayout(
                            
                            sidebarPanel(
                                uiOutput("choose_dataset"),
                                uiOutput("choose_columns"),
                                uiOutput("choose_columns2"),
                                br(),
                            ),
                            mainPanel(leafletOutput("map1"),
                                      leafletOutput("map2"))
                        )),
               
               tabPanel("About",
                        
                        # Give the page a title
                        titlePanel("About this project"),
                        p("My name is Julia Englebert. I am a senior at Harvard College studying Near Eastern Languages and Civilizations with a secondary field in Ethnicity, Migration, and Rights. Some of my academic interests include the history and future of Midwestern agriculture, and the role of data science in shaping social policy."),
                        br(),
                        p("This project uses historical census and American Community Survey (ACS) data to explore correlations between agriculture and education in the Midwestern United States. By joining the two datasets, the former containing agricultural data and the later containing demographic data, I was able to look at the impact of farming, land value, crop value, and more on education in Midwestern communities over the course of a century. I chose this particular time range in part because of the data that was available to me, and in part due to the economic, industrial, and education policy changes that ocurred from 1870 to 1960. My data is sourced from the IPUMS National Historical Geographic Information System (NHGIS) at nhgis.org. IPUMS provides census data from around the world, but my project focuses specifically on the United States from 1870 to 1960. All datasets and gis files used for this project can be downloaded from the NHGIS Data Finder:"),
                        p(a(href="https://data2.nhgis.org/main", "NHGIS Data Finder")),
                        br(),
                        p("To see more of my work, please visit my Github account:"),
                        p(a(href="https://github.com/julia-englebert", "Julia Englebert")),
                        br(),
                        titlePanel("Citations"),
                        p("Steven Manson, Jonathan Schroeder, David Van Riper, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 14.0 [Database]. Minneapolis, MN: IPUMS. 2019. http://doi.org/10.18128/D050.V14.0"),
                        p("The College of William and Mary and the Minnesota Population Center. School Attendance Boundary Information System (SABINS): Version 1.0. Minneapolis, MN: University of Minnesota 2011.")
               ),
               
               tabPanel("Codebook",
                        titlePanel("How were variables measured for this project?"),
                        p("Put some info here. An interactive gt table would be nice if you have time."))
    )
)



# use plotly to allow hover text to appear on plots, giving more info about the point

server <- (function(input, output, session) {
    
    output$acre_gif <- renderImage({
        list(src = "acre_gif.gif")
        
    }, deleteFile = FALSE)
    
    output$pop_gif <- renderImage({
        list(src = "pop_gif.gif")
        
    }, deleteFile = FALSE)
    
    output$plot2 <- renderImage({
        list(src = "plot2.gif")
        
    }, deleteFile = FALSE)
    
    # Fill in the spot we created for a plot
    output$agPlot <- renderPlot({
        
        # Render a plot
        allData %>%
            filter(YEAR %in% input$cowYear) %>%
            mutate(YEAR = as.factor(YEAR)) %>%
            mutate(YEAR = fct_explicit_na(YEAR, na_level = NA)) %>%
            group_by(YEAR) %>%
            select(YEAR, cows_per_farm, pct_enrolled) %>%
            na.omit() %>%
            ggplot(aes(x = cows_per_farm, y = pct_enrolled, color = YEAR)) +
            geom_point(alpha = 0.1) +
            geom_smooth(method = "lm", se = FALSE) +
            scale_color_viridis_d() +
            ylim(0, 45) +
            scale_x_log10(labels = comma) +
            labs(title = "Cows and Education",
                 subtitle = "Relationship between the number of cows per farm\nand school enrollment, by county",
                 x = "Average Cows per Farm",
                 y = "Percent of Population Enrolled in School") +
            theme_classic()
    })
    
    output$choose_dataset <- renderUI({
        selectInput("dataset", "Choose Census Year:", as.list(data_sets))
    })
    
    # Check boxes
    output$choose_columns <- renderUI({
        # If missing input, return to avoid error later in function
        if(is.null(input$dataset))
            return()
        
        # Get the data set with the appropriate name
        dat <- get(input$dataset)
        
        # Get rid of "YEAR" and "STATE" so that users don't pick them and make senseless maps
        
        colnames1 <- names(dat)
        colnames <- colnames1[!colnames1 %in% "YEAR" & !colnames1 %in% "STATE"]
        
        # Create the dropdown menu and select all columns by default
        selectizeInput("columns", "Choose Variable:", 
                       choices  = colnames)
    })
    
    
    # Map output
    output$map1 <- renderLeaflet({
        # If missing input, return to avoid error later in function
        if(is.null(input$dataset))
            return()
        
        # Get the data set
        dat <- get(input$dataset)
        
        # Make sure columns are correct for data set (when data set changes, the
        # columns will initially be for the previous data set)
        if (is.null(input$columns) || !(input$columns %in% names(dat)))
            return()
        
        year <- dat %>% slice(1) %>% pull(YEAR)
        
        # Keep the selected columns
        dat <- dat[, input$columns, drop = FALSE]
        
        myvar <- input$columns
        
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
    
    # map2 dropdown menu
    output$choose_columns2 <- renderUI({
        # If missing input, return to avoid error later in function
        if(is.null(input$dataset))
            return()
        
        # Get the data set with the appropriate name
        dat <- get(input$dataset)
        
        # Get rid of "YEAR" and "STATE" so that users don't pick them and make senseless maps
        
        colnames1 <- names(dat)
        colnames <- colnames1[!colnames1 %in% "YEAR" & !colnames1 %in% "STATE"]
        
        # Create the dropdown menu and select all columns by default
        selectizeInput("columns2", "Choose Variable to Compare:", 
                       choices  = colnames,
                       options = list(
                           placeholder = 'Variables...',
                           onInitialize = I('function() { this.setValue(""); }')))
    })
    
    output$map2 <- renderLeaflet({
        # If missing input, return to avoid error later in function
        if(is.null(input$dataset))
            return()
        
        # Get the data set
        dat <- get(input$dataset)
        
        # Make sure columns are correct for data set (when data set changes, the
        # columns will initially be for the previous data set)
        if (is.null(input$columns2) || !(input$columns2 %in% names(dat)))
            return()
        
        year <- dat %>% slice(1) %>% pull(YEAR)
        
        # Keep the selected columns
        dat <- dat[, input$columns2, drop = FALSE]
        
        myvar <- input$columns2
        
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
})



# Run the application 

shinyApp(ui = ui, server = server)
