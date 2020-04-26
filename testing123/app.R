# testing123

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(scipen = 999)

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
library(readxl)
library(gganimate)

# packages for mapping
library(leaflet)
library(spdplyr)
library(rmapshaper)
library(sf)

# Prep the shapefiles
# Now save the gis data with short names for sake of the shiny app
# hopefully this will work...

dsn1870 <- "raw-data/nhgis0002_shape/nhgis0002_shapefile_tl2008_us_county_1870"
layer1870 <- "US_county_1870_conflated"
dsn1880 <- "raw-data/nhgis0002_shape/nhgis0002_shapefile_tl2008_us_county_1880"
layer1880 <- "US_county_1880_conflated"
dsn1890 <- "raw-data/nhgis0002_shape/nhgis0002_shapefile_tl2008_us_county_1890"
layer1890 <- "US_county_1890_conflated"
dsn1900 <- "raw-data/nhgis0003_shape/nhgis0003_shapefile_tl2008_us_county_1900"
layer1900 <- "US_county_1900_conflated"
dsn1910 <- "raw-data/nhgis0003_shape/nhgis0003_shapefile_tl2008_us_county_1910"
layer1910 <- "US_county_1910_conflated"
dsn1920 <- "raw-data/nhgis0003_shape/nhgis0003_shapefile_tl2008_us_county_1920"
layer1920 <- "US_county_1920_conflated"
dsn1930 <- "raw-data/nhgis0003_shape/nhgis0003_shapefile_tl2008_us_county_1930"
layer1930 <- "US_county_1930_conflated"
dsn1940 <- "raw-data/nhgis0003_shape/nhgis0003_shapefile_tl2008_us_county_1940"
layer1940 <- "US_county_1940_conflated"
dsn1950 <- "raw-data/nhgis0003_shape/nhgis0003_shapefile_tl2008_us_county_1950"
layer1950 <- "US_county_1950_conflated"
dsn1960 <- "raw-data/nhgis0003_shape/nhgis0003_shapefile_tl2008_us_county_1960"
layer1960 <- "US_county_1960_conflated"

# Make a tibble of all the shapefiles that you'll need
shapefiles <- tibble(Year = c(1870, 1880, 1890, 1900, 1910, 1920, 1930, 1940, 1950, 1960), 
                     Dsn = c(dsn1870, dsn1880, dsn1890, dsn1900, dsn1910, dsn1920, dsn1930, 
                             dsn1940, dsn1950, dsn1960),
                     Layer = c(layer1870, layer1880, layer1890, layer1900, layer1910, layer1920, 
                               layer1930, layer1940, layer1950, layer1960))

x <- readRDS("my_data.rds") %>%
    mutate("Value of Farms" = as.numeric(value_farm), "Average Farm Value" = as.numeric(avg_value_farm), 
           "Value of Implements" = as.numeric(value_implements), 
           "Average Implement Value" = as.numeric(avg_value_implements), 
           "Average Farm Size" = as.numeric(avg_size), "Number of Farms" = as.numeric(n_farm), 
           "Acres of Farmland" = as.numeric(acres_farms), "Irrigated Acres" = as.numeric(acres_irrigated), 
           "Percentage of Farmland Irrigated" = as.numeric(pct_irrigated), "Number of Cows" = as.numeric(cows),
           "Number of Cows per Farm" = as.numeric(cows_per_farm), "Number of Horses" = as.numeric(horses), 
           "Total Population" = as.numeric(pop), "Male Population" = as.numeric(male), 
           "Female Population" = as.numeric(female), "Male Percentage of Population" = as.numeric(pct_male),
           "Total School Enrollment" = as.numeric(enrolled), 
           "Percentage School Enrollment" = as.numeric(pct_enrolled), 
           "Urban Population" = as.numeric(urban_pop), 
           "Percent of Population in Urban Areas" = as.numeric(pct_urban), 
           "Illiterate Population" = as.numeric(illiterate), 
           "Illiteracy Rate" = as.numeric(pct_illiterate)) %>%
    select(STATEA, COUNTYA, GISJOIN, YEAR, STATE, COUNTY, 
           "Value of Farms", "Average Farm Value", "Value of Implements", "Average Implement Value",
           "Average Farm Size", "Number of Farms", "Acres of Farmland", "Irrigated Acres", 
           "Percentage of Farmland Irrigated", "Number of Cows", "Number of Cows per Farm", 
           "Number of Horses", "Total Population", "Male Population", "Female Population", 
           "Male Percentage of Population", "Total School Enrollment", 
           "Percentage School Enrollment", "Urban Population", 
           "Percent of Population in Urban Areas", "Illiterate Population", "Illiteracy Rate")

# Now subset by year
# Really should have saved these as Rds files. Will do that later
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

data_sets <- c("Census_1870", "Census_1880", "Census_1890", "Census_1900", "Census_1910", "Census_1920", "Census_1930", "Census_1940", "Census_1950", "Census_1960")

ui <- fluidPage(
    pageWithSidebar(
    
    headerPanel("Midwest Map Explorer"),
    
    sidebarPanel(
        uiOutput("choose_dataset"),
        
        uiOutput("choose_columns"),
        br(),
        a(href = "https://gist.github.com/4211337", "Source code")
    ),
    
    
    mainPanel(
        leafletOutput("map1")
    )
))

server <- (function(input, output) {
    
    # Drop-down selection box for which data set
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
        selectInput("columns", "Choose Variable:", 
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
})
    

shinyApp(ui = ui, server = server)

