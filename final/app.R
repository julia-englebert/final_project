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

# I'm getting a warning about the foreign package when I try to deploy
# Luckily, it's not necessary
#library(foreign)

library(scales)
library(shinythemes)
#library(plotly)
#library(leaflet)
library(broom)
# probably don't need these, but double check
#library(patchwork)
#library(magick)
#library(giphyr)
library(gridExtra)
library(readxl)


# packages for mapping
library(leaflet)
library(spdplyr)
#library(rmapshaper)
library(sf)


# READ IN THE DATA

# Read in the model for gt and plot

maleedu_model <- readRDS("maleedu_model.rds") 

cowedu_model <- readRDS("cowedu_model.rds")

cows_size_model <- readRDS("cows_size_model.rds")

# Read in the codebook
# This is an excel sheet that I made in order to condense the original codebooks provided by NHGIS

codebook <- read_excel("mw_codebook.xlsx") %>% 
    remove_rownames %>% 
    column_to_rownames(var="Year")

# Read in the shapefiles
# These filepaths were initially too long (101 bytes, I kid you not)
# So I changed the file names

shapefiles <- readRDS(file = "shapefiles.rds") %>% 
    head(n = 3L)


# Read in the dataset made specifically for mapping

x <- readRDS("mwData.rds")

# Build a tibble for the map pngs

mapPngs <- tibble(year = c(1870, 1870, 1870, 1870, 1870, 1880, 1880, 1880, 1880, 1880, 1890, 1890, 1890, 1890, 1890),
                  variable = c("Percentage School Enrollment", "Average Farm Value", "Average Cows per Farm", "Average Implement Value", "Average Farm Size (Acres)", "Percentage School Enrollment", "Average Farm Value", "Average Cows per Farm", "Average Implement Value", "Average Farm Size (Acres)", "Percentage School Enrollment", "Average Farm Value", "Average Cows per Farm", "Average Implement Value", "Average Farm Size (Acres)"),
                  path = c("enroll_1870.png", "avg_value_1870.png", "cows_1870.png", "implement_1870.png", "avg_size_1870.png", NA, "avg_value_1880.png", "cows_1880.png", "implement_1880.png", "avg_size_1880.png", "enroll_1890.png", "avg_value_1890.png", "cows_1890.png", "implement_1890.png", "avg_size_1890.png"))

# Read in the dataset for maps and models

allData <- readRDS("my_data.rds")


# START THE APP

# Input

ui <- fluidPage(
    
    # flatly is a nice theme to make the tabs look put together
    
    # Application title
    
    navbarPage("Agriculture and Education in the Midwest", theme = shinytheme("flatly"),
               
               # remember that tab panel is for what shows up in the tab, and title panel is 
               # for what shows up at the top of the page within the tab                  
               
               tabPanel("Welcome",
                        titlePanel("Midwest Agriculture in Brief"),
                        
                        # Create a spot for the plot
                        # Why is the text on top of the image?? :(
                        mainPanel(
                            helpText("To understand the relationship between agriculture and education, let's look at some trends in agriculture over time. Click the tabs below to visualize major changes."),
                            tabsetPanel(id = "tabsMain",
                                        tabPanel("Farms & Acreage",
                                                 
                                                 # Create a fluid row in order to comment next to the gifs
                                                 # column specifications might need to be changed
                                                 
                                                 fluidRow(column(width = 5, textOutput("acre_com")),
                                                          column(width = 7, align = "center",
                                                                 imageOutput("acre_gif")))
                                        ),
                                        tabPanel("Urbanization",
                                                 # Generate a row with a sidebar
                                                 fluidRow(column(width = 5, textOutput("pop_com")),
                                                              column(width = 7, align = "center",
                                                                     imageOutput("pop_gif")))
                                                 ),
                                        tabPanel("Mechanization",
                                                fluidRow(column(width = 5, textOutput("plot2_com")),
                                                              column(width = 7, align = "center",
                                                                     imageOutput("plot2")))
                            )))),
               
               
               tabPanel("Models",
                        
                        # Give the page a title
                        titlePanel("Agriculture and Education, by County"),
                        
                        
                        # Create a spot for the plot
                        mainPanel("",
                                  tabsetPanel(id = "tabsMain",
                                              
                                              tabPanel("Implements",
                                                       # Generate a row with a sidebar
                                                       sidebarLayout(      
                                                           
                                                           # Define the sidebar with one input
                                                           # comprehensive options:
                                                           # c("1870" = "1870", "1880" = "1880", "1890" = "1890", "1900" = "1900","1910" = "1910", "1920" = "1920", "1930" = "1930", "1940" = "1940", "1950" = "1950", "1960" = "1960")
                                                           sidebarPanel(
                                                               selectInput("impYear", "Year:", 
                                                                           choices=c("1870" = "1870", "1890" = "1890", "1910" = "1910", "1920" = "1920", "1930" = "1930", "1940" = "1940"), 
                                                                           multiple = TRUE)),
                                                           helpText("Select a year from the dropdown menu in order to explore the correlation between mechanization and school enrollment over time.")
                                                       ),
                                                       
                                                       # this tells the UI what plot to put 
                                                       
                                                       plotOutput("impedu_plot"),
                                                       p("The plot shows a positive correlation between school enrollment and implement value in 1870, but that correlation disappears in later years."),
                                                       gt_output("cowedu_gt"),
                                                       p("This linear regression model shows that an increase in average implement value has essentially no effect on school enrollment. Curiously, the number of cows per farm is strongly associated with higher school enrollment, as we will explore in the next tab.")
                                                       
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
                                                           helpText("Select a year from the dropdown menu in order to explore the correlation between cattle ownership and school enrollment over time.")
                                                       ),
                                                       
                                                       # this tells the UI what plot to put 
                                                       
                                                       plotOutput("agPlot"),
                                                       p("The plot shows a positive correlation between school enrollment and the average number of cows per farm for the years where data was available. The number of cows might simply be a confounding variable that acts as a stand-in for farm size, but as the table below will show, this does not seem to be the case."),
                                                       gt_output("cows_size_gt"),
                                                       p("This linear regression model shows that a one-cow increase in the number of cows per farm in a given county results in a 0.23% increase in school enrollment. This is significant given that school enrollment hovers around just 25% in most counties for the years studied. Curiously, farm size is not associated with a similarly significant increase in enrollment, and the interaction between cows per farm and farm size actually leads to a decrease in enrollment.")
                                              ),
                                              
                                              tabPanel("Gender",
                                                       
                                                       #mainPanel("",
                                                                 #gt_output("maleedu_gt"))
                                                       
                                                       fluidRow(column(width = 6, plotOutput("maleedu_plot")),
                                                                column(width = 6, #align = "center",
                                                                       gt_output("maleedu_gt"))), 
                                                       fluidRow(column(width = 6, textOutput("maletext1")),
                                                                column(width = 6, #align = "center",
                                                                       textOutput("maletext2")))
                                                       
                                              )
                                  )
                        )),
               
               
               tabPanel("Maps",
                        titlePanel("Midwest Map Explorer"),
                        p("Choose a census year and variable to view maps of the midwest."),
                        helpText("Please note that no enrollment data was available for the year 1880."),
                        sidebarLayout(
                            
                            # App keeps disconnecting from the server
                            # Try doing only one map
                            
                            sidebarPanel(
                                selectInput("mapYear", "Year:", 
                                            choices=c("1870" = "1870", "1880" = "1880", "1890" = "1890"), 
                                            multiple = FALSE),
                            selectInput("mapVariable", "Variable:", 
                                        choices=c("Percentage School Enrollment", "Average Farm Value", "Average Cows per Farm", "Average Implement Value", "Average Farm Size (Acres)"), 
                                        multiple = FALSE)),

                            mainPanel(imageOutput("map"))
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
                        p("The College of William and Mary and the Minnesota Population Center. School Attendance Boundary Information System (SABINS): Version 1.0. Minneapolis, MN: University of Minnesota 2011."),
                        p("Ian Webster. U.S. Inflation Calculator: 1635-2020, Department of Labor Data. Official Data Foundation, https://www.officialdata.org."),
                        p("Todd Kosmerick. “World War I and Agriculture.” NC State University Libraries, 18 Aug. 2017, https://www.lib.ncsu.edu/news/special-collections/world-war-i-and-agriculture.")
                        ),
               
               tabPanel("Codebook",
                        titlePanel("How were variables measured for this project?"),
                        p("Choose a variable to find out! Take special note of the changes in school enrollment and farm value metrics over time."),
                        p("Please note that all dollar values are adjusted for inflation, using 2020 as the base year."),
                        varSelectInput("variables", "Variable:", codebook, multiple = FALSE),
                        helpText("These variable descriptions represent a condensed version of the codebooks provided by IPUMS NHGIS."),
                        tableOutput("codebook")
                        )
    )
)



# use plotly to allow hover text to appear on plots, giving more info about the point

server <- (function(input, output, session) {
    
    
    output$acre_com <- renderText({
        
        "The bar plot on the left show a general upward trend in the number of acres allotted to farming each year. What explains the jump in 1920? When the United States entered World War I in 1917, Europe was facing serious food shortages due to war casualties and farmers leaving their homes to fight. The US played an essential role in sustaining the Allied war effort through increased production— the government called for increases of up to 10% on many commodities.
        The war spurred on the Economic Boom of the 1920s followed by the Great Crash of 1929, causing farmers to purchase and then sell off vast amounts of land. The line plot on the right shows that the number of farms increased until the early 20th century before beginning to decline in most states. Taken together, these plots show that farms in the Midwest have been steadily increasing in size, while the number of people owning farms has been steadily decreasing."
    })
    
    output$acre_gif <- renderImage({
        list(src = "acre_gif.gif")
        
    }, deleteFile = FALSE)
    
    
    output$pop_com <- renderText({
        
        "Along with most regions of the world, the Midwest was experiencing rapid population growth during the years studied. As the plot on the left shows, population in all states except for North and South Dakota were trending upward. At the same time, the plot on the right illustrates that people were increasingly dwelling in urban areas. Although the urban population was almost nonexistent in 1870, it far surpasses the rural population by 1950."
    })
    
    output$pop_gif <- renderImage({
        list(src = "pop_gif.gif")
        
    }, deleteFile = FALSE)
    
    output$plot2_com <- renderText({
        
        "Farming methods were undergoing dramatic change during the period studied.This plot shows the value of farm implements and machinery each year, demonstrating the rapid mechanization of Midwest agriculture."
    })
    
    output$plot2 <- renderImage({
        list(src = "plot2.gif")
        
    }, deleteFile = FALSE)
    
    # Implements plot
    
    output$impedu_plot <- renderPlot({
        
        allData %>%
            filter(YEAR %in% input$impYear) %>%
            mutate(YEAR = as.factor(YEAR)) %>%
            mutate(YEAR = fct_explicit_na(YEAR, na_level = NA)) %>%
            group_by(YEAR) %>%
            select(YEAR, avg_value_implements, pct_enrolled) %>%
            na.omit() %>%
            ggplot(aes(x = avg_value_implements, y = pct_enrolled, color = YEAR)) + 
            geom_point(alpha = 0.1) +
            geom_smooth(method = "lm", se = FALSE) +
            scale_fill_viridis_d(aesthetics = "color") +
            ylim(0, 45) + 
            labs(title = "Relationship Between Farm Implements and Education, by County",
                 subtitle = "Midwestern United States", 
                 x = "Average Value of Implements", 
                 y = "Percent Population Enrolled in School") +
            scale_x_log10(labels = comma) +
            theme_classic()
    })
    
    output$cowedu_gt <- render_gt({
        cowedu_model %>%
            select(term, conf.low, estimate, conf.high) %>%
            gt() %>%
            tab_header(title = "Effect Cows and Implements on Enrollment",
                       subtitle = "With 95% Confidence Interval") %>%
            cols_label(term = "Coefficient Name", conf.low = "Lower Bound", estimate = "Estimated Value", conf.high = "Upper Bound") %>%
            fmt_number(columns = vars(conf.low, estimate, conf.high), decimals = 4)
        
    })
    
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
    
    
    output$cows_size_gt <- render_gt({
        cows_size_model %>%
            select(term, conf.low, estimate, conf.high) %>%
            gt() %>%
            tab_header(title = "Effect Cows and Farm Size on Enrollment",
                       subtitle = "With 95% Confidence Interval") %>%
            cols_label(term = "Coefficient Name", conf.low = "Lower Bound", estimate = "Estimated Value", conf.high = "Upper Bound") %>%
            fmt_number(columns = vars(conf.low, estimate, conf.high), decimals = 4)
        
    })
    
    # maleedu plot and gt
    
    output$maleedu_plot <- renderPlot({
        maleedu_model %>%
            ungroup() %>%
            select(YEAR, pct_male_coef, pct_male_upper, pct_male_lower) %>%
            mutate(YEAR = as.factor(YEAR)) %>%
            group_by(YEAR) %>%
            ggplot(aes(x = YEAR, y = pct_male_coef)) + 
            geom_errorbar(ymin = maleedu_model$pct_male_lower, ymax = maleedu_model$pct_male_upper, color = "#453781FF") + 
            geom_point(color = "#440154FF") +
            theme_classic() +
            ylim(-1.5, 0.5) + 
            labs(x = "Year",
                 y = "Coefficient",
                 title = "Percent Male Coefficients Over Time",
                 subtitle = "Measuring the effect of being male\non school enrollment")
    })
    
    output$maleedu_gt <- render_gt({
        maleedu_model%>%
            ungroup() %>%
            
            # Select the columns needed, as per the instructions
            
            select(-data:-reg_results) %>%
            
            gt() %>%
            
            # Add the desired titles, subtitles, and column labels
            
            tab_header(title = "Effect of Gender and Number of Farms on Enrollment",
                       subtitle = "With 95% Confidence Interval") %>%
            tab_spanner(label = "Farm Coefficients", columns = vars(n_farm_lower, n_farm_coef, n_farm_upper)) %>%
            tab_spanner(label = "Percent Male Coefficients", columns = vars(pct_male_lower, pct_male_coef,pct_male_upper)) %>%
            tab_spanner(label = "Interaction Coefficients", columns = vars(interaction_lower, interaction_coef, interaction_upper)) %>%
            cols_label(YEAR = "Year", n_farm_coef	= "Estimate", n_farm_upper = "Upper", n_farm_lower = "Lower", pct_male_coef = "Estimate", pct_male_upper = "Upper", pct_male_lower = "Lower",	interaction_coef = "Estimate", interaction_upper = "Upper", interaction_lower = "Lower") %>%
            # Round all values to two decimal places
            
            fmt_number(columns = vars(n_farm_lower, n_farm_coef, n_farm_upper, pct_male_lower, pct_male_coef,pct_male_upper, interaction_lower, interaction_coef, interaction_upper), decimals = 4)
        
    })
    
    # Gender text
    output$maletext1 <- renderText({
        
        "This plot shows shows how the effect of being male on school enrollment reversed over time. From 1870 to 1920, having a higher percentage of males in the population associated with a decrease in school enrollment. From 1930 onward, however, that males and enrollment were positively correlated."
    })
    
    output$maletext2 <- renderText({
        
        "This table shows a linear regression model of the number of farms in a given county, the percent of the population that was male, and the interaction of those two as compared to the percent of the population enrolled in school. In contrast to gender, having more farms in a given county was not associated with a significant change in enrollment in any year."
    })
    
    
    # Map output
    
    output$map <- renderImage({
        filename <- mapPngs %>% filter(year == input$mapYear, variable == input$mapVariable) %>%
            slice(1) %>%
            pull(path)
        list(src = filename)
        
    }, deleteFile = FALSE)
    
    # sometimes R flags this code chunk and says there's a bug, but it still works
    # (just press continue in the R console)
    
    output$codebook <- renderTable({
        if (length(input$variables) == 0) return(codebook)
        codebook %>% dplyr::select(!!input$variables)
    }, rownames = TRUE)
    
})



# Run the application 

shinyApp(ui = ui, server = server)
