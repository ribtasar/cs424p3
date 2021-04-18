

# Project 3 CS 424 Spring 2020 UIC - Rabia Ibtasar
#To deploy 
#library(rsconnect)
#rsconnect::deployApp('path/to/your/app')
#libraries to include


library(usmap)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(DT)
library(gridExtra)
library(leaflet)
library(sf)
library(leaflet.extras)
library(tidyverse)
library(readxl)
library(scales)
library(rgdal)
library(usdata)
library(sp)
library(tigris)
library(mapview)

#set the correct wd
#setwd('C:/Users/ribta/OneDrive/Desktop/UIC/Courses/Spring 2021/CS424/project3/cs424p3')

set.seed(122)


#read the excel data into d
d1 <-read_csv('Energy_Usage_2010.csv')
d2<- read_csv('Energy_Usage_2010.csv')

#load shapefile for IL and Cook County for Chicago
chicago <- blocks(state = "IL", county = "Cook County")

#change datatypes into correct types for data analysis
d1$`GAS ACCOUNTS`  <-  as.numeric(d1$`GAS ACCOUNTS`)
d1$`ELECTRICITY ACCOUNTS`<-  as.numeric(d1$`ELECTRICITY ACCOUNTS`)

#change the NAs in the character columns to blanks
d1[1][is.na(d1[1])] <- ""   
d1[3:4][is.na(d1[3:4])] <- ""

#colSums(is.na(d1)) :to check the sume of NAs
#rename column name
names(d1)[2] <- "GEOID10"
d1$GEOID10<-   as.character(d1$GEOID10) #make sure they are the same typ

#===========
#start of UI
#===========

#create the sidebar menu for dashboard

sidebar <-dashboardSidebar (disable = FALSE, collapsed = FALSE,
                            
                            sidebarMenu(
                              menuItem("Near West Side", tabName = "westSide", icon =NULL),
                              menuItem("Community Comparison", tabName = "comparison", icon =icon("dashboard")),
                              menuItem("City of Chicago", tabName = "chicago", icon =icon("globe")),
                              menuItem("About", tabName = "about", icon = NULL)
                            )
)

body<-dashboardBody(
tabItems(
  tabItem(tabName = "westSide",
          #first row
          fluidRow(
            column(width=10, offset=1,
                   box(
                     title = "Select Energy Sources", status = "primary", width = 12,
                     collapsible = TRUE, collapsed = TRUE,
                     h4("Near West Side Community Area"),
                     ),
                   )
            ),
          
          #second row
          fluidRow(
            column(width=10, offset=1,
                   box(
                     title = "Near West Side Map", status = "primary", width = 12,
                     collapsible = TRUE
                   )
            )
          )
          
  ),
  
  #second Tab Item is for state comparison
  tabItem(tabName = "comparison",
          fluidRow(class = "myRow1", 
                   column(width=6, offset=0, 
                          box(
                            title = "Map 1", status = "primary", width = 12,
                            collapsible = TRUE, collapsed = TRUE,
                            )
                  ),
        
                   column(width=6, offset=0, 
                          box(
                            title = "Map 2", status = "primary", width = 12,
                            collapsible = TRUE, collapsed = TRUE, 
                            )
                          
                   )
                   
          ),
          # ),#end of myRow1
          fluidRow(),
          fluidRow(class = "myRow2", 
                   column(6,
                          fluidRow(
                            column(4, 
                                   
                                   div(style = "height:10.5px")
                                  
                            ),
                            
                            column(4,
                                   div(style = "height:10.5px")
                                  
                            )
                          ),
                          
                          
                          fluidRow(
                            box(
                              title = "Map 1", solidHeader = TRUE, status = "primary", width = 12,
                              collapsible = TRUE,
                              h5("Locations of plants and energy production")
                            )
                          ),
                   )
             )
          
  ), #end of second tab Item
  
  #
  #ABOUT TAB
  #
  
  tabItem(tabName = "about",
          h2("About"),
          mainPanel(
            h4("Thanks for visiting the App."),
            h4("Data Source:The original data is available from  
            https://www.kaggle.com/chicago/chicago-energy-usage-2010
              and also available at 
              https://data.cityofchicago.org/Environment-Sustainable-Development/Energy-Usage-2010/8yq3-m6wp"),
            
            h5("App developer: Rabia Ibtasar"),
            
            h5("What is this application for?"),
            h5(" 
                  This interactive application has been designed to allow visitors to
                  ")
            
          )
    )
)
)
  
# Create the shiny dashboard
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "CS 424 Project 2 Comparing Energy sources in the US"), sidebar, body
)

#========================
#START OF SERVER FUNCTION
#========================

server <- function(input, output,session) {
  
}

shinyApp(ui = ui, server = server)
