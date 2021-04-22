

# Project 3 CS 424 Spring 2020 UIC - Rabia Ibtasar
#To deploy 
#library(rsconnect)
#rsconnect::deployApp('path/to/your/app')
#libraries to include



library(shiny)
library(shinydashboard,warn.conflicts = FALSE)
library(ggplot2)
# library(RColorBrewer)
# library(dplyr)
# library(DT)
# library(gridExtra)
# library(leaflet)
# library(sf)
# library(leaflet.extras)
# library(tidyverse)
# library(scales)
# library(rgdal)
library(sp)
library(tigris)
options(tigris_use_cache = TRUE)
library(mapview)


#set the correct wd
#setwd('C:/Users/ribta/OneDrive/Desktop/UIC/Courses/Spring 2021/CS424/project3/cs424p3')

set.seed(122)


#read the excel data into d
d1<-read.csv('Energy_Usage_2010.csv')
#d1[18:19]<-NULL
#d1[31:61]<-NULL

#data cleanup to start analysis
#change datatypes into correct types for data analysis

d1$CENSUS.BLOCK<-as.character(d1$CENSUS.BLOCK)
#take care of all the missing values in the data
#change the NAs in the columns to 0s
d1[2][is.na(d1[2])] <- "" 
d1[, 5:40][is.na(d1[, 5:40])] <- 0
colnames(d1)[1] <-"COMMUNITY.AREA.NAME"

#colSums(is.na(d1)) :to check the sume of NAs
#glimpse to check the dataset

#load shapefiles for use 
chicagoblks <- blocks(state = "IL", county = "Cook County")
#subset to get only the blocks for Chicago that are in our data set

chicagoblks <- subset(chicagoblks, chicagoblks$GEOID10 %in% d1$CENSUS.BLOCK)



#lists to use for UI inputs
selections1<- c("Electricity","Gas","Building Age","Building Type","Building Height",
                                 "Total Population")

selections2<-c("TOTAL","JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY",
                "AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER" )

selections3<-c("All","Residential","Commercial","Industrial")
selections4<-unique(d1$COMMUNITY.AREA.NAME)
selections5<-c("oldest buildings","newest buildings","tallest buildings",
               "Blocks with highest electricity","Blocks with most has used",
               "Most population","Most occupied percentage","highest percentage of renters",
               "Greatest Housesize","Largest number of units")
              
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
                column(width=3, offset=1,
                selectInput("input1", "Select option:", selections1, selected=1)
                ),
                column(width = 3,
                selectInput("input2", "Select the Month:", selections2, selected=1)
                ),
                column(width = 3,
                selectInput("input3", "Select Building Type:", selections3, selected=1)
                ),
                ),
              
              #second row
              fluidRow(
                column(width=10, offset=1,
                    tabBox(title = "Near West Side Data", id="tabset1", selected = "Map", width = NULL,               
                      tabPanel("Map", mapviewOutput("map1", width = "100%", height = 400)), 
                      tabPanel("Graph"),
                      tabPanel("Table")
                      )#end of tabbox
                        
                )
              )
              
      ),
      
      #second Tab Item is for state comparison
      tabItem(tabName = "comparison",
              fluidRow(class = "myRow1", 
                       column(width=6, offset=0, 
                              box(
                                title = "Map1", status = "primary", width = 12,
                                collapsible = TRUE, collapsed = TRUE,
                                selectInput("input4", "Select Community Area:", selections4, selected=33),
                                selectInput("input5", "Select option:", selections1, selected=1),
                                selectInput("input6", "Select the Month:", selections2, selected=1),
                                selectInput("input7", "Select Building Type:", selections3, selected=1)
                                            )
                              ),
                       column(width=6, offset=0, 
                              box(
                                title = "Map 2", status = "primary", width = 12,
                                collapsible = TRUE, collapsed = TRUE, 
                                selectInput("input8", "Select Community Area:", selections4, selected=38),
                                selectInput("input9", "Select option:", selections1, selected=1),
                                selectInput("input10", "Select the Month:", selections2, selected=1),
                                selectInput("input11", "Select Building Type:", selections3, selected=1),
                                checkboxInput("check1", "Select for city of Chicago", value = FALSE, width = NULL),
                                selectInput("input12", "Select Census Tract option to show:", selections5, selected=NULL)
                                )
                             ),
              ),   
    
              fluidRow(),
              fluidRow(class = "myRow2", 
                       column(6,
                              fluidRow(
                                tabBox(title = "Map 1", id="tabset2", selected = "Map", width = NULL,
                                       tabPanel("Map", mapviewOutput("map2", width = "100%", height = 400)), 
                                       tabPanel("Graph"),
                                       tabPanel("Table")
                                )#end of tabbox
                                ),
                              ),
                              column(6,
                              fluidRow(
                                tabBox(title = "Map 2", id="tabset3", selected = "Map", width = NULL,
                                      
                                       tabPanel("Map", mapviewOutput("map3", width = "100%", height = 400)), 
                                       tabPanel("Graph"),
                                       tabPanel("Table")
                                )#end of tabbox
                                ),
                              ),
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
                    dashboardHeader(title = "CS 424 Project 3 Comparing Energy in Chicago"), sidebar, body
)

#========================
#START OF SERVER FUNCTION
#========================

server <- function(input, output,session) {
  
  
  #NWS Tab reactives
  #setup for the first drop down 
  
  #loopdata<-subset(d1, d1$COMMUNITY.AREA.NAME=="Loop")
  
  #subset data for Near West Side
  nws<-subset(d1, d1$COMMUNITY.AREA.NAME=="Near West Side")
  #Merge spatial data with NWS data frame
  #merged_d1<-geo_join (chicagoblks,nws,'GEOID10','CENSUS.BLOCK',how="inner")
  merged_nws<-geo_join (chicagoblks,nws,'GEOID10','CENSUS.BLOCK',how="inner")
  
  option1Reactive <- reactive({
    
    f<-option2Reactive()
    
    if(input$input1=="Electricity")
    {
     
      if(input$input3=="All"){
        
      m <- mapview(merged_nws, zcol = f) 
      m
      }
      else{
      m<-filter(merged_nws,BUILDING.TYPE==input$input3) %>% mapview(zcol=f)
      m
      }
    }
    else if(input$input1=="Gas")
    {
      if(input$input3=="All"){
      m <- mapview(merged_nws, zcol = f)
      m
      }
      else{
        m<-filter(merged_nws,BUILDING.TYPE==input$input3) %>%mapview(zcol=f)
        m
        
      }
    }
    else if(input$input1=="Building Type")
    {
      if(input$input3=="All"){
        m <- mapview(merged_nws, zcol = "BUILDING.TYPE")
        m
      }
      else{
        m<-filter(merged_nws,BUILDING.TYPE==input$input3) %>%mapview(zcol="BUILDING.TYPE")
        m
        
      }

    }
    else if(input$input1=="Building Height")
    {
      
      if(input$input3=="All"){
        m <- mapview(merged_nws, zcol = "AVERAGE.STORIES")
        m
      }
      else{
        m<-filter(merged_nws,BUILDING.TYPE==input$input3) %>%mapview(zcol="AVERAGE.STORIES")
        m
        
      }

    }
    else if(input$input1=="Building Age")
    {
      if(input$input3=="All"){
        m <- mapview(merged_nws, zcol = "AVERAGE.BUILDING.AGE")
        m
      }
      else{
        m<-filter(merged_nws,BUILDING.TYPE==input$input3) %>%mapview(zcol="AVERAGE.BUILDING.AGE")
        m
        
      }
    
    }
    
    else
    {
      if(input$input3=="All"){
        m <- mapview(merged_nws, zcol = "TOTAL.POPULATION")
        m
      }
      else{
        m<-filter(merged_nws,BUILDING.TYPE==input$input3) %>%mapview(zcol="TOTAL.POPULATION")
        m
        
      }
    }

  })
  

  
  # 
  # #reactive for third dropdown
  # option3Reactive<-reactive ({
  #   if(input$input3=="Residential")
  #   {
  #    m<-subset(merged_nws,merged_nws$BUILDING.TYPE=='Residential')
  #    m
  #   }
  #   else if(input$input3=="Commercial")
  #   {
  #     m <- subset(merged_nws,merged_nws$BUILDING.TYPE=='Commercial')
  #     m
  #   }
  #   else if(input$input3=="Industrial")
  #   {
  #     m <- subset(merged_nws,merged_nws$BUILDING.TYPE=='Industrial')
  #     m
  #   }
  #   else
  #   {
  #     m<-merged_nws
  #     m
  #   }
  # })
  option2Reactive<-reactive({

    if(input$input1=="Electricity"){
      if(input$input2=="TOTAL"){
        t=paste0(input$input2,".KWH")
        t
      }
      else{
        t=paste0("KWH.",input$input2,".2010")
        t
      }
    }
    else if(input$input1=="Gas")
    {
      if(input$input2=="TOTAL"){
        t=paste0(input$input2,".THERMS")
        t
      }
      else
      {
        t=paste0("THERM.",input$input2,".2010")
        t
      }
    }
    else{
      t="TOTAL.KWH"
      t

    }
  })
  
  #########################################
  #Reactives for TAB2
  #########################################
  
  
  option4Reactive<-reactive({
    d<-subset(d1,d1$COMMUNITY.AREA.NAME==input$input4)
    d<-geo_join (chicagoblks,d,'GEOID10','CENSUS.BLOCK',how="inner")
    d
  })
  
  option5Reactive<-reactive({
    
    if(input$input5=="Electricity"){
      if(input$input6=="TOTAL"){
        t=paste0(input$input6,".KWH")
        t
      }
      else{
        t=paste0("KWH.",input$input6,".2010")
        t
      }
    }
    else if(input$input5=="Gas")
    {
      if(input$input6=="TOTAL"){
        t=paste0(input$input6,".THERMS")
        t
      }
      else
      {
        t=paste0("THERM.",input$input6,".2010")
        t
      }
    }
    else{
      t="TOTAL.KWH"
      t
      
    }
  })
  
  optionmap2Reactive <- reactive({
    
    f<-option5Reactive() #get the correct month/total column values to display
    s<-option4Reactive() #get the subsetted data for community area
    
    if(input$input5=="Electricity")
    {
      
      if(input$input7=="All"){
        
        m <- mapview(s, zcol = f) 
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input7) %>% mapview(zcol=f)
        m
      }
    }
    else if(input$input5=="Gas")
    {
      if(input$input7=="All"){
        m <- mapview(s, zcol = f)
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input7) %>%mapview(zcol=f)
        m
        
      }
    }
    else if(input$input5=="Building Type")
    {
      if(input$input7=="All"){
        m <- mapview(s, zcol = "BUILDING.TYPE")
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input7) %>%mapview(zcol="BUILDING.TYPE")
        m
        
      }
      
    }
    else if(input$input5=="Building Height")
    {
      
      if(input$input7=="All"){
        m <- mapview(s, zcol = "AVERAGE.STORIES")
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input7) %>%mapview(zcol="AVERAGE.STORIES")
        m
        
      }
      
    }
    else if(input$input5=="Building Age")
    {
      if(input$input7=="All"){
        m <- mapview(s, zcol = "AVERAGE.BUILDING.AGE")
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input7) %>%mapview(zcol="AVERAGE.BUILDING.AGE")
        m
        
      }
      
    }
    
    else
    {
      if(input$input7=="All"){
        m <- mapview(s, zcol = "TOTAL.POPULATION")
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input7) %>%mapview(zcol="TOTAL.POPULATION")
        m
        
      }
    }
    
  })
  
  ###### Map3 Reactives
  
  option8Reactive<-reactive({
    d<-subset(d1,d1$COMMUNITY.AREA.NAME==input$input8)
    d<-geo_join (chicagoblks,d,'GEOID10','CENSUS.BLOCK',how="inner")
    d
  })
  
  option9Reactive<-reactive({
    
    if(input$input9=="Electricity"){
      if(input$input10=="TOTAL"){
        t=paste0(input$input10,".KWH")
        t
      }
      else{
        t=paste0("KWH.",input$input10,".2010")
        t
      }
    }
    else if(input$input9=="Gas")
    {
      if(input$input10=="TOTAL"){
        t=paste0(input$input10,".THERMS")
        t
      }
      else
      {
        t=paste0("THERM.",input$input10,".2010")
        t
      }
    }
    else{
      t="TOTAL.KWH"
      t
      
    }
  })
  
  optionmap3Reactive <- reactive({
    
    f<-option9Reactive() #get the correct month/total column values to display
    s<-option8Reactive() #get the subsetted data for community area
    
    if(input$input9=="Electricity")
    {
      
      if(input$input11=="All"){
        
        m <- mapview(s, zcol = f) 
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input11) %>% mapview(zcol=f)
        m
      }
    }
    else if(input$input9=="Gas")
    {
      if(input$input11=="All"){
        m <- mapview(s, zcol = f)
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input11) %>%mapview(zcol=f)
        m
        
      }
    }
    else if(input$input9=="Building Type")
    {
      if(input$input11=="All"){
        m <- mapview(s, zcol = "BUILDING.TYPE")
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input11) %>%mapview(zcol="BUILDING.TYPE")
        m
        
      }
      
    }
    else if(input$input9=="Building Height")
    {
      
      if(input$input11=="All"){
        m <- mapview(s, zcol = "AVERAGE.STORIES")
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input11) %>%mapview(zcol="AVERAGE.STORIES")
        m
        
      }
      
    }
    else if(input$input9=="Building Age")
    {
      if(input$input11=="All"){
        m <- mapview(s, zcol = "AVERAGE.BUILDING.AGE")
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input11) %>%mapview(zcol="AVERAGE.BUILDING.AGE")
        m
        
      }
      
    }
    
    else
    {
      if(input$input9=="All"){
        m <- mapview(s, zcol = "TOTAL.POPULATION")
        m
      }
      else{
        m<-filter(s,BUILDING.TYPE==input$input11) %>%mapview(zcol="TOTAL.POPULATION")
        m
        
      }
    }
    
  })
  
  #Census Tract reactive setup
  option12Reactive<-reactive({
    
  ch<-geo_join (chicagoblks,d1,'GEOID10','CENSUS.BLOCK',how="inner")
  
  if(input$input12=="Tallest Buildings"){
    
    #subset data to get the 10% blocks with the tallest stories
    #first group by tracts
    #then arrange by height
    #then grab the first 10% of the rows and send that dataset
    
    chi<-mapview(chi, zcol = "AVERAGE.STORIES")
    chi
  }
  else
  {
    chi<-mapview(s, zcol = "TOTAL.POPULATION")
    chi
  }
  
  })
  
 
  #########################################################################
  #output for TAB1 
  output$map1 <- renderLeaflet({
  option1Reactive()@map
  })
  
  #outputs for TAB2
  output$map2 <- renderLeaflet({
    optionmap2Reactive()@map
  })
  
  
  output$map3 <- renderLeaflet({
   
    optionmap3Reactive()@map
 
  })
}

shinyApp(ui = ui, server = server)
