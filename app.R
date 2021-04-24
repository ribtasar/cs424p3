

# Project 3 CS 424 Spring 2021 UIC - Rabia Ibtasar
#To deploy 
#library(rsconnect)
#rsconnect::deployApp('path/to/your/app')
#libraries to include



library(shiny)
library(shinydashboard,warn.conflicts = FALSE)
library(ggplot2)
# library(RColorBrewer)
# library(dplyr)
 library(DT)
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

#data cleanup 
#change data types into correct types for data analysis

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
chicagoblks[,7:15]<-NULL #remove columns we don't need to make file smaller

#d2 dataset is for City of Chicago
d2<-d1
d2<-geo_join(chicagoblks,d2,'GEOID10','CENSUS.BLOCK',how="inner")
d2[,1:2]<-NULL
d2[,4:6]<-NULL
d2[,7:18]<-NULL
d2[,8:19]<-NULL
d2$geometry<-NULL

#load spatial data information for Tracts in Chicago
chitracts<-tracts("IL","Cook County")
chitracts[,1:2]<-NULL
chitracts[,3:10]<-NULL

#merge dataframe d2 for tracts data
d2<-geo_join(chitracts,d2,'TRACTCE','TRACTCE10',how="inner")
d2<-d2 %>% group_by(TRACTCE)

#subset data for Near West Side
nws<-subset(d1, d1$COMMUNITY.AREA.NAME=="Near West Side")
#Merge spatial data with NWS data frame
#merged_d1<-geo_join (chicagoblks,nws,'GEOID10','CENSUS.BLOCK',how="inner")

merged_nws<-geo_join (chicagoblks,nws,'GEOID10','CENSUS.BLOCK',how="inner")

#lists to use for UI inputs
selections1<- c("Electricity","Gas","Building Age","Building Type","Building Height",
                                 "Total Population")

selections2<-c("TOTAL","JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY",
                "AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER" )

selections3<-c("All","Residential","Commercial","Industrial")
selections4<-unique(d1$COMMUNITY.AREA.NAME)
selections4<-c(selections4, "Chicago")
selections5<-c("Oldest buildings","Newest buildings","Tallest buildings",
               "Blocks with highest electricity","Blocks with most gas used",
               "Most population","Most occupied percentage","Highest percentage of renters",
               "Greatest Housesize","Largest number of units")

months1<-c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec")
              
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
                      tabPanel("Graph", plotOutput("graph1", height = 300)),
                      tabPanel("Table",dataTableOutput("table1", height = 200))
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
                                selectInput("input4", "Select Community Area:", selections4, selected="Near West Side"),
                                selectInput("input5", "Select option:", selections1, selected=1),
                                selectInput("input6", "Select the Month:", selections2, selected=1),
                                selectInput("input7", "Select Building Type:", selections3, selected=1)
                                            )
                              ),
                       column(width=6, offset=0, 
                              box(
                                title = "Map 2", status = "primary", width = 12,
                                collapsible = TRUE, collapsed = TRUE, 
                                selectInput("input8", "Select Community Area:", selections4, selected="Loop"),
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
                      This interactive application has been designed to allow visitors to understand the electricity and gas energy use 
                      in the city of Chicago for 2010 at the block level in different communities and also at the Census tract level 
                      for the entire city. Anyone interested in exploring how electricity and gas usage differs by different
                      variables will find this application useful.
                      ")
                
              )
        )
    )
)
  
# Create the shiny dashboard
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "CS 424 Project 3 Comparing Electricity and Gas usage in Chicago"), sidebar, body
)

#========================
#START OF SERVER FUNCTION
#========================

server <- function(input, output,session) {
  
  
  #NWS Tab reactives
  #setup for the first drop down 
  
  graphReactive <- reactive({
    if(input$input1=="Electricity")
    {
      choice="elec"
      choice      
    }
    else if(input$input1=="Gas")
    {
      
      choice="gas"
      choice
    }
    else{
      choice="elec"
      choice
    }
    
  })

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
  
  # increase the default font size and create theme for blank backgrounds for plots and legends at the bottom
  theme_set(theme_grey(base_size = 11)) 
  theme_bare<- theme(panel.background=element_blank(),
                     legend.direction = "horizontal", legend.position = "bottom")
  
  # output$graph1 <- renderPlot({
  #   temp1<-option2Reactive()
  #   
  #   # ggplot(data=merged_nws)+ geom_line(mapping=aes(x=,y=temp1)+
  #   #   ggtitle("Total Amount of each energy source per year from 1990-2019")+ylab("Energy Amount (MWh)")+ theme_bare+
  #   #   scale_fill_manual(values=useColors)
  # })
  # 
  # output$table1 <- DT::renderDataTable({
  #   #DT::datatable(displayAmounts, rownames=FALSE)
  # })
  
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
    
    if(input$input8=="Chicago")
    {
      d<-d2
      
      if(input$input12=="Oldest buildings")
      {
        d<- d %>% select(TRACTCE,AVERAGE.BUILDING.AGE,geometry) %>% slice_max(AVERAGE.BUILDING.AGE,prop=0.1) %>% mapview(zcol="AVERAGE.BUILDING.AGE")
        d
      }
      else if(input$input12=="Newest buildings")
      {
        d<- d %>% select(TRACTCE,AVERAGE.BUILDING.AGE,geometry) %>% slice_min(AVERAGE.BUILDING.AGE,prop=0.1) %>% mapview(zcol="AVERAGE.BUILDING.AGE")
        d
      }
      else if(input$input12=="Tallest buildings")
      {
        d<- d %>% select(TRACTCE,AVERAGE.STORIES,geometry) %>% slice_max(AVERAGE.STORIES.AGE,prop=0.1) %>% mapview(zcol="AVERAGE.STORIES")
        d
      }
      else if(input$input12=="Blocks with highest electricity")
      {
        d<- d %>% select(TRACTCE,TOTAL.KWH,geometry) %>% slice_max(TOTAL.KWH.AGE,prop=0.1) %>% mapview(zcol="TOTAL.KWH")
        d 
        
      }
      else if(input$input12=="Blocks with most gas used")
      {
        d<- d %>% select(TRACTCE,TOTAL.THERMS,geometry) %>% slice_max(TOTAL.THERMS,prop=0.1) %>% mapview(zcol="TOTAL.THERMS")
        d
      }
      else if(input$input12=="Highest population")
      {
        d<- d %>% select(TRACTCE,TOTAL.POPULATION,geometry) %>% slice_max(TOTAL.POPULATION,prop=0.1) %>% mapview(zcol="TOTAL.POPULATION")
        d
      }
      else if(input$input12=="Most occupied percentage")
      {
        d<- d %>% select(TRACTCE,OCCUPIED.UNITS.PERCENTAGE,geometry) %>% slice_max(OCCUPIED.UNITS.PERCENTAGE,prop=0.1) %>% mapview(zcol="OCCUPIED.UNITS.PERCENTAGE")
        d
      }
      else if(input$input12=="Highest percentage of renters")
      {
        d<- d %>% select(TRACTCE,RENTER.OCCUPIED.HOUSING.PERCENTAGE,geometry) %>% slice_max(RENTER.OCCUPIED.HOUSING.PERCENTAGE,prop=0.1) %>% 
          mapview(zcol="RENTER.OCCUPIED.HOUSING.PERCENTAGE")
        d
      }
      else if(input$input12=="Greatest Housesize")
      {
        d<- d %>% select(TRACTCE,AVERAGE.HOUSESIZE,geometry) %>% slice_max(AVERAGE.HOUSESIZE,prop=0.1) %>% 
          mapview(zcol="AVERAGE.HOUSESIZE")
        d
      }
      else if(input$input12=="Largest number of units")
      {
        d<- d %>% select(TRACTCE,TOTAL.UNITS,geometry) %>% slice_max(TOTAL.UNITS,prop=0.1) %>% 
          mapview(zcol="TOTAL.UNITS")
        d
      }
    }
    else{
    d<-subset(d1,d1$COMMUNITY.AREA.NAME==input$input8)
    d<-geo_join (chicagoblks,d,'GEOID10','CENSUS.BLOCK',how="inner")
    d
    }
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
    s<-option8Reactive() #get the subsetted data for community area or mapview object if Chicago is selected
    
    if(input$input8=="Chicago")
      
    {
      s
    }
    
    else
    {
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
    }#end of chicago else
  })
  
  #Census Tract reactive setup
  option12Reactive<-reactive({
  
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
  
  
  output$graph1<-renderPlot({
   
    elec<-merged_nws[,12:23]
    gas<-merged_nws[,25:36]
    elec$geometry<-NULL
    gas$geometry<-NULL
    elec<-colSums(elec)
    gas<-colSums(gas)
    #create dataframe for graph and tables
    data1<-data.frame(months1,elec,gas)
    #create datatable to output
    
    #call a reactive that just looks at first input to see if its elec or gas
    #and then pass that onto to geom_point
    
    ggplot(data=data1,aes(x=months1,y=graphReactive()))+ 
      geom_point(aes(months1,elec))+
      scale_y_continuous(name=graphReactive())+
      theme_bare
  })
  
  
  
  output$table1<-DT::renderDataTable({
    #new1<-subset(percentsData, percentsData$SOURCE==input$sourceA & percentsData$STATE==input$stateA & percentsData$YEAR==input$yearA )
    #new1<-select(new1,YEAR,STATE,GENERATION,PERCENTS)
    elec<-merged_nws[,12:23]
    gas<-merged_nws[,25:36]
    elec$geometry<-NULL
    gas$geometry<-NULL
    elec<-colSums(elec)
    gas<-colSums(gas)
    #create dataframe for graph and tables
    data1<-data.frame(months1,elec,gas)
    
    DT::datatable(data1, rownames=FALSE)
  })
  
  
  #################
  #outputs for TAB2
  output$map2 <- renderLeaflet({
    optionmap2Reactive()@map
  })
  
  
  output$map3 <- renderLeaflet({
   
    optionmap3Reactive()@map
 
  })
}

shinyApp(ui = ui, server = server)
