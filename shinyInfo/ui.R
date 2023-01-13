library(shiny)
library(shinydashboard)
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(leaflet)
library(rgdal)
library(sp)
library(grid)
library(data.table)

dashboardPage(
  dashboardHeader(title = "Crime in Chicago"),
  dashboardSidebar(
    
    sidebarMenu(
      
      #List of menu items
      menuItem("Home", tabName="home",icon = icon("home")),
      menuItem("About The Project", tabName="aboutprj", icon = icon("th")),
      menuItem("Overall Crimes", tabName="intro2",icon = icon("dashboard")),
      #menuItem("Map of Crimes", tabeName="mapPage", icon = icon("dashboard")),
      menuItem("View by Ward", tabName = "dashboardA", icon = icon("dashboard")),
      menuItem("View Throughout Year", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("View by Time of Day", tabName = "dashboardC", icon = icon("dashboard")),
      menuItem("View By Weekday", tabName = "widgets", icon = icon("dashboard")),
      menuItem("About Me", tabName = "conc", icon = icon("th"))
      
    )
  ),
  dashboardBody(
    tabItems(
      
      # Home page- contains title and stock image of city.
      tabItem(tabName = "home",
              fluidRow(
                box(h1("Investigating Crime Rates in Chicago"),width='100%', status="primary"),
                tags$img(src = "pexels-chait-goli-1823681.jpg",height='350',width='500',style = "padding-left: 28px"),
                
              )
        
      ),
      
      # Info tab- contains citation of data, and text explaining the goals of the project.
      tabItem(tabName = "aboutprj",
              fluidRow(
                box(h1("Exploring the statistics of crime in 2021 in Chicago"),width='100%', status="primary"),
                br(),
                br(),
                h4("This application was built to allow the user to interactively explore crime statistics from the City of Chicago for the year 2021. In particuar, we seek to answer the following questions:", style = "padding-left: 12px"),
                br(),
                h4("1. Which types of crime are the most common?",style = "padding-left: 36px"),
                h4("2. Which crimes are likeliest to result in an arrest?", style = "padding-left: 36px"),
                h4("3. Where within the city are most crimes occurring?", style = "padding-left: 36px"),
                h4("4. When do crimes occur most often?", style = "padding-left: 36px"),
                #p("This application allows the user to view and investigate crimes that occurred in the city of Chicago in 2021. Click on a tab to break down the statistics by location, time of week, time of day, and time of year. ", style = "padding-left: 10px"),
                br(),
                br(),
                br(),
                box(h2("Source"),width='100%', status="primary"),
                h4("The raw data may be found at: ", style = "padding-left: 10px"),
                tags$a(href="https://data.cityofchicago.org/Public-Safety/Crimes-2021/dwme-t96c/data", "Crimes of 2021", style = "padding-left: 25px"),
                br(),
                br(),
                h4("The shapefile used for the chloropleth may be found at: ", style = "padding-left: 10px"),
                tags$a(href="https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Wards-2015-2023-/sp34-6z76","Shapefile", style = "padding-left: 25px")
              )

      ),
      
      
      # Bar graphs of total crimes, and percent of arrests by crimes.
      # User can also drill down by crime to see a description of different crimes.
      tabItem(tabName = "intro2",
              fluidRow(
                box(h2("Total Crimes from 2021"),width='100%', status="primary"),
                tags$img(src = 'totalCrimes_redo.png',width='700',style = "padding-left: 28px"),
                br(),
                br(),
                box(h2("Percent of Crimes Resulting in Arrest"),width='100%', status="primary"),
                tags$img(src = 'arrestpct_redo.png',width='700',style = "padding-left: 28px"),
                br(),
                br(),
                box(
                  title = "Drill Down by Crime",
                  selectInput(inputId = "crimeIn",label = "Choose a crime: ",choices = c("All",unique(crimes2$Primary.Type)),selected='THEFT',width='40%'),
                  width='100%',
                  status="primary"
                ),
                box(plotOutput("plot2", width = 700,height=750),width=6)
              )
              ),
      
      tabItem(tabName = "mapPage",
              fluidRow(
                h2("Map of Crimes"),
                tags$img(src = 'totalCrimes_redo.png')
              )
      ),
      
      # View by ward.
      tabItem(tabName = "dashboardA",
              fluidRow(
                box(h2("View Crimes by Ward"),width='100%', status="primary"),
                #box(plotOutput("plot1", height = 250)),
                box(leafletOutput("plotMap",height=650),width=8),
                box(
                  title = "Crime",
                  selectInput(inputId = "crimeCrime",label = "Choose a crime: ",choices = c("All","OTHER OFFENSE", "OFFENSE INVOLVING CHILDREN","THEFT","BATTERY","ARSON","BURGLARY","SEX OFFENSE","ASSAULT","CRIMINAL DAMAGE","DECEPTIVE PRACTICE","CRIMINAL SEXUAL ASSAULT","WEAPONS VIOLATION","ROBBERY","CRIMINAL TRESPASS","MOTOR VEHICLE THEFT","NARCOTICS","STALKING")),
                  width=4
                ),
                #box(leafletOutput("plotMap"),width=10),
                
              )),
      
      
      # Users can select a crime and view how it varies by year.
      tabItem(tabName = "dashboard",
              fluidRow(
                box(h1("Explore how crimes vary throughout the year"),width='100%', status="primary"),
                box(plotOutput("plot3", height = 600),width=10),
                #box(plotOutput("plot1", height = 250)),
                br(),
                box(
                  title = "Crime",
                  selectInput(inputId = "crimeYear",label = "Choose a crime: ",choices = c("All",unique(crimes2$Primary.Type)))
                  ,width=4
                ),
                #box(plotOutput("plot3", height = 250),width=8)
              )
      ),
      
      tabItem(tabName = "dashboardC",
              fluidRow(
                box(h1("Explore how crimes vary throughout times of the the day"),width='100%', status="primary"),
                box(plotOutput("plotX", height = 600),width=10),
                box(
                  title = "Crime",
                  selectInput(inputId = "crimeHour",label = "Choose a crime: ",choices = c("All",unique(crimes2$Primary.Type))),
                  width=4
                ),
                #box(plotOutput("plotX", height = 250),width=8)
              )),
      
      # Users can select a crime and view how it varies by weekday.
      tabItem(tabName = "widgets",
              box(h1("Explore how crimes vary by day of the week"),width='100%', status="primary"),
              box(plotOutput("plot4", height = 400),width=8),
              br(),
              box(
                title = "Crime",
                selectInput(inputId = "crimeWeek",label = "Choose a crime: ",choices = c("All",unique(crimes2$Primary.Type))),
                width=6
                
                
              ),
              #box(plotOutput("plot4", height = 250))
      ),
      
      tabItem(tabName = "conc",
              box(h2("About Me"),width='100%',status="primary"),
              #br(),
              tags$img(src = 'D6AD1433-1B1E-43F3-B8E1-31CAE49E712D.jpeg',width='400',style = "padding-left: 22px"),
              h4(""),
              br(),
              h4("Dillon Edwards is an aspiring data scientist and member of the Winter 2022-23 cohort at NYC Data Science Academy. He currently works in the medical software field."),
              br(),
              h4("The code for this project may be found here:"),
              tags$a(href="https://github.com/DillonOEdwards/RShinyPrj","GitHub", style = "padding-left: 16px")
              
    )
  )
))




