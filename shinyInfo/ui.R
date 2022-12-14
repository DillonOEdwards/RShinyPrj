library(shiny)
library(shinydashboard)
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)


dashboardPage(
  dashboardHeader(title = "Crime in Chicago"),
  dashboardSidebar(
    
    sidebarMenu(
      
      #List of menu items
      menuItem("Home", tabName="home",icon = icon("home")),
      menuItem("About The Project", tabName="aboutprj", icon = icon("th")),
      menuItem("Overall Crimes", tabName="intro2",icon = icon("dashboard")),
      menuItem("View by Ward", tabName = "dashboardA", icon = icon("dashboard")),
      menuItem("View Throughout Year", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("View By Weekday", tabName = "widgets", icon = icon("dashboard")),
      menuItem("Conclusions", tabName = "conc", icon = icon("th"))
      
    )
  ),
  dashboardBody(
    tabItems(
      
      # Home page- contains title and stock image of city.
      tabItem(tabName = "home",
              fluidRow(
                h1("Investigating Crime Rates in Chicago"),
                box(img(src = "pexels-chait-goli-1823681.png", height = 500, width = 500)),
                
              )
        
      ),
      
      # Info tab- contains citation of data, and text explaining the goals of the project.
      tabItem(tabName = "aboutprj",
              fluidRow(
                h1("Exploring the statistics of crime in 2021 in Chicago"),
                br(),
                br(),
                p("This application allows the user to view and investigate crimes that occurred in the city of Chicago in 2021. Click on a tab to break down the statistics by location, time of week, and time of year. "),
                br(),
                h2("Source"),
                p("The raw data may be downloaded at: https://data.cityofchicago.org/Public-Safety/Crimes-2021/dwme-t96c/data")
              )

      ),
      
      
      # Bar graphs of total crimes, and percent of arrests by crimes.
      # User can also drill down by crime to see a description of different crimes.
      tabItem(tabName = "intro2",
              fluidRow(
                h2("Total Crimes from 2021"),
                img(src = 'totalcrimesimg.png"'),
                h2("Percent of Crimes Resulting in Arrest"),
                img(src = 'pctcrimesimg.png"'),
                br(),
                br(),
                box(
                  title = "Drill Down by Crime",
                  selectInput(inputId = "crimeIn",label = "Choose a crime: ",choices = c("All",unique(crimes2$Primary.Type)))
                  
                ),
                box(plotOutput("plot2", height = 300))
              )
              ),
      
      
      # View by ward.
      tabItem(tabName = "dashboardA",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(p("View of the number of crimes of different wards in the city."),
                    p("Note that lack of population data convolutes interpretation of the data."))
              )),
      
      
      # Users can select a crime and view how it varies by year.
      tabItem(tabName = "dashboard",
              fluidRow(
                h1("Explore how crimes vary throughout the year"),
                #box(plotOutput("plot1", height = 250)),
                br(),
                box(
                  title = "Crime",
                  selectInput(inputId = "crimeYear",label = "Choose a crime: ",choices = c("All",unique(crimes2$Primary.Type)))
                ),
                box(plotOutput("plot3", height = 250))
              )
      ),
      
      # Users can select a crime and view how it varies by weekday.
      tabItem(tabName = "widgets",
              h1("Explore how crimes vary by day of the week"),
              box(
                title = "Crime",
                selectInput(inputId = "crimeWeek",label = "Choose a crime: ",choices = c("All",unique(crimes2$Primary.Type)))
                
                
              ),
              box(plotOutput("plot4", height = 250))
      ),
      
      tabItem(tabName = "conc",
              h2("Conclusions"),
              br(),
              h4("Different crimes have different peaks throughout the year, with most crimes increasing throughout the year."),
              h4("Some crimes how higher prevalence on weekend days, with violent crimes such as battery, and alcohol-related crimes, being more likely to occur on Saturdays and Sundays."),
              h4("Different crimes have differing percentages of arrests, with public indecency and liquor law violations being the most likely to lead to an arrest."),
              h4("Different wards have varying levels of crime, though lack of population data makes the data hard to interpret."))
    )
  )
)




