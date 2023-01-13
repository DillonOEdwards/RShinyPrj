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



function(input, output, session) {

  output$plot1 <- renderPlot({
    hist(crimes2$Ward, breaks = 51, 
         main = "Crimes in Each Ward", 
         xlab = "Ward", ylab = "Number of Crimes",col='#69b3a2')
  })
  
  mapInput <- reactive({
    if (input$crimeCrime == 'All') {
      dummy=crimes2full
    } else {dummy = filter(crimes2full, Primary.Type == input$crimeCrime)}
    
    crime_dat = as.data.table(dummy)
    
    coordinates(crime_dat) = c("Longitude","Latitude")
    crs.geol = CRS("+proj=longlat")
    proj4string(crime_dat) = crs.geol
    chi_agg = aggregate(x=crime_dat,by=chicago,FUN=length)
    qpal = colorBin("Reds", chi_agg$Location, bins=4)
    
    leaflet(chi_agg) %>% addPolygons(stroke = TRUE, opacity = 1, fillOpacity = 0.5,
                                     smoothFactor = 0.5, color = "black",
                                     fillColor = ~qpal(Location),
                                     weight = 1) %>% addLegend(values=~location,pal=qpal,title="Crimes")
    
    
    
  })
  
  
  output$plotMap <- renderLeaflet({
    mapInput()
  })
  
  
  chartplot2 <- reactive({
    #req(input$crimeIn)
    if (input$crimeIn == 'All') {
      dummy=crimes2
    } else {dummy = filter(crimes2, Primary.Type == input$crimeIn)}

    ctdummy = dummy %>% count(dummy$Description)
    #df <- data[order(data$num,decreasing = TRUE),]
    ctdummy <- ctdummy[order(ctdummy$n),]
    par(mar = c(12, 30, 2, 1) + 0.1)
    barplot(height = ctdummy$n, names = ctdummy$`dummy$Description`, col='#69b3a2', xlab="Count",horiz=T , las=1)
    
  }
  )
  
  output$plot2 <- renderPlot({
    chartplot2()
  })
  
  
  chartplot3 <- reactive({
    #req(input$crimeYear)
    if (input$crimeYear == 'All') {
      dummy = crimes2
    } else {dummy = filter(crimes2, Primary.Type == input$crimeYear)}
    ctdummy = dummy %>% count(dummy$month2)
    ctdummy = ctdummy %>% arrange(factor(`dummy$month2`, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct','Nov','Dec','zzz')))
    barplot(height=ctdummy$n, names=ctdummy$`dummy$month2`, xlab = "Month",col='#69b3a2',las=2)
    
  }
  )
  
  output$plot3 <- renderPlot({
    chartplot3()
  })
  
  
  chartplotX <- reactive({
    #req(input$crimeYear)
    if (input$crimeHour == 'All') {
      dummy = crimes2
    } else {dummy = filter(crimes2, Primary.Type == input$crimeHour)}
    ctdummy = dummy %>% count(dummy$hourZ)
    barplot(height = ctdummy$n, names=ctdummy$`dummy$hourZ`, col='#69b3a2',las=1,xlab = "Hour")
    
  }
  )
  
  output$plotX <- renderPlot({
    chartplotX()
  })
  
  chartplot4 <- reactive({
    #req(input$crimeWeek)
    if (input$crimeWeek == 'All') {
      dummy = crimes2
    } else {dummy = filter(crimes2, Primary.Type == input$crimeWeek)}
    ctdummy = dummy %>% count(dummy$weekday)
    barplot(height = ctdummy$n, names=ctdummy$`dummy$weekday`,
            col='#69b3a2', horiz=T , las=1)
    
  }
  )  
  
  output$plot4 <- renderPlot({
    chartplot4()
  })
  
  

  
  
  
  
  
  
  output$cityPic <- renderImage({
    list(src = 'pexels-chait-goli-1823681.jpg',
         alt = "Alternate text")
  })
  
}



