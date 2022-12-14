library(shiny)
library(shinydashboard)
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)


function(input, output, session) {

  output$plot1 <- renderPlot({
    hist(crimes2$Ward, breaks = 51, 
         main = "Crimes in Each Ward", 
         xlab = "Ward", ylab = "Number of Crimes",col='#69b3a2')
  })
  
  
  chartplot2 <- reactive({
    req(input$crimeIn)
    dummy = filter(crimes2, Primary.Type == input$crimeIn)
    ctdummy = dummy %>% count(dummy$Description)
    #df <- data[order(data$num,decreasing = TRUE),]
    ctdummy <- ctdummy[order(ctdummy$n),]
    barplot(height = ctdummy$n, names = ctdummy$`dummy$Description`, col='#69b3a2', horiz=T , las=1)
    
  }
  )
  
  output$plot2 <- renderPlot({
    chartplot2()
  })
  
  
  chartplot3 <- reactive({
    req(input$crimeYear)
    dummy = filter(crimes2, Primary.Type == input$crimeYear)
    ctdummy = dummy %>% count(dummy$Day)
    barplot(ctdummy$n)
    
  }
  )
  
  output$plot3 <- renderPlot({
    chartplot3()
  })
  
  
  chartplot4 <- reactive({
    req(input$crimeWeek)
    dummy = filter(crimes2, Primary.Type == input$crimeWeek)
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



