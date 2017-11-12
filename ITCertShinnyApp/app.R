#
# This is a Shinny App to manipulate ITCO Data, het input and 
# display output interactively

library(shiny)
library(shinydashboard)
library(rCharts)
library(dygraphs)
library(httr)
library(twitteR)
library(tm)
require(xts)
require(stringr)
library(rvest)
library(dplyr)
library(ggplot2)

setwd("P:/NITA/New/Personal/Training/Data Science/John Hopkins/Course 9 - Developing Data Products/Week 4/Peer-Graded")

# Define UI for the application 
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Monthly Registration of IT Service Providers for Certification"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("barwidth",
                     "Width of Bars:",
                     min = 0.5,
                     max = 1,
                     value = 0.75),
        selectInput("select", label="Select Certification Type", 
                    choices=c("All", "ccl1", "ccl2", "ccl3", "NCITP"),
                    selected = "All") 
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
))

# Define server logic 
server <-shinyServer(function(input, output) {
  
  # load data 
  options(warn=-1)
  
  source("readCleanICTO.R")
  dat0 <- readCleanData("itcoDBDump.csv")
  
  
  options(warn=0)
  
  ## Subset to pick out only the used Date
  dat1 <- dat0[, c("BUSINESS.TRADING.NAME", "CERTIFICATION.TYPE", "DATE.CREATED")]
  dat1 <- dat1[!is.na(dat0$BUSINESS.TRADING.NAME) & dat0$BUSINESS.TRADING.NAME!= "NA" , ]
  
  df_subset <- reactive({
    
    if (input$select == "All")
      dat2 <- dat1
    else
      dat2 <- subset(dat1, CERTIFICATION.TYPE==input$select)
    
    ## Create a Year and Month Field
    dat <- dat2 %>%
      mutate(
        YearCreated = factor(year(DATE.CREATED)),
        MonthCreated = factor(month(DATE.CREATED)))
    
    monthData <- dat %>%
      group_by(YearCreated, MonthCreated, months(DATE.CREATED),CERTIFICATION.TYPE) %>%
      summarise(total.count=n())
    
    ## Change the Column Names 
    colnames(monthData) <- c("Year", "Month", "Month_Name", "Certification_Type", "Registered")
    ## Change the Month_Name Column to factor 
    monthData$Month_Name = factor(monthData$Month_Name, levels = month.name)
    return(monthData)
  })

  
  output$distPlot <- renderPlot({
      # generate the width of bars based on input$width from ui.R
      barwidth <- input$barwidth
      
      ggplot(df_subset(), aes(x=as.factor(Month_Name), y=Registered)) +
        geom_bar(aes(fill = Certification_Type), stat="identity", width=barwidth, position = "dodge") +
        xlab("Month")+ ylab("Number of Registered IT Service Providers")
        # scale_x_discrete(limits = month.abb)
      
      })
})

# Run the application 
shinyApp(ui = ui, server = server)

