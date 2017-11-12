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

# setwd("P:/NITA/New/Personal/Training/Data Science/John Hopkins/Course 9 - Developing Data Products/Week 4/Peer-Graded")


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
  
  # source("readCleanICTO.R")
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

## Function to Read and Clean Data

readCleanData <- function(fileName)
{
  
  library(lubridate)
  library(dplyr)
  library(stringr)
  require(data.table)  
  
  ##This script is for reading and cleaning ITCO Data
  # setwd("P:/NITA/New/Personal/Training/Data Science/John Hopkins/
  #       Course 9 - Developing Data Products/Week 4/Project")
  
  itco <- read.csv(fileName, na.strings=c(" ", ""), stringsAsFactors = FALSE, header=T)
  
  # itco <- read.csv("itcoDBDump.csv", na.strings=c(" ", ""), stringsAsFactors = FALSE, header=T)
  
  ## Remove double quotes from the entries
  itco <- data.frame(lapply(itco, function(x) {
    gsub('\\"', "", x);
  }))
  
  
  ## Replace all instanbces of "None" and Blank Cells with NAs
  itco <- data.frame(lapply(itco, function(x) {
    gsub('None|""|^$|^ $', "NA", x)
  }))
  
  
  ## Convert the data Fields from Character to Date
  dateFieldID <- grep("Date|date|DATE", names(itco), ignore.case=T) ## Identify all columns with Date
  dateFieldNames <- names(itco[dateFieldID])
  dateFieldData <- itco[dateFieldID] ## For testing purposes
  
  ## Truncate all the long Date Data to format "yyyy-mm-dd" - size = 12
  ## Only leaving the Year, Month and Day
  
  itco[dateFieldID] <- data.frame(lapply(itco[dateFieldID], function(x) {
    substr(x, 1, 10)
  }))
  
  # Convert the columns with dates to Date Datatype
  itco[dateFieldID] <- lapply(itco[dateFieldID], FUN = function(column) {
    parse_date_time(column, orders = c("ymd", "ymd HM", "ymd
                                       HMS"), tz = "UTC")
  })
  
  ## Convert the columns with fees to Numeric
  ammountFieldID <- grep("Ammount|ammount|AMOUNT", names(itco), ignore.case=T) ## get the amount fields
  
  itco[ammountFieldID] <- lapply(itco[ammountFieldID], FUN = function(column) {
    as.numeric(levels(column))[column]
  })
  
  ## Convert all the variables (other than the Date and NMumeric) to Character varibles
  ## Merge the Date and Numeric IDs
  mergeIDs <- c(dateFieldID,ammountFieldID)
  
  itco[-mergeIDs] <- lapply(itco[-mergeIDs], FUN = function(column) {
    as.character(column)
  })
  
  ## "TYPE.OF.SERVICE" ## Needs to be broken down
  
  # QUALIFICATION 1 VERIFIED ## Missing 
  # ORGANISATION 1 VERIFIED ## Missing
  # CERTIFICATION 1 VERIFIED ## Missing
  
  ## Convert specific Columns back to Factor Variable
  toConvertToFactor <- c("IT.DOMAIN", "TYPE.OF.ENTITY", "CERTIFICATION.TYPE", "CERTIFICATION.STATUS",
                         "REGION", "DISTRICT", "NATIONALITY", "ANNUAL.INCOME",
                         "INDIVIDUAL.GENDER", "CERTIFICATION.CATEGORY.1", "FIRE.SAFETY.CLEARANCE.FOR.SITE",
                         "OCCUPATIONAL.HEALTH...SAFETY.CLEARANCE", 
                         "TRADING.LICENCE...TRAINING.LICENCE.FROM.MOE.VERIFIED",
                         "TOTAL.NO..OF.EMPLOYEES...NO..OF.TOTAL.TEACHING.FACULTY",
                         "TRAINING.COURSES.AND.TRAINING.PLANS.VERIFIED",
                         "EMPLOYEE.CERTIFICATION.VERIFIED", "EXPERIENCE.IN.IT.SERVICE.VERIFIED",
                         "STRATEGIC.AND.BUSINESS.PLANS.VERIFIED", "INCOME.TAX.CLEARANCE..CERTIFICATE.VERIFIED",
                         "NOT.DEBARRED...AFFIDAVIT...SELF.DECLARATION.VERIFIED",
                         "EVIDENCE.OF.QMS.OR.ITSM.OR.ISMS.VERIFIED", 
                         "VALID.RELEVANT.ISO.CERTIFICATE.VERIFIED",
                         "LAST.AUDIT.REPORT.VERIFIED",
                         "CLOSURE.REPORT.OF.NON.COMPLIANCE.VERIFIED",
                         "ADDRESS.PROOF.VERIFIED",
                         "ENTITY.REGISTRATION.NUMBER..OWNERSHIP.DOCUMENT.REFERENCE.VERIFIED",
                         "NATIONAL.ID.VERIFIED", "PASSPORT.PICTURE.VERIFIED",
                         "PRIMARY.REFEREE.VERIFIED", "ALTERNATE.REFEREE.VERIFIED")
  
  toConvertToFactorIDs <- match(toConvertToFactor, names(itco)) ## Using the match function on IDs
  
  itco[toConvertToFactorIDs] <- lapply(itco[toConvertToFactorIDs], FUN = function(column) {
    as.factor(column)
  })
  
  # itco[, toConvertToFactor] <- lapply(itco[, toConvertToFactor], factor)
  
  
  ## Convert rows to Integer 
  toConvertToInteger <- c("NO..OF.IT.PROFESSIONALS...NO..OF.CERTIFIED.TRAINERS")
  
  toConvertToIntegerIDs <- match(toConvertToInteger, names(itco)) ## Using the match function on IDs
  
  itco[toConvertToIntegerIDs] <- lapply(itco[toConvertToIntegerIDs], FUN = function(column) {
    as.integer(as.character(column))
  })
  
  
  ## Convert rows to Date
  toConvertToDate <- c("LAST.AUDIT.REPORT")
  toConvertToDateID <- match(toConvertToDate, names(itco))
  
  itco[toConvertToDateID] <- lapply(itco[toConvertToDateID], FUN = function(column) {
    parse_date_time(column, orders = c("ymd", "ymd HM", "ymd
                                       HMS"), tz = "UTC")
  })
  
  
  ## Convert Rows to Numeric
  toConvertToNumeric <- c("NITA.APPLICATION.FEE", "NITA.CERTIFICATION.FEE",
                          "COMTEL.APPLICATION.FEE", "COMTEL.CERTIFICATION.FEE")
  
  toConvertToNumericIDs <- match(toConvertToNumeric, names(itco))
  
  itco[toConvertToNumericIDs] <- lapply(itco[toConvertToNumericIDs], FUN = function(column) {
    as.numeric(as.character(column))
  })
  
  write.csv(itco, file = "itcoClean.csv")
  
  return(itco)
  
  }


# Run the application 
shinyApp(ui = ui, server = server)



