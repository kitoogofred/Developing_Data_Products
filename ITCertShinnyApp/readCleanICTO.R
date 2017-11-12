## Code to clean the ITCO Dumps Data and store into a new file

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












