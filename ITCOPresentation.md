Course Project: Shiny Application and Reproducible Pitch
========================================================
author: Kitoogo Fredricvk
date:   Friday, 10 Nov 2017
autosize: true

Overview
========================================================
This was built as part of a deliverable for the course Developing Data Products as part of the Coursera Data Science Specialization.

- Create a Shiny application 

- Create a presentation in Slidify or Rstudio Presenter to pitch for the application.

The Date and Application 
========================================================
The Data used for the Application was data collected on Information Technology (IT)
Service providers that have registered for certification with the National IT Authority

The information was cleaned up and aggregated with a view of generating a bar graph for the 
monthly registrations for different certification Types (Level1, Level2, Level3 and Individual Service Provider).

There application has slider input used to choose the size of the bars and interactively change them, in addition the application prompts user to choose the certification level to filter out and use in the graph and the graph interactively changes as per the users choice.

The Summary of the Data
========================================================

```
 BUSINESS.TRADING.NAME CERTIFICATION.TYPE  DATE.CREATED                
 Length:249            ccl1 :154          Min.   :2017-05-12 00:00:00  
 Class :character      ccl2 : 94          1st Qu.:2017-06-16 00:00:00  
 Mode  :character      ccl3 :  1          Median :2017-07-25 00:00:00  
                       NCITP:  0          Mean   :2017-07-31 06:33:15  
                                          3rd Qu.:2017-08-31 00:00:00  
                                          Max.   :2017-11-01 00:00:00  
```

Viewing the Source Code and the Application
========================================================
Source Code: https://github.com/kitoogofred/Developing_Data_Products

The Appiication can be viewed at: https://kitoogofred.shinyapps.io/ITCertShinnyApp/
