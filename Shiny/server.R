library(shiny)
library(ggplot2)

# list of violent crimes
violent <- c("ASSAULT", "BATTERY", "CRIM SEXUAL ASSAULT", "KIDNAPPING", "SEX OFFENSE", "HOMICIDE", "INTIMIDATION")

crimes <-  table(data$primary.description) # 31 crimes
crimes <- crimes[crimes > 100] # only 25 with over 100 observations

# download previous year's data at: https://data.cityofchicago.org/Public-Safety/Crimes-One-year-prior-to-present/x2n5-8w5q
# x2n5-8w5q is crimes in past year
# last_year <- read.csv("http://data.cityofchicago.org/views/x2n5-8w5q/rows.csv", stringsAsFactors = F)
     
# download all the data at: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2
# ijzp-q8t2 is crimes from 2001 to the present
# all <- read.csv("http://data.cityofchicago.org/views/ijzp-q8t2/rows.csv", stringsAsFactors = F)}

# should be faster if data is read in manually once, rather than the app downloading it every time...

shinyServer(function(input, output) {
  
      dataInput <- reactive({
            
            # 360 days length(unique(substring(data$DATE..OF.OCCURRENCE, 1, 10)))
          
            data <- switch(input$dataset,
                  "One Year Prior to Present" = last_year,
                  "2001 to Present" = all)
            
            names(data) <- tolower(names(data))
            data <- subset(data, !is.na(longitude) & !is.na(latitude))
            
            data$violent <- ifelse(data$primary.description %in% violent, "Violent", "Not Violent")

            # subset by crime
            if(input$crime == "Violent Crimes Only") { data <- subset(data, data$violent == "Violent")}
            if(input$crime != "All Crimes" && input$crime != "Violent Crimes Only"  & input$crime != "Violent vs Non Violent Crimes") { data <- subset(data, primary.description %in% input$crime)}
            
            return(data)
      })
  
      output$Plot <- renderPlot({
            data <- dataInput()
            
            p <- ggplot(data)+geom_point(aes(x=longitude, y=latitude), alpha = .1, size =.3) + xlab(paste("latitude", "\n\nNumber of Observations Plotted: ", nrow(data)))
            
            if(input$crime == "All Crimes") { p <- ggplot(data)+geom_point(aes(x=longitude, y=latitude), alpha = .1, size =.3) + xlab(paste("latitude", "\n\nNumber of Observations Plotted: ", nrow(data)))}
            # add color for each crime now since there are fewer
            if(input$crime == "Violent Crimes Only") { p <- ggplot(data)+geom_point(aes(x=longitude, y=latitude, colour = primary.description), alpha = .1, size =.3) + xlab(paste("latitude", "\n\nNumber of Observations Plotted: ", nrow(data)))}
            # add color for violent vs non violent crimes
            if(input$crime == "Violent vs Non Violent Crimes") { p <- ggplot(data)+geom_point(aes(x=longitude, y=latitude, colour = violent), alpha = .1, size =.3) + xlab(paste("latitude", "\n\nNumber of Observations Plotted: ", nrow(data)))}
            # for plotting a single crime, there are so few points (as few as 100), smaller/lighter points (alpha/size) are not necessary and are hard to see
            if(input$crime %in% crimes){ p <- ggplot(data)+geom_point(aes(x=longitude, y=latitude)) + xlab(paste("latitude", "\n\nNumber of Observations Plotted: ", nrow(data)))}
            
            print(p)
    
      })
      
      output$BarChart <- renderPlot({
            data <- dataInput()
            
            day <- c("06 AM", "07 AM", "08 AM", "09 AM", "10 AM", "11 AM", "12 PM", "01 PM", "02 PM", "03 PM", "04 PM", "05 PM", "06 PM", "07 PM", "08 PM", "09 PM")
            data$hour <- paste(substring(data$date..of.occurrence, 12, 13), substring(data$date..of.occurrence, 21, 22))
            data$hour <- ifelse(data$hour %in% day, "Day", "Night")
            
            ifelse(input$crime == "Violent vs Non Violent Crimes",
                  p <- ggplot(data) + geom_bar(aes(x = primary.description, fill = violent)),
                  p <- ggplot(data) + geom_bar(aes(x = primary.description, fill = hour))
            )
            print(p)
      })
  
})
# run the chicago crime data shiny app with the following code
# runApp("~/Desktop/ChicagoCrimeMap/Chicago_Crime_Map/Shiny")
