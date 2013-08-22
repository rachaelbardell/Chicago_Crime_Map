# Define UI for Chicago Crime Data application
# have to have headerPanel(), sidebarPanel(), and mainPanel()

crimes <-  table(data$primary.description) # 31 crimes
crimes <- crimes[crimes > 100] # only 25 with over 100 observations
crimes <- c("All Crimes", "Violent Crimes Only", "Violent vs Non Violent Crimes",  names(crimes))

shinyUI(pageWithSidebar(
      headerPanel("Chicago Crime Data"),
  
      # Sidebar with viewing and filtering options
      sidebarPanel(
            selectInput("dataset", "Select a Dataset", choices = c("One Year Prior to Present", "2001 to Present")),
            selectInput("crime", "Crime(s): ", crimes)#,
            #checkboxInput("map_underlay", "Put the Map of Chicago Underneath", TRUE)
      ),
  
      mainPanel(
            tabsetPanel(
                  tabPanel("Map of Chicago", plotOutput("Plot")),
                  tabPanel("Bar Chart of Crimes", plotOutput("BarChart"), h6("Day: 6 AM - 10 PM,  Night: 10 PM - 6 AM"))
      ))
))