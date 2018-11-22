#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)

app_data <- readr::read_rds("alldata")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("2018 Election - Polling vs Results Errors vs Educational Makeup of Those Polled"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("education",
                     "Education Level:",
                     c("High School Grad. or Less" = "highschool",
                       "Some College Educ." = "somecollege",
                       "4-year College Grad." = "fouryear",
                       "Postgraduate Degree" = "postgrad",
                       "[DO NOT READ] Don't know/Refused" = "refused"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("educ4Plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

#I needed to be able to generate different graphics for each level of education.  I determined that the best
#way to do this is if and else if statements, one for each level.  Below are some links that helped me figure
#this out:

# https://stackoverflow.com/questions/34820952/using-a-reactive-expression-in-an-if-statement-in-shiny
# https://stackoverflow.com/questions/46327997/if-condition-in-shiny-server
# https://stackoverflow.com/questions/50347070/using-else-if-in-a-r-shiny-app

   output$educ4Plot <- renderPlot({
      
     if (input$education == "highschool")
     {highschoolplot <- app_data %>%
         ggplot(aes(x= highschool_per, y=error, color = win_party)) +
            geom_jitter()
     print(highschoolplot)}

     else if (input$education == "somecollege")
    {somecollegeplot <- app_data %>%
      ggplot(aes(x= somecollege_per, y=error, color = win_party)) +
        geom_jitter()
    print(somecollegeplot)}
     
     else if (input$education == "fouryear")
     {fouryearplot <- app_data %>%
       ggplot(aes(x= fouryear_per, y=error, color = win_party)) +
       geom_jitter()
     print(fouryearplot)}
     
     else if (input$education == "postgrad")
     {postgradplot <- app_data %>%
       ggplot(aes(x= postgrad_per, y=error, color = win_party)) +
       geom_jitter()
     print(postgradplot)}
     
     else if (input$education == "refused")
     {refusedplot <- app_data %>%
       ggplot(aes(x= refused_per, y=error, color = win_party)) +
       geom_jitter()
     print(refusedplot)}
   
})
}

# Run the application 
shinyApp(ui = ui, server = server)