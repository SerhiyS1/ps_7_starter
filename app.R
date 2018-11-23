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
library(ggplot2)

app_data <- readr::read_rds("fulldata")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("2018 Election - Polling vs. Results Errors vs Educational Makeup of Those Polled"),
   
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
         plotOutput("educ4Plot"),
         HTML(paste("The interactive graphic above allows one to investigate if the error between the results 
and polls can be attributed to the educational makeup of the surveys.  On the Y-axis is the Percent Error, 
which is the Actual Democratic Advantage minus the Polling Democratic Advantage.  Thus, a positive error
signifies that a Democrat outperformed the polls, and a negative error means that the Republicans outperformed
the polls.  The closer the plot points are to zero on the y-axis, the more accurate the polls were.  Thus,
if a certain percentage on the x-axis corresponds with y-values closer to 0, one can hypothesize that such a
proportion is more likely to reduce errors in polling."))
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

#Needed help making Democrats blue and Republicans red, as my initial app had a orange-reddish color for the
#Democrats and a blue color for the Republicans, which is obviously confusing.  Here's a useful link:
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually

   output$educ4Plot <- renderPlot({
      
     if (input$education == "highschool")
     {highschoolplot <- app_data %>%
         ggplot(aes(x= highschool_per, y=error, color = win_party)) +
            geom_jitter() +
            xlab("Percentage of Those Surveyed That Were High School Grads or Less") +
            ylab("Percent Error: Actual Dem Advantage - Polled Dem Advantage") +
            labs(title = "Error vs. Proportion of High School Grads or Less", color = "Winning Party") +
            theme_grey() +
            scale_color_manual(values=c("blue", "red"))
       
     print(highschoolplot)}

     else if (input$education == "somecollege")
    {somecollegeplot <- app_data %>%
      ggplot(aes(x= somecollege_per, y=error, color = win_party)) +
        geom_jitter()+
      xlab("Percentage of Those Surveyed That Completed Some College") +
      ylab("Percent Error: Actual Dem Advantage - Polled Dem Advantage") +
      labs(title = "Error vs. Proportion of People with Some College Completed", color = "Winning Party") +
      theme_grey() +
      scale_color_manual(values=c("blue", "red"))
    print(somecollegeplot)}
     
     else if (input$education == "fouryear")
     {fouryearplot <- app_data %>%
       ggplot(aes(x= fouryear_per, y=error, color = win_party)) +
       geom_jitter()+
       xlab("Percentage of Those Surveyed That Were 4 Year College Grads") +
       ylab("Percent Error: Actual Dem Advantage - Polled Dem Advantage") +
       labs(title = "Error vs. Proportion of 4 Year College Grads", color = "Winning Party") +
       theme_grey() +
       scale_color_manual(values=c("blue", "red"))
     print(fouryearplot)}
     
     else if (input$education == "postgrad")
     {postgradplot <- app_data %>%
       ggplot(aes(x= postgrad_per, y=error, color = win_party)) +
       geom_jitter()+
       xlab("Percentage of Those Surveyed That Had Postgraduate Degrees") +
       ylab("Percent Error: Actual Dem Advantage - Polled Dem Advantage") +
       labs(title = "Error vs. Proportion of People with Postgraduate Degrees", color = "Winning Party") +
       theme_grey() +
       scale_color_manual(values=c("blue", "red"))
     print(postgradplot)}
     
     else if (input$education == "refused")
     {refusedplot <- app_data %>%
       ggplot(aes(x= refused_per, y=error, color = win_party)) +
       geom_jitter()+
       xlab("Percentage of Those Surveyed That Refused to Answer Education Level") +
       ylab("Percent Error: Actual Dem Advantage - Polled Dem Advantage") +
       labs(title = "Error vs. Proportion of Those Who Refused to Answer", color = "Winning Party") +
       theme_grey() +
       scale_color_manual(values=c("blue", "red"))
     print(refusedplot)}
   
})
}

# Run the application 
shinyApp(ui = ui, server = server)