library(shiny)
library(lubridate)

shinyUI(
  pageWithSidebar(
    
    headerPanel(
      "Predicted and Actual Crime in Baltimore",
      windowTitle = "Predicted and Actual Shootings in Baltimore"
    ),
    
    sidebarPanel(
      h2("Select the Week for Prediction"),

      sliderInput("predict.week.start",
                  label="Predict for Week Starting",
                  min = floor_date(today() - dweeks(52), "week"),
                  max = floor_date(today() - dweeks(2), "week"), 
                  #max = floor_date(today() - dweeks(1), "week"),
                  step = dweeks(1),
                  value = floor_date(today(), "week")
      ),

      submitButton("Update")
    ),
    
    
    mainPanel(
      p("The purpose of this tool is to compare a predicted number of weekly total shootings (including both non-fatal and homicides) to the actual number that occured. Data for all shootings since 2012 are used from Open Baltimore. It can be used to gauge the performance of city-wide crime interventions (lower actual shootings might be considered over-performance, while higher actual shootings might be considered under-performance of crime prevention or crime fight tactics."),
      h3("Week of:"),
      verbatimTextOutput("predict.week.start"),
      #verbatimTextOutput("training.start"),
      
      h3("Predicted Number of Shootings (Non-Fatal + Homicide)"),
      verbatimTextOutput("predicted.value"),
      
      h3("Actual Number of Shootings (Non-Fatal + Homicide)"),
      verbatimTextOutput("actual.value"),
      
      h3("Notes"),
      p("The data used in the model to predict the number of weekly shootings include the outdoor temperature and the prior 15 weeks worth of shootings.")
      
  
      #plotOutput('newPlot'),
    )
    
    
    
    
  )
)