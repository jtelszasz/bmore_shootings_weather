library(shiny)
library(lubridate)

shinyUI(
  pageWithSidebar(
    
    headerPanel(
      "Predicted and Actual Crime in Baltimore"
    ),
    
    sidebarPanel(
      h2("Select the Week for Prediction"),
      "Anticipated ambient temperature in degrees Fahrenheit at space shuttle launch time.",

      sliderInput("predict.week.start",
                  label="Predict for Week Starting",
                  min = as.Date("2018-01-01"),
                  max = floor_date(today() - dweeks(1), "week"),
                  step = dweeks(1),
                  value = floor_date(today(), "week")
      ),

      submitButton("Update")
    ),
    
    
    mainPanel(
      p('In 1986, the Space Shuttle Challenger exploded after launch and its crew were killed. Subsequent investigations revealed that the cause was hot gases escaping past the o-rings in the solid rocket boosters.  Cold ambient temperature (36 degrees Fahrenheit) on the morning of the launch lessened the resilience of the o-rings, resulting in "blow-by" of hot gases and an eventual explosion.  Data on the risk of thermal degradation and blow-by was available for the 23 shuttle launches prior to the Challenger, however no data existed for ambient temperatures below 53 degrees Fahrenheit.'),
      p('This app fits a linear regression model to predict number of o-rings at risk of thermal damage based on ambient temperature in degrees Fahrenheit at launch.  The data are for 22 of the previous 23 launches (one was considered an outlier, labeled on the plot below.)  The linear model is used for ambient temperatures below 75 degrees Fahrenheit.  Above this temperature, it is assumed that there is zero thermal damage to o-rings.  If the predicted value for number of o-rings at risk is one or greater, the decision should be made not to launch.  (This occurs at ambient temperatures below 58 degrees Fahrenheit.)'),
      h3("Ambient Temperature (deg F) at Launch:"),
      verbatimTextOutput("out.predict.week.start"),
      verbatimTextOutput("training.start"),
      h3("Predicted Number of O-Rings at Risk for Thermal Damage"),
      p("Previous data records number of o-rings at risk (integers), but this model predicts fractional values and uses the result as a proxy to determine whether to launch."),
      #verbatimTextOutput("prediction"),
      h3("Safe to Launch?"),
      "For the purposes of this app, if the predicted value for number of o-rings at risk is less than one, it is safe to launch.",
      #verbatimTextOutput("launch"),
      h3("Data and Linear Model"),
      p("The black line on the plot shows the linear model, with extrapolation below 53 degrees Fahrenheit (the lowest temperature at which data were available).  The blue vertical line shows the temperature at which the Challenger was launched, while the red line updates to display the temperature selected in the left side panel."),
      #plotOutput('newPlot'),
      h3("References"),
      "University of California - Irvine, Machine Learning Repository:", tags$a(href="https://archive.ics.uci.edu/ml/datasets/Challenger+USA+Space+Shuttle+O-Ring","Space Shuttle O-Ring Data Set")
    )
    
    
    
    
  )
)