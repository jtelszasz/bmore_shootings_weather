library(shiny)
library(tidyverse)
library(RSocrata)
#library(rwunderground)
library(lubridate)

weekly.df <- readRDS("cache.Rdata")

predict.week.start <- floor_date(today() - dweeks(1), "week")
#train.start.week.start <- predict.week.start - dweeks(1)

get_training_data <- function(predict.week.start){
  training.df <- dplyr::filter(weekly.df, 
                               week.start < predict.week.start,
                               week.start >= (predict.week.start - lubridate::dweeks(53)))
  return(training.df)
}

fit_weekly_model <- function(training.df){
  mod <- glm(all.shootings ~ ., 
             data = training.df, 
             family = "poisson")
  return(mod)
}

predict_weekly_crime <- function(week.input){
  prediction <- predict.glm(mod, new, type = "response")
  return(prediction)
}

shinyServer(
  function(input, output){
    
    training.df <- reactive(get_training_data(input$predict.week.start))
    
    # 
    # launch <- reactive({
    #   if(predict() < 1)
    #     return("Yes")
    #   else
    #     return("No")
    # })

    output$out.predict.week.start <- renderPrint(input$predict.week.start)
    output$training.start <- renderPrint(pull(training.df()[1, "week.start"]))
    # output$prediction <- renderPrint(predict())
    # output$launch <- renderPrint(launch())
    # output$newPlot <- renderPlot({g <- plot_temp_oring_risk()
    # g <- g + geom_vline(xintercept = input$'temp', color='red')
    # g <- g + annotate("text", x = input$temp + 5, y = 2.5, label="Launch Temp",color="red")
    # print(g)

    }
  )

