library(shiny)
library(tidyverse)
library(RSocrata)
#library(rwunderground)
library(lubridate)

n.lags <- 15

weekly.df <- readRDS("cache.Rdata")



predict.week.start <- floor_date(today() - dweeks(1), "week")
#train.start.week.start <- predict.week.start - dweeks(1)

apply_lags <- function(df, n.lags){
  lags <- seq(n.lags)
  lag_names <- paste("lag", 
                     formatC(n.lags, width = nchar(max(n.lags)), flag = "0"),                      sep = "_")
  lag_functions <- setNames(paste("dplyr::lag(., ", n.lags, ")"), lag_names)
 
  df <- dplyr::mutate_at(df, vars(all.shootings), funs_(lag_functions))
  
  return(df)
}

build_training_data <- function(predict.week.start){

  
  training.df <- weekly.df %>%
    dplyr::filter(week.start < as.Date(predict.week.start)) %>%
    #week.start >= (predict.week.start - lubridate::dweeks(53))) %>%
    dplyr::select(all.shootings, mean.max.temp, days.precip) %>%
    apply_lags(n.lags) %>%
    na.omit()
  
  return(training.df)
}

fit_weekly_model <- function(training.df){
  mod <- glm(all.shootings ~ ., 
             data = training.df, 
             family = "poisson")
  return(mod)
}

predict_weekly_crime <- function(model, predict.week.start){
  new <- weekly.df %>% 
    filter(week.start == predict.week.start) %>%
    apply_lags(n.lags) %>%
    select(-all.shootings)
  prediction <- predict.glm(model, new, type = "response")
  return(prediction)
}


shinyServer(function(input, output){
    
    results <- reactive({
      predict.week.start <- input$predict.week.start
      
      if (is.null(predict.week.start))
        return(NULL)
      
      # load input data
      # new_data_df <- read.csv(
      #   inFile$datapath, 
      #   sep='\t', 
      #   header=FALSE, 
      #   quote = "",
      #   stringsAsFactor=F,
      #   col.names=c("Text")
      # )
      
      training.df <- build_training_data(predict.week.start)
      
      #model_and_data <- build_model(new_data_df, input$sparsity)
      model <- fit_weekly_model(training.df)
      
      
      pred <- predict_weekly_crime(model, predict.week.start)
      
      #new_data_df$Prob <- pred[,2]
      
      #new_data_df
      #model
      pred
    })

    output$contents <- renderPrint({
     results()
    })
    
    output$out.predict.week.start <- renderPrint(input$predict.week.start)
    #output$training.start <- renderPrint(pull(training.df()[1, "week.start"]))
    #output$model <- renderPrint(summary(model))
    # output$prediction <- renderPrint(predict())
    # output$launch <- renderPrint(launch())
    # output$newPlot <- renderPlot({g <- plot_temp_oring_risk()
    # g <- g + geom_vline(xintercept = input$'temp', color='red')
    # g <- g + annotate("text", x = input$temp + 5, y = 2.5, label="Launch Temp",color="red")
    # print(g)

    }
  )

