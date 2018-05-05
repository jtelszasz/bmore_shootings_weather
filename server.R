library(shiny)
library(tidyverse)
library(RSocrata)
#library(rwunderground)
library(lubridate)
library(broom)

n.lags <- 15

weekly.df <- readRDS("cache.Rdata") %>%
  mutate(pred = NA_real_)


#predict.week.start <- floor_date(today() - dweeks(1), "week")
#train.start.week.start <- predict.week.start - dweeks(1)

apply_lags <- function(df, n.lags){
  lags <- seq(n.lags)
  lag_names <- paste("lag", 
                     formatC(lags, width = nchar(max(lags)), flag = "0"),                      sep = "_")
  lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
 
  df <- dplyr::mutate_at(df, vars(all.shootings), funs_(lag_functions))
  
  return(df)
}

build_training_data <- function(predict.week.start){

  training.df <- weekly.df %>%
    apply_lags(n.lags) %>%
    dplyr::filter(week.start < as.Date(predict.week.start)) %>%
    #week.start >= (predict.week.start - lubridate::dweeks(53))) %>%
    dplyr::select(all.shootings, 
                  mean.max.temp, 
                  days.precip, 
                  starts_with("lag")) %>%
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
    apply_lags(n.lags) %>%
    filter(week.start == predict.week.start) %>%
    select(-all.shootings)
  prediction <- predict.glm(model, new, type = "response")
  return(prediction)
}

actual_n_shootings <- function(predict.week.start){
  actual.n.shootings <- weekly.df %>%
    filter(week.start == predict.week.start) %>%
    select(all.shootings) %>%
    pull()
  return(actual.n.shootings)
}



# generate models/predictions for last 100 weeks
for(i in seq(nrow(weekly.df) - 100, nrow(weekly.df), 1)){
  predict.week.start <- weekly.df$week.start[[i]]
  train.df <- build_training_data(predict.week.start)
  mod <- fit_weekly_model(train.df)
  weekly.df$pred[[i]] <- predict_weekly_crime(mod, predict.week.start)

}

weekly.df %>% 
  filter(!is.na(pred)) %>%
  ggplot(aes(week.start)) + 
  geom_line(aes(y = all.shootings), color = "red") +
  geom_line(aes(y = pred), color = "black")

weekly.df %>% 
  filter(!is.na(pred)) %>%
  ggplot(aes(week.start)) + 
  geom_col(aes(y = all.shootings), fill = "red") +
  geom_col(aes(y = pred), fill = "black")

weekly.df %>%
  filter(!is.na(pred)) %>%
  mutate(perf = all.shootings - pred) %>%
  ggplot(aes(week.start, perf)) +
  geom_col(aes(fill = (perf < 0)))

weekly.df %>%
  filter(!is.na(pred)) %>%
  ggplot(aes(pred, all.shootings)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  ylim(c(0,40)) +
  xlim(c(0,40))


shinyServer(function(input, output){
    
    predicted.value <- reactive({
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
      round(pred[[1]], 0)
    })

    actual.value <- reactive({
      predict.week.start <- input$predict.week.start
      actual.n.shootings <- actual_n_shootings(predict.week.start)
      actual.n.shootings
    })
    
    output$predicted.value <- renderPrint({predicted.value()})
    output$predict.week.start <- renderPrint(input$predict.week.start)
    output$actual.value <- renderPrint(actual.value())
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



# training.df %>%
#   ggplot(aes(lag_01, all.shootings)) +
#   geom_point() +
#   geom_smooth(method = "lm")
