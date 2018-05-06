library(shiny)
library(tidyverse)
library(RSocrata)
#library(rwunderground)
library(lubridate)
library(broom)
library(ggiteam)
library(plotly)


n.lags <- 15

weekly.df <- readRDS("cache.Rdata") %>%
  mutate(predicted = NA_real_)


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
  weekly.df$predicted[[i]] <- predict_weekly_crime(mod, predict.week.start)

}

plot_time_series <- function(){
  g <- weekly.df %>% 
    filter(!is.na(predicted)) %>%
    select(week.start, all.shootings, predicted) %>%
    gather(cat, shots, 2:3) %>%
    ggplot(aes(week.start)) + 
    geom_line(aes(y = shots, color = cat)) +
    #geom_line(aes(y = predicted), color = "black") +
    theme_iteam_presentations() +
    ylab("Total Weekly Shootings") +
    xlab("Week Starting") +
    ggtitle("Actual and Predicted Weekly Shootings")
    
    
  ggplotly(g) %>%
    layout(hovermode = 'compare',
           legend = list(orientation = "h", x = -0.0, y = 1.0)) 
}

plot_ts_diff_bar <- function(){
  g <- weekly.df %>%
    filter(!is.na(predicted)) %>%
    mutate(perf = all.shootings - predicted) %>%
    ggplot(aes(week.start, perf)) +
    geom_col(aes(fill = perf < 0))+
    theme_iteam_presentations() +
    theme(legend.position = "none") +
    ylab("Actual Over/Under Prediction") +
    xlab("Week Starting") +
    ggtitle("Difference between Actual and Predicted\nWeekly Shootings")
  print(ggplotly(g))
}
# weekly.df %>% 
#   filter(!is.na(predicted)) %>%
#   ggplot(aes(week.start)) + 
#   geom_col(aes(y = all.shootings), fill = "red") +
#   geom_col(aes(y = predicted), fill = "black")
# 

# 
# weekly.df %>%
#   filter(!is.na(predicted)) %>%
#   ggplot(aes(predicted, all.shootings)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0) +
#   ylim(c(0,40)) +
#   xlim(c(0,40))


shinyServer(function(input, output){
    
    predicted.value <- reactive({
      predict.week.start <- input$predict.week.start
      
      if (is.null(predict.week.start))
        return(NULL)
    
      #training.df <- build_training_data(predict.week.start)
      #model <- fit_weekly_model(training.df)
      #predicted <- predict_weekly_crime(model, predict.week.start)
      
      predicted <- weekly.df %>% 
        filter(week.start == predict.week.start) %>%
        select(predicted) %>%
        pull()
      
      round(predicted[[1]], 0)
    })

    actual.value <- reactive({
      predict.week.start <- input$predict.week.start
      actual.n.shootings <- actual_n_shootings(predict.week.start)
      actual.n.shootings
    })
    
    output$predicted.value <- renderPrint({predicted.value()})
    output$predict.week.start <- renderPrint(input$predict.week.start)
    output$actual.value <- renderPrint(actual.value())


    output$plot.time.series <- renderPlotly(plot_time_series())
    output$plot.ts.diff <- renderPlotly(plot_ts_diff_bar())

    }
  )



# training.df %>%
#   ggplot(aes(lag_01, all.shootings)) +
#   geom_point() +
#   geom_smooth(method = "lm")
