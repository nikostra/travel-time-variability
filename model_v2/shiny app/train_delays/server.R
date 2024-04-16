#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(fst)
library(tidyverse)
library(rstan)
library(caret)
library(brms)

delay_model_lognormal_mixture_symmetric_mu_sigma = readRDS("delay_model_lognormal_mixture_mu_sigma.rds")
#delay_model_lognormal_mixture_symmetric_mu_theta = readRDS("delay_model_lognormal_mixture_mu_theta.rds")
#delay_model_lognormal_mixture_symmetric_mu = readRDS("delay_model_lognormal_mixture.rds")

connection_model_1 = readRDS("connection_model_1_v3_horseshoe.rds")
connection_model_2 = readRDS("connection_model_2_v3_horseshoe.rds")
connection_model_3 = readRDS("connection_model_3_v3_horseshoe.rds")
connection_model_4 = readRDS("connection_model_4_v3_horseshoe.rds")

# constants for scaling transfer time
m_tt1 = 15.51035
sd_tt1 = 10.7276
m_tt2 = 35.71495
sd_tt2 = 14.10356
m_tt3 = 46.57676
sd_tt3 = 12.37518
m_tt4 = 59.41667
sd_tt4 = 0.4971671

# constant from current version of load_delays
minDelay = -6.91666666667

# Define server logic required to draw a histogram
function(input, output, session) {
  output$input_groups <- renderUI({
    input_list <- lapply(1:input$connections, function(i) {
      tagList(
        hr(),
        selectInput(
          paste0("line", i),
          label = paste0("Line of train ", i),
          choices = c(
            "Hässleholm - Växjö",
            "Jönköping/Nässjö - Växjö",
            "Värnamo - Växjö",
            "Copenhagen - Kalmar",
            "Göteborg - Kalmar"
          )
        ),
        
        selectInput(
          paste0("operator", i),
          label = paste0("Operator of train ", i),
          choices = c("TDEV", "SJ")
        ),
        
        numericInput(
          paste0("transferTime", i),
          label = paste0("Transfer time of train ", i),
          value = 10 * i
        )
      )
    })
    do.call(tagList, input_list)
  })
  
  
  output$delayPlot <- renderPlot({
    
    # select the model
    # if(input$model == "Mu+sigma"){
    #   delay_model = delay_model_lognormal_mixture_symmetric_mu_sigma
    # } else if (input$model == "Mu+theta"){
    #   delay_model = delay_model_lognormal_mixture_symmetric_mu_theta
    # } else if(input$model == "Mu"){
    #   delay_model = delay_model_lognormal_mixture_symmetric_mu
    # }
    
    delay_model = delay_model_lognormal_mixture_symmetric_mu_sigma
    
    if (input$connections == 1) {
      d = data.frame(irr = 1)
      
      d$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d$arr.WeekendTRUE = d$weekend
      
      #time of day
      d$time_morning = ifelse(input$time == "morning", 1, 0)
      d$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d$time_evening = ifelse(input$time == "evening", 1, 0)
      d$time_night = ifelse(input$time == "night", 1, 0)
      d$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                 d$time_night == 1, 1, 0)
      
      # transfer time
      d$PlannedTransferTime = scale(
        input[[paste0("transferTime", input$connections)]],
        center = m_tt1,
        scale = sd_tt1
      )
      
      # arrival train characteristics
      d$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d$dep.Operator.TDEV = ifelse(input[[paste0("operator", input$connections)]] == "TDEV", 1, 0)
      d$dep.Operator.SJ = ifelse(input[[paste0("operator", input$connections)]] == "SJ", 1, 0)
      d$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", input$connections)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", input$connections)]] == "Värnamo - Växjö", 1, 0)
      d$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", input$connections)]] == "Hässleholm - Växjö", 1, 0)
      d$dep.line.name.G...KAC = ifelse(input[[paste0("line", input$connections)]] == "Göteborg - Kalmar", 1, 0)
      
      test_delays = posterior_predict(delay_model, d[1, ]) + minDelay
      test_delays = test_delays[, 1]
      preds_1 = posterior_predict(connection_model_1, d[1, ])
      
      for (i in 1:length(test_delays)) {
        if (preds_1[i] == 0) {
          test_delays[i] = NA
        }
      }
      
      print(paste0(sum(is.na(test_delays_1))/length(test_delays_1) * 100 , "% of travelers missed all connections"))
      
      delay_plot = ggplot(data.frame(delay=test_delays_1),aes(x=delay)) + geom_density(lwd = 1) + 
        xlab("Arrival Delay in Växjö") + ylab("Density")
      for (i in 1:input$connections) {
        x = input[[paste0("transferTime", i)]] - input[[paste0("transferTime", 1)]]
        delay_plot = delay_plot + geom_vline(xintercept = x, linetype="dashed", color = "red")
        delay_plot = delay_plot + annotate("text",x=x, label=paste("Connection", i), y=-0.005*i, colour="red")
      }
      delay_plot
      
    } else if (input$connections == 2) {
      
      ### setup first train
      
      d = data.frame(irr = 1)
      
      d$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d$arr.WeekendTRUE = d$weekend
      
      #time of day
      d$time_morning = ifelse(input$time == "morning", 1, 0)
      d$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d$time_evening = ifelse(input$time == "evening", 1, 0)
      d$time_night = ifelse(input$time == "night", 1, 0)
      d$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                 d$time_night == 1, 1, 0)
      
      # transfer time
      d$PlannedTransferTime = scale(
        input[[paste0("transferTime", 1)]],
        center = m_tt1,
        scale = sd_tt1
      )
      
      # arrival train characteristics
      d$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d$dep.Operator.TDEV = ifelse(input[[paste0("operator", 1)]] == "TDEV", 1, 0)
      d$dep.Operator.SJ = ifelse(input[[paste0("operator", 1)]] == "SJ", 1, 0)
      d$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", 1)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", 1)]] == "Värnamo - Växjö", 1, 0)
      d$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", 1)]] == "Hässleholm - Växjö", 1, 0)
      d$dep.line.name.G...KAC = ifelse(input[[paste0("line", 1)]] == "Göteborg - Kalmar", 1, 0)
      
      
      ### setup second train
      
      d2 = data.frame(irr = 1)
      d2$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d2$arr.WeekendTRUE = d$weekend
      
      #time of day
      d2$time_morning = ifelse(input$time == "morning", 1, 0)
      d2$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d2$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d2$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d2$time_evening = ifelse(input$time == "evening", 1, 0)
      d2$time_night = ifelse(input$time == "night", 1, 0)
      d2$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                  d$time_night == 1, 1, 0)
      
      # transfer time
      d2$PlannedTransferTime = scale(
        input[[paste0("transferTime", 2)]],
        center = m_tt2,
        scale = sd_tt2
      )
      
      # arrival train characteristics
      d2$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d2$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d2$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d2$dep.Operator.TDEV = ifelse(input[[paste0("operator", 2)]] == "TDEV", 1, 0)
      d2$dep.Operator.SJ = ifelse(input[[paste0("operator", 2)]] == "SJ", 1, 0)
      d2$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", 2)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d2$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", 2)]] == "Värnamo - Växjö", 1, 0)
      d2$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", 2)]] == "Hässleholm - Växjö", 1, 0)
      d2$dep.line.name.G...KAC = ifelse(input[[paste0("line", 2)]] == "Göteborg - Kalmar", 1, 0)
      
      test_delays_1 = posterior_predict(delay_model, d[1, ]) + minDelay
      test_delays_1 = test_delays_1[, 1]
      test_delays_2 = posterior_predict(delay_model, d2[1, ]) + minDelay
      test_delays_2 = test_delays_2[, 1]
      
      preds_1 = posterior_predict(connection_model_1, d[1, ])
      print(mean(preds_1))
      preds_2 = posterior_predict(connection_model_2, d2[1, ])
      
      for (i in 1:length(test_delays_1)) {
        if (preds_1[i] == 0) {
          if (preds_2[i] == 0) {
            test_delays_1[i] = NA
          } else {
            test_delays_1[i] = test_delays_2[i] + input[[paste0("transferTime", 2)]] - input[[paste0("transferTime", 1)]]
          }
        }
      }
      
      print(paste0(sum(is.na(test_delays_1))/length(test_delays_1) * 100 , "% of travelers missed all connections"))
      
      delay_plot = ggplot(data.frame(delay=test_delays_1),aes(x=delay)) + geom_density(lwd = 1) + 
        xlab("Arrival Delay in Växjö") + ylab("Density")
      for (i in 1:input$connections) {
        x = input[[paste0("transferTime", i)]] - input[[paste0("transferTime", 1)]]
        delay_plot = delay_plot + geom_vline(xintercept = x, linetype="dashed", color = "red")
        delay_plot = delay_plot + annotate("text",x=x, label=paste("Connection", i), y=-0.005*i, colour="red")
      }
      delay_plot
      
      
    } else if (input$connections == 3) {
      d = data.frame(irr = 1)
      
      d$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d$arr.WeekendTRUE = d$weekend
      
      #time of day
      d$time_morning = ifelse(input$time == "morning", 1, 0)
      d$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d$time_evening = ifelse(input$time == "evening", 1, 0)
      d$time_night = ifelse(input$time == "night", 1, 0)
      d$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                 d$time_night == 1, 1, 0)
      
      # transfer time
      d$PlannedTransferTime = scale(
        input[[paste0("transferTime", 1)]],
        center = m_tt1,
        scale = sd_tt1
      )
      
      # arrival train characteristics
      d$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d$dep.Operator.TDEV = ifelse(input[[paste0("operator", 1)]] == "TDEV", 1, 0)
      d$dep.Operator.SJ = ifelse(input[[paste0("operator", 1)]] == "SJ", 1, 0)
      d$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", 1)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", 1)]] == "Värnamo - Växjö", 1, 0)
      d$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", 1)]] == "Hässleholm - Växjö", 1, 0)
      d$dep.line.name.G...KAC = ifelse(input[[paste0("line", 1)]] == "Göteborg - Kalmar", 1, 0)
      
      
      ### setup second train
      
      d2 = data.frame(irr = 1)
      d2$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d2$arr.WeekendTRUE = d$weekend
      
      #time of day
      d2$time_morning = ifelse(input$time == "morning", 1, 0)
      d2$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d2$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d2$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d2$time_evening = ifelse(input$time == "evening", 1, 0)
      d2$time_night = ifelse(input$time == "night", 1, 0)
      d2$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                  d$time_night == 1, 1, 0)
      
      # transfer time
      d2$PlannedTransferTime = scale(
        input[[paste0("transferTime", 2)]],
        center = m_tt2,
        scale = sd_tt2
      )
      
      # arrival train characteristics
      d2$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d2$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d2$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d2$dep.Operator.TDEV = ifelse(input[[paste0("operator", 2)]] == "TDEV", 1, 0)
      d2$dep.Operator.SJ = ifelse(input[[paste0("operator", 2)]] == "SJ", 1, 0)
      d2$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", 2)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d2$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", 2)]] == "Värnamo - Växjö", 1, 0)
      d2$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", 2)]] == "Hässleholm - Växjö", 1, 0)
      d2$dep.line.name.G...KAC = ifelse(input[[paste0("line", 2)]] == "Göteborg - Kalmar", 1, 0)
      
      
      ### setup third train
      
      d3 = data.frame(irr = 1)
      d3$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d3$arr.WeekendTRUE = d$weekend
      
      #time of day
      d3$time_morning = ifelse(input$time == "morning", 1, 0)
      d3$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d3$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d3$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d3$time_evening = ifelse(input$time == "evening", 1, 0)
      d3$time_night = ifelse(input$time == "night", 1, 0)
      d3$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                  d$time_night == 1, 1, 0)
      
      # transfer time
      d3$PlannedTransferTime = scale(
        input[[paste0("transferTime", 3)]],
        center = m_tt3,
        scale = sd_tt3
      )
      
      # arrival train characteristics
      d3$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d3$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d3$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d3$dep.Operator.TDEV = ifelse(input[[paste0("operator", 3)]] == "TDEV", 1, 0)
      d3$dep.Operator.SJ = ifelse(input[[paste0("operator", 3)]] == "SJ", 1, 0)
      d3$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", 3)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d3$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", 3)]] == "Värnamo - Växjö", 1, 0)
      d3$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", 3)]] == "Hässleholm - Växjö", 1, 0)
      d3$dep.line.name.G...KAC = ifelse(input[[paste0("line", 3)]] == "Göteborg - Kalmar", 1, 0)
      
      test_delays_1 = posterior_predict(delay_model, d[1, ]) + minDelay
      test_delays_1 = test_delays_1[, 1]
      test_delays_2 = posterior_predict(delay_model, d2[1, ]) + minDelay
      test_delays_2 = test_delays_2[, 1]
      test_delays_3 = posterior_predict(delay_model, d3[1, ]) + minDelay
      test_delays_3 = test_delays_3[, 1]
      
      
      preds_1 = posterior_predict(connection_model_1, d[1, ])
      preds_2 = posterior_predict(connection_model_2, d2[1, ])
      preds_3 = posterior_predict(connection_model_3, d3[1, ])
      
      for (i in 1:length(test_delays_1)) {
        if (preds_1[i] == 0) {
          if (preds_2[i] == 0) {
            if(preds_3[i] == 0){
              test_delays_1[i] = NA
              
            } else {
              test_delays_1[i] = test_delays_3[i] + input[[paste0("transferTime", 3)]] - input[[paste0("transferTime", 1)]]
            }
          } else {
            test_delays_1[i] = test_delays_2[i] + input[[paste0("transferTime", 2)]] - input[[paste0("transferTime", 1)]]
          }
        }
      }
      
      print(paste0(sum(is.na(test_delays_1))/length(test_delays_1) * 100 , "% of travelers missed all connections"))
      
      delay_plot = ggplot(data.frame(delay=test_delays_1),aes(x=delay)) + geom_density(lwd = 1) + 
        xlab("Arrival Delay in Växjö") + ylab("Density")
      for (i in 1:input$connections) {
        x = input[[paste0("transferTime", i)]] - input[[paste0("transferTime", 1)]]
        delay_plot = delay_plot + geom_vline(xintercept = x, linetype="dashed", color = "red")
        delay_plot = delay_plot + annotate("text",x=x, label=paste("Connection", i), y=-0.005*i, colour="red")
      }
      delay_plot
      
      
    } else if (input$connections == 4) {
      d = data.frame(irr = 1)
      
      d$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d$arr.WeekendTRUE = d$weekend
      
      #time of day
      d$time_morning = ifelse(input$time == "morning", 1, 0)
      d$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d$time_evening = ifelse(input$time == "evening", 1, 0)
      d$time_night = ifelse(input$time == "night", 1, 0)
      d$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                 d$time_night == 1, 1, 0)
      
      # transfer time
      d$PlannedTransferTime = scale(
        input[[paste0("transferTime", 1)]],
        center = m_tt1,
        scale = sd_tt1
      )
      
      # arrival train characteristics
      d$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d$dep.Operator.TDEV = ifelse(input[[paste0("operator", 1)]] == "TDEV", 1, 0)
      d$dep.Operator.SJ = ifelse(input[[paste0("operator", 1)]] == "SJ", 1, 0)
      d$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", 1)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", 1)]] == "Värnamo - Växjö", 1, 0)
      d$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", 1)]] == "Hässleholm - Växjö", 1, 0)
      d$dep.line.name.G...KAC = ifelse(input[[paste0("line", 1)]] == "Göteborg - Kalmar", 1, 0)
      
      
      ### setup second train
      
      d2 = data.frame(irr = 1)
      d2$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d2$arr.WeekendTRUE = d$weekend
      
      #time of day
      d2$time_morning = ifelse(input$time == "morning", 1, 0)
      d2$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d2$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d2$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d2$time_evening = ifelse(input$time == "evening", 1, 0)
      d2$time_night = ifelse(input$time == "night", 1, 0)
      d2$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                  d$time_night == 1, 1, 0)
      
      # transfer time
      d2$PlannedTransferTime = scale(
        input[[paste0("transferTime", 2)]],
        center = m_tt2,
        scale = sd_tt2
      )
      
      # arrival train characteristics
      d2$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d2$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d2$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d2$dep.Operator.TDEV = ifelse(input[[paste0("operator", 2)]] == "TDEV", 1, 0)
      d2$dep.Operator.SJ = ifelse(input[[paste0("operator", 2)]] == "SJ", 1, 0)
      d2$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", 2)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d2$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", 2)]] == "Värnamo - Växjö", 1, 0)
      d2$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", 2)]] == "Hässleholm - Växjö", 1, 0)
      d2$dep.line.name.G...KAC = ifelse(input[[paste0("line", 2)]] == "Göteborg - Kalmar", 1, 0)
      
      
      ### setup third train
      
      d3 = data.frame(irr = 1)
      d3$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d3$arr.WeekendTRUE = d$weekend
      
      #time of day
      d3$time_morning = ifelse(input$time == "morning", 1, 0)
      d3$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d3$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d3$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d3$time_evening = ifelse(input$time == "evening", 1, 0)
      d3$time_night = ifelse(input$time == "night", 1, 0)
      d3$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                  d$time_night == 1, 1, 0)
      
      # transfer time
      d3$PlannedTransferTime = scale(
        input[[paste0("transferTime", 3)]],
        center = m_tt3,
        scale = sd_tt3
      )
      
      # arrival train characteristics
      d3$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d3$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d3$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d3$dep.Operator.TDEV = ifelse(input[[paste0("operator", 3)]] == "TDEV", 1, 0)
      d3$dep.Operator.SJ = ifelse(input[[paste0("operator", 3)]] == "SJ", 1, 0)
      d3$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", 3)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d3$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", 3)]] == "Värnamo - Växjö", 1, 0)
      d3$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", 3)]] == "Hässleholm - Växjö", 1, 0)
      d3$dep.line.name.G...KAC = ifelse(input[[paste0("line", 3)]] == "Göteborg - Kalmar", 1, 0)
      
      
      ### setup forth train
      
      d4 = data.frame(irr = 1)
      d4$weekend = ifelse(input$weekday == "weekend", 1, 0)
      d4$arr.WeekendTRUE = d$weekend
      
      #time of day
      d4$time_morning = ifelse(input$time == "morning", 1, 0)
      d4$time_mid_day = ifelse(input$time == "mid_day", 1, 0)
      d4$time_afternoon = ifelse(input$time == "afternoon", 1, 0)
      d4$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
      d4$time_evening = ifelse(input$time == "evening", 1, 0)
      d4$time_night = ifelse(input$time == "night", 1, 0)
      d4$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                                  d$time_night == 1, 1, 0)
      
      # transfer time
      d4$PlannedTransferTime = scale(
        input[[paste0("transferTime", 4)]],
        center = m_tt4,
        scale = sd_tt4
      )
      
      # arrival train characteristics
      d4$arr.Operator.SNÄLL = ifelse(input$arrOperator == "Snälltåget", 1, 0)
      d4$arr.ProductName.SJ.EuroNight = ifelse(input$arrProduct == "SJ Euronight", 1, 0)
      d4$arr.ProductName.Snälltåget = ifelse(input$arrProduct == "Snälltåget", 1, 0)
      
      # departure train characteristics
      d4$dep.Operator.TDEV = ifelse(input[[paste0("operator", 4)]] == "TDEV", 1, 0)
      d4$dep.Operator.SJ = ifelse(input[[paste0("operator", 4)]] == "SJ", 1, 0)
      d4$dep.line.name.JÖ.N...VÖ.AV = ifelse(input[[paste0("line", 4)]] == "Jönköping/Nässjö - Växjö", 1, 0)
      d4$dep.line.name.V...VÖ.AV = ifelse(input[[paste0("line", 4)]] == "Värnamo - Växjö", 1, 0)
      d4$dep.line.name.HM...VÖ.AV = ifelse(input[[paste0("line", 4)]] == "Hässleholm - Växjö", 1, 0)
      d4$dep.line.name.G...KAC = ifelse(input[[paste0("line", 4)]] == "Göteborg - Kalmar", 1, 0)
      
      test_delays_1 = posterior_predict(delay_model, d[1, ]) + minDelay
      test_delays_1 = test_delays_1[, 1]
      test_delays_2 = posterior_predict(delay_model, d2[1, ]) + minDelay
      test_delays_2 = test_delays_2[, 1]
      test_delays_3 = posterior_predict(delay_model, d3[1, ]) + minDelay
      test_delays_3 = test_delays_3[, 1]
      test_delays_4 = posterior_predict(delay_model, d4[1, ]) + minDelay
      test_delays_4 = test_delays_4[, 1]
      
      
      preds_1 = posterior_predict(connection_model_1, d[1, ])
      preds_2 = posterior_predict(connection_model_2, d2[1, ])
      preds_3 = posterior_predict(connection_model_3, d3[1, ])
      preds_4 = posterior_predict(connection_model_4, d4[1, ])
      
      for (i in 1:length(test_delays_1)) {
        if (preds_1[i] == 0) {
          if (preds_2[i] == 0) {
            if(preds_3[i] == 0){
              if(preds_4[i] == 0){
                test_delays_1[i] = NA
              } else{
                test_delays_1[i] = test_delays_4[i] + input[[paste0("transferTime", 4)]] - input[[paste0("transferTime", 1)]]
              }
            } else {
              test_delays_1[i] = test_delays_3[i] + input[[paste0("transferTime", 3)]] - input[[paste0("transferTime", 1)]]
            }
          } else {
            test_delays_1[i] = test_delays_2[i] + input[[paste0("transferTime", 2)]] - input[[paste0("transferTime", 1)]]
          }
        }
      }
      
      print(paste0(sum(is.na(test_delays_1))/length(test_delays_1) * 100 , "% of travelers missed all connections"))
      
      delay_plot = ggplot(data.frame(delay=test_delays_1),aes(x=delay)) + geom_density(lwd = 1) + 
        xlab("Arrival Delay in Växjö") + ylab("Density")
      for (i in 1:input$connections) {
        x = input[[paste0("transferTime", i)]] - input[[paste0("transferTime", 1)]]
        delay_plot = delay_plot + geom_vline(xintercept = x, linetype="dashed", color = "red")
        delay_plot = delay_plot + annotate("text",x=x, label=paste("Connection", i), y=-0.005*i, colour="red")
      }
      delay_plot
    }
    
  })
  
  output$warnings = renderText({
    t = ""
    if(input$connections == 4 & (input$time == "night" | input$time == "afternoon"| input$time == "morning")){
      t = paste(t,"WARNING: The model for 4 connections is only trained on the time categories mid_day and evening. 
      The current time setting is not compatible with this model.", sep="\n")
    }
    if(input$connections > 2 & (input$arrProduct == "SJ Euronight")){
      t = paste(t,"WARNING: The models for 3 and 4 connections is not trained on the selected arrival product. 
                Please choose a different train category.", sep="\n")
    }
    if(input$connections == 4){
      if(input[[paste0("line", 4)]] != "Göteborg - Kalmar" & input[[paste0("line", 4)]] != "Hässleholm - Växjö"){
        t = paste(t,"WARNING: The model for 4 connections is not trained on the selected train line. 
        Please choose another train line.", sep="\n")
      }
    }
    
    t
  })
  
}
