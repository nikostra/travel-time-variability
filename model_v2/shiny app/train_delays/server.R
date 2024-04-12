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

delay_model = readRDS("../../delay_model_lognormal_mixture_mu_sigma.rds")
connection_model_1 = readRDS("../../connection_model_1_v3_horseshoe.rds")
connection_model_2 = readRDS("../../connection_model_2_v3_horseshoe.rds")
connection_model_3 = readRDS("../../connection_model_3_v3_horseshoe.rds")
connection_model_4 = readRDS("../../connection_model_4_v3_horseshoe.rds")

# Scale Transfer time
connections = load_data_classification_v2()

connections_1 = connections$connections_1
connections_2 = connections$connections_2
connections_3 = connections$connections_3
connections_4 = connections$connections_4

transfer_time_prescale_1 = connections_1$PlannedTransferTime
connections_1$PlannedTransferTime = scale(connections_1$PlannedTransferTime)[, 1]
transfer_time_prescale_2 = connections_2$PlannedTransferTime
connections_2$PlannedTransferTime = scale(connections_2$PlannedTransferTime)[, 1]
transfer_time_prescale_3 = connections_3$PlannedTransferTime
connections_3$PlannedTransferTime = scale(connections_3$PlannedTransferTime)[, 1]
transfer_time_prescale_4 = connections_4$PlannedTransferTime
connections_4$PlannedTransferTime = scale(connections_4$PlannedTransferTime)[, 1]

delays = load_delays_all()
y = delays$ArrivalDelay
minDelay = min(y) - 1

build_test_sample = function(index) {
  d = data.frame(weekend = ifelse(input$weekday == "weekend", 1, 0))
  d$arr.WeekendTRUE = d$weekend
  
  #time of day
  d$time_morning = ifelse(input$time == "morning", 1, 0)
  d$time_mid_day = ifelse(time == "mid_day", 1, 0)
  d$time_afternoon = ifelse(time == "afternoon", 1, 0)
  d$arr.TimeOfDay.afternoon..14.18. = d$time_afternoon
  d$time_evening = ifelse(time == "evening", 1, 0)
  d$time_night = ifelse(time == "night", 1, 0)
  d$arr.TimeOfDay.evening..18.22. = ifelse(d$time_evening == 1 |
                                             d$time_night == 1, 1, 0)
  
  # transfer time
  if (index == 1) {
    d$PlannedTransferTime = scale(
      input[[paste0("transferTime", index)]],
      center = mean(transfer_time_prescale_1),
      scale = sd(transfer_time_prescale_1)
    )
  } else if (index == 2) {
    d$PlannedTransferTime = scale(
      input[[paste0("transferTime", index)]],
      center = mean(transfer_time_prescale_2),
      scale = sd(transfer_time_prescale_2)
    )
  } else if (index == 3) {
    d$PlannedTransferTime = scale(
      input[[paste0("transferTime", index)]],
      center = mean(transfer_time_prescale_3),
      scale = sd(transfer_time_prescale_3)
    )
  } else if (index == 4) {
    d$PlannedTransferTime = scale(
      input[[paste0("transferTime", index)]],
      center = mean(transfer_time_prescale_4),
      scale = sd(transfer_time_prescale_4)
    )
    
  }
  
  return(d)
  
}

# Define server logic required to draw a histogram
function(input, output, session) {
  output$input_groups <- renderUI({
    input_list <- lapply(1:input$connections, function(i) {
      tagList(
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
          value = 10
        )
      )
    })
    do.call(tagList, input_list)
  })
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      col = 'darkgray',
      border = 'white',
      xlab = 'Waiting time to next eruption (in mins)',
      main = 'Histogram of waiting times'
    )
    
  })
  
  output$delayPlot <- renderPlot({
    # TODO display warning if incorrect combination is entered (e.g. night and 4 connections)
    
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
        center = mean(transfer_time_prescale_1),
        scale = sd(transfer_time_prescale_1)
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
        center = mean(transfer_time_prescale_1),
        scale = sd(transfer_time_prescale_1)
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
        center = mean(transfer_time_prescale_2),
        scale = sd(transfer_time_prescale_2)
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
        center = mean(transfer_time_prescale_1),
        scale = sd(transfer_time_prescale_1)
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
        center = mean(transfer_time_prescale_2),
        scale = sd(transfer_time_prescale_2)
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
        center = mean(transfer_time_prescale_3),
        scale = sd(transfer_time_prescale_3)
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
        center = mean(transfer_time_prescale_1),
        scale = sd(transfer_time_prescale_1)
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
        center = mean(transfer_time_prescale_2),
        scale = sd(transfer_time_prescale_2)
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
        center = mean(transfer_time_prescale_3),
        scale = sd(transfer_time_prescale_3)
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
        center = mean(transfer_time_prescale_4),
        scale = sd(transfer_time_prescale_4)
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
  
  
}
