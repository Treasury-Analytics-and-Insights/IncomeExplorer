#
# This is the server logic of the Income Explorer Shiny web application. You can
# run the application by clicking 'Run App' above.
#

suppressMessages({
  library(shiny)
  library(openxlsx)
  library(rhandsontable)
  library(magrittr)
  library(data.table)
})

# Define server logic required to run the app
shinyServer(function(input, output, session) {
  ##########################################
  # Pop up warning on start 
  ##########################################
  # the modal dialog where the user can enter the query details.
  warning_modal <- modalDialog(
    title = "Warning", 
    paste0(
      "This software is provided as-is, for research purposes only, ",
      "with absolutely no warranty or guarantee of correctness."
    ),
    easyClose = FALSE, fade = FALSE
  )
  
  # Show the model on start up ...
  showModal(warning_modal)
  
  ##########################################
  # Loading data and calculating incomes 
  ##########################################
  
  WEEKS_IN_YEAR <- 52L

  load_parameters_data <- reactive({
    show_all_parameters(
      input$parameters_SQ$datapath,
      input$parameters_Reform$datapath
    )
  })
  
  # display changed parameters 
  output$show_parameters <- renderRHandsontable({
    DF <- load_parameters_data()
    rhandsontable(DF, height = 800) %>%
      hot_cols(
        colWidths = c(500, 200, 200),
        fixedColumnsLeft = 1,
        stretchH = "all"
      ) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })    
  
  # Read in the parameters from files
  reload_data <- reactive({
    req(input$parameters_SQ, input$parameters_Reform)
    
    if(!is.null(input$show_parameters)){
      DF <- hot_to_r(input$show_parameters)
      parameters_SQ <- parameters_from_df(DF, parameters_column = 2)
      parameters_Reform <- parameters_from_df(DF, parameters_column = 3)
    } else {
      parameters_SQ  <-  parameters_from_file(req(input$parameters_SQ)$datapath)
      parameters_Reform <- parameters_from_file(req(input$parameters_Reform)$datapath)
    }
    
    return(list(
      parameters_SQ = parameters_SQ,
      parameters_Reform = parameters_Reform
    ))
  })
  
  
  # calculate incomes 
  calculate_income <- reactive({
    parameters = req(reload_data())
    
    # convert inputs
    MAX_WAGE <- input$max_hours*input$wage1_hourly
    children <- convert_ages(input$Children_ages)
    
    if (input$Acc_type == "Renting"){
      AS_Accommodation_Rent <- TRUE
    } else {
      AS_Accommodation_Rent <- FALSE
    }
    
    if (input$Partnered == 1){
      partner_wages <- input$gross_wage2*input$hours2
      partner_hours <- input$hours2
    } else {
      partner_wages <- 0
      partner_hours <- 0
    }
    
    # Create helper emtr function using current inputs
    hot_emtr <- function(params) {
      emtr_df <- emtr(
        # System parameters
        params,
        # Family parameters
        input$Partnered, input$wage1_hourly, children, partner_wages, partner_hours,
        input$AS_Accommodation_Costs, AS_Accommodation_Rent, as.numeric(input$AS_Area),
        pov_thresholds = input$pov_thresholds,bhc_median = input$bhc_median,
        ahc_median = input$ahc_median,
        # Presentation parameters
        max_wage = MAX_WAGE, steps_per_dollar = 1L, weeks_in_year = WEEKS_IN_YEAR, 
        MFTC_WEP_scaling = as.numeric(input$MFTC_WEP_scaling)
      )
      return(emtr_df)
    }
    
    X_SQ <- hot_emtr(parameters$parameters_SQ)
    X_Reform <- hot_emtr(parameters$parameters_Reform)
    

    
    # MFTC is meant to make families always better off being off-benefit than staying
    # on a benefit. We let the user set whether the family stays on benefit
    # or gets IWTC when they work, with the parameter input$WFFBEN_SQ. This can be:
    # "Max" - choose the option which maximises the family income.
    # "WFF" - go off the benefit while working, and get IWTC + MFTC.
    # "Benefit" - stay on the benefit while working, and never get IWTC or MFTC
    #             (benefit abates away as earned income increases).
    # Note that these are only applicable when beneficiaries are ineligible for IWTC;
    # MFTC eligibility currently depends on IWTC eligibility in the `emtr` function.
    
    if (input$WFFBEN_SQ != "WFF") {
      SQ_params_with_no_IWTC <- remove_IWTC_from_params(parameters$parameters_SQ)
      X_SQ_without_IWTC <- hot_emtr(SQ_params_with_no_IWTC)
      
      if (input$WFFBEN_SQ == "Max") {
        # Choose which of benefit or IWTC gives max net income
        X_SQ <- choose_IWTC_or_benefit(X_SQ, X_SQ_without_IWTC)
        
      } else if (input$WFFBEN_SQ == "Benefit") {
        X_SQ <- X_SQ_without_IWTC
      }
    }
    
    if (input$WFFBEN_reform != "WFF") {
      Reform_params_with_no_IWTC <- remove_IWTC_from_params(parameters$parameters_Reform)
      X_Reform_without_IWTC <- hot_emtr(Reform_params_with_no_IWTC)
      
      if (input$WFFBEN_reform == "Max") {
        # Choose which of benefit or IWTC gives max net income
        X_Reform <- choose_IWTC_or_benefit(X_Reform, X_Reform_without_IWTC)
        
      }  else if (input$WFFBEN_reform == "Benefit") {
        X_Reform <- X_Reform_without_IWTC
      }
    }
    
    # used to ensure that SQ and reform plots have the same axes
    max_income <- max(
      max(X_SQ$gross_wage1_annual),
      max(X_Reform$gross_wage1_annual)
    )
    
    max_net_income <- 1.1*WEEKS_IN_YEAR*max(
      max(X_SQ$Net_Income), max(X_Reform$Net_Income)
    )
    min_y_SQ <- X_SQ[, WEEKS_IN_YEAR*min(
      -(
        gross_benefit1 + gross_benefit2 - net_benefit1 - net_benefit2 +
          wage1_tax + wage2_tax + wage1_ACC_levy + wage2_ACC_levy
      )
    )]
    min_y_Reform <- X_Reform[, WEEKS_IN_YEAR*min(
      -(
        gross_benefit1 + gross_benefit2 - net_benefit1 - net_benefit2 +
          wage1_tax + wage2_tax + wage1_ACC_levy + wage2_ACC_levy
      )
    )]
    min_y <- 1.1*min(min_y_SQ, min_y_Reform) 
    
    return(list(
      X_SQ = X_SQ, 
      X_Reform = X_Reform, 
      max_income = max_income,
      max_net_income = max_net_income, 
      min_y = min_y
    ))
  })
  
  ########################################## 
  ### EMTR Tab
  ########################################## 
  
  # net income plots
  output$plot_netincome<- renderPlotly({
    X_results <- req(calculate_income())
    
    X_SQ <- X_results$X_SQ
    X_Reform <- X_results$X_Reform
    
    compare_net_income_plot(X_SQ, X_Reform, inc_limit = X_results$max_income,
                            title = "Net income comparison", policy_name1 = 'Status Quo',
                            policy_name2 = 'Reform', watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR
    )
  })
  
  # emtr plots
  output$plot_emtr<- renderPlotly({
    X_results <- req(calculate_income())
    
    X_SQ <- X_results$X_SQ
    X_Reform <- X_results$X_Reform
    
    compare_plots(X_SQ, X_Reform, type = "EMTR", min_rate = 0, max_rate = 1.1,
      inc_limit = X_results$max_income, title = "EMTR comparison",
      policy_name1 = 'Status Quo', policy_name2 = 'Reform',
      watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR
    )
  })
  
  # replacement rate plot
  output$plot_replacement_rate <- renderPlotly({
    X_results <- req(calculate_income())
    
    X_SQ <- X_results$X_SQ
    X_Reform <- X_results$X_Reform
    
    compare_plots(X_SQ, X_Reform, type = "RR", min_rate = 0, max_rate = 1.1,
      inc_limit = X_results$max_income, title = "Replacement Rate comparison",
      policy_name1 = 'Status Quo', policy_name2 = 'Reform', watermark = FALSE,  
      weeks_in_year = WEEKS_IN_YEAR
    )
  })
  
  # participation tax rate plot
  output$plot_participation_tax_rate <- renderPlotly({
    X_results <- req(calculate_income())
    
    X_SQ <- X_results$X_SQ
    X_Reform <- X_results$X_Reform
    
    compare_plots(X_SQ, X_Reform, type = "PTR", min_rate = 0, max_rate = 1.1,
      inc_limit = X_results$max_income, title = "Participation Tax Rate comparison",
      policy_name1 = 'Status Quo', policy_name2 = 'Reform',
      watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR
    )
  })
  
  ########################################## 
  ### Poverty impact Tab
  ########################################## 
  
  # equivalised income plots
  output$plot_equivincome<- renderPlotly({
    X_results <- req(calculate_income())
    
    X_SQ <- X_results$X_SQ
    X_Reform <- X_results$X_Reform
    
    compare_equiv_income_plot(X_SQ, X_Reform, inc_limit = X_results$max_income,
                            title = "Equivalised income comparison", policy_name1 = 'Status Quo',
                            policy_name2 = 'Reform', watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR
    )
  })
  
  # BHC poverty depth plots
  output$plot_bhc_depth<- renderPlotly({
    X_results <- req(calculate_income())
    
    X_SQ <- X_results$X_SQ
    X_Reform <- X_results$X_Reform
    
    poverty_depth_plot(X_SQ, X_Reform, inc_limit = X_results$max_income,
                       policy_name1 = 'Status Quo',
                       policy_name2 = 'Reform', watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR,
                       type = "BHC"
    )
  })
  
  # AHC poverty depth plots
  
  output$plot_ahc_depth<- renderPlotly({
    X_results <- req(calculate_income())
    
    X_SQ <- X_results$X_SQ
    X_Reform <- X_results$X_Reform
    
    poverty_depth_plot(X_SQ, X_Reform, inc_limit = X_results$max_income,
                       policy_name1 = 'Status Quo',
                       policy_name2 = 'Reform', watermark = FALSE, weeks_in_year = WEEKS_IN_YEAR,
                       type = "AHC"
    )
  })
  
  ########################################## 
  ### Income compostion tab
  ########################################## 
  
  # income composition plots
  output$plot_incomecomposition_SQ<- renderPlotly({
    X_results <- req(calculate_income())
    X_SQ <- X_results$X_SQ
    amounts_net_plot(
      X_SQ,
      inc_limit = X_results$max_income,
      y_min = X_results$min_y,
      y_max = X_results$max_net_income,
      display_cols = input$income_types
    )
  })
  
  output$plot_incomecomposition_Reform <- renderPlotly({
    X_results <- req(calculate_income())
    X_Reform <- X_results$X_Reform
    amounts_net_plot(
      X_Reform,
      inc_limit = X_results$max_income,
      y_min = X_results$min_y,
      y_max = X_results$max_net_income,
      display_cols = input$income_types
    )
  })
  
  ########################################## 
  ### Parameters tab
  ##########################################  
  
  # display changed parameters 
  output$changed_parameters <- renderTable({
    parameters = reload_data()
    if (is.null(input$parameters_SQ) | is.null(input$parameters_Reform)) {
      return()
    }

    check_for_changed_parameters(
      parameters$parameters_SQ, parameters$parameters_Reform
    )
  })
  
  ########################################## 
  ### Outputs
  ##########################################
  
  # Download everything 
  output$downloadData <- downloadHandler(
    filename = function() {
      "IncomeExplorerResults.xlsx"
    },
    content = function(file) {
      X_results <- calculate_income()
      parameters = reload_data()
      
      wb <- createWorkbook()
      
      # Details of the example family and input files
      details <- c(
        SQ_file = input$parameters_SQ$name,
        Reform_file = input$parameters_Reform$name,
        HourlyWage = input$wage1_hourl,
        Partnered = input$Partnered,
        Partner_HourlyWage = input$gross_wage2*(input$Partnered == 1),
        Partner_HoursWorked = input$hours2*(input$Partnered == 1),
        Accomodation_Costs = input$AS_Accommodation_Costs,
        Accomodation_Type = input$Acc_type,
        AS_Area = input$AS_Area,
        Children_Ages = input$Children_ages
      )
      
      addWorksheet(wb, 'Details')
      writeData(wb, 'Details', names(details), startCol=1)
      writeData(wb, 'Details', details, startCol=2)
      
      # Parameters that changed
      parameter_differences <- check_for_changed_parameters(
        parameters$parameters_SQ, parameters$parameters_Reform
      )
      addWorksheet(wb, 'Scenario Differences')
      writeData(wb, 'Scenario Differences', parameter_differences)
      
      # Full sets of results (should probably be more selective)
      addWorksheet(wb, 'SQ')
      writeData(wb, 'SQ', X_results$X_SQ)
      addWorksheet(wb, 'Reform')
      writeData(wb, 'Reform', X_results$X_Reform)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
})
