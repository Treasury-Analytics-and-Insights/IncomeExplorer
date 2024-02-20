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
  
  #### Selecting and/or Uploading parameter files ####
  # Get default parameter files for scenarios
  default_files <- list.files(
    path = file.path(system.file(package = "IncomeExplorer"), "MFTC_calculator/App_Parameters"),
    pattern = "*.xlsx", full.names = TRUE
  )
  default_files_names <- basename(default_files) %>% tools::file_path_sans_ext()
  names(default_files) <- default_files_names
  
  # Store scenario files in a reactive variable
  scenarios <- reactiveValues(files = default_files, newly_uploaded = NULL)
  
  # Update selection inputs when the available scenario files change
  observeEvent(scenarios$files, {
    selected_files <- c(input$select_scenarios, scenarios$newly_uploaded)
    updateSelectizeInput(
      session, "select_scenarios",
      choices = names(scenarios$files), selected = selected_files
    )
  })
  
  # Enable download buttons only when a selection exists
  observe({
    if (!is.null(input$select_scenarios)) {
      shinyjs::enable("download_params_button")
      shinyjs::enable("download_results_button")
    } else {
      shinyjs::disable("download_params_button")
      shinyjs::disable("download_results_button")
    }
  })
  
  # Show a modal dialog when the upload_scenario_button is clicked
  observeEvent(input$upload_scenario_button, {
    showModal(
      modalDialog(
        fileInput(
          "upload_scenario_file", label = "Upload scenario", buttonLabel = "Browse",
          multiple = FALSE, accept = c('.xlsx'),
        ), footer = modalButton("Close"), easyClose = TRUE
      )
    )
  })
  
  # Add a scenario to the list when a new scenario file is uploaded
  observeEvent(input$upload_scenario_file, {
    new_scenario <- input$upload_scenario_file
    req(new_scenario)
    new_scenario_name <- new_scenario$name %>% tools::file_path_sans_ext()
    if (new_scenario_name %in% names(scenarios$files)) {
      # Avoid using an existing name
      new_scenario_name <- paste0("Uploaded_", new_scenario_name)
    }
    new_scenario_file <- list(new_scenario$datapath)
    names(new_scenario_file) <- new_scenario_name
    
    scenarios$newly_uploaded <- new_scenario_name
    scenarios$files <- c(new_scenario_file, scenarios$files)
    
    removeModal()
  })
  
  #######################################################
  # Loading parameters from files and calculating incomes
  #######################################################
  
  parameters <- eventReactive(input$select_scenarios, {
    parameter_files <- scenarios$files[input$select_scenarios]
    parameters <- lapply(parameter_files, parameters_from_file)
    names(parameters) <- input$select_scenarios
    print("Reloaded parameters")
    return(parameters)
  })
  
  # calculate incomes
  calculate_income <- reactive({
    params_list <- parameters()
    req(length(params_list) > 0)
    
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
        max_wage = MAX_WAGE, steps_per_dollar = 1L, weeks_in_year = 52L,
        MFTC_WEP_scaling = as.numeric(input$MFTC_WEP_scaling)
      )
      return(emtr_df)
    }
    
    X_results <- lapply(params_list, hot_emtr)
    names(X_results) <- names(params_list)
    
    X_results <- rbindlist(X_results, idcol = "Scenario")
    
    # MFTC is meant to make families always better off being off-benefit than staying
    # on a benefit. We let the user set whether the family stays on benefit
    # or gets IWTC when they work, with the parameter input$WFFBEN_SQ. This can be:
    # "Max" - choose the option which maximises the family income.
    # "WFF" - go off the benefit while working, and get IWTC + MFTC.
    # "Benefit" - stay on the benefit while working, and never get IWTC or MFTC
    #             (benefit abates away as earned income increases).
    # Note that these are only applicable when beneficiaries are ineligible for IWTC;
    # MFTC eligibility currently depends on IWTC eligibility in the `emtr` function.
    
    scenario_names <- X_results[, unique(Scenario)]
    
    if (input$WFFBEN_SQ != "WFF") {
      SQ_params_with_no_IWTC <- remove_IWTC_from_params(params_list[[1]])
      X_SQ <- X_results[Scenario == first(Scenario)]
      X_SQ_without_IWTC <- hot_emtr(SQ_params_with_no_IWTC)
      
      if (input$WFFBEN_SQ == "Max") {
        # Choose which of benefit or IWTC gives max net income
        X_SQ <- choose_IWTC_or_benefit(X_SQ, X_SQ_without_IWTC)
        
      } else if (input$WFFBEN_SQ == "Benefit") {
        X_SQ <- X_SQ_without_IWTC
      }
      X_SQ[, Scenario := scenario_names[1]]
      X_results <- rbind(X_results[Scenario != first(Scenario)], X_SQ, fill = TRUE)
    }
    
    if (length(scenario_names) > 1) {
      for (scenario_name in scenario_names[2:length(scenario_names)]) {
        if (input$WFFBEN_reform != "WFF") {
          Reform_params_with_no_IWTC <- remove_IWTC_from_params(params_list[[scenario_name]])
          X_Reform <- X_results[Scenario == scenario_name]
          X_Reform_without_IWTC <- hot_emtr(Reform_params_with_no_IWTC)
          
          if (input$WFFBEN_reform == "Max") {
            # Choose which of benefit or IWTC gives max net income
            X_Reform <- choose_IWTC_or_benefit(X_Reform, X_Reform_without_IWTC)
            
          }  else if (input$WFFBEN_reform == "Benefit") {
            X_Reform <- X_Reform_without_IWTC
          }
          X_Reform[, Scenario := scenario_name]
          X_results <- rbind(X_results[Scenario != scenario_name], X_Reform, fill = TRUE)
        }
      }
    }
    
    X_results[, Scenario := factor(Scenario, levels = scenario_names)]
    
    return(X_results)
  })
  
  #### Net Income plot ####
  output$plot_netincome <- renderPlotly({
    X_results <- req(calculate_income())
    output_plot <- compare_net_income_plot(X_results)
    print("Plotting Net Income")
    return(output_plot)
  })
  
  #### EMTR plot ####
  output$plot_emtr <- renderPlotly({
    X_results <- req(calculate_income())
    output_plot <- plot_rates(X_results, "EMTR", "Effective Marginal Tax Rate")
    print("Plotting EMTR")
    return(output_plot)
  })
  
  #### RR plot ####
  output$plot_replacement_rate <- renderPlotly({
    X_results <- req(calculate_income())
    output_plot <- plot_rates(X_results, "Replacement_Rate", "Replacement Rate")
    print("Plotting RR")
    return(output_plot)
  })
  
  #### PTR plot ####
  output$plot_participation_tax_rate <- renderPlotly({
    X_results <- req(calculate_income())
    output_plot <- plot_rates(X_results, "Participation_Tax_Rate", "Participation Tax Rate")
    print("Plotting PTR")
    return(output_plot)
  })
  
  #### Income composition plots ####
  selected_income_composition_tab <- reactiveValues(tab = NULL)
  
  output$income_composition_tabs <- renderUI({
    current_tab <- isolate(selected_income_composition_tab$tab)
    do.call(tabsetPanel, c(
      id = "income_composition_tabs",
      lapply(input$select_scenarios, function(scenario) {
        tabPanel(
          scenario,
          plotlyOutput(paste0("plot_income_composition_", scenario), height = "500px")
        )
      }),
      selected = current_tab, type = "pills"
    ))
  })
  
  observeEvent(input$select_scenarios, {
    for (scenario in input$select_scenarios) {
      plot_id <- paste0("plot_income_composition_", scenario)
      observe({
        X_results <- req(calculate_income())
        output[[plot_id]] <- renderPlotly({
          amounts_net_plot(X_results[Scenario == scenario])
        })
      })
    }
    selected_income_composition_tab$tab <- input$income_composition_tabs
  })
  
  #### Changed parameters ####
  param_changes <- eventReactive(parameters(), {
    scenarios <- input$select_scenarios
    params_list <- parameters()
    changes <- get_changes(params_list)
    return(changes)
  })
  
  output$changed_parameters <- renderTable({
    param_changes()
  })
  
  #### Download parameters for all selected scenarios, in a single zip file ####
  output$download_params_button <- downloadHandler(
    filename = function() {
      "Scenarios.zip"
    },
    content = function(file) {
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      selected_file_names <- input$select_scenarios
      for (selected_file_name in selected_file_names) {
        selected_file_path <- scenarios$files[[selected_file_name]]
        output_file_path <- file.path(temp_directory, paste0(selected_file_name, ".xlsx"))
        file.copy(selected_file_path, output_file_path)
      }
      zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
    },
    contentType = "application/zip"
  )
  
  #### Download everything ####
  output$download_results_button <- downloadHandler(
    filename = function() {
      "IncomeExplorerResults.xlsx"
    },
    content = function(file) {
      X_results <- calculate_income()
      parameter_differences <- param_changes()
      
      wb <- openxlsx::createWorkbook()
      
      # Details of the example family and input files
      details <- c(
        HourlyWage = input$wage1_hourl,
        Partnered = input$Partnered,
        Partner_HourlyWage = input$gross_wage2*(input$Partnered == 1),
        Partner_HoursWorked = input$hours2*(input$Partnered == 1),
        Accomodation_Costs = input$AS_Accommodation_Costs,
        Accomodation_Type = input$Acc_type,
        AS_Area = input$AS_Area,
        Children_Ages = input$Children_ages
      )
      
      openxlsx::addWorksheet(wb, 'Details')
      openxlsx::writeData(wb, 'Details', names(details), startCol=1)
      openxlsx::writeData(wb, 'Details', details, startCol=2)
      
      # Parameters that changed
      openxlsx::addWorksheet(wb, 'Scenario Differences')
      openxlsx::writeData(wb, 'Scenario Differences', parameter_differences)
      
      # Full sets of results (should probably be more selective)
      for (scenario in X_results[, unique(Scenario)]) {
        openxlsx::addWorksheet(wb, scenario)
        openxlsx::writeData(wb, scenario, X_results[Scenario == scenario])
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
})
