#
# This is the server logic of the Income Explorer Shiny web application. You can
# run the application by clicking 'Run App' above.
#

suppressMessages({
  library(shiny)
  library(shinyjs)
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
      enable("download_params_button")
      enable("download_results_button")
    } else {
      disable("download_params_button")
      disable("download_results_button")
    }
  })
  
  # Show a modal dialog when the upload_scenario_button is clicked
  observeEvent(input$upload_scenarios_button, {
    showModal(
      modalDialog(
        fileInput(
          "upload_scenario_files", label = "Upload scenarios", buttonLabel = "Browse",
          multiple = TRUE, accept = c('.xlsx'),
        ), footer = modalButton("Close"), easyClose = TRUE
      )
    )
  })
  
  # Add a scenario to the list when a new scenario file is uploaded
  observeEvent(input$upload_scenario_files, {
    new_scenarios <- input$upload_scenario_files
    req(new_scenarios)
    new_scenario_names <- new_scenarios$name %>% tools::file_path_sans_ext()
    overlapping_names <- intersect(new_scenario_names, names(scenarios$files))
    for (overlapping_name in overlapping_names) {
      # Avoid using any existing names
      new_scenario_names[new_scenario_names == overlapping_name] <-
        paste0("Uploaded_", overlapping_name)
    }
    new_scenario_files <- as.list(new_scenarios$datapath)
    names(new_scenario_files) <- new_scenario_names
    
    scenarios$newly_uploaded <- new_scenario_names
    scenarios$files <- c(new_scenario_files, scenarios$files)
    
    removeModal()
  })
  
  #######################################################
  # Loading parameters from files and calculating incomes
  #######################################################
  
  get_params <- reactive({
    selected_scenarios <- req(input$select_scenarios)
    params_files <- req(scenarios$files[selected_scenarios])
    params <- lapply(params_files, parameters_from_file)
    names(params) <- selected_scenarios
    return(params)
  })
  
  get_scenario_income <- function(params) {
    scenario_income <- calculate_income(
      # System parameters
      params,
      # Family parameters
      max_hours = input$max_hours,
      hourly_wage = input$wage1_hourly,
      children_ages = input$Children_ages,
      # Partner parameters
      partnered = input$Partnered,
      partner_wages = input$gross_wage2,
      partner_hours = input$hours2,
      # Accommodation parameters
      accommodation_type = input$Acc_type,
      as_accommodation_costs = input$AS_Accommodation_Costs,
      as_area = as.numeric(input$AS_Area),
      # Presentation parameters
      steps_per_dollar = 1L,
      weeks_in_year = 52L
    )
    return(scenario_income)
  }
  
  #### Join cached incomes together as one data.table ####
  get_scenario_incomes <- reactive({
    params <- get_params()
    scenario_income_list <- lapply(params, get_scenario_income)
    names(scenario_income_list) <- names(params)
    scenario_incomes <- rbindlist(scenario_income_list, idcol = "Scenario")
    return(scenario_incomes)
  })
  
  #### Net Income plot ####
  output$plot_netincome <- renderPlotly({
    X_results <- req(get_scenario_incomes())
    output_plot <- compare_net_income_plot(X_results)
    return(output_plot)
  })
  
  #### EMTR plot ####
  output$plot_emtr <- renderPlotly({
    X_results <- req(get_scenario_incomes())
    output_plot <- plot_rates(X_results, "EMTR", "Effective Marginal Tax Rate")
    return(output_plot)
  })
  
  #### RR plot ####
  output$plot_replacement_rate <- renderPlotly({
    X_results <- req(get_scenario_incomes())
    output_plot <- plot_rates(X_results, "Replacement_Rate", "Replacement Rate")
    return(output_plot)
  })
  
  #### PTR plot ####
  output$plot_participation_tax_rate <- renderPlotly({
    X_results <- req(get_scenario_incomes())
    output_plot <- plot_rates(X_results, "Participation_Tax_Rate", "Participation Tax Rate")
    return(output_plot)
  })
  
  #### Income composition plots ####
  selected_income_composition_tab <- reactiveValues(tab = NULL)
  
  # Render a tab for each scenario
  output$income_composition_tabs <- renderUI({
    do.call(tabsetPanel, c(
      id = "income_composition_tabs",
      lapply(input$select_scenarios, function(scenario) {
        tabPanel(
          scenario,
          plotlyOutput(paste0("plot_income_composition_", scenario), height = "500px")
        )
      }),
      selected = selected_income_composition_tab$tab, type = "pills"
    ))
  })
  
  # When the input selection changes, update the plot for each scenario
  observeEvent(input$select_scenarios, {
    for (scenario in input$select_scenarios) {
      plot_id <- paste0("plot_income_composition_", scenario)
      observe({
        X_results <- req(get_scenario_incomes())
        output[[plot_id]] <- renderPlotly({
          income_composition_plot(X_results[Scenario == scenario])
        })
      })
    }
    # Remember which tab we were on
    selected_income_composition_tab$tab <- input$income_composition_tabs
  })
  
  #### Changed parameters ####
  output$changed_parameters <- renderTable({
    get_parameter_changes(get_params())
  })
  
  #### Download parameters for all selected scenarios, in a single zip file ####
  output$download_params_button <- downloadHandler(
    filename = function() {
      if (length(input$select_scenarios) == 1) {
        paste0(input$select_scenarios, ".xlsx")
      } else {
        "Scenarios.zip"
      }
    },
    content = function(file) {
      if (length(input$select_scenarios) == 1) {
        file.copy(scenarios$files[[input$select_scenarios]], file)
      } else {
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        selected_file_names <- input$select_scenarios
        for (selected_file_name in selected_file_names) {
          selected_file_path <- scenarios$files[[selected_file_name]]
          output_file_path <- file.path(temp_directory, paste0(selected_file_name, ".xlsx"))
          file.copy(selected_file_path, output_file_path)
        }
        zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
      }
    }
  )
  
  #### Download everything ####
  output$download_results_button <- downloadHandler(
    filename = function() {
      "IncomeExplorerResults.xlsx"
    },
    content = function(file) {
      scenario_incomes <- get_scenario_incomes()
      scenario_names <- scenario_incomes[, unique(Scenario)]
      
      parameter_differences <- get_parameter_changes(get_params())
      
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
      
      openxlsx::addWorksheet(wb, "Details")
      openxlsx::writeData(wb, "Details", names(details), startCol = 1)
      openxlsx::writeData(wb, "Details", details, startCol = 2)
      
      if (length(scenario_names) > 1) {
        # Parameters that changed
        openxlsx::addWorksheet(wb, "Scenario Differences")
        openxlsx::writeData(wb, "Scenario Differences", parameter_differences)
      }
      
      # Full sets of results (should probably be more selective)
      for (scenario in scenario_names) {
        openxlsx::addWorksheet(wb, scenario)
        openxlsx::writeData(wb, scenario, scenario_incomes[Scenario == scenario])
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
})
