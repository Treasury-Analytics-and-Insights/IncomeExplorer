#
# This is the server logic of the Income Explorer Shiny web application. You can
# run the application by clicking 'Run App' above.
#

suppressMessages({
  library(shiny)
  library(shinyjs)
  library(shinyvalidate)
  library(openxlsx)
  library(magrittr)
  library(data.table)
  library(zip)
})

# Define server logic required to run the app
shinyServer(function(input, output, session) {
  ##########################################
  # Pop up warning on start 
  ##########################################
  # the modal dialog where the user can enter the query details.
  warning_modal <- modalDialog(
    title = "Disclaimer", 
    paste0(
      "This app analyses the effect of New Zealand's tax and welfare system on an example family's net income and work incentives. The app is provided as-is and for research purposes only. Despite reasonable measures taken to ensure quality and accuracy, the Treasury makes no warranty, or guarantee, express or implied, nor assumes any legal liability or responsibility for the accuracy, correctness, completeness or use of any information that is provided through the app."
    ),
    footer = actionButton("close_warning", "Acknowledge"),
    easyClose = FALSE, fade = FALSE
  )
  
  # Show the model on start up ...
  showModal(warning_modal)
  
  observeEvent(input$Partnered, {
    if (input$Partnered) {
      show("gross_wage2")
      show("hours2")
    } else {
      hide("gross_wage2")
      hide("hours2")
    }
  })
  
  # Input validation
  iv <- InputValidator$new()
  iv$add_rule("wage1_hourly", sv_gt(0))
  iv$add_rule("max_hours", sv_gt(0))
  iv$add_rule("AS_Accommodation_Costs", sv_gte(0))
  iv$add_rule("gross_wage2", sv_gte(0))
  iv$add_rule("hours2", sv_gte(0))
  
  # Regex for validating children's ages input:
  # ^: start
  # (\\s*([0-9]|1[0-7]): match 0-17 with optional leading space,
  # (\\s*,\\s*([0-9]|1[0-7]))*: repeat for multiple ages
  # (,\\s*)?: optional trailing comma
  # $: end
  # - Allows ages 0-17 (inclusive)
  # - Accepts multiple ages separated by commas
  # - Permits optional whitespace around numbers and commas
  # - Allows a trailing comma with optional whitespace
  # - Accepts blank input
  # - Rejects non-numeric input and ages 18+
  children_ages_regex <-
    "^(\\s*([0-9]|1[0-7])(\\s*,\\s*([0-9]|1[0-7]))*\\s*(,\\s*)?)?$"
  iv$add_rule(
    "Children_ages",
    sv_regex(
      pattern = children_ages_regex,
      message = "Children must be aged between 0 and 17"
    )
  )
  
  iv$enable()
  
  #### Selecting and/or Uploading parameter files ####
  # Get default parameter files for scenarios
  default_files <- list.files(
    path = "inst/parameters", pattern = glob2rx("*.yaml"), full.names = TRUE
  )
  default_files_names <- basename(default_files) %>% tools::file_path_sans_ext()
  names(default_files) <- default_files_names
  
  # Store scenario files in a reactive variable
  all_scenarios <- reactiveValues(names = default_files_names, paths = default_files)
  loaded_scenarios <- reactiveValues(names = list(), params = list(), incomes = list())
  
  # When the warning dialog is closed, update the selection to add
  # default scenarios as choices - this is a workaround for a shinylive issue
  # because with normal R shiny the observeEvent after this one updates it for us
  observeEvent(input$close_warning, {
    updateSelectizeInput(
      session, "selected_scenarios",
      choices = all_scenarios$names, selected = NULL
    )
    removeModal()
  })
  
  # Add a scenario to the list when a new scenario file is uploaded
  observeEvent(input$upload_scenarios_button, {
    new_scenarios <- req(input$upload_scenarios_button)
    
    # Validate uploaded files
    valid_scenarios <- rep(0, length(new_scenarios$name))
    for (ii in seq_along(new_scenarios$name)) {
      new_scenario_path <- new_scenarios$datapath[ii]
      params <- tryCatch(
        expr = {
          parameters_from_file(new_scenario_path)
        },
        error = function(e) {
          return(NULL)
        }
      )
      if (is.null(params)) {
        warning("Invalid parameters uploaded:", new_scenarios$name[ii])
      } else {
        valid_scenarios[ii] <- TRUE
      }
    }
    
    # Warn about any invalid scenarios
    if (length(valid_scenarios[valid_scenarios == FALSE]) > 0) {
      invalid_upload_modal <- modalDialog(
        title = "Invalid scenario files",
        sprintf(
          "These uploaded scenario files are invalid and have been ignored: \n%s",
          paste(new_scenarios$name[valid_scenarios == FALSE], collapse = ", ")
        ),
        footer = modalButton("Dismiss"),
        easyClose = FALSE, fade = FALSE
      )
      showModal(invalid_upload_modal)
    }
    
    # Subset to valid scenarios
    new_scenarios <- lapply(new_scenarios, function(x) x[valid_scenarios == TRUE])
    
    new_scenario_names <- new_scenarios$name %>% tools::file_path_sans_ext()
    overlapping_names <- intersect(new_scenario_names, all_scenarios$names)
    for (overlapping_name in overlapping_names) {
      # Avoid using any existing names
      new_scenario_names[new_scenario_names == overlapping_name] <-
        paste0("Uploaded_", overlapping_name)
    }
    new_scenario_files <- as.list(new_scenarios$datapath)
    names(new_scenario_files) <- new_scenario_names
    
    all_scenarios$names <- c(new_scenario_names, all_scenarios$names)
    all_scenarios$paths <- c(new_scenario_files, all_scenarios$paths)
    
    # Add the new scenarios to the current selection
    new_selection <- c(input$selected_scenarios, new_scenario_names)
    updateSelectizeInput(
      session, "selected_scenarios",
      choices = all_scenarios$names, selected = new_selection
    )
  })
  
  #######################################################
  # Loading parameters from files and calculating incomes
  #######################################################
  
  # Check for newly selected scenarios, and load them
  # Note that incomes are loaded as "reactive" values,
  # and will be recalculated if family parameters are changed
  observe({
    req(iv$is_valid())
    if (length(loaded_scenarios$params) > 0) {
      loaded_scenarios_names <- names(loaded_scenarios$params)
    } else {
      loaded_scenarios_names <- c()
    }
    newly_selected_scenarios <- setdiff(input$selected_scenarios, loaded_scenarios_names)
    if (length(newly_selected_scenarios) > 0) {
      lapply(newly_selected_scenarios, function(newly_selected_scenario) {
        new_params <- parameters_from_file(all_scenarios$paths[[newly_selected_scenario]])
        new_income <- reactive({get_scenario_income(new_params)})
        loaded_scenarios$params[[newly_selected_scenario]] <- new_params
        loaded_scenarios$incomes[[newly_selected_scenario]] <- new_income
      })
      # If on the "About" tab, change to the "Net Income" tab to show some results
      if (input$resultsTabset == "About") {
        updateTabsetPanel(session, "resultsTabset", selected = "Net Income")
      }
    }
    # Check for any loaded scenarios that are un-selected, and delete them
    unselected_scenarios <- setdiff(loaded_scenarios_names, input$selected_scenarios)
    if (length(unselected_scenarios) > 0) {
      loaded_scenarios$params[unselected_scenarios] <- NULL
      loaded_scenarios$incomes[unselected_scenarios] <- NULL
    }
    # Set loaded order based on selection order
    loaded_scenarios$names <- input$selected_scenarios
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
  
  #### Join incomes together as one data.table ####
  get_scenario_incomes <- reactive({
    req(loaded_scenarios$incomes)
    req(iv$is_valid())
    # Index into the loaded incomes using the selection order rather than loaded order
    loaded_scenario_incomes <- loaded_scenarios$incomes[loaded_scenarios$names]
    scenario_incomes_list <- lapply(loaded_scenario_incomes, function(x) x())
    scenario_incomes <- rbindlist(scenario_incomes_list, idcol = "Scenario")
    return(scenario_incomes)
  })
  
  # An empty placeholder plot for when no scenarios are selected
  # This is for shinylive which needs empty plots otherwise it errors
  empty_plot <- plot_ly(type = "scatter", mode = "none") %>% layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
  )
  
  #### Net Income plot ####
  output$plot_netincome <- renderPlotly({
    X_results <- req(get_scenario_incomes())
    if (nrow(X_results) > 0) {
      output_plot <- plot_net_income(X_results)
    } else {
      output_plot <- empty_plot
    }
    return(output_plot)
  })
  
  #### EMTR plot ####
  output$plot_emtr <- renderPlotly({
    X_results <- req(get_scenario_incomes())
    if (nrow(X_results) > 0) {
      output_plot <- plot_rates(X_results, "EMTR", "Effective Marginal Tax Rate")
    } else {
      output_plot <- empty_plot
    }
    return(output_plot)
  })
  
  #### RR plot ####
  output$plot_replacement_rate <- renderPlotly({
    X_results <- req(get_scenario_incomes())
    if (nrow(X_results) > 0) {
      output_plot <- plot_rates(X_results, "RR", "Replacement Rate")
    } else {
      output_plot <- empty_plot
    }
    return(output_plot)
  })
  
  #### PTR plot ####
  output$plot_participation_tax_rate <- renderPlotly({
    X_results <- req(get_scenario_incomes())
    if (nrow(X_results) > 0) {
      output_plot <- plot_rates(X_results, "PTR", "Participation Tax Rate")
    } else {
      output_plot <- empty_plot
    }
    return(output_plot)
  })
  
  #### Income composition plots ####
  selected_income_composition_tab <- reactiveValues(tab = NULL)
  
  # Render a tab for each scenario
  output$income_composition_tabs <- renderUI({
    do.call(tabsetPanel, c(
      id = "income_composition_tabs",
      lapply(input$selected_scenarios, function(scenario) {
        tabPanel(
          scenario,
          plotlyOutput(paste0("plot_income_composition_", scenario), height = "500px")
        )
      }),
      selected = selected_income_composition_tab$tab, type = "pills"
    ))
  })
  
  # When the input selection changes, update the plot for each scenario
  observeEvent(input$selected_scenarios, {
    lapply(input$selected_scenarios, function(scenario) {
      plot_id <- paste0("plot_income_composition_", scenario)
      output[[plot_id]] <- renderPlotly({
        X_results <- req(get_scenario_incomes())
        if (nrow(X_results) > 0) {
          y_min <- 52*min(X_results[, wage_tax_and_ACC + benefit_tax])
          y_max <- 52*max(X_results[, Net_Income])
          output_plot <- plot_income_composition(
            X_results[Scenario == scenario], y_min = y_min, y_max = y_max
          )
        } else {
          output_plot <- empty_plot
        }
        return(output_plot)
      })
    })
    # Remember which tab we were on
    if (!is.null(input$income_composition_tabs)) {
      if (input$income_composition_tabs %in% input$selected_scenarios) {
        # Keep tab we are on since it is still in the selected scenarios
        currently_selected_tab <- input$income_composition_tabs
        selected_income_composition_tab$tab <- currently_selected_tab
      } else if (length(input$selected_scenarios) > 0) {
        # The tab we were on has been deleted, change to the last selected scenario
        num_scenarios <- length(input$selected_scenarios)
        last_selected_scenario <- input$selected_scenarios[num_scenarios]
        selected_income_composition_tab$tab <- last_selected_scenario
      }
    }
  })
  
  #### Display changed parameters ####
  output$changed_parameters <- renderTable({
    # Index into loaded params using the saved selection order
    params <- req(loaded_scenarios$params[loaded_scenarios$names])
    get_parameter_changes(params)
  })
  
  #### Download buttons ####
  
  # Enable download buttons only when a selection exists
  observe({
    if (length(input$selected_scenarios) > 0) {
      enable("download_params_button")
      enable("download_results_button")
    } else {
      disable("download_params_button")
      disable("download_results_button")
    }
  })
  
  # Download scenario parameters
  observeEvent(input$download_params_button, {
    params <- req(loaded_scenarios$params)
    param_names <- names(params)
    param_paths <- all_scenarios$paths[param_names]
    if (length(param_paths) == 0) {
      disable(input$download_params_button)
    } else {
      # Create new excel files from the loaded parameters
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      output_paths <- file.path(temp_directory, paste0(param_names, ".xlsx"))
      names(output_paths) <- param_names
      for (param_name in param_names) {
        save_excel_params(params[[param_name]], output_paths[[param_name]])
      }
      if (length(param_names) == 1) {
        server_file_path <- output_paths[1]
        output_name <- basename(output_paths[1])
      } else {
        server_file_path <- file.path(temp_directory, "Scenarios.zip")
        zip::zip(zipfile = server_file_path, files = dir(temp_directory), root = temp_directory)
        output_name <- "Scenarios.zip"
      }
      js_download_file <- get_js_download_file(
        server_file_path = server_file_path, download_file_name = output_name
      )
      runjs(js_download_file)
      
      updateActionButton(session, "download_params_button", icon = icon("download"))
      enable("download_params_button")
    }
  })
  
  # Download scenario results
  observeEvent(input$download_results_button, {
    params <- req(loaded_scenarios$params)
    if (length(params) == 0) {
      disable(input$download_results_button)
    } else {
      output_path <- "IncomeExplorerResults.xlsx"
      scenario_incomes <- req(get_scenario_incomes())
      parameter_differences <- get_parameter_changes(params)

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
      
      save_app_results(
        details, parameter_differences, scenario_incomes, output_path
      )

      js_download_file <- get_js_download_file(
        server_file_path = output_path, download_file_name = output_path
      )
      runjs(js_download_file)
      
      updateActionButton(session, "download_results_button", icon = icon("download"))
      enable("download_results_button")
    }
  })
})
