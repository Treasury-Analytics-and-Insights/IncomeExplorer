#
# This is the user-interface definition of the Income Explorer Shiny web application. You can
# run the application by clicking 'Run App' above.
#
###############################
# Libraries
###############################

suppressMessages({
  library(shiny)
  library(shinyjs)
  library(shinythemes)
  library(plotly)
  library(rhandsontable)
  library(data.table)
})

# TY24 minimum wage
DEFAULT_HOURLY_WAGE <- 22.70

# Define UI
shinyUI(fluidPage(
  # different themes from shinythemes R package, https://rstudio.github.io/shinythemes/
  theme = shinytheme("sandstone"),
  useShinyjs(),
  # Application title
  titlePanel("Income Explorer (version 1.0.0)"),
  # Side menu
  sidebarLayout(
    sidebarPanel(
      width = 4,
      fluidRow(
        column(
          12,
          selectizeInput(
            "selected_scenarios", "Policy Scenarios",
            choices = NULL, selected = NULL, multiple = TRUE,
            options = list(
              placeholder = "Select policy scenarios...",
              plugins = list("remove_button")
            )
          )
        )
      ),
      fluidRow(
        column(
          12, align = "center",
          fileInputButton(
            "upload_scenarios_button", buttonLabel = "Add scenarios", icon = icon("plus"),
            multiple = TRUE, accept = c(".xlsx", ".xls", ".yaml", ".yml"),
          ),
          actionButtonLoading(
            "download_params_button", "Download scenarios", icon = icon("download")
          ),
          actionButtonLoading(
            "download_results_button", "Download Results", icon = icon("download")
          )
        )
      ),
      
      hr(style = "border-top: 1px solid #ccc"),
      
      # Input the hourly wage and hours
      fluidRow(
        column(6, numericInput(
          "wage1_hourly", "Hourly wage ($)",
          value = DEFAULT_HOURLY_WAGE, step = .1
        )),
        column(6, numericInput("max_hours", "Max hours/week", "50"))
      ),
      
      # Input accomodation cost settings
      fluidRow(
        column(4, selectInput(
          "Acc_type", label = "Accommodation type",
          choices = c("Renting", "Mortgage"), selected = "Renting"
        )),
        column(5, numericInput(
          "AS_Accommodation_Costs", "Weekly housing cost ($)",
          min = 0, max = 2000, value = 450, step = 5
        )),
        column(3, selectInput(
          "AS_Area", label = "AS area code",
          choices = c(1, 2, 3, 4), selected = 2
        ))
      ),
      # Input the children's ages
      textInput(
        "Children_ages",
        "Age of children (e.g. '1, 4' or leave blank)",
        "0, 10"
      ),
      # Input partner status
      checkboxInput("Partnered", "Partnered", value = FALSE),
      # Input parter wage details, note that this is only
      # displayed if there is a partner
      fluidRow(
        column(6, hidden(numericInput(
          "gross_wage2", "Partner's hourly wage ($)",
          min = 15, max = 100,
          value = DEFAULT_HOURLY_WAGE, step = .1
        ))),
        column(6, hidden(
          numericInput(
            "hours2", "Partner's hours worked",
            min = 0, max = 80, value = 0, step = 5
          )
        ))
      )
    ),
    # Main panel containing plots etc.
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Net Income",
          br(),
          plotlyOutput("plot_netincome", height = "500px")
        ),
        tabPanel(
          "EMTR",
          br(),
          plotlyOutput("plot_emtr", height = "500px")
        ),
        tabPanel(
          "RR",
          br(),
          plotlyOutput("plot_replacement_rate", height = "500px")
        ),
        tabPanel(
          "PTR",
          br(),
          plotlyOutput("plot_participation_tax_rate", height = "500px")
        ),
        tabPanel(
          "Income Composition",
          p(style = "margin-bottom: 0.5em"),
          uiOutput("income_composition_tabs")
        ),
        tabPanel("Policy Changes", tableOutput("changed_parameters")),
        tabPanel(
          "About",
          mainPanel(
            h2("Overview"),
            p(
              "This app is designed to support policy analysts in understanding the relationship between gross income and net income due to the Tax and Welfare policies of New Zealand.",
              "The app considers a theoretical family, and calculates various measures of work incentives, including net income, effective marginal tax rates (EMTR's), replacement rates (RR's), and participation tax rates (PTR's)."
            ),
            h2("Usage"),
            tags$ul(
              tags$li("The app is preloaded with policy scenarios from the most recently released Economic & Fiscal Update (EFU) from the New Zealand Treasury. You can select any number of these scenarios by clicking in the 'Select policy scenarios...' selection field and then selecting the drop-down list that appears."),
              tags$li("Selected policy scenarios can then be downloaded in Excel format by clicking the 'Download Scenarios' button. These can be edited, and then uploaded back into the app using the 'Add Scenarios' button."),
              tags$li("The full calculation results can be downloaded in Excel format by clicking the 'Download Results' button."),
              tags$li("The app allows you to customise the example family by setting parameters relating to their earnings, accommodation costs, number of children, and whether the primary earner has a partner or not and what their partner's earnings are.")
            ),
            h2("Disclaimer"),
            p(
              "The IncomeExplorer app was developed by the Analytics & Insights team in the New Zealand Treasury's Office of the Chief Economic Adviser.",
              "It is provided as-is, for research purposes only, with absolutely no warranty or guarantee of correctness.",
              "The public version of this app should not be used for any sensitive work.",
              "Please contact the Analytics & Insights team if you require assistance in running the app in a secure environment."
            )
          )
        )
      )
    )
  )
))
