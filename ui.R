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
      fluidRow(column(12, selectizeInput("select_scenarios", "Scenarios", choices = NULL, selected = NULL, multiple = TRUE))),
      fluidRow(
        column(
          12, align = "right",
          actionButton("upload_scenarios_button", "Add scenarios", icon = shiny::icon("plus")),
          downloadButton("download_params_button", "Scenarios"),
          downloadButton("download_results_button", "Results")
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
        column(6, conditionalPanel(
          condition = "input.Partnered == 1",
          numericInput(
            "gross_wage2", "Partner's hourly wage ($)",
            min = 15, max = 100,
            value = DEFAULT_HOURLY_WAGE, step = .5
          )
        )),
        column(6, conditionalPanel(
          condition = "input.Partnered == 1",
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
        tabPanel("Policy Changes", tableOutput("changed_parameters"))
      )
    )
  )
))
