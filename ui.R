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
  titlePanel(
    sprintf(
      "Income Explorer Prototype (version %s)", packageVersion("IncomeExplorer")
    )
  ),
  # Side menu
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Policy scenarios"),
      # # Select SQ parameters file
      # selectInput("selectedFileSQ", "Status quo", choices = c("", "Browse...")),
      # # Select reform parameters file
      # selectInput("selectedFileReform", "Reform", choices = c("", "Browse...")),
      fluidRow(
        column(3, align = "center", style = "padding-top: 1em; padding-right: 0;", "Status Quo"),
        column(7, style = "padding-top: 0.5em; padding-left:0; padding-right: 0;", selectInput(
          inputId = "selectedFileSQ", label = NULL,
          choices = c("", "Browse...")
        )),
        column(2, align = "right", disabled(actionButton(
          inputId = paste0("download_", "sq"), label = NULL, icon = shiny::icon("download")
        )))
      ),
      
      fluidRow(
        column(3, align = "center", style = "padding-top: 1em; padding-right: 0;", "Reform 1"),
        column(7, style = "padding-top: 0.5em; padding-left:0; padding-right: 0;", selectInput(
          inputId = "selectedFileReform", label = NULL,
          choices = c("", "Browse...")
        )),
        column(2, align = "right", disabled(actionButton(
          inputId = paste0("download_", "reform1"), label = NULL, icon = shiny::icon("download")
        )))
      ),
      
      fluidRow(
        column(
          12, align = "right",
          disabled(actionButton(inputId = "add_reform", label = "Add", icon = shiny::icon("plus"))),
          disabled(actionButton(inputId = "remove_reform", label = "Remove", icon = shiny::icon("minus"))),
          downloadButton("downloadData", "Download Results")
        )
      ),
      
      hr(style = "border-top: 1px solid #ccc"),
      
      h4("Family options"),
      # Input the hourly wage and hours
      fluidRow(
        column(6, numericInput(
          "wage1_hourly", "Hourly wage ($)",
          min = 18, max = 100,
          value = DEFAULT_HOURLY_WAGE, step = .5
          # pre = "$", sep = ",",
          # animate = FALSE
        )),
        column(6, numericInput(
                 "max_hours", "Max hours/week", "50"
        ))
      ),
      
      # Input accomodation cost settings
      fluidRow(
        column(4, selectInput(
          "Acc_type", label = "Accommodation type",
          choices = c("Renting", "Mortgage"), selected = "Renting"
        )),
        column(5, numericInput(
          "AS_Accommodation_Costs", "Weekly housing cost ($)",
          min = 0, max = 2000, value = 450, step = 25, 
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
      ),
      checkboxInput("Advanced", "Advanced", value = FALSE),
      conditionalPanel(
        condition = "input.Advanced == 1",
        fluidRow(
          column(12, selectInput(
            "MFTC_WEP_scaling", "Winter Energy Payment",
            c("Average week" = 1, "Winter week" = 12/5, "Summer week" = 0),
            selected = "Average week"
          ))
        ),
        # Input to allow the user to choose situations of people
        # stay on benefit, or on WFF or showing the maximum amount
        # between the two options
        fluidRow(
          column(6, selectInput(
            "WFFBEN_SQ", "SQ: work or stay on benefit?",
            c("Max", "WFF", "Benefit"), selected = "Max"
          )),
          column(6, selectInput(
            "WFFBEN_reform", "Reform: work or stay on benefit?",
            c("Max", "WFF", "Benefit"), selected = "Max"
          ))
        )
      ),
    ),
    # Main panel containing plots etc.
    mainPanel(
      tabsetPanel(
        # Plot net income and EMTR
        tabPanel(
          "EMTR",
          h2("Net Income"), plotlyOutput("plot_netincome", height = "300px"),
          h2("Effective Marginal Tax Rate"), plotlyOutput("plot_emtr", height = "300px"),
          h2("Replacement Rate"), plotlyOutput("plot_replacement_rate", height = "300px"),
          h2("Participation Tax Rate"), plotlyOutput("plot_participation_tax_rate", height = "300px")
        ),
       # Plot income composition/budget constraint
        tabPanel(
          "Income composition",
          plotlyOutput("plot_incomecomposition_SQ", height = "400px"),
          plotlyOutput("plot_incomecomposition_Reform", height = "400px")
        ),
        # Table containing the parameters that changed
        tabPanel("Policy Changes", tableOutput("changed_parameters"))
      )
    )
  )
))
