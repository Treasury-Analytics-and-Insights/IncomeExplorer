#
# This is the user-interface definition of the Income Explorer Shiny web application. You can
# run the application by clicking 'Run App' above.
#
###############################
# Libraries
###############################

suppressMessages({
  library(shiny)
  library(shinythemes)
  library(plotly)
  library(rhandsontable)
  library(data.table)
})


#Extract the files names with path for the SQ from the inst folder
file_names <- list.files(path = "inst/MFTC_calculator/App_Parameters/",
                         full.names = TRUE)

names(file_names) <- 
  basename(file_names) |> strsplit("[_.]") |> sapply(function(x) x[2])

# Define UI
shinyUI(fluidPage(
  # different themes from shinythemes R package, https://rstudio.github.io/shinythemes/
  theme = shinytheme("readable"),
  # Application title
  titlePanel("Income Explorer Prototype (version 0.3.0)"),
  
  # Side menu
  sidebarLayout(
    sidebarPanel(
      width = 4,
      # Select SQ parameters file
      selectInput(inputId = 'parameters_SQ',
                  label = 'Choose a Status quo setting of Tax Year',
                  choices = file_names, 
                  selected = file_names[grep("TY22", file_names)]),
      
      # Select reform parameters file
      fileInput('parameters_Reform', 'Reform parameters', accept = c('xlsx')),
      # Download the data used in the app
      downloadButton("downloadData", "Download Results"),
      
      # Input the hourly wage and hours
      fluidRow(
        column(6, numericInput(
          "wage1_hourly", "Hourly wage ($):",
          min = 18, max = 100,
          value = 20, step = .5,
          # pre = "$", sep = ",",
          # animate = FALSE
        )),
        column(6, numericInput(
                 "max_hours", "Max hours/week", "50"
        ))),
        
      # Input to allow the user to choose situations of people
      # stay on benefit, or on WFF or showing the maximum amount
      # between the two options
      fluidRow(
        column(4, selectInput(
          "WFFBEN_SQ", "SQ: WFF or Benefit",
          c("Max", "WFF", "Benefit"), selected = "Max"
        )),
        column(4, selectInput(
          "WFFBEN_reform", "Reform: WFF or Benefit",
          c("Max", "WFF", "Benefit"), selected = "Max"
        )),
        column(4, selectInput(
          "MFTC_WEP_scaling", "WEP:",
          c("Average week" = 1, "Winter week" = 12/5, "Summer week" = 0),
          selected = "Average week"
        ))
      ),
      # Input accomodation cost settings
      fluidRow(
        column(4, numericInput(
          "AS_Accommodation_Costs", "Weekly accomodation cost ($):",
          min = 0, max = 1000, value = 450, step = 1, 
        )),
        column(4, selectInput(
          "AS_Area", label = "AS Area:",
          choices = c(1, 2, 3, 4), selected = 2
        )),
        column(4, selectInput(
          "Acc_type", label = "Accomodation type:",
          choices = c("Renting", "Mortgage"), selected = "Renting"
        ))
      ),
      # Input the poverty threshold
      fluidRow(
        column(4, numericInput(
          "bhc_median",
          "BHC equivalised median",
          "43000"
        )),
        column(4, numericInput(
          "ahc_median",
          "AHC equivalised median",
          "33100"
        )),
        column(4, numericInput(
          "pov_thresholds",
          "Poverty thresholds (fraction of medians)",
          "0.5",
          step = 0.1
        ))
      ),
      # Input partner status
      checkboxInput("Partnered", "Partnered", value = FALSE),
      # Input parter wage details, note that this is only
      # displayed if there is a partner
      fluidRow(
        column(6, conditionalPanel(
          condition = "input.Partnered == 1",
          numericInput(
            "gross_wage2", "Partner's hourly wage ($):",
            min = 15, max = 100, value = 20, step = .5
          )
        )),
        column(6, conditionalPanel(
          condition = "input.Partnered == 1",
          numericInput(
            "hours2", "Partner's hours worked:",
            min = 0, max = 80, value = 0, step = 1
          )
        ))
      ),
      # Input the children's ages
      textInput(
        "Children_ages",
        "Age of children (e.g. '1, 4' or leave blank)",
        "0, 10"
      ),
      # Check boxes to only display certain payments
      checkboxGroupInput(
        "income_types",
        "Select income types for Income composition plot:",
        c(
          "Net Income", "Best Start", "Winter Energy", "Accomodation Supplement",
          "IWTC", "FTC", "MFTC", "IETC", 
          "Net Core Benefit", "Net Wage", "Net Wage (Partner)",
          "Tax on Core Benefit", "Tax on Wage and ACC"
        ),
        inline = TRUE,
        selected = c(
          "Best Start", "Winter Energy", "Accomodation Supplement",
          "IWTC", "FTC", "MFTC", "IETC", 
          "Net Core Benefit", "Net Wage", "Net Wage (Partner)",
          "Tax on Core Benefit", "Tax on Wage and ACC"
        )
      )
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
        # Plot poverty
        tabPanel(
          "Poverty",
          h2("Equivalised Income"), plotlyOutput("plot_equivincome", height = "300px"),
          h2("BHC Poverty"), plotlyOutput("plot_bhc_depth", height = "300px"),
          h2("AHC Poverty"), plotlyOutput("plot_ahc_depth", height = "300px")
        ),
        # Plot income composition/budget constraint
        tabPanel(
          "Income composition",
          plotlyOutput("plot_incomecomposition_SQ", height = "400px"),
          plotlyOutput("plot_incomecomposition_Reform", height = "400px")
        ),
        # Table containing the parameters that changed
        tabPanel("Parameters Adjustment", rHandsontableOutput("show_parameters")),
        # Table containing the parameters that changed
        tabPanel("Parameters", tableOutput("changed_parameters"))
      )
    )
  )
))
