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
  library(data.table)
})

# TY25 minimum wage
DEFAULT_HOURLY_WAGE <- 23.15

# Define UI
shinyUI(fluidPage(
  # different themes from shinythemes R package, https://rstudio.github.io/shinythemes/
  theme = shinytheme("sandstone"),
  useShinyjs(),
  tags$head(tags$style(HTML(
    "a {color:#000000} a:hover, a:focus {color:#707070} .btn {margin-bottom:5px}
    .tooltip-span {
    text-decoration: underline;
    color:#fe6e00
    }
    .tooltip-span:hover{
    color: #3e3f3a;
    cursor: pointer;
    }"
  ))),
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")), 
  # Side menu
  sidebarLayout(
    sidebarPanel(
      width = 4,
      # Application title
      titlePanel(
        title = div(
          style = "margin-top:-25px",
          div(
            style = "float:left; padding:0 10px 8px 0;",
            a(
              target = "_blank",
              href = "https://www.treasury.govt.nz/",
              img(style = "display:block", src = "logo.svg", height = "30px")
            )
          ),
          div(style = "float:left; padding:0 0 10px 0", "Income Explorer")
        ),
        windowTitle = "Income Explorer"
      ),
      br(style = "clear:both"),
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
      
      br(),
      
      # Input the hourly wage and hours
      fluidRow(
        column(6, numericInput(
          "wage1_hourly", "Hourly wage ($)",
          value = DEFAULT_HOURLY_WAGE, step = 0.1, min = 0.1
        )),
        column(6, numericInput("max_hours", "Max hours/week", "50", step = 1, min = 1))
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
      ),
      
      fluidRow(
        style = "text-align:center",
        a("Source", target = "_blank", href = "https://github.com/Treasury-Analytics-and-Insights/IncomeExplorer"),
        " | ",
        a("License", target = "_blank", href = "https://github.com/Treasury-Analytics-and-Insights/IncomeExplorer/blob/master/LICENSE.md"),
        " | ",
        a("The Treasury", target = "_blank", href = "https://www.treasury.govt.nz/"),
        " | ",
        a("www.govt.nz", target = "_blank", href = "https://www.govt.nz/"),
        br(),
        span(
          style = "font-variant:small-caps; font-size:14px;",
          a(
            "© 2024 ", strong("Analytics"), em("&"), strong("Insights"),
            target = "_blank", href = "https://github.com/Treasury-Analytics-and-Insights/"
          )
        )
      )
    ),
    # Main panel containing plots etc.
    mainPanel(
      style = "padding: 20px 0 0 0;",
      tabsetPanel(
        id = "resultsTabset",
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
              tags$li("The app is preloaded with policy scenarios from the most recently released Economic & Fiscal Update (EFU) from the New Zealand Treasury. You can select any number of these scenarios by clicking in the 'Select policy scenarios...' selection field and then selecting from the drop-down list that appears."),
              tags$li("Results are displayed in the different tabs: Net Income, EMTR, RR, PTR, and Income Composition. The Policy Changes tab shows the differences in policy settings between consecutive selected policy scenarios."),
              tags$li("The app allows you to customise the example family by setting parameters relating to their earnings, accommodation costs, number of children, and whether the primary earner has a partner or not and what their partner's earnings are."),
              tags$li("Selected policy scenarios can be downloaded in Excel format by clicking the 'Download Scenarios' button. These can be edited, and then uploaded back into the app using the 'Add Scenarios' button."),
              tags$li("The full calculation results can be downloaded in Excel format by clicking the 'Download Results' button.")
            ),
            h2("Disclaimer"),
            p(
              "This app analyses the effect of New Zealand's tax and welfare system on an example family's net income and work incentives. The app is provided as-is and for research purposes only. Despite reasonable measures taken to ensure quality and accuracy, the Treasury makes no warranty, or guarantee, express or implied, nor assumes any legal liability or responsibility for the accuracy, correctness, completeness or use of any information that is provided through the app."
            )
          )
        ),
        tabPanel(
          "Description",
          mainPanel(
            h3("Measures of work incentives:"),
            tags$ul(
              tags$li(strong("Net Income"), " – The Net Income tab shows how an individual’s income in the hand, that is income after taxes and government welfare transfers, changes as their earnings from work change."),
              br(),
              tags$li(withMathJax(strong("Effective Marginal Tax Rate (EMTR)"), " – The EMTR tab shows what percentage of a one dollar increase in gross earnings is not received in the hand due to taxation and the abatement of government welfare transfers.
                                  $$EMTR=1-\\frac{\\text{change in income in hand}}{\\text{change in gross earnings}}$$")),
              tags$li(strong("Replacement Rate (RR)"), " – The RR tab shows what percentage of an individual’s income in the hand would be replaced by government welfare transfers if they chose to stop working.",
                      span(title = "If someone earned $1,000 per week at their job and receives $600 in unemployment benefits when they stop working, their replacement rate would be 60%. A higher replacement rate means people can receive benefits that are closer to their work income, which may reduce their incentives to remain in employment. Replacement rates are also used to evaluate the adequacy of social safety nets and to compare welfare systems across different countries or regions.",
                           class = "tooltip-span",
                           "Example"),
                      "$$RR=\\frac{\\text{income in the hand when not working}}{\\text{income in the hand when working}}$$"),
              tags$li(strong("Participation Tax Rate (PTR)"), " – The PTR tab shows what percentage of gross earnings is not received in the hand when an individual moves from unemployment into work, due to additional tax paid and reduced government welfare transfers.",
                      span(title = "If someone could earn $1000 by working but would lose $400 in benefits and pay $200 in taxes, their participation tax rate would be 60% (losing $600 compared to a gross gain of $1000). A higher participation tax rate means people keep less of their earnings when they start working, which might discourage them from entering the workforce. This measure helps policymakers understand how tax and transfer systems together affect people's incentives to work versus remain on benefits.",
                           class = "tooltip-span",
                           "Example"),
                      "$$PTR=1-\\frac{\\text{income in the hand from working}-\\text{income in the hand when not working}}{\\text{gross earnings from working}}$$"),
              tags$li(strong("Income Composition"), " – The Income Composition tab shows how earnings, taxes, and various government welfare transfers combine to give an individual’s net income, and how this changes as their gross earnings change.")
            ),
            h3("The following taxes and transfers are modelled in the Income Explorer:"),
            tags$ul(
              tags$li("Personal Income Tax (PIT)"),
              tags$li("ACC Earners' Levy"),
              tags$li("Working for Families (WFF):"),
              tags$ul(
                tags$li("Family Tax Credit (FTC)"),
                tags$li("In-Work Tax Credit (IWTC)"),
                tags$li("Minimum Family Tax Credit (MFTC)"),
                tags$li("Best Start Tax Credit (BSTC)"),
              ),
              tags$li("Independent Earners Tax Credit (IETC)"),
              tags$li("Core Benefits:"),
              tags$ul(
                tags$li("Jobseeker Support (JSS)"),
                tags$li("Sole Parent Support (SPS)"),
              ),
              tags$li("Winter Energy Payment (WEP)"),
              tags$li("Accommodation Supplement (AS)"),
            ),
            h3("Limitations of the Income Explorer"),
            tags$ul(
              tags$li(strong("Accommodation Supplement (AS)"), " – This tool assumes full
                      take up of AS for all individuals with qualifying incomes and accommodation costs.
                      However, AS eligibility is subject to additional tests, including asset tests, that are not currently included in the Income Explorer tool. To model an
                      individual that is ineligible for/chooses not to take up AS, set their accommodation costs to zero."),
              tags$li(strong("Jobseeker Support (JSS)"), " – This tool applies the ", em("Single, 25 years or over"), " JSS rate to 
              all qualifying single people without children. It does not account for the different rates of JSS that are 
              provided to people aged between 18 and 24 years older.")
            ),
            h3("The Income Explorer does not currently model:"),
            tags$ul(
              tags$li(strong("NZ Superannuation (NZS)"),
                      " –  income modelling for superannuitants will be added to this tool soon."),
              tags$li(strong("Family Boost"),
                      " – payment that helps the parents of young children with the costs of early childhood education."),
              tags$li(strong("Supported Living Payment (SLP)"),
                      " – provides income support to people who have a permanent and severe health condition, injury or disability,
                      or for people who are caring for someone who requires fulltime care."),
              tags$li(strong("Young Parent Payment (YPP)"),
                      " – provides income support for 16 – 19 year olds with children."),
              tags$li(strong("Youth Payment (YP)"),
                      " – provides income support for 16 and 17 year olds who aren’t supported by a parent, guardian, or other person."),
              tags$li(strong("Income Related Rent Subsidy (IRRS)"),
              " – subsidises rent for people on low incomes."),
              tags$li(strong("Temporary Additional Support (TAS)"),
              " – temporary weekly payment that helps people who can’t afford their essential living costs."),
              tags$li(strong("KiwiSaver Contributions"), 
              " – payments made by employees and employers into retirement savings schemes. 
                      Employee contributions reduce income in the hand and so may affect work incentives. However, savings benefit the individual, 
                      meaning the impacts on work incentives will be more complicated."),
              tags$li(strong("Student Loan Repayments"),
              " – repaid by loan holders at a set rate on earnings over a specified earnings threshold. Student loan repayments reduce income in the hand
              and so may affect work incentives. However, loan repayments benefit the individual, meaning the impacts on work incentives
              will be more complicated."),
              tags$li(strong("Student Allowance and Student Loan Living Costs"), " – which are payments and loans provided to tertiary students."),
              tags$li(strong("Paid Parental Leave"),
              " – a payment that allows new parents to take paid time off work."),
              tags$li(strong("Child Support Pass On"),
                      " – measures that determine how child support payments interact with benefit eligibility and abatement.")
            ),
          )
        ),
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
