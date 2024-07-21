
# Install all the required package for the EMTR function and income explorer app
pkg <- c("shiny", "shinyvalidate", "rhandsontable", "RColorBrewer", "shinythemes",
         "plotly", "openxlsx", "data.table", "ggplot2", 
         "scales", "magrittr", "zoo")

new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg) > 0) {
  install.packages(new.pkg)
}

if (packageVersion("shiny") < "1.5.0") {
  install.packages("shiny")
}

shiny::runApp()