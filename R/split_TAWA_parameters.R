################################################################################
# Generates TAWA parameter files from given parameters, inflators, and mapping files.
# All HES years and tax years share the same inflator and mapping file,
# with parameters being specific to each tax year,
# and the baseyear parameter set equal to the HES year.
# The parameters for all tax years are taken to be columns in the parameters file.
################################################################################

split_TAWA_parameters <- function(params_path, output_suffix, output_dir) {
  all_params <- openxlsx::read.xlsx(params_path) %>% setDT()
  
  # Don't hard-code these parameters
  all_params <- all_params[!(Parameter %in% c("Database_File", "baseyear", "Out_File"))]
  
  # Extract tax years from the parameters file
  tax_years <- all_params[Parameter == "modelyear", .SD, .SDcols = !"Parameter"] %>%
    unlist() %>% as.numeric()
  tax_years <- tax_years - 2000
  
  STYLE_FONT <- openxlsx::createStyle(fontName = "Segoe UI", fontSize = 10)
  STYLE_BOLD <- openxlsx::createStyle(textDecoration = "bold")
  STYLE_BORDER_BOTTOM <- openxlsx::createStyle(border = "bottom")
  STYLE_HIGHLIGHTED <- openxlsx::createStyle(fgFill = "#FFFF00")
  
  rows_with_bottom_border <- c(2, 5, 7, 28, 30, 50, 69, 72, 78, 79, 88, 96, 97)
  
  for(tax_year in tax_years) {
    logging::loginfo("TY%d", tax_year)
    
    this_TY_col <- names(all_params) %>% .[. %like% paste0("TY", tax_year)]
    params <- all_params[, .SD, .SDcols = c("Parameter", this_TY_col)]
    this_params_col <- paste0("SQ_TY", tax_year, output_suffix)
    setnames(params, this_TY_col, this_params_col)
    
    # Cast character data to numeric,
    # and shuffle the column names around while saving the character data
    # to fill in the numeric NAs manually later
    params[, numeric_data := lapply(.SD, function(.) {
      suppressWarnings(as.numeric(.))
    }), .SDcols = this_params_col]
    
    setnames(params, c(this_params_col, "numeric_data"), c("character_data", this_params_col))
    setcolorder(params, c("Parameter", this_params_col, "character_data"))
    
    # Create xlsx file with required sheets
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Parameters")
    
    # Parameter Data
    openxlsx::writeData(
      wb, "Parameters",
      x = params[, .SD, .SDcols = c("Parameter", this_params_col)],
      startRow = 2, startCol = 2, colNames = TRUE
    )
    
    # Replace individual cells having NA numerical data with character data
    na_rows <- which(is.na(params[, .SD, .SDcols = this_params_col]))
    for (na_row in na_rows) {
      openxlsx::writeData(
        wb, "Parameters",
        x = params[na_row, character_data],
        startRow = 2 + na_row, startCol = 3
      )
    }
    
    # Style Parameters sheet
    openxlsx::showGridLines(wb, "Parameters", showGridLines = FALSE)
    openxlsx::setRowHeights(wb, "Parameters", rows = 1 + 1:nrow(params), heights = 16)
    openxlsx::setColWidths(wb, "Parameters", cols = 2:3, widths = c(60.55, 22.55))
    
    openxlsx::addStyle(
      wb, "Parameters", STYLE_BOLD,
      rows = 2:5, cols = 2:3, gridExpand = TRUE, stack = TRUE
    )
    openxlsx::addStyle(
      wb, "Parameters", STYLE_FONT,
      rows = 1 + 1:nrow(params), cols = 2:3, gridExpand = TRUE, stack = TRUE
    )
    openxlsx::addStyle(
      wb, "Parameters", STYLE_BORDER_BOTTOM,
      rows = rows_with_bottom_border, cols = 2:3, gridExpand = TRUE, stack = TRUE
    )
    openxlsx::addStyle(
      wb, "Parameters", STYLE_HIGHLIGHTED,
      rows = 89:97, cols = 2:3, gridExpand = TRUE, stack = TRUE
    )
    
    this_filename <- file.path(
      output_dir, sprintf("TY%d%s.xlsx", tax_year, output_suffix)
    )
    openxlsx::saveWorkbook(wb, this_filename, overwrite = TRUE)
  }
}