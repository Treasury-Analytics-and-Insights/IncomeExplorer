scale_vector_to_tawa_param_string <- function(x) {
  thresholds <- x$thresholds
  rates <- x$rates
  combined <- purrr::map2(thresholds, rates, function(threshold, rate) {
    paste0("['", threshold, "'; '", rate, "']")
  })
  out <- paste0("[", paste0(combined, collapse = "; "), "]")
  return(out)
}

save_excel_params <- function(
    params, output_path,
    firstRow = 2, firstCol = 2,
    template_path = "inst/parameters/policy_parameters_template.csv"
) {
  params <- copy(params)
  
  # Encode Scales as the string "[['x1', 'x2']; ['y1', 'y2']]"
  scale_cols <- names(params) %>% .[. %like% "Scale$|PhaseIn"]
  for (scale_col in scale_cols) {
    params[[scale_col]] <- scale_vector_to_tawa_param_string(params[[scale_col]])
  }
  
  params_dt <- transpose(as.data.table(params), keep.names = "Parameter")
  setnames(params_dt, "V1", "Value")
  params_dt[
    !(Parameter %in% c("MFTC_WEP_scaling", "WFF_or_Benefit")),
    Parameter := stringr::str_replace_all(Parameter, "_", "/")
  ]
  
  params_output_template <- fread(template_path)
  params_dt <- merge(params_output_template[, .(Parameter, Description)], params_dt, by = "Parameter")
  params_dt[, Parameter := factor(Parameter, levels = unique(params_output_template$Parameter))]
  setorder(params_dt, Parameter)
  setcolorder(params_dt, c("Parameter", "Value", "Description"))
  
  # Styling variables
  STYLE_HEADER_FONT <- openxlsx::createStyle(fontSize = 13, fontColour = "#1F497D", textDecoration = "bold")
  STYLE_BORDER_BOTTOM <- openxlsx::createStyle(border = "bottom")
  STYLE_NORMAL <- openxlsx::createStyle(fgFill = "#DCE6F1", halign = "left")
  STYLE_HIGHLIGHTED_BLUE <- openxlsx::createStyle(fgFill = "#B8CCE4")

  # Calculate parameter groups so we can give borders around them
  dt_param_names <- copy(params_dt)
  dt_param_names[, row := .I]

  # Most parameters have a suffix separated by a "/"
  # Some parameters are in CamelCase and start with "Use"
  dt_param_names[, prefix := tstrsplit(Parameter, split = "/", keep = 1)]
  dt_param_names[prefix %>% startsWith("Use"), prefix := "Use"]
  
  # Special treatment for these "groups"
  dt_param_names[Parameter %like% "^Benefits/JSS", prefix := "JSS"]
  dt_param_names[Parameter %like% "^Benefits.*SPS", prefix := "SPS"]
  dt_param_names[Parameter %like% "^Benefits/WinterEnergy", prefix := "WinterEnergy"]
  dt_param_names[Parameter %like% "^FamilyAssistance/Abatement", prefix := "WFF_Abatement"]
  dt_param_names[Parameter %like% "^FamilyAssistance/FTC", prefix := "FTC"]
  dt_param_names[Parameter %like% "^FamilyAssistance/MFTC", prefix := "MFTC"]
  dt_param_names[Parameter %like% "^FamilyAssistance/BestStart", prefix := "BestStart"]
  
  # Some extra parameters have underscores - treat these as their own group
  dt_param_names[Parameter %like% "_", prefix := "Other"]

  # Give a bottom border to rows when they are the last in the group
  dt_param_names[, use_top_border := prefix != shift(prefix, type = "lead")]

  # Add firstRow to skip the empty row and the header row
  rows_with_bottom_border <- firstRow + dt_param_names[use_top_border == TRUE, row]

  # Also give bottom border to header and last row
  rows_with_bottom_border <- unique(
    c(firstRow, rows_with_bottom_border, firstRow + nrow(params) + 1)
  )

  # Cells to highlight in blue
  prefixes_in_blue <- dt_param_names$prefix %>% unique() %>% .[seq(2, length(.), 2)] %>% paste0(collapse = "|")
  rows_in_blue <- c(firstRow, firstRow + dt_param_names[prefix %like% prefixes_in_blue, row])
  
  # Create workbook and sheet
  wb <- openxlsx::createWorkbook()
  sheet <- openxlsx::addWorksheet(wb, "Parameters")
  
  # Set zoom
  wb$worksheets[[1]]$sheetViews <- stringr::str_replace(wb$worksheets[[1]]$sheetViews, '(?<=zoomScale=")[0-9]+', "85")
  
  openxlsx::writeData(
    wb, sheet, x = params_dt,
    startRow = firstRow, startCol = firstCol, colNames = TRUE
  )
  
  # Convert everything to numeric
  params_numeric <- params_dt[
    , lapply(.SD, function(.) suppressWarnings(as.numeric(.))), .SDcols = !c("Parameter", "Description")
  ]
  openxlsx::writeData(
    wb, sheet, x = params_numeric,
    startRow = firstRow, startCol = firstCol + 1, colNames = TRUE
  )
  # Check the first column of numeric data to find the NA's
  na_rows <- which(is.na(params_numeric[, .SD, .SDcols = 1]))
  for (na_row in na_rows) {
    openxlsx::writeData(
      wb, sheet,
      x = params_dt[na_row, .SD, .SDcols = !c("Parameter", "Description")],
      startRow = firstRow + na_row, startCol = firstCol + 1, colNames = FALSE
    )
  }

  # Column settings
  out_cols <- firstCol - 1 + seq(1, ncol(params_dt))
  col_widths <- c(50, 50, 180)

  # Style Parameters sheet
  openxlsx::setColWidths(wb, "Parameters", cols = 1, widths = 8)
  openxlsx::setColWidths(wb, "Parameters", cols = out_cols, widths = col_widths)

  openxlsx::addStyle(
    wb, "Parameters", STYLE_HEADER_FONT,
    rows = firstRow, cols = out_cols,
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(
    wb, "Parameters", STYLE_NORMAL,
    rows = firstRow - 1 + 1:(nrow(params_dt) + 1), cols = out_cols,
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(
    wb, "Parameters", STYLE_BORDER_BOTTOM,
    rows = rows_with_bottom_border, cols = out_cols,
    gridExpand = TRUE, stack = TRUE
  )
  openxlsx::addStyle(
    wb, "Parameters", STYLE_HIGHLIGHTED_BLUE,
    rows = rows_in_blue, cols = out_cols,
    gridExpand = TRUE, stack = TRUE
  )

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
}
