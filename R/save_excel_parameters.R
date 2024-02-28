scale_vector_to_tawa_param_string <- function(x) {
  thresholds <- x$thresholds
  rates <- x$rates
  combined <- purrr::map2(thresholds, rates, function(threshold, rate) {
    paste0("['", threshold, "'; '", rate, "']")
  })
  out <- paste0("[", paste0(combined, collapse = "; "), "]")
  return(out)
}

PARAMS_TEMPLATE <- data.table::as.data.table(tibble::tribble(
  ~Parameter, ~Default_Value, ~Description,
  "modelyear", "0", 'Tax year that you are modelling',
  "ACC/LevyRate", "0", 'Rate is specified with each forecast update',
  "ACC/MaxLeviableIncome", "0", 'Specified with each forecast update',
  "Accommodation/AbatementRate", "0", 'Abatement rate for AS payments (currently 0.25, so lose 25c for every extra dollar earned)',
  "Accommodation/BaseRateThreshold/Mortgage", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/BaseRateThreshold/Rent", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/CoupleDeps/Single2/Deps/Area1", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/CoupleDeps/Single2/Deps/Area2", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/CoupleDeps/Single2/Deps/Area3", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/CoupleDeps/Single2/Deps/Area4", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/CoupleNoDeps/Single1Dep/Area1", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/CoupleNoDeps/Single1Dep/Area2", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/CoupleNoDeps/Single1Dep/Area3", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/CoupleNoDeps/Single1Dep/Area4", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/SingleNoDeps/Area1", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/SingleNoDeps/Area2", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/SingleNoDeps/Area3", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/MaxRate/SingleNoDeps/Area4", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Accommodation/PaymentPercentage", "0", 'AS = min[MaxRate, (PaymentPercentage) * (HousingCosts - (BaseRateThreshold)*BaseIncome)].',
  "Benefits/JSS/AbatementScale", "[['0'; '0']]", 'Job Seeker Support settings',
  "Benefits/JSS/CoupleAbatementScale", "[['0'; '0']]", 'Job Seeker Support settings',
  "Benefits/JSS/Rate/Couple", "0", 'Job Seeker Support settings',
  "Benefits/JSS/Rate/CoupleParent", "0", 'Job Seeker Support settings',
  "Benefits/JSS/Rate/Single", "0", 'Job Seeker Support settings',
  "Benefits/JSS/Rate/SoleParent", "0", 'Job Seeker Support settings',
  "Benefits/SPS/AbatementScale", "[['0'; '0']]", 'Sole Parent Support settings',
  "Benefits/SPS/Rate", "0", 'Sole Parent Support settings',
  "Benefits/Entitlement/Age/SPS/ChildLowerBound", "0", 'Age threshold for the youngest child such that an individual is eligible for SPS',
  "Benefits/WinterEnergy/Rates/Single", "0", 'Winter energy payment amount. Given to families receiving a core benefit or NZ Super. ',
  "Benefits/WinterEnergy/Rates/CoupleOrDeps", "0", 'Winter energy payment amount. Given to families receiving a core benefit or NZ Super. ',
  "FamilyAssistance/Abatement/AbatementScale", "[['0'; '0']]", 'Working for Families abatement',
  "FamilyAssistance/Abatement/Order", "0", 'Working for Families abatement FTC and IWTC order. 0 (default): FTC first then IWTC, 1: IWTC first then FTC.',
  "FamilyAssistance/FTC/Rates/FirstChild", "0", 'Family Tax Credit amount for first child',
  "FamilyAssistance/FTC/Rates/SubsequentChild", "0", 'Family Tax Credit amount for subsequent children',
  "FamilyAssistance/IWTC/Rates/UpTo3Children", "0", 'In Work Tax Credit amount given for up to three children',
  "FamilyAssistance/IWTC/Rates/SubsequentChildren", "0", 'In Work Tax Credit amount for each subsequent child',
  "FamilyAssistance/IWTC/Eligibility", "0", 'Eligibility test for In Work Tax Credit. 0: Status quo, based on hours worked and not given to beneficiaries. 1: Income test, option to give to beneficiaries (see below), 2: Phase in amount of IWTC based on phase-in scales (see below).',
  "FamilyAssistance/FullTimeWorkingHours/Couple", "0", 'Number of hours for a couple to be classified as working full time, for IWTC and MFTC eligibility  (FamilyAssistance/IWTC/Eligibility must be set to 0)',
  "FamilyAssistance/FullTimeWorkingHours/Single", "0", 'Number of hours for a single parent to be classified as working full time, for IWTC and MFTC eligibility (FamilyAssistance/IWTC/Eligibility must be set to 0)',
  "FamilyAssistance/IWTC/IncomeThreshold/Single", "0", 'Income test for a single parent to receive IWTC and MFTC (FamilyAssistance/IWTC/Eligibility must be set to 1)',
  "FamilyAssistance/IWTC/IncomeThreshold/Couple", "0", 'Income test for a couple to receive IWTC and MFTC (FamilyAssistance/IWTC/Eligibility must be set to 1)',
  "FamilyAssistance/IWTC/ToBeneficiaries", "0", 'Determines if beneficiaries can receive IWTC (FamilyAssistance/IWTC/Eligibility must be set to 1)',
  "FamilyAssistance/IWTC/PhaseIn/Single", "[['0'; '0']]", 'Single parent phase-in scale for IWTC, e.g. a phase-in scale of 20% is specified as -0.2 (FamilyAssistance/IWTC/Eligibility must be set to 2)',
  "FamilyAssistance/IWTC/PhaseIn/Couple", "[['0', '0']]", 'Couple phase-in scale for IWTC, e.g. a phase-in scale of 20% is specified as -0.2 (FamilyAssistance/IWTC/Eligibility must be set to 2)',
  "FamilyAssistance/MFTC/Rates/MinimumIncome", "0", 'Minimum income that ensures that a family is better off in work than on benefit; the Minimum Family Tax Credit brings their income up to this amount. Value is calculated by the TAWA version of the IRD calculator.',
  "FamilyAssistance/BestStart/Abatement/AbatementScale", "[['0'; '0']]", 'Abatement scale for Best Start payments (however best start payments for 0 year olds are not abated)',
  "FamilyAssistance/BestStart/Rates/Age0", "0", 'Best start rate for a 0 year old',
  "FamilyAssistance/BestStart/Rates/Age1or2", "0", 'Best start rate for 1 and 2 year olds',
  "IETC/AbatementScale", "[['0'; '0']]", 'Independent Earner Tax Credit, abatement scale',
  "IETC/MinimalIncome", "0", 'Independent Earner Tax Credit, minimum bound on earned income to determine eligibility',
  "IETC/PerYear", "0", 'Independent Earner Tax Credit, amount for each year',
  "IETC/OnlyFamiliesWithoutChildren", "0", 'Independent Earner Tax Credit, 0: give to everyone, 1: only give to families without children',
  "Tax/BaseScale", "[['0'; '0']]", 'Tax scale',
  "MFTC_WEP_scaling", "1", 'How should the Winter Energy Payment be scaled? Average week = 1, Winter week = 12/5, Summer week = 0',
  "WFF_or_Benefit", "Max", 'What work decision should we assume? Go off-benefit and receive IWTC = "WFF", stay on-benefit = "Benefit", or whichever gives a higher net income = "Max"'
))

save_excel_params <- function(
    params, output_path,
    firstRow = 2, firstCol = 2,
    params_output_template = PARAMS_TEMPLATE
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
