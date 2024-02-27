tsy_palette <- c( 
  rgb(0, 131, 172, maxColorValue = 255),
  rgb(0, 188, 226, maxColorValue = 255),
  rgb(103, 168, 84, maxColorValue = 255),
  rgb(188, 214, 81, maxColorValue = 255),
  rgb(241, 164, 45, maxColorValue = 255),
  rgb(239, 150, 108, maxColorValue = 255),
  rgb(0, 79, 103, maxColorValue = 255),
  rgb(0, 113, 136, maxColorValue = 255),
  rgb(62, 101, 50, maxColorValue = 255),
  rgb(122, 143, 34, maxColorValue = 255),
  rgb(13, 143, 34, maxColorValue = 255),
  rgb(122, 42, 34, maxColorValue = 255)
)

# Converts a string used to input children's ages into numeric values
convert_ages <- function(input_string){
  ages = as.numeric(strsplit(input_string, ",")[[1]])
  ages = sort(ages)
  return(ages)
}

calculate_income <- function(
    params,
    max_hours, hourly_wage, children_ages,
    partnered, partner_wages, partner_hours,
    accommodation_type, as_accommodation_costs, as_area,
    steps_per_dollar = 1L, weeks_in_year = 52L
) {
  # convert inputs
  max_wage <- max_hours*hourly_wage
  children <- convert_ages(children_ages)
  
  as_accommodation_rent <- (accommodation_type == "Renting")
  
  partner_wages <- (partnered == 1)*partner_wages*partner_hours
  partner_hours <- (partnered == 1)*partner_hours
  
  mftc_wep_scaling <- params$MFTC_WEP_scaling
  
  # Create helper emtr function using current inputs
  calc_emtr <- function(params) {
    emtr(
      # System parameters
      params,
      # Family parameters
      Partnered = partnered,
      wage1_hourly = hourly_wage,
      Children_ages = children,
      gross_wage2 = partner_wages,
      hours2 = partner_hours,
      # Accommodation
      AS_Accommodation_Costs = as_accommodation_costs,
      AS_Accommodation_Rent = as_accommodation_rent,
      AS_Area = as.numeric(as_area),
      # Presentation parameters
      max_wage = max_wage,
      steps_per_dollar = 1L,
      weeks_in_year = 52L,
      MFTC_WEP_scaling = as.numeric(mftc_wep_scaling)
    )
  }
  
  # MFTC is meant to make families always better off being off-benefit than staying
  # on a benefit. We let the user set whether the family stays on benefit
  # or gets IWTC when they work, with the parameter input$WFFBEN_SQ. This can be:
  # "Max" - choose the option which maximises the family income.
  # "WFF" - go off the benefit while working, and get IWTC + MFTC.
  # "Benefit" - stay on the benefit while working, and never get IWTC or MFTC
  #             (benefit abates away as earned income increases).
  # Note that these are only applicable when beneficiaries are ineligible for IWTC;
  # MFTC eligibility currently depends on IWTC eligibility in the `emtr` function.
  
  emtr_income_with_IWTC <- calc_emtr(params)
  
  wff_or_ben <- params$WFF_or_Benefit
  
  if (wff_or_ben == "WFF") {
    # print("Going off benefit (IWTC)")
    emtr_income <- emtr_income_with_IWTC
  } else {
    params_without_IWTC <- remove_IWTC_from_params(params)
    emtr_income_without_IWTC <- calc_emtr(params_without_IWTC)
    if (wff_or_ben == "Benefit") {
      # print("Staying on benefit (no IWTC)")
      emtr_income <- emtr_income_without_IWTC
    } else {
      # Choose which of benefit or IWTC gives max net income
      # print("Choosing max between IWTC or benefit")
      emtr_income <- choose_IWTC_or_benefit(
        emtr_income_with_IWTC, emtr_income_without_IWTC
      )
    }
  }
  
  return(emtr_income)
}

# Data table containing only the parameters that changed
# and their values
params_to_string_dt <- function(params) {
  params_string <- data.table()
  for (p in names(params)) {
    Value = params[[p]]
    # convert scales to strings
    if (length(Value) > 1) {
      Value <- as.data.frame(Value)
      Value <- paste0(
        "Thresholds:", paste0("$", Value$thresholds, collapse = ", "), "; ",
        "Rates:", paste0(100*Value$rates, "%", collapse = ", ")
      )
    }
    this_params_string <- data.table(Parameter = p, Value = Value)
    params_string <- rbind(params_string, this_params_string)
  }
  return(params_string)
}

get_parameter_changes <- function(params_list) {
  scenarios <- names(params_list)
  if (length(scenarios) < 2) {
    changes <- data.table()
  } else {
    # Load all parameters as strings, joining Scenario's in long format
    params_list_str <- lapply(params_list, params_to_string_dt) %>%
      setNames(names(params_list)) %>%
      rbindlist(idcol = "Scenario")
    params_list_str[, Scenario := factor(Scenario, levels = scenarios)]
    params_list_str[, Parameter := factor(Parameter, levels = unique(Parameter))]
    
    # Calculate all sequential changes
    params_list_str[, Change_Value := ifelse(Value == shift(Value), NA, Value), by = Parameter]
    
    changes <- dcast(
      params_list_str[!is.na(Change_Value), .(Scenario, Parameter, Change_Value)],
      ... ~ Scenario, value.var = "Change_Value"
    )
    if (nrow(changes) > 0) {
      # There are some changes, so lets merge on the first scenario parameters
      first_vals <- dcast(
        params_list_str[Scenario == first(Scenario), .(Scenario, Parameter, Value)],
        ... ~ Scenario, value.var = "Value"
      )
      changes <- merge(changes, first_vals, by = "Parameter", all.x = TRUE)
      setcolorder(changes, c("Parameter", scenarios[1]))
      
      # Now add blanks to any "in-between" scenarios
      inbetween_scenarios <- setdiff(scenarios, setdiff(names(changes), "Parameter"))
      changes[, (inbetween_scenarios) := NA]
      setcolorder(changes, c("Parameter", scenarios))
      
      # Set any NA's to be blank
      changes[, (scenarios) := lapply(.SD, function(x) ifelse(is.na(x), "", x)), .SDcols = scenarios]
      
    } else {
      # There are no changes
      changes <- data.table()
    }
  }
  return(changes)
}



# Plot income composition
income_composition_plot <- function(
    EMTR_table, y_min = NULL, y_max = NULL, weeks_in_year = 52L
) {
  
  plot_data <- copy(EMTR_table)
  extended_tsy_palette <- colorRampPalette(tsy_palette)(20)
  
  set_colours <- c(
    "Best Start" = extended_tsy_palette[1],
    "Winter Energy" = extended_tsy_palette[2],
    "Accomodation Supplement" = extended_tsy_palette[3],
    "IWTC" = extended_tsy_palette[4],
    "FTC" = extended_tsy_palette[5],
    "MFTC" = extended_tsy_palette[6],
    "IETC" = extended_tsy_palette[7],
    "Net Core Benefit" = extended_tsy_palette[13],
    "Net Wage" = extended_tsy_palette[18],
    "Net Wage (Partner)" = extended_tsy_palette[11],
    "Tax on Core Benefit" = extended_tsy_palette[10],
    "Tax on Wage and ACC" = extended_tsy_palette[8]
  )
  
  data_component_cols <- c(
    "Best Start" = "BestStart_Total",
    "Winter Energy" = "WinterEnergy",
    "Accomodation Supplement" = "AS_Amount",
    "IWTC" = "IWTC_abated",
    "FTC" = "FTC_abated",
    "MFTC" = "MFTC",
    "IETC" = "IETC_abated",
    "Net Core Benefit" = "net_benefit",
    "Net Wage" = "net_wage1",
    "Net Wage (Partner)" = "net_wage2",
    "Tax on Core Benefit" = "benefit_tax",
    "Tax on Wage and ACC" = "wage_tax_and_ACC"
  )
  
  display_cols <- c(data_component_cols, "Net Income" = "Net_Income")
  plot_data[, (display_cols) := lapply(.SD, function(x) x*weeks_in_year), .SDcols = display_cols]
  
  add_income_component <- function(p, income_component, stackgroup) {
    p %>% add_trace(
      data = plot_data, type = "scatter", mode = "none",
      x = ~gross_wage1_annual,
      y = ~.data[[data_component_cols[[income_component]]]],
      name = income_component,
      fillcolor = set_colours[[income_component]],
      hovertemplate = paste0(income_component, ": %{y:$,.0f}<extra></extra>"),
      stackgroup = stackgroup
    )
  }
  
  p <- plot_ly()
  # Add tax first
  tax_col_names <- names(display_cols) %>% .[. %like% "Tax"]
  for (tax_col_name in rev(tax_col_names)) {
    p <- p %>% add_income_component(tax_col_name, stackgroup = "one")
  }
  # Add transfers second
  transfer_col_names <- names(display_cols) %>% .[!(. %like% "Tax") & . != "Net Income"]
  for (transfer_col_name in rev(transfer_col_names)) {
    p <- p %>% add_income_component(transfer_col_name, stackgroup = "two")
  }
  
  p <- p %>%
    add_lines(
      data = plot_data, x = ~ gross_wage1_annual,
      y = ~ Net_Income, name = "Net Income", color = I("black"), inherit = FALSE,
      hovertemplate = paste("Net Income: %{y:$,.0f}<extra></extra>")
    ) %>%
    add_trace(
      data = plot_data, x =  ~ hours1, y =  ~ 0, xaxis = "x2",
      type = "scatter", mode = "lines",
      line = list(width = 0), hoverinfo = "none",
      showlegend = FALSE, inherit = FALSE
    ) %>%
    layout(
      xaxis2 = list(
        overlaying = "x", nticks = 10, side = "top",
        title = "Hours/week", automargin = TRUE, size = 8, showline = TRUE
      ),
      xaxis = list(
        title = "Annual gross wage income ($)", tickformat = "$,",
        automargin = TRUE, zeroline = TRUE, showline = TRUE, mirror = TRUE
      ),
      yaxis = list(
        title = "Income ($)", tickformat = "$,",
        automargin = TRUE, zeroline = TRUE, showline = TRUE, mirror = TRUE,
        range = list(y_min, y_max)
      ),
      legend = list(x = 100, y = 0.5),
      hovermode = "x"
    )
  return(p)
}

# Plot net incomes by Scenario
compare_net_income_plot <- function(input_data, weeks_in_year = 52L) {
  income <- copy(input_data)
  #convert weekly income to annual
  income[, Net_Income_annual := Net_Income*weeks_in_year]
  income[, Scenario := factor(Scenario, levels = unique(Scenario))]
  
  output_plot <- plot_ly(
    data = income, x = ~gross_wage1_annual, y = ~Net_Income_annual, split = ~Scenario,
    mode = "lines", type = "scatter", showlegend = TRUE,
    line = list(color = tsy_palette, width = 3),
    text = ~paste(
      sprintf("<i>%s</i>", Scenario),
      sprintf(
        "<br><b>Weekly wage:</b> %s (%s hrs)",
        scales::dollar(gross_wage1, accuracy = 0.01),
        scales::comma(hours1, accuracy = 0.1)
      ),
      sprintf("<br><b>Net weekly income:</b> %s", scales::dollar(Net_Income)),
      "<br><b>Income breakdown</b>",
      "<br>Net wage:", scales::dollar(net_wage),
      "<br>Net benefit:", scales::dollar(net_benefit),
      "<br>WFF:", scales::dollar(WFF_abated),
      "<br>MFTC:", scales::dollar(MFTC),
      "<br>IETC:", scales::dollar(IETC_abated),
      "<br>Winter Energy:", scales::dollar(WinterEnergy),
      "<br>Best start:", scales::dollar(BestStart_Total),
      "<br>AS:", scales::dollar(AS_Amount)
    ),
    hoverinfo = "text"
  ) %>%
    add_trace(
      x = ~hours1, y = 0, xaxis = "x2", showlegend = FALSE, inherit = FALSE,
      type = "scatter", mode = "none"
    ) %>%
    layout(
      xaxis2 = list(
        overlaying = "x", nticks = 10, side = "top",
        title = "Hours/week", automargin = TRUE, size = 8, showline = TRUE
      ),
      xaxis = list(
        title = "Annual gross wage income ($)",
        tickformat = "$,", automargin = TRUE, showline = TRUE, mirror = TRUE
      ),
      yaxis = list(
        title = "Income ($)", tickformat = "$,", 
        automargin = TRUE, showline = TRUE,  mirror = TRUE, rangemode = "tozero"
      ),
      legend = list(x = 100, y = 0.5),
      hovermode = "closest"
    )
  return(output_plot)
}

# Plot rates by Scenario
plot_rates <- function(input_data, rate_type, ylabel) {
  income <- copy(input_data)
  income[, Scenario := factor(Scenario, levels = unique(Scenario))]
  
  if (rate_type == "PTR") {
    # Drop zero since PTR is undefined
    income <- income[gross_wage1_annual != 0]
  }
  
  output_plot <- plot_ly(
    data = income, x = ~gross_wage1_annual, y = ~get(rate_type), split = ~Scenario,
    mode = "lines", type = "scatter", showlegend = TRUE,
    line = list(color = tsy_palette, width = 3),
    text = ~paste(
      sprintf("<i>%s</i>", Scenario),
      sprintf(
        "<br><b>Weekly wage:</b> %s (%s hrs)",
        scales::dollar(gross_wage1, accuracy = 0.01),
        scales::comma(hours1, accuracy = 0.1)
      ),
      sprintf("<br><b>%s:</b> %s", rate_type, scales::percent(get(rate_type), accuracy = 0.1)),
      sprintf("<br><b>%s breakdown</b>", rate_type),
      "<br>Wage Tax/ACC:", scales::percent(get(paste0(rate_type, "_net_wage")), accuracy = 0.1),
      "<br>Benefit:", scales::percent(get(paste0(rate_type, "_net_benefit")), accuracy = 0.1),
      "<br>WFF:", scales::percent(get(paste0(rate_type, "_WFF_abated")), accuracy = 0.1),
      "<br>MFTC:", scales::percent(get(paste0(rate_type, "_MFTC")), accuracy = 0.1),
      "<br>IETC:", scales::percent(get(paste0(rate_type, "_IETC_abated")), accuracy = 0.1),
      "<br>Winter Energy:", scales::percent(get(paste0(rate_type, "_WinterEnergy")), accuracy = 0.1),
      "<br>Best start:", scales::percent(get(paste0(rate_type, "_BestStart_Total")), accuracy = 0.1),
      "<br>AS:", scales::percent(get(paste0(rate_type, "_AS_Amount")), accuracy = 0.1)
    ),
    hoverinfo = "text"
  ) %>%
    add_trace(
      x = ~hours1, y = ~0, xaxis = "x2", showlegend = FALSE, inherit = FALSE,
      type = "scatter", mode = "none"
    ) %>%
    layout(
      xaxis2 = list(
        overlaying = "x", nticks = 10, side = "top",
        title = "Hours/week", automargin = TRUE, size = 8, showline = TRUE
      ),
      xaxis = list(
        title = "Annual gross wage income ($)",
        tickformat = "$,", automargin = TRUE, showline = TRUE, mirror = TRUE
      ),
      yaxis = list(
        title = ylabel, tickformat = ".0%", 
        automargin = TRUE, showline = TRUE,  mirror = TRUE,
        range = c(0, 1.1)
      ),
      legend = list(x = 100, y = 0.5),
      hovermode = "closest"
    )
  return(output_plot)
}

remove_IWTC_from_params <- function(input_params) {
  output_params <- copy(input_params)
  output_params$FamilyAssistance_IWTC_Rates_UpTo3Children <- 0
  output_params$FamilyAssistance_IWTC_Rates_SubsequentChildren <- 0
  return(output_params)
}

choose_IWTC_or_benefit <- function(X, X_without_IWTC) {
  # Merge max
  SQ_net_income_comparison <- cbind(
    X[, .(With_IWTC = Net_Income)],
    X_without_IWTC[, .(Without_IWTC = Net_Income)]
  )
  SQ_net_income_comparison[, row_ID := 1:.N]
  With_IWTC_indices <- SQ_net_income_comparison[With_IWTC >= Without_IWTC, row_ID]
  Without_IWTC_indices <- SQ_net_income_comparison[Without_IWTC > With_IWTC, row_ID]
  
  With_IWTC <- X[With_IWTC_indices]
  Without_IWTC <- X_without_IWTC[Without_IWTC_indices]
  
  X <- rbind(With_IWTC, Without_IWTC, fill = TRUE)
  setorderv(X, "hours1")
  
  X[, EMTR := 1 - 1L * (shift(Net_Income, 1L, type = "lead") - Net_Income)]
  X[, EMTR := zoo::na.locf(EMTR)]
  
  return(X)
}
