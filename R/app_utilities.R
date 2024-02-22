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
check_for_changed_parameters <- function(p1, p2){
  changed <- data.table()
  for (p in names(p1)){
    if (all(all.equal(p1[[p]], p2[[p]]) != TRUE)){
      SQ = p1[[p]]
      Reform = p2[[p]]
      # convert scales to strings
      if (length(SQ) > 1) {
        SQ <- as.data.frame(SQ)
        Reform <- as.data.frame(Reform)
        SQ <- paste0(
          "Thresholds:", paste0("$", SQ$V1, collapse = ", "), "; ",
          "Rates:", paste0(100*SQ$V2, "%", collapse = ", ")
        )
        Reform <- paste0(
          "Thresholds:", paste0("$", Reform$V1, collapse = ", "), "; ",
          "Rates:", paste0(100*Reform$V2, "%", collapse = ", ")
        )
      }
      this_changed <- data.table(Parameter = p, SQ = SQ, Reform = Reform)
      changed <- rbind(changed, this_changed)
    }
  }
  return(changed)
}

get_parameter_changes <- function(params_list) {
  scenarios <- names(params_list)
  if (length(scenarios) < 2) {
    changes <- data.table()
  } else {
    # Calculate sequential changes
    changes <- data.table(Parameter = "")[0]
    for (ii in seq_len(length(scenarios) - 1)) {
      scenario1 <- scenarios[[ii]]
      scenario2 <- scenarios[[ii + 1]]
      this_scenario_changes <- check_for_changed_parameters(
        params_list[[scenario1]], params_list[[scenario2]]
      )
      if (nrow(this_scenario_changes) != 0) {
        setnames(this_scenario_changes, c("SQ", "Reform"), c(scenario1, scenario2))
        by_cols <- "Parameter"
        if (ncol(changes) > 1) {
          by_cols <- c(by_cols, scenario1)
        }
        this_scenario_changes <- this_scenario_changes[, lapply(.SD, as.character)]
        changes <- merge(changes, this_scenario_changes, by = by_cols, all = TRUE)
      }
    }
    changes <- changes[, lapply(.SD, function(x) ifelse(is.na(x), "", x))]
    if (length(scenarios) > 1) {
      setcolorder(changes, c("Parameter"))
    }
  }
  return(changes)
}



# Plot income composition
income_composition_plot <- function(
    EMTR_table, inc_limit = NULL, y_min = NULL, y_max = NULL,
    watermark = FALSE, weeks_in_year = 52L, 
    display_cols = c(
      "Net Income", "Best Start", "Winter Energy", "Accomodation Supplement",
      "IWTC", "FTC", "MFTC", "IETC", "Net Core Benefit", "Net Wage",
      "Net Wage (Partner)", "Tax on Core Benefit", "Tax on Wage and ACC"
    )
) {
  
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
    
    plot_data <- copy(EMTR_table)
    
    if (is.null(inc_limit)) {
      inc_limit <- max(plot_data$gross_wage1_annual)
    }
    
    plot_data[, ":="(
      net_benefit = net_benefit1 + net_benefit2,
      benefit_tax = -(gross_benefit1 + gross_benefit2 - net_benefit1 - net_benefit2),
      gross_wage = gross_wage1 + gross_wage2,
      wage_tax_and_ACC = -(wage1_tax + wage2_tax + wage1_ACC_levy + wage2_ACC_levy),
      IETC_abated = IETC_abated1 + IETC_abated2
    )]
    
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
    
    weekly_cols <- c(data_component_cols, "Net_Income")
    plot_data[, (weekly_cols) := lapply(.SD, function(x) x*weeks_in_year), .SDcols = weekly_cols]
    
    add_income_component <- function(p, income_component, stackgroup) {
      # print(paste0("Adding ", income_component, " with stackgroup ", stackgroup))
      p %>% add_trace(
        data = plot_data, type = "scatter", mode = "none",
        x = ~gross_wage1_annual,
        y = ~.data[[data_component_cols[[income_component]]]],
        name = income_component,
        fillcolor = set_colours[[income_component]],
        hovertemplate = paste0(income_component, ": %{y:$,.0f}<extra></extra>"),
        stackgroup = stackgroup
      ) %>% plotly_build()
    }
    
    p <- plot_ly(plot_data)
    # Add tax first
    tax_col_names <- display_cols %>% .[. %like% "Tax"]
    for (tax_col_name in rev(tax_col_names)) {
      p <- p %>% add_income_component(
        income_component = tax_col_name, stackgroup = "one"
      ) %>% plotly_build()
    }
    # Add transfers second
    transfer_col_names <- display_cols %>% .[!(. %like% "Tax") & . != "Net Income"]
    for (transfer_col_name in rev(transfer_col_names)) {
      p <- p %>% add_income_component(
        income_component = transfer_col_name, stackgroup = "two"
      ) %>% plotly_build()
    }
    
    p <- p %>%
      add_lines(
        data = plot_data, x = ~ gross_wage1_annual,
        y = ~ Net_Income, name = "Net Income", color = I("black"), inherit = FALSE,
        hovertemplate = paste("Net Income: %{y:$,.0f}<extra></extra>")
      ) %>%
      add_trace(
        x =  ~ hours1, y =  ~ 0, line = list(width = 0), xaxis = "x2",
        data = plot_data, showlegend = FALSE, inherit = FALSE,
        hoverinfo = "none", type = "scatter", mode = "lines"
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
          automargin = TRUE, zeroline = TRUE, showline = TRUE, mirror = TRUE
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
  income[, Net_Income := Net_Income*weeks_in_year]
  income[, Scenario := factor(Scenario, levels = unique(Scenario))]
  output_plot <- plot_ly(
    data = income, x = ~gross_wage1_annual, y = ~Net_Income, split = ~Scenario,
    mode = "lines", type = "scatter", showlegend = TRUE,
    line = list(color = tsy_palette, width = 3),
    customdata = ~Scenario,
    hovertemplate = "%{customdata}: %{y:$,.2f} <extra></extra>"
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
      hovermode = "x"
    )
  return(output_plot)
}

# Plot rates by Scenario
plot_rates <- function(input_data, rate_type, ylabel) {
  income <- copy(input_data)
  income[, Scenario := factor(Scenario, levels = unique(Scenario))]
  setnames(income, rate_type, "y")
  
  if (rate_type == "Participation_Tax_Rate") {
    # Drop zero since PTR is undefined
    income <- income[gross_wage1_annual != 0]
  }
  
  output_plot <- plot_ly(
    data = income, x = ~gross_wage1_annual, y = ~y, split = ~Scenario,
    mode = "lines", type = "scatter", showlegend = TRUE,
    line = list(color = tsy_palette, width = 3),
    customdata = ~Scenario,
    hovertemplate = "%{customdata}: %{y:.2%} <extra></extra>"
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
      legend = list(x = 100, y = 0.5),#list(orientation = "h", xanchor = "center", x = 0.5),
      hovermode = "x"
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
