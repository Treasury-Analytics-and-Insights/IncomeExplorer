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


# Data table containing all the parameter setting for adjustment from the app
show_all_parameters <- function(Parameters_File_SQ, Parameters_File_Reform) {
  Params_text_SQ <- read.xlsx(Parameters_File_SQ)
  Params_text_Reform <- read.xlsx(Parameters_File_Reform)
  
  Params_text_SQ <- select(Params_text_SQ, Parameter, Value) %>% rename(SQ = Value)
  Params_text_Reform <- select(Params_text_Reform, Parameter, Value) %>% rename(Reform = Value)
  Params_text_Combined <- merge(Params_text_SQ, Params_text_Reform, by = "Parameter")
  
  return(Params_text_Combined)
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

get_changes <- function(params_list) {
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
      setnames(this_scenario_changes, c("SQ", "Reform"), c(scenario1, scenario2))
      by_cols <- "Parameter"
      if (ii > 1) {
        by_cols <- c(by_cols, scenario1)
      }
      this_scenario_changes <- this_scenario_changes[, lapply(.SD, as.character)]
      changes <- merge(changes, this_scenario_changes, by = by_cols, all = TRUE)
    }
    changes <- changes[, lapply(.SD, function(x) ifelse(is.na(x), "", x))]
    if (length(scenarios) > 1) {
      setcolorder(changes, c("Parameter", scenarios))
    }
  }
  return(changes)
}



# Plot budget contraints/income composition
#new plotting functions based off plotly R package
amounts_net_plot <- 
  function(EMTR_table, inc_limit=NULL, y_min=NULL, y_max=NULL,
           watermark=FALSE, weeks_in_year=52L,
           display_cols = 
             c("Net Income", "Best Start", "Winter Energy", "Accomodation Supplement", 
               "IWTC", "FTC", "MFTC", "IETC", "Net Core Benefit", "Net Wage", 
               "Net Wage (Partner)", "Tax on Core Benefit", "Tax on Wage and ACC")) {
    
    extended_tsy_palette <- colorRampPalette(tsy_palette)(20)
    
    set_colours <- c(
      "Best Start" = extended_tsy_palette[1],
      "Winter Energy" = extended_tsy_palette[2],
      "Accomodation Supplement" = extended_tsy_palette[3],
      "IWTC" = extended_tsy_palette[4],
      "FTC" = extended_tsy_palette[5],
      "MFTC" = extended_tsy_palette[6],
      "IETC" = extended_tsy_palette[7],
      "Net Core Benefit" = extended_tsy_palette[9],
      "Net Wage" = extended_tsy_palette[10],
      "Net Wage (Partner)" = extended_tsy_palette[12],
      "Tax on Core Benefit" = extended_tsy_palette[12],
      "Tax on Wage and ACC" = extended_tsy_palette[11]
    )
    
    X <- copy(EMTR_table)
    
    two_adults <- (X[, max(net_benefit2)]>0) # Do we need this? 
    
    wage1_hourly <- X[2, gross_wage1/hours1] # Do we need this? 
    
    if (is.null(inc_limit))
      inc_limit <- X[,max(gross_wage1_annual)]
    
    Y <- EMTR_table[,.(gross_wage1_annual,
                       gross_benefit1,
                       gross_benefit2,
                       net_benefit = net_benefit1 + net_benefit2,
                       net_wage1,
                       net_wage2,
                       benefit_tax=-(gross_benefit1+gross_benefit2-net_benefit1-net_benefit2),
                       gross_wage=gross_wage1+gross_wage2,
                       wage_tax_and_ACC=-(wage1_tax+wage2_tax+wage1_ACC_levy+wage2_ACC_levy),
                       IETC_abated=IETC_abated1+IETC_abated2,
                       
                       FTC_abated,MFTC,
                       IWTC_abated,
                       AS_Amount,
                       WinterEnergy,
                       BestStart_Total, 
                       Net_Income)]
    
    Y[, ':=' (gross_benefit1 = NULL,
              gross_benefit2 = NULL )]
    
    Y <- Y[, lapply(.SD, function(x) x*weeks_in_year), by = .(gross_wage1_annual)]
    
    p <- 
      plot_ly(Y) %>%
      layout(xaxis2 = list(overlaying = "x", nticks = 10, side = "top",
                           title = "Hours/week", automargin=TRUE, size=8,
                           showline = TRUE),
             xaxis = list(title = "Annual gross wage income ($)", 
                          tickformat = "$,", 
                          automargin=TRUE,
                          zeroline = TRUE,
                          showline = TRUE,
                          mirror=TRUE),
             yaxis = list (title = "Income ($)", tickformat = "$,", 
                           automargin=TRUE,
                           zeroline = TRUE,
                           showline = TRUE,
                           mirror=TRUE),
             legend = list(x = 100, y = 0.5),
             hovermode = "x") 
    
    if("Tax on Wage and ACC" %in% display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~wage_tax_and_ACC, name = 'Tax on Wage and ACC', 
                           fillcolor = set_colours[13], stackgroup = 'one',
                           hovertemplate = paste("Tax on Wage and ACC: %{y:$,.0f}<extra></extra>"))  
    
    if("Tax on Core Benefit" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~benefit_tax, name = "Tax on Core Benefit",
                           fillcolor = set_colours[12], stackgroup = 'one',
                           hovertemplate = paste("Tax on Core Benefit: %{y:$,.0f}<extra></extra>"))

    if("Net Wage (Partner)" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~net_wage2, name = 'Net Wage (Partner)', stackgroup = 'two',
                           fillcolor = set_colours[11],
                           hovertemplate = paste("Net Wage (Partner): %{y:$,.0f}<extra></extra>"))

    if("Net Wage" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~net_wage1, name = 'Net Wage', stackgroup = 'two',
                           fillcolor = set_colours[10],
                           hovertemplate = paste(
                             "Annual gross wage income:\n %{x:$,.2f} \n",
                             "Net Wage: %{y:$,.0f}<extra></extra>"))

    if("Net Core Benefit" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~net_benefit, name = 'Net Core Benefit',
                           fillcolor = set_colours[9], stackgroup = 'two',
                           hovertemplate = paste("Net Core Benefit: %{y:$,.0f}<extra></extra>"))



    if("IETC" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~IETC_abated, name = 'IETC',
                           fillcolor = set_colours[7], stackgroup = 'two',
                           hovertemplate = paste("IETC: %{y:$,.0f}<extra></extra>"))

    if("MFTC" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~MFTC, name = 'MFTC',
                           fillcolor = set_colours[6], stackgroup = 'two',
                           hovertemplate = paste("MFTC: %{y:$,.0f}<extra></extra>"))

    if("FTC" %in% display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~FTC_abated, name = 'FTC',
                           fillcolor = set_colours[5], stackgroup = 'two',
                           hovertemplate = paste("FTC: %{y:$,.0f}<extra></extra>"))

    if("IWTC" %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~IWTC_abated, name = 'IWTC',
                           fillcolor = set_colours[4], stackgroup = 'two',
                           hovertemplate = paste("IWTC: %{y:$,.0f}<extra></extra>"))

    if("Accomodation Supplement" %in%  display_cols)
      p <- p %>%   add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                             y = ~AS_Amount,
                             name = 'Accomodation Supplement',
                             fillcolor = set_colours[3], stackgroup = 'two',
                             hovertemplate = paste("Accomodation Supplement: %{y:$,.0f}<extra></extra>"))

    if("Winter Energy"  %in%  display_cols )
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~WinterEnergy, name = 'Winter Energy',
                           fillcolor = set_colours[2], stackgroup = 'two',
                           hovertemplate = paste("Winter Energy: %{y:$,.0f}<extra></extra>"))

    if("Best Start"  %in%  display_cols)
      p <- p %>% add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                           y = ~BestStart_Total,
                           name = 'Best Start', fillcolor = set_colours[1],
                           stackgroup = 'two',
                           hovertemplate = paste("Best Start: %{y:$,.0f}<extra></extra>"))


    #Adding a line for Net Income
    if("Net Income"  %in%  display_cols)
      p <-
      p %>% add_lines(data = Y, x = ~gross_wage1_annual,
                      y = ~Net_Income, name = 'Net Income', color = I("black"),
                      hovertemplate = paste("Net Income: %{y:$,.0f}<extra></extra>"))
    
    # Add secondary axis with hours
    p <- p %>% add_trace(x=~hours1, y=~0, line=list(width = 0), xaxis="x2",
              data=X, showlegend=FALSE, inherit=FALSE,
              hoverinfo = "none", type = "scatter", mode = "lines")
    
    return(p)
    
  }



# Plot two net incomes
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



# Plot two Effective Marginal Tax Rates/ Replacement Rates / Participation Tax Rates
compare_plots <- function(data1, data2, 
                          type = c( "EMTR", "RR", "PTR"),
                          min_rate=0, max_rate=1.1,
                          inc_limit=NULL, title=NULL,
                          policy_name1 = 'Status Quo',
                          policy_name2 = 'Policy 1',
                          watermark=FALSE,
                          weeks_in_year=52L
                          ) {

  type <- match.arg(type)  
  
  data1_for_plot <- copy(data1)
  data2_for_plot <- copy(data2)
  
  if(type == "EMTR"){
    setnames(data1_for_plot, "EMTR", "value1")
    setnames(data2_for_plot, "EMTR", "value2")
    
    y_axis_title <- "Effective Marginal Tax Rate"
    
  } else if(type == "RR"){
    setnames(data1_for_plot, "Replacement_Rate", "value1")
    setnames(data2_for_plot, "Replacement_Rate", "value2")
    
    y_axis_title <- "Replacement Rate"
  } else if(type == "PTR"){
    setnames(data1_for_plot, "Participation_Tax_Rate", "value1")
    setnames(data2_for_plot, "Participation_Tax_Rate", "value2")
    
    y_axis_title <- "Participation Tax Rate"
  } 
  
  
  data1_for_plot[, value1 := pmax(pmin(value1, max_rate), min_rate)]
  data2_for_plot[, value2 := pmax(pmin(value2, max_rate), min_rate)]
  
  data1_for_plot[, value2 := data2_for_plot[,.(value2)]]

  data1_for_plot <- 
    data1_for_plot[, .(gross_wage1, gross_wage1_annual, value1, value2)]
  
  data1_for_plot %<>% melt(id.vars=c('gross_wage1','gross_wage1_annual'),
               variable.name='Scenario')
  
  data1_for_plot[Scenario=="value1", Scenario:=policy_name1] 
  data1_for_plot[Scenario=="value2", Scenario:=policy_name2]
  
  hours_data <- data1
  
  if (type == "PTR") {
    # PTR is undefined at zero
    data1_for_plot <- data1_for_plot[gross_wage1 > 0]
    hours_data <- hours_data[hours1 > 0]
  }
  
  
  data1_for_plot %>% dcast(gross_wage1 + gross_wage1_annual ~ Scenario) %>%
    plot_ly(x = ~gross_wage1_annual, y = ~`Status Quo`, name = policy_name1, 
            mode = "lines", type = 'scatter',
            line = list(color = tsy_palette[1], width = 3),
            hovertemplate =  paste0(
              "Annual gross wage income:\n %{x:$,.2f}\n ",
              policy_name1, ": %{y:.2%}<extra></extra>")) %>% 
    add_trace(y = ~`Reform`, name = policy_name2,
              mode = "lines", type = 'scatter', 
              line = list(color = tsy_palette[2], width = 3, dash = 'dot'),
              hovertemplate = paste(policy_name2, ": %{y:.2%}<extra></extra>"))%>%
    layout(xaxis2 = list(overlaying = "x", nticks = 10, side = "top",
                         title = "Hours/week", automargin=TRUE, size=8,
                         showline = TRUE),
           xaxis = list(title = "Annual gross wage income ($)", 
                        tickformat = "$,",
                        automargin=TRUE,
                        showline = TRUE,
                        mirror=TRUE),
           yaxis = list (title = y_axis_title,
                         tickformat = ".0%", 
                         automargin=TRUE,
                         showline = TRUE,
                         mirror=TRUE),
           legend = list(x = 100, y = 0.5),
           hovermode = "x") %>%
    add_trace(data=hours_data, x = ~hours1, y = ~0, xaxis = "x2",
              showlegend=FALSE, inherit=FALSE,
              hoverinfo="none", type = "scatter", mode = "none")
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
  
  X[, EMTR := 1 - 1L*(shift(Net_Income,1L,type="lead")-Net_Income)]
  X[, EMTR := zoo::na.locf(EMTR)]
  
  return(X)
}

# Plot equivalised incomes
compare_equiv_income_plot <- function(EMTR_table1, EMTR_table2,
                                    inc_limit=NULL, title=NULL,
                                    policy_name1 = 'Status Quo',
                                    policy_name2 = 'Policy 1',
                                    watermark=FALSE,
                                    weeks_in_year=52L
                                    ) {
  
  X1 <- copy(EMTR_table1)
  X2 <- copy(EMTR_table2)
  
  X1[, Equivalised_Income_2 := X2[,.(Equivalised_Income)]]
  X1[, AHC_Equivalised_Income_2 := X2[,.(AHC_Equivalised_Income)]]
  
  X1 <- X1[, .(gross_wage1, gross_wage1_annual, Equivalised_Income, Equivalised_Income_2, AHC_Equivalised_Income, AHC_Equivalised_Income_2)]
  
  X1 %<>% melt(id.vars=c('gross_wage1','gross_wage1_annual'),
               variable.name='Scenario')
  
  X1[Scenario== "Equivalised_Income",Scenario:= paste0("BHC ", policy_name1)] 
  X1[Scenario== "Equivalised_Income_2",Scenario:= paste0("BHC ", policy_name2)]
  
  X1[Scenario=="AHC_Equivalised_Income",Scenario := paste0("AHC ", policy_name1)] 
  X1[Scenario=="AHC_Equivalised_Income_2",Scenario := paste0("AHC ", policy_name2)]
  

  #convert weekly income to annual
  X1[, value:=value*weeks_in_year]
  
  
  X1 %>% dcast(gross_wage1 + gross_wage1_annual ~ Scenario) %>% 
    plot_ly(x = ~gross_wage1_annual,
            y = ~`BHC Status Quo`, name = paste0("BHC ", policy_name1), 
            mode = "lines", type = 'scatter',
            line = list(color = "#56B4E9", width = 3),
            hovertemplate = 
              paste0(
                "Annual gross wage income:\n %{x:$,.2f} \n",
                paste0("BHC ", policy_name1), ": %{y:$,.2f} <extra></extra>")) %>% 
    add_trace(y = ~`BHC Reform`, name = paste0("BHC ", policy_name2),
              mode = "lines", type = 'scatter', 
              line = list(color = "#E69F00", width = 3, dash = 'dot'),
              hovertemplate =
                paste0(paste0("BHC ", policy_name2), ": %{y:$,.2f}  <extra></extra>"))%>%
    add_trace(y = ~`AHC Status Quo`, name = paste0("AHC ", policy_name1),
              mode = "lines", type = 'scatter', 
              line = list(color = "#99eeba", width = 3, dash = 'line'),
              hovertemplate =
                paste0(paste0("AHC ", policy_name1), ": %{y:$,.2f}  <extra></extra>"))%>%
    add_trace(y = ~`AHC Reform`, name = paste0("AHC ", policy_name2),
              mode = "lines", type = 'scatter', 
              line = list(color = "#ee99a3", width = 3, dash = 'dot'),
              hovertemplate =
                paste0(paste0("AHC ", policy_name2), ": %{y:$,.2f}  <extra></extra>"))%>%

    layout(xaxis2 = list(overlaying = "x", nticks = 10, side = "top",
                         title = "Hours/week", automargin=TRUE, size=8,
                         showline = TRUE),
           xaxis = list(title = "Annual gross wage income ($)", 
                        tickformat = "$,", automargin=TRUE,
                        showline = TRUE, mirror=TRUE),
           yaxis = list (title = "Income ($)", tickformat = "$,", 
                         automargin=TRUE,
                         showline = TRUE,  mirror=TRUE),
           legend = list(x = 100, y = 0.5),
           hovermode = "compare") 
}


# Plot poverty depth
poverty_depth_plot <- 
  function(EMTR_table, EMTR_table2,inc_limit=NULL, y_min=NULL, y_max=NULL,
           policy_name1 = 'Status Quo',
           policy_name2 = 'Policy 1',
           watermark=FALSE, weeks_in_year=52L,
           type = "BHC" ){
    
    X1 <- copy(EMTR_table)
    X2 <- copy(EMTR_table2)
    
    if (is.null(inc_limit))
      inc_limit <- X1[,max(gross_wage1_annual)]
    
    if(type == "BHC"){
      setnames(X1, c("Net_Income","BHC_Unequiv_Poverty_Line"), c("Net_Income1","poverty_threshold"))
      setnames(X2, c("Net_Income"), c("Net_Income2"))
      
      y_axis_title <- "BHC "
      
    } else if(type == "AHC"){
      setnames(X1, c("AHC_Net_Income","AHC_Unequiv_Poverty_Line"), c("Net_Income1","poverty_threshold"))
      setnames(X2, c("AHC_Net_Income"), c("Net_Income2"))
      
      y_axis_title <- "AHC "

    }
      
      X1[, Net_Income2 := X2[,.(Net_Income2)]]
      
      Y <- X1[,.(gross_wage1_annual,
                 Net_Income1,
                 Net_Income2,
                 poverty_threshold)]
      
      Y <- Y[, lapply(.SD, function(x) x*weeks_in_year), by = .(gross_wage1_annual, poverty_threshold)]
  
      p <- 
        plot_ly(Y) %>%
        add_trace(x=~hours1, y=~0, line=list(width = 0), xaxis="x2", 
                  data=X1, showlegend=FALSE, inherit=FALSE, 
                  hoverinfo = "none", type = "scatter", mode = "lines") %>%
        layout(xaxis2 = list(overlaying = "x", nticks = 10, side = "top",
                             title = "Hours/week", automargin=TRUE, size=8,
                             showline = TRUE),
               xaxis = list(title = "Annual gross wage income ($)", 
                            tickformat = "$,", 
                            automargin=TRUE,
                            zeroline = TRUE,
                            showline = TRUE,
                            mirror=TRUE),
               yaxis = list (title = "Income ($)", tickformat = "$,", 
                             automargin=TRUE,
                             zeroline = TRUE,
                             showline = TRUE,
                             mirror=TRUE),
               legend = list(x = 100, y = 0.5),
               hovermode = "compare")%>% 
        add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                  y = ~Net_Income1, name = paste0(y_axis_title, policy_name1),
                  fillcolor = 'rgba(168,216,234,0.5)', fill = 'tozeroy',
                  hovertemplate = paste0(paste0(y_axis_title, policy_name1),": %{y:$,.0f}<extra></extra>")) %>% 
        
        add_trace(data = Y, x = ~gross_wage1_annual, type = 'scatter', mode = 'none',
                  y = ~Net_Income2, name = paste0(y_axis_title, policy_name2), 
                  fillcolor = 'rgba(255,212,96,0.5)', fill = 'tozeroy',
                  hovertemplate = paste0(paste0(y_axis_title, policy_name2),": %{y:$,.0f}<extra></extra>")) %>%
        
        add_lines(data = Y, x = ~gross_wage1_annual, 
                        y = ~poverty_threshold, name = 'Poverty threshold (unequivalised)', color = I("black"),
                        hovertemplate = paste("Poverty threshold: %{y:$,.0f}<extra></extra>"))
    
  }

