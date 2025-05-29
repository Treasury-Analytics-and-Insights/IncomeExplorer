parameters_from_file <- function(parameters_file) {
  params_type <- tools::file_ext(parameters_file)
  if (params_type %in% c("xlsx", "xls")) {
    parameters_df <- openxlsx::read.xlsx(parameters_file, sheet = 1) %>% as.data.frame()
    parameters <- parameters_from_df(parameters_df)
  } else if (params_type %in% c("yaml", "yml")) {
    parameters <- yaml::read_yaml(parameters_file)
  } else {
    error_msg <- paste0("Unsupported params type: ", params_type)
    stop(error_msg)
  }
  # Rename any missing app parameters if possible
  renamed_app_params <- c(
    "FamilyAssistance_FTC_Rates_FirstChild" = "FamilyAssistance_FTC_Rates_FirstChild0to15",
    "FamilyAssistance_FTC_Rates_SubsequentChild" = "FamilyAssistance_FTC_Rates_SecondChild0to12",
    "FamilyAssistance_IWTC_Eligibility" = "FamilyAssistance_IWTC_IncomeTest",
    "FamilyAssistance_IWTC_IncomeThreshold_Couple" = "FamilyAssistance_IWTC_IncomeThreshold",
    "FamilyAssistance_IWTC_IncomeThreshold_Single" = "FamilyAssistance_IWTC_IncomeThreshold"
  )
  for (app_param in names(renamed_app_params)) {
    if (!(app_param %in% names(parameters))) {
      tawa_param <- renamed_app_params[[app_param]]
      if (tawa_param %in% names(parameters)) {
        print(sprintf("Copying %s to %s", tawa_param, app_param))
        parameters[[app_param]] <- parameters[[tawa_param]]
      } else {
        error_msg <- sprintf(
          "Missing parameter, neither app (%s) nor tawa (%s) parameter found ",
          app_param, tawa_param
        )
        stop(error_msg)
      }
    }
  }
  # Add defaults for any missing system params
  default_system_params <- parameters_from_df(as.data.frame(PARAMS_TEMPLATE))
  for (app_param in names(default_system_params)) {
    if (!(app_param %in% names(parameters))) {
      print(sprintf("Setting missing parameter %s to the default: %s", app_param, default_system_params[app_param]))
      parameters[[app_param]] <- default_system_params[[app_param]]
    }
  }
  # Subset to only the required parameters, drop any extras
  parameters <- parameters[names(default_system_params)]
  return(parameters)
}

parameters_from_df <- function(parameters_df, parameters_column = 2) {
  
  Params_text <- parameters_df[complete.cases(parameters_df), c(1, parameters_column)]
  
  # Replace the names
  Params_text$Parameter <- gsub("\\+", "_", gsub("/", "_", Params_text$Parameter))
  
  # Clean up the parameters
  Params_text[, 2] <- gsub("\\\\", "/", Params_text[, 2])
  Params_text[, 2] <- gsub("\\[\\[", "rbind(c(", Params_text[, 2])
  Params_text[, 2] <- gsub("\\[", "c(", Params_text[, 2])
  Params_text[, 2] <- gsub("]", ")", Params_text[, 2])
  Params_text[, 2] <- gsub(";", ",", Params_text[, 2])
  Params_text[, 2] <- gsub("'", "", Params_text[, 2])
  
  Parameters <- vector(mode = "list", length = dim(Params_text)[1])
  names(Parameters) <- Params_text$Parameter
  
  for (i in 1:nrow(Params_text)) {
    if (!is.na(suppressWarnings(as.numeric(Params_text[i, 2])))) {
      Parameters[[Params_text[i, 1]]] <- suppressWarnings(as.numeric(Params_text[i, 2]))
    } else if (grepl("rbind", Params_text[i, 2])) {
      # AbatementScale or TaxScale - convert to list with thresholds and rates
      a_matrix <- eval(parse(text = Params_text[i, 2]))
      Parameters[[Params_text[i, 1]]] <- list(thresholds = a_matrix[, 1], rates = a_matrix[, 2])
    } else {
      Parameters[[Params_text[i, 1]]] <- Params_text[i, 2]
    }
  }
  
  return(Parameters)
}



wks_in_year <- function(year) {
  is_leap_year <- (!(year %% 4) & (year %% 100)) | !(year %% 400)
  if (is_leap_year) {
    weeks <- 366/7
  } else {
    weeks <- 365/7
  }
  return(weeks)
}

# EMTR - effective marginal tax rate
get_emtr <- function(net_income, next_net_income, steps_per_dollar) {
  emtr <- 1 - steps_per_dollar*(next_net_income - net_income)
  emtr <- zoo::na.locf(emtr)
  return(emtr)
}

# RR - Replacement rate
get_rr <- function(non_working_net_income, working_net_income) {
  rr <- non_working_net_income / working_net_income
  return(rr)
}

# PTR - Participation tax rate
get_ptr <- function(net_income, gross_income) {
  ptr <- 1 - (net_income - first(net_income)) / gross_income
  return(ptr)
}

# The sum of these columns is the definition of Net_Income
net_income_components <- c(
  "net_wage",
  "net_benefit",
  "WFF_abated",
  "MFTC",
  "IETC_abated",
  "WinterEnergy",
  "BestStart_Total",
  "AS_Amount"
)

# Calculate EMTR, RR, and PTR; including decompositions of each rate
calculate_rates <- function(X, net_income_components, steps_per_dollar) {
  # EMTR, including decomposition
  X[, EMTR := get_emtr(Net_Income, shift(Net_Income, 1L, type = "lead"), steps_per_dollar)]
  
  X[, paste0("EMTR_", net_income_components) := lapply(.SD, function(net_income_component) {
    emtr_component <- get_emtr(
      net_income = net_income_component,
      next_net_income = shift(net_income_component, 1L, type = "lead"),
      steps_per_dollar
    ) - 1 # Subtract 1 from components
    return(emtr_component)
  }), .SDcols = net_income_components]
  X[, EMTR_net_wage := EMTR_net_wage + 1] # Add 1 back for the main component
  
  # Replacement rate - note not linear in Net_Income,
  # so for decomposition let's fix the working net income
  X[, RR := get_rr(first(Net_Income), Net_Income)]
  
  X[, paste0("RR_", net_income_components) := lapply(.SD, function(net_income_component) {
    get_rr(
      non_working_net_income = first(net_income_component),
      working_net_income = Net_Income # Keep this fixed when calculating components
    )
  }), .SDcols = net_income_components]
  
  # Participation tax rate, including decomposition
  X[, PTR := get_ptr(Net_Income, gross_wage1 + gross_wage2)]
  
  X[, paste0("PTR_", net_income_components) := lapply(.SD, function(net_income_component) {
    get_ptr(
      net_income = net_income_component,
      gross_income = gross_wage1 + gross_wage2
    ) - 1 # Subtract 1 from components
  }), .SDcols = net_income_components]
  X[, PTR_net_wage := PTR_net_wage + 1] # Add 1 back for the main component
  
  return(X)
}

emtr <- function(
  # System parameters
  Parameters,
  # Family parameters
  Partnered = FALSE,
  wage1_hourly = 16.50,
  Children_ages = c(),
  gross_wage2 = 0,
  hours2 = 0,
  AS_Accommodation_Costs = 0,
  AS_Accommodation_Rent = TRUE,
  AS_Area = 1L,
  # Presentation parameters
  max_wage = 1900,
  steps_per_dollar = 1L,
  weeks_in_year = NULL,
  MFTC_WEP_scaling = NULL,
  pov_thresholds = 0.5,
  bhc_median = 43000,
  ahc_median = 33100) {
  
  # Work out length of the year
  model_year <- Parameters$modelyear
  
  if (is.null(weeks_in_year)){
    weeks_in_year <- wks_in_year(model_year)
  }
  if (is.null(MFTC_WEP_scaling)) {
    MFTC_WEP_scaling <- 1/weeks_in_year
  } else {
    MFTC_WEP_scaling <- MFTC_WEP_scaling / weeks_in_year
  }
  
  # Pull AS MaxRates together
  AS_MaxRate_Mortgage <- matrix(data=c(
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area1,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area2,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area3,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area4,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area1,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area2,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area3,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area4,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area1,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area2,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area3,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area4
  ),  nrow=4, ncol=3)
  
  AS_MaxRate_Rent <- matrix(data=c(
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area1,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area2,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area3,
    Parameters$Accommodation_MaxRate_CoupleDeps_Single2_Deps_Area4,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area1,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area2,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area3,
    Parameters$Accommodation_MaxRate_CoupleNoDeps_Single1Dep_Area4,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area1,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area2,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area3,
    Parameters$Accommodation_MaxRate_SingleNoDeps_Area4
  ),  nrow=4, ncol=3)
  
  # Count children
  N_kids <- length(Children_ages)

  # Convert tax and abatement schedules to weekly -------------------------------
  # Note that benefit abatement scales in TAWA parameters are weekly values scaled to annual assuming a 52.2 week year
  # Other amounts are annual values, and weekly values are obtained by dividing by the number of weeks in a year,
  # except where the legislation specifies otherwise.
  convert_scale_to_weekly <- function(Scale, wks_in_year = 52L) {
    Scale_Weekly <- Scale
    Scale_Weekly$thresholds <- Scale_Weekly$thresholds / wks_in_year
    return(Scale_Weekly)
  }
  
  Tax_BaseScale_Weekly <-
    convert_scale_to_weekly(Parameters$Tax_BaseScale, weeks_in_year)
  Benefits_SPS_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$Benefits_SPS_AbatementScale, 52.2)
  Benefits_JSS_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$Benefits_JSS_AbatementScale, 52.2)
  Benefits_JSS_CoupleAbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$Benefits_JSS_CoupleAbatementScale, 52.2)
  FamilyAssistance_Abatement_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$FamilyAssistance_Abatement_AbatementScale, 365/7)
  IETC_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$IETC_AbatementScale, weeks_in_year)
  IWTC_PhaseInScale_Single_Weekly<-
    convert_scale_to_weekly(Parameters$FamilyAssistance_IWTC_PhaseIn_Single, 52.2)
  IWTC_PhaseInScale_Couple_Weekly<-
    convert_scale_to_weekly(Parameters$FamilyAssistance_IWTC_PhaseIn_Couple, 52.2)  
  FamilyAssistance_BestStart_Abatement_AbatementScale_Weekly <-
    convert_scale_to_weekly(Parameters$FamilyAssistance_BestStart_Abatement_AbatementScale, 365/7)

  
  
  # Convert rates to weekly------------------------------------------------------
  ACC_max_Weekly <- Parameters$ACC_MaxLeviableIncome / weeks_in_year
  FTC_eldest <- Parameters$FamilyAssistance_FTC_Rates_FirstChild / (365/7)
  FTC_subsequent <- Parameters$FamilyAssistance_FTC_Rates_SubsequentChild / (365/7)
  IWTC_first3 <- Parameters$FamilyAssistance_IWTC_Rates_UpTo3Children / 52L
  IWTC_subsequent <- Parameters$FamilyAssistance_IWTC_Rates_SubsequentChildren / 52L
  IETC_rate <- Parameters$IETC_PerYear / weeks_in_year
  IETC_minimum_income <- Parameters$IETC_MinimalIncome / weeks_in_year
  BS_Rate0 <- Parameters$FamilyAssistance_BestStart_Rates_Age0 / (365/7)
  
  BS_Rate1or2 <- Parameters$FamilyAssistance_BestStart_Rates_Age1or2 / (365/7)
  
  WE_Couple_or_Deps_amount <- Parameters$Benefits_WinterEnergy_Rates_CoupleOrDeps*
    MFTC_WEP_scaling
  
  WE_Single_amount <- Parameters$Benefits_WinterEnergy_Rates_Single*
    MFTC_WEP_scaling
  
  MFTC_amount <- Parameters$FamilyAssistance_MFTC_Rates_MinimumIncome / 52L
  
  # Calculate inverse thresholds for the tax system (weekly)
  NIT <- Net_Thresholds(Tax_BaseScale_Weekly)
  
  # Assign benefit rates and abatement schedules ------------------------
  if (Partnered == TRUE) {
    # Couple family 
    if (N_kids == 0L) {
      # No kids 
      Benefit1_Net_0hrs <- Parameters$Benefits_JSS_Rate_Couple
      Benefit2_Net_0hrs <- Parameters$Benefits_JSS_Rate_Couple
      Benefit_Abatement_Scale <- Benefits_JSS_CoupleAbatementScale_Weekly
    } else {
      # At least one kid
      Benefit1_Net_0hrs <- Parameters$Benefits_JSS_Rate_CoupleParent
      Benefit2_Net_0hrs <- Parameters$Benefits_JSS_Rate_CoupleParent
      Benefit_Abatement_Scale <- Benefits_JSS_CoupleAbatementScale_Weekly
    }
    
  } else {
    # Single family 
    Benefit2_Net_0hrs <- 0
    if (N_kids == 0) {
      # No kids
      Benefit1_Net_0hrs <- Parameters$Benefits_JSS_Rate_Single
      Benefit_Abatement_Scale <- Benefits_JSS_AbatementScale_Weekly
      
    } else {
      # At least one kid
      if (min(Children_ages) < Parameters$Benefits_Entitlement_Age_SPS_ChildLowerBound) {
        Benefit1_Net_0hrs <- Parameters$Benefits_SPS_Rate
        Benefit_Abatement_Scale <- Benefits_SPS_AbatementScale_Weekly
      } else {
        # Special case for sole parent whose youngest children is older than
        # Benefits_Entitlement_Age_SPS_ChildLowerBound so the rate is based on JSS,
        # But AbatementScale is based on SPS
        Benefit1_Net_0hrs <- Parameters$Benefits_JSS_Rate_SoleParent
        Benefit_Abatement_Scale <- Benefits_SPS_AbatementScale_Weekly # NB!
      }
      
    }
  }
  
  # Assign AS abatement point, entry threshold, and maximum --------------------
  if (N_kids > 0L & !Partnered) {
    AS_Abate_Point <- 
      ceiling(weeks_in_year *
                Abatement_Vanishing_Point(Benefits_JSS_AbatementScale_Weekly, 
                                          Parameters$Benefits_JSS_Rate_SoleParent))/weeks_in_year
  }else{
    AS_Abate_Point <-
      ceiling(weeks_in_year * 
              Abatement_Vanishing_Point(Benefit_Abatement_Scale,
                                        Benefit1_Net_0hrs))/weeks_in_year
  }
  
  AS_entry_threshold <- 
    ifelse(AS_Accommodation_Rent,
           Parameters$Accommodation_BaseRateThreshold_Rent,
           Parameters$Accommodation_BaseRateThreshold_Mortgage) *
    ((Benefit1_Net_0hrs + Benefit2_Net_0hrs) + (N_kids > 0L) * FTC_eldest)
  
  
  if (AS_Accommodation_Rent){
    AS_Maximum <-
      AS_MaxRate_Rent[AS_Area, pmax(2L - N_kids - 1L * Partnered, 0L) + 1L]
  } else {
    AS_Maximum <-
      AS_MaxRate_Mortgage[AS_Area, pmax(2L - N_kids - 1L * Partnered, 0L) + 1L]
  }
  
  # Initiate the output table --------------------------------------------------
  X <- data.table(gross_wage1 = seq(0, max_wage, 1 / steps_per_dollar))
  X[, hours1 := gross_wage1 / wage1_hourly]
  X[, gross_wage1_annual := weeks_in_year * gross_wage1]
  
  # Partner wage ---------------------------------------------------------------
  X[, gross_wage2 := gross_wage2]

  # These are zero by default --------------------------------------------------
  X[, wage2_tax := 0]
  X[, wage2_ACC_levy := 0]
  X[, net_wage2 := 0]
  X[, net_benefit2 := 0]
  X[, gross_benefit2 := 0]

  X[, IETC_abated1 := 0]
  X[, IETC_abated2 := 0]

  # Abate benefit --------------------------------------------------------------
  X[, net_benefit1 := 
      Abate(TRUE, Benefit1_Net_0hrs, 
            Benefit_Abatement_Scale, gross_wage1 + gross_wage2)]

  
  
  if (Partnered){
    X[, net_benefit2 := Abate(TRUE, Benefit2_Net_0hrs, Benefit_Abatement_Scale,
                              gross_wage1+gross_wage2)]
  }
  
  # Assign IWTC ----------------------------------------------------------------
  # Parameters$FamilyAssistance_IWTC_Eligibility
  #  0 : hours test and do not give to beneficiaries
  #  1 : income test
  #  2 : phase in 
  if (Parameters$FamilyAssistance_IWTC_Eligibility == 1){
    # Use income test 
    
    # Assign income test based on single/couple
    if (Partnered == TRUE){
      IWTC_IncomeThreshold <- Parameters$FamilyAssistance_IWTC_IncomeThreshold_Couple/52.2
    } else {
      IWTC_IncomeThreshold <- Parameters$FamilyAssistance_IWTC_IncomeThreshold_Single/52.2
    }
    
    # Eligible if have children and wage exceeds the threshold
    X[, IWTC_eligible := 1L * (N_kids > 0L) * ((gross_wage1 + gross_wage2) >=
                                                 IWTC_IncomeThreshold)]
    
    # Calculate unabated IWTC
    X[, IWTC_unabated := IWTC_eligible * (IWTC_first3 + max(0L, N_kids - 3L) *
                                            IWTC_subsequent)]
    
    # Parameters$FamilyAssistance_IWTC_ToBeneficiaries determines if beneficiaries are eligible for IWTC
    # 0 : Not eligible
    # 1 : Eligible
    if (Parameters$FamilyAssistance_IWTC_ToBeneficiaries == 0){
      # If IWTC eligible, zero out benefit
      X[IWTC_unabated>0, 
        `:=`(net_benefit1 = 0,  net_benefit2 = 0)]
    }
    
    # tidy up
    X[, IWTC_eligible := NULL]
    
  } else if (Parameters$FamilyAssistance_IWTC_Eligibility == 2){
    # Use phase-in method 
    
    # Phase in scale
    if (Partnered == TRUE){
      IWTC_PhaseInScale_Weekly <- IWTC_PhaseInScale_Couple_Weekly
    } else {
      IWTC_PhaseInScale_Weekly <- IWTC_PhaseInScale_Single_Weekly
    }
    # Maximum amount
    IWTC_max <- IWTC_first3 + max(0L, N_kids - 3L) * IWTC_subsequent
    
    # Calcuated phased-in/unabated amount
    X[, IWTC_unabated := (N_kids > 0L)*
        pmin(IWTC_max,
             Abate(TRUE, 0, IWTC_PhaseInScale_Weekly, gross_wage1+gross_wage2))]
  } else {
    # Hours test and do not give to beneficiaries
    
    # Hours threshold
    if (Partnered == TRUE){ 
      FullTimeWorkingHours <- Parameters$FamilyAssistance_FullTimeWorkingHours_Couple
      
    } else {
      FullTimeWorkingHours <- Parameters$FamilyAssistance_FullTimeWorkingHours_Single
    }
    
    X[, IWTC_eligible := 1L * (N_kids > 0L) * ((hours1 + hours2) >= FullTimeWorkingHours)]
    
    # Calculate unabated IWTC
    X[, IWTC_unabated := IWTC_eligible * (IWTC_first3 + max(0L, N_kids - 3L) *
                                            IWTC_subsequent)]
    
    # If receiving IWTC, zero out benefit
    X[IWTC_unabated >0, 
      `:=`(net_benefit1 = 0, net_benefit2 = 0)]
    
    # tidy up
    X[, IWTC_eligible := NULL]
  }
  
  # Back out Gross benefit
  X[, gross_benefit1 := Gross_From_Net(net_benefit1, NIT, Tax_BaseScale_Weekly)]
  X[, gross_benefit1 := Gross_From_Net(net_benefit1, NIT, Tax_BaseScale_Weekly)]

  
  if (Partnered){
    X[, gross_benefit2 := Gross_From_Net(net_benefit2, NIT, Tax_BaseScale_Weekly)]
  }
  
  # Add wage on to benefit and tax
  X[, gross_benefit_and_wage1 := gross_benefit1 + gross_wage1]
  X[, net_benefit_and_wage1 := Net_From_Gross(gross_benefit_and_wage1, Tax_BaseScale_Weekly)]
  X[, wage1_tax := (gross_benefit_and_wage1 - net_benefit_and_wage1) - 
      (gross_benefit1 - net_benefit1)]
  
  if (Partnered) {
    X[, gross_benefit_and_wage2 := gross_benefit2 + gross_wage2]
    X[, net_benefit_and_wage2 := Net_From_Gross(gross_benefit_and_wage2, Tax_BaseScale_Weekly)]
    X[, wage2_tax := (gross_benefit_and_wage2 - net_benefit_and_wage2) - (gross_benefit2 - net_benefit2)]
  }
  
  # Work out ACC levy
  X[, wage1_ACC_levy := pmin(gross_wage1,ACC_max_Weekly)*Parameters$ACC_LevyRate]
  
  if (Partnered){
    X[, wage2_ACC_levy := pmin(gross_wage2,ACC_max_Weekly)*Parameters$ACC_LevyRate]
  }
  
  # Form net wage
  X[, net_wage1 := gross_wage1 - wage1_tax - wage1_ACC_levy]
  
  if (Partnered){
    X[, net_wage2 := gross_wage2 - wage2_tax - wage2_ACC_levy]
  }
  
  # Form unabated FTC
  X[, FTC_unabated := (N_kids>0L)*FTC_eldest+max(N_kids-1L,0L)*FTC_subsequent]
  
  
  
  # Hours test for MFTC ------------
  
  # Hours threshold
  if (Partnered == TRUE){ 
    FullTimeWorkingHours <- Parameters$FamilyAssistance_FullTimeWorkingHours_Couple
    
  } else {
    FullTimeWorkingHours <- Parameters$FamilyAssistance_FullTimeWorkingHours_Single
  }
  
  
  X[, MFTC_eligible :=  (N_kids > 0L) &
      ((hours1 + hours2) >= FullTimeWorkingHours) &
      ((net_benefit1 + net_benefit2) == 0)]
  
  
  # Work out MFTC amount (if benefit == 0)
  X[, MFTC := MFTC_eligible *
      (pmax(MFTC_amount - (gross_wage1 + gross_wage2 - wage1_tax - wage2_tax),0))]
  
  
  # tidy up
  X[, MFTC_eligible := NULL]
  
  
  # Abate FTC and IWTC ------------
  X[, AbateAmount :=
      Apply(gross_wage1 + gross_wage2 + gross_benefit1 + gross_benefit2,
            FamilyAssistance_Abatement_AbatementScale_Weekly)]
  
  
  # If FamilyAssistance_Abatement_Order has not been defined
  # then define it with default value: 0
  # So old parameter files should still work 
  if(is.null(Parameters$FamilyAssistance_Abatement_Order)){
    Parameters$FamilyAssistance_Abatement_Order <- 0
  }
  
  # Changing the order FTC and IWTC abatement ---------------------
  if(Parameters$FamilyAssistance_Abatement_Order == 0){
    # Abate FTC first then IWTC
    X[, FTC_abated := pmax(0, FTC_unabated - AbateAmount)]
    X[, RemainingAbatement := pmax(0, AbateAmount - FTC_unabated)]
    X[, IWTC_abated := pmax(0, IWTC_unabated - RemainingAbatement)]
  } else{
    # Abate IWTC first then FTC
    X[, IWTC_abated := pmax(0, IWTC_unabated - AbateAmount)]
    X[, RemainingAbatement := pmax(0, AbateAmount - IWTC_unabated)]
    X[, FTC_abated := pmax(0, FTC_unabated - RemainingAbatement)]
  }
  
  # tidy up
  X[, RemainingAbatement := NULL]
  
  # If receiving best start they shouldn’t be able to receive IETC ---------------
  # Best Start ------------
  BS_kids_aged0 <- sum(Children_ages == 0L) * (model_year >= 2019)
  BS_kids_aged1or2 <- (
    sum(Children_ages == 1L) * (model_year >= 2020) +
      sum(Children_ages == 2L) * (model_year >= 2021)
  )
  
  X[, c("BestStart_Abated", "BestStart_Universal") := 0] # initialise to zero
  
  if (model_year %between% c(2019, 2026)) {
    # When BestStart was first introduced in TY19,
    # it was universal for children aged 0, and abated for children aged 1 or 2
    X[, BestStart_Universal := BS_kids_aged0*BS_Rate0]
    X[, BestStart_Abated := Abate(TRUE, BS_kids_aged1or2*BS_Rate1or2,
                                  FamilyAssistance_BestStart_Abatement_AbatementScale_Weekly,
                                  gross_wage1+gross_wage2+gross_benefit1+gross_benefit2)]
  } else if (model_year >= 2027) {
    # Budget 2025: remove universality for children aged 0, from TY27 onwards
    X[, BestStart_Abated := Abate(TRUE, BS_kids_aged0*BS_Rate0 + BS_kids_aged1or2*BS_Rate1or2,
                                  FamilyAssistance_BestStart_Abatement_AbatementScale_Weekly,
                                  gross_wage1+gross_wage2+gross_benefit1+gross_benefit2)]
  }
  X[, BestStart_Total := BestStart_Universal + BestStart_Abated]
  
  
  # Work out IETC -----------
  X[, IETC_eligible1 := (net_benefit1 == 0) & (FTC_abated == 0) &
      (IWTC_abated == 0) & (MFTC == 0) & (BestStart_Total == 0) &
      (gross_wage1 >= IETC_minimum_income)]
  
  X[IETC_eligible1 == TRUE, 
    IETC_abated1 := Abate(TRUE, IETC_rate, IETC_AbatementScale_Weekly, 
                          gross_wage1)]
  
  
  X[, IETC_eligible2 := (net_benefit2 == 0) & (FTC_abated == 0) & 
      (IWTC_abated == 0) & (MFTC == 0) & (BestStart_Total == 0) & 
      (gross_wage2 >= IETC_minimum_income)]
  
  
  X[IETC_eligible2 == TRUE, 
    IETC_abated2 := Abate(TRUE, IETC_rate, IETC_AbatementScale_Weekly, gross_wage2)]
   
  
  X[Parameters$IETC_OnlyFamiliesWithoutChildren == TRUE & N_kids > 0L, 
    ':=' (IETC_abated1 = 0,
          IETC_abated2 = 0)]
  
  # tidy up
  X[, IETC_eligible1 := NULL]
  X[, IETC_eligible2 := NULL]
  
  
  
  
  # Winter Energy --------
  X[, WE_Couple_or_Deps := 0]
  X[, WE_Single := 0]
  
  X[, WE_eligible := (net_benefit1 + net_benefit2) > 0]
  
  X[, WE_Couple_or_Deps_eligible := WE_eligible == TRUE & 
      (N_kids > 0L | Partnered)]

  X[WE_Couple_or_Deps_eligible == TRUE,
    WE_Couple_or_Deps := WE_Couple_or_Deps_amount]

  X[, WE_Single_eligible :=  WE_eligible == TRUE & 
      ((!Partnered) & N_kids == 0L)]

  X[WE_eligible == TRUE & WE_Single_eligible == TRUE,
    WE_Single := WE_Single_amount]
  
  X[, WinterEnergy := WE_Couple_or_Deps +  WE_Single]

  # tidy up
  X[, WE_eligible := NULL]
  X[, WE_Couple_or_Deps := NULL]
  X[, WE_Couple_or_Deps_eligible := NULL]
  X[, WE_Single := NULL]
  X[, WE_Single_eligible := NULL]
  
  # Accommodation Supplement -----------
  X[, AS_Amount := pmax(
    pmin(Parameters$Accommodation_PaymentPercentage * (AS_Accommodation_Costs - AS_entry_threshold),
         AS_Maximum) -
      pmax(gross_wage1 + gross_wage2 - AS_Abate_Point, 0) * 
      Parameters$Accommodation_AbatementRate * ((net_benefit1 + net_benefit2) == 0), 0)]
  
  # Combo columns
  X[, ":="(
    net_wage = net_wage1 + net_wage2,
    net_benefit = net_benefit1 + net_benefit2,
    benefit_tax = -(gross_benefit1 + gross_benefit2 - net_benefit1 - net_benefit2),
    gross_wage = gross_wage1 + gross_wage2,
    wage_tax_and_ACC = -(wage1_tax + wage2_tax + wage1_ACC_levy + wage2_ACC_levy),
    IETC_abated = IETC_abated1 + IETC_abated2,
    WFF_abated = FTC_abated + IWTC_abated
  )]
  
  # Net income - see definition of `net_income_components`
  # As in TAWA proc the disposable income is calculated as: 
  # P_Income_Total + P_FamilyAssistance_Total + P_TaxCredit_IETC - P_Income_TaxPayable - P_ACC_LevyPayable
  # Same definition as "Net_Income", so use "Net_Income" as disposable income.
  X[, Net_Income := rowSums(.SD), .SDcols = net_income_components]
  X[, Net_Income_annual := Net_Income*weeks_in_year]
  
  # Calculate EMTRs, RRs, and PTRs; and their decomposition into each
  # net income component
  X <- calculate_rates(X, net_income_components, steps_per_dollar)
  
  # Number of children under 14
  LT_14 <- sum(Children_ages < 14)
  
  # Number of children over 14
  GTE_14 <- sum( Children_ages >= 14) + ifelse(Partnered == FALSE, 1, 2)
  
  # Modified OECD
  moecd_eq_factor <- 1 + 0.5 * (GTE_14 - 1) + 0.3 * (LT_14)
  
  # Equivalised Income
  X[, Equivalised_Income := Net_Income / moecd_eq_factor]

  # BHC Depth
  X[, BHC_Depth := (moecd_eq_factor * (pov_thresholds*bhc_median - pmax(0, Equivalised_Income*weeks_in_year)))/weeks_in_year]
 
  # After housing cost disposable income
  X[, AHC_Net_Income := Net_Income - AS_Accommodation_Costs]
  
  # Equivalised Income
  X[, AHC_Equivalised_Income := AHC_Net_Income / moecd_eq_factor]
  
  # AHC Depth
  X[, AHC_Depth := (moecd_eq_factor * (pov_thresholds*ahc_median - pmax(0, AHC_Equivalised_Income*weeks_in_year)))/weeks_in_year]
  
  # Unequivalised median
  X[, ":=" (BHC_Unequiv_Poverty_Line = pov_thresholds * moecd_eq_factor * bhc_median,
            AHC_Unequiv_Poverty_Line = pov_thresholds * moecd_eq_factor * ahc_median,
            Eq_Factor = moecd_eq_factor)]
  
  return(X)
}


