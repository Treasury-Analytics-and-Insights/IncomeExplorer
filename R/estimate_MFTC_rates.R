##### Find Net income crossing for staying on benefit vs getting IWTC ####
## Then MFTC is the gross_wage - wage_tax + net_income_diff amount
## at the point just before the crossing.

find_MFTC <- function(
  emtr_on_ben,
  emtr_working,
  min_wage,
  ben_hours_test
) {
  wage_threshold <- min_wage*ben_hours_test
  start_index <- emtr_on_ben[gross_wage1 < wage_threshold, .N]
  
  # Check if there is a crossing
  net_income_diff <-
    emtr_on_ben[gross_wage1 >= wage_threshold, Net_Income] -
      emtr_working[gross_wage1 >= wage_threshold, Net_Income]
  MFTC_required <- any(net_income_diff > 0) & any(net_income_diff < 0)
  
  if (!MFTC_required) {
    print("MFTC not required")
    proposed_MFTC <- NA
  } else {
    relative_crossing_index <- which(net_income_diff < 0)[1] - 1
    crossing_index <- start_index + relative_crossing_index
    
    boost <- net_income_diff[relative_crossing_index]
    
    proposed_MFTC_weekly <- emtr_working[
      crossing_index, gross_wage1 - wage1_tax + gross_wage2 - wage2_tax + boost
    ]
    proposed_MFTC <- ceiling(proposed_MFTC_weekly)*52L
    if (is.na(proposed_MFTC)) {
      stop("MFTC estimated to be NA but there should have been a zero crossing")
    }
  }
  return(proposed_MFTC)
}

#' Estimate MFTC rates for a given parameter file
#'
#' @param parameter_file Parameter file (*.xlsx) in the IncomeExplorer format
#'                       supported by the `emtr` function
#' @param min_wage Minimum wage
#' @param steps_per_dollar Number of steps to calculate, per dollar of gross earned income
#' @param WEP_scaling Scaling of the annual Winter Energy amount.
#'                    Prior to Budget 2021, was 12/5 (annualised winter week).
#'                    Since Budget 2021, defaults to 1 (average over year).
#'
#' @return
#' @export
#'
#' @examples
estimate_MFTC_rates <- function(
  parameter_file,
  min_wage,
  steps_per_dollar = 1L,
  WEP_scaling = 1
) {
  parameters <- parameters_from_file(parameter_file)

  # Parameters for family working and not on benefit (IWTC not turned off)
  parameters_NoMFTC <- copy(parameters)
  parameters_NoMFTC$FamilyAssistance_MFTC_Rates_MinimumIncome <- 0
  
  # Parameters for family on Benefit and working (IWTC turned off)
  parameters_NoMFTC_NoIWTC <- copy(parameters_NoMFTC)
  parameters_NoMFTC_NoIWTC$FamilyAssistance_IWTC_Rates_UpTo3Children <- 0
  parameters_NoMFTC_NoIWTC$FamilyAssistance_IWTC_Rates_SubsequentChildren <- 0
  
  ##### Family 1 - JSS couple, 50/50 income split
  MFTC_Couple_5050_working <- emtr(
    parameters_NoMFTC,
    Partnered = T,
    wage1_hourly = min_wage,
    Children_ages = c(10),
    gross_wage2 = min_wage*15,
    hours2 = 15,
    AS_Accommodation_Costs = 0,
    AS_Accommodation_Rent = T,
    AS_Area = 1L,
    max_wage = min_wage*50,
    steps_per_dollar = steps_per_dollar,
    weeks_in_year = 52L,
    MFTC_WEP_scaling = WEP_scaling
  )
  
  MFTC_Couple_5050_benefit <- emtr(
    parameters_NoMFTC_NoIWTC,
    Partnered = T,
    wage1_hourly = min_wage,
    Children_ages = c(10),
    gross_wage2 = min_wage*15,
    hours2 = 15,
    AS_Accommodation_Costs = 0,
    AS_Accommodation_Rent = T,
    AS_Area = 1L,
    max_wage = min_wage*50,
    steps_per_dollar = steps_per_dollar,
    weeks_in_year = 52L,
    MFTC_WEP_scaling = WEP_scaling
  )
  
  # Family 2 - SPS sole parent
  MFTC_SoleParent_working <- emtr(
    parameters_NoMFTC,
    Partnered = F,
    wage1_hourly = min_wage,
    Children_ages = c(10),
    gross_wage2 = 0,
    hours2 = 0,
    AS_Accommodation_Costs = 0,
    AS_Accommodation_Rent = T,
    AS_Area = 1L,
    max_wage = min_wage*50,
    steps_per_dollar = steps_per_dollar,
    weeks_in_year = 52L,
    MFTC_WEP_scaling = WEP_scaling
  )
  
  MFTC_SoleParent_benefit <- emtr(
    parameters_NoMFTC_NoIWTC,
    Partnered = F,
    wage1_hourly = min_wage,
    Children_ages = c(10),
    gross_wage2 = 0,
    hours2 = 0,
    AS_Accommodation_Costs = 0,
    AS_Accommodation_Rent = T,
    AS_Area = 1L,
    max_wage = min_wage*50,
    steps_per_dollar = steps_per_dollar,
    weeks_in_year = 52L,
    MFTC_WEP_scaling = WEP_scaling
  )
  
  ##### Find Net income crossing for staying on benefit vs getting IWTC ####
  ## Then MFTC is the gross_wage - wage_tax amount at the crossing.
  proposed_MFTC_SoleParent <- find_MFTC(
    MFTC_SoleParent_benefit,
    MFTC_SoleParent_working,
    min_wage = min_wage,
    ben_hours_test = 20
  )
  
  proposed_MFTC_Couple_5050 <- find_MFTC(
    MFTC_Couple_5050_benefit,
    MFTC_Couple_5050_working,
    min_wage = min_wage,
    ben_hours_test = 15
  )
  
  out <- data.table(
    File = basename(parameter_file),
    Family = c("Couple_50-50", "SoleParent"),
    MFTC_minincome = c(proposed_MFTC_Couple_5050, proposed_MFTC_SoleParent)
  )
  return(out)
}
