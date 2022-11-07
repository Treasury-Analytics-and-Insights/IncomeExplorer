DEFAULT_MIN_WAGES <- c(
  "2014" = 13.75,
  "2015" = 14.25,
  "2016" = 14.75,
  "2017" = 15.25,
  "2018" = 15.75,
  "2019" = 16.50,
  "2020" = 17.70,
  "2021" = 18.90,
  "2022" = 20.00,
  # Assumes the minimum wage is unchanged for future tax years
  "2023" = 21.20,
  "2024" = 21.20,
  "2025" = 21.20
)

estimate_MFTC_from_files <- function(
  parameter_files,
  min_wages = DEFAULT_MIN_WAGES,
  WEP_scaling = 1
) {
  estimated_MFTC_rates <- data.table()
  for (parameter_file in parameter_files){
    tax_year <- parameter_file %>% basename() %>%
      stringr::str_extract("TY[0-9]{2}") %>%
      stringr::str_remove("TY") %>% as.numeric()
    tax_year <- tax_year + 2000
    if (tax_year > 2022) {
      logging::logwarn(
        "Minimum wage for tax year %d is not legislated yet, subject to change",
        tax_year
      )
    }
    if (tax_year %in% names(min_wages)) {
      min_wage <- min_wages[[as.character(tax_year)]]
    } else {
      stop("Minimum wage not defined for tax year ", tax_year)
    }
    this_MFTC_rates <- estimate_MFTC_rates(
      parameter_file,
      min_wage = min_wage,
      steps_per_dollar = 52L,
      WEP_scaling = WEP_scaling
    )
    
    estimated_MFTC_rates <- rbind(estimated_MFTC_rates, this_MFTC_rates, fill = TRUE)
  }
  
  estimated_MFTC_rates[, max_MFTC := max(MFTC_minincome), by = File]
  return(estimated_MFTC_rates)
}