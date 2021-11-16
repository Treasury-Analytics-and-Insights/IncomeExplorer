
est_MFTC <- IncomeExplorer::estimate_MFTC_from_files(
  parameter_files = list.files("App_Parameters/", full.names = TRUE),
  min_wages = DEFAULT_MIN_WAGES
)

fwrite(est_MFTC, "MFTC_rates.csv")
