# Creates app parameter yaml files from TAWA yaml parameters.
# To be used and run with each new EFU once it is publicly available.

tawa_param_paths <- c(
  "inst/TY25_HYEFU24.yaml",
  "inst/TY26_HYEFU24.yaml",
  "inst/TY27_HYEFU24.yaml",
  "inst/TY28_HYEFU24.yaml",
  "inst/TY29_HYEFU24.yaml"
)
output_paths <- c(
  "inst/parameters/TY25_HYEFU24.yaml",
  "inst/parameters/TY26_HYEFU24.yaml",
  "inst/parameters/TY27_HYEFU24.yaml",
  "inst/parameters/TY28_HYEFU24.yaml",
  "inst/parameters/TY29_HYEFU24.yaml"
)
for (ii in seq_along(tawa_param_paths)) {
  tawa_param_path <- tawa_param_paths[[ii]]
  output_path <- output_paths[[ii]]
  params <- IncomeExplorer::parameters_from_file(tawa_param_path)
  yaml::write_yaml(params, output_path)
}
