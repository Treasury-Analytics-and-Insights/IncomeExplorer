# this is to be used and run with each new EFU

# Split up a TAWA parameters file into several TAWA parameter files,
# one for each Tax Year

# before running: set working directory to MFTC_calculator and create a folder
# named TAWA_Parameters

IncomeExplorer::split_TAWA_parameters(
  params_path = "",
  output_suffix = "",
  output_dir = "TAWA_Parameters"
)

# Convert each TAWA parameter file to IncomeExplorer App format
IncomeExplorer::TAWA_to_app_param_files(
  input_param_paths = list.files("TAWA_Parameters/", full.names = TRUE),
  output_param_dir = "App_Parameters/",
  output_suffix = "IncomeExplorer_",
  output_template_path = "App_Parameters_Template.xlsx"
)
