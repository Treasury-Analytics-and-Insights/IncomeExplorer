library(data.table)
library(magrittr)
library(glue)

efu <- "BEFU24"
param_dir <- glue("inst/tawa_parameters")
param_paths <- list.files(param_dir, pattern = glob2rx("*.yaml"), full.names = TRUE)
for (param_path in param_paths) {
  params <- IncomeExplorer::parameters_from_file(param_path)
  yaml_param_filename <- basename(param_path) %>%
    tools::file_path_sans_ext() %>%
    stringr::str_remove(paste0("_", efu)) %>%
    {paste0(., "_", efu, ".yaml")}
  yaml::write_yaml(params, file.path("inst/parameters", yaml_param_filename))
}
