# Convert TAWA parameter files to IncomeExplorer app parameter files
TAWA_to_app_param_files <- function(
  input_param_paths, output_param_dir, output_suffix,
  output_template_path = "App_Parameters_Template.xlsx") {
  
  # Rename
  new_names <- data.table(
    Parameter = c(
      "FamilyAssistance/FTC/Rates/FirstChild0to15",
      "FamilyAssistance/FTC/Rates/SecondChild0to12",
      "FamilyAssistance/IWTC/IncomeTest",
      "FamilyAssistance/IWTC/IncomeThreshold",
      "FamilyAssistance/IWTC/IncomeThreshold",
      "Accommodation/MaxRate/CoupleDeps_Single2+Deps/Area1",
      "Accommodation/MaxRate/CoupleDeps_Single2+Deps/Area2",
      "Accommodation/MaxRate/CoupleDeps_Single2+Deps/Area3",
      "Accommodation/MaxRate/CoupleDeps_Single2+Deps/Area4"
    ),
    New_Parameter = c(
      "FamilyAssistance/FTC/Rates/FirstChild",
      "FamilyAssistance/FTC/Rates/SubsequentChild",
      "FamilyAssistance/IWTC/Eligibility",
      "FamilyAssistance/IWTC/IncomeThreshold/Couple",
      "FamilyAssistance/IWTC/IncomeThreshold/Single",
      "Accommodation/MaxRate/CoupleDeps_Single2_Deps/Area1",
      "Accommodation/MaxRate/CoupleDeps_Single2_Deps/Area2",
      "Accommodation/MaxRate/CoupleDeps_Single2_Deps/Area3",
      "Accommodation/MaxRate/CoupleDeps_Single2_Deps/Area4"
    )
  ) 
  # template
  wb_template <- openxlsx::loadWorkbook(output_template_path)
  parameters <- openxlsx::readWorkbook(wb_template) %>% setDT()
  parameters <- parameters[, .(Parameter,Value, .I)]
  
  for (old_file in input_param_paths) {
    # old file
    old_parameters <- openxlsx::readWorkbook(old_file) %>% setDT()
    setnames(old_parameters, c("Parameter", "Value"))
    
    old_parameters <- merge(old_parameters, new_names, by = "Parameter", all = TRUE)
    changed_parameters <- old_parameters[
      New_Parameter != Parameter, .(old = Parameter, new = New_Parameter)
    ]
    
    old_parameters[is.na(New_Parameter), New_Parameter := Parameter]
    old_parameters <- old_parameters[, .(Parameter = New_Parameter, Value)]
    
    # merge existing values with template structure
    new_parameters <- merge(
      parameters, old_parameters,
      by = "Parameter", all = TRUE
    )
    new_parameters[, Value := Value.y]
    new_parameters[is.na(Value.y), Value := Value.x]
    
    logging::loginfo(paste0("Reformatting file: ", old_file))
    logging::loginfo(paste0("Dropped Parameters: ", new_parameters[is.na(Value.x), Parameter]))
    logging::loginfo(paste0("New Parameters: ", new_parameters[is.na(Value.y), Parameter]))
    logging::loginfo(paste0("Renamed Parameters: ", changed_parameters[, new]))
    
    # save new parameters
    new_parameters <- new_parameters[!is.na(Value.x)]
    openxlsx::writeData(
      wb_template, 1,
      new_parameters[order(I), .(Parameter, Value)],
      startRow = 2, startCol = 2
    )
    output_path <- file.path(output_param_dir, paste0(output_suffix, basename(old_file)))
    openxlsx::saveWorkbook(wb_template, output_path, overwrite = TRUE)
  }
}
