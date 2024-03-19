# Based on the `create_blob` function of downloadthis
# https://github.com/fmmattioni/downloadthis/blob/master/R/utils.R
get_js_download_file <- function(server_file_path, download_file_name) {
  base64_data <- sprintf(
    "data:%s;base64,%s",
    mime::guess_type(file = server_file_path),
    base64enc::base64encode(server_file_path)
  )
  js_function <- sprintf(
    "
      fetch('%s').then(res => res.blob()).then(blob => {
        const downloadURL = window.URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.style.display = 'none'; // Hide the link
        document.body.appendChild(a);
        a.href = downloadURL;
        a.download = '%s';
        a.click();
        window.URL.revokeObjectURL(downloadURL);
        document.body.removeChild(a);
        return false;
      });",
    base64_data, download_file_name
  )
  return(js_function)
}

save_app_results <- function(details, parameter_differences, scenario_incomes, output_path) {
  scenario_names <- scenario_incomes[, unique(Scenario)]
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Details")
  openxlsx::writeData(wb, "Details", names(details), startCol = 1)
  openxlsx::writeData(wb, "Details", details, startCol = 2)
  
  if (length(scenario_names) > 1) {
    # Parameters that changed
    openxlsx::addWorksheet(wb, "Scenario Differences")
    openxlsx::writeData(wb, "Scenario Differences", parameter_differences)
  }
  
  # Full sets of results (should probably be more selective)
  for (scenario in scenario_names) {
    openxlsx::addWorksheet(wb, scenario)
    openxlsx::writeData(wb, scenario, scenario_incomes[Scenario == scenario])
  }
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
}
