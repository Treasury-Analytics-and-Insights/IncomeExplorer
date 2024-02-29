fileInputButton <- function(
    inputId, buttonLabel = "Browse...", icon = NULL,
    multiple = FALSE, accept = NULL, width = NULL, capture = NULL
)
{
  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(
    id = inputId, class = "shiny-input-file",
    name = inputId, type = "file",
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restoredValue
  )
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  if (!is.null(capture)) {
    inputTag$attribs$capture <- capture
  }
  tags$label(span(class = "btn btn-default", list(icon, buttonLabel), inputTag))
}

actionButtonLoading <- function(inputId, label, icon = NULL, ...) {
  busy_loading_class <- "fa-spinner fa-spin"
  js_loading_busy <- sprintf(
    "$('#%s').attr('disabled', true); $('#%s i').toggleClass('%s');",
    inputId, inputId, busy_loading_class
  )
  actionButton(inputId, label, icon, onclick = js_loading_busy, ...)
}
