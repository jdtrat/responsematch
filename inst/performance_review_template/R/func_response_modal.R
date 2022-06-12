

#' Title
#'
#' @param survey_info
#'
#' @return
#' @export
#'
#' @examples
show_response_modal <- function(survey_info) {
  if (is.na(survey_info$report_file)) {
    shiny::showModal(
      shiny::modalDialog(
        title = "Thank you!",
        "Your response has been recorded. \n
        You may now close your browser window."
        )
      )
  } else {
    shiny::showModal(
      shiny::modalDialog(title = "Thank you!",
                         "Your response has been recorded. \n
                         You may now close your browser window or click 'Generate Report' to view and/or download your results.",
                         footer = actionButton("generateReport", "Generate Report")
      )
    )
  }
}
