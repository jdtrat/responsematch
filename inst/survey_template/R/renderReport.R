
#' Render an RMarkdown Report Based on User Input
#'
#' This function allows you to render an RMarkdown report within a Shiny app. It
#' displaying a notification "Generating report...".
#'
#' @param input_file The name of the .Rmd file -- this must be placed within the
#'   "www" subdirectory of a Shiny app.
#' @param output_format The format to render the .Rmd file as. Valid options are
#'   "html", "pdf", or "word".
#' @param output_file Where should the file be rendered to?
#' @param params NULL by default. If this is a parameterized RMarkdown report,
#'   supply a named list. See
#'   \url{https://bookdown.org/yihui/rmarkdown/params-knit.html} for more
#'   details.
#' @param ... Additional arguments to pass to [rmarkdown::render()]
#'
#' @return A rendered report to the 'output_file' location.
#' @export
#'
renderReport <- function(input_file, output_format, output_file, params = NULL, ...) {

  # process file in temp directory to avoid any user-permissions problems upon deployment
  temp_path <- file.path(tempdir(), input_file)
  file.copy(paste0("www/", input_file), temp_path, overwrite = TRUE)

  # Show a notification that report is generating
  report_id <- shiny::showNotification(
    "Generating report...",
    duration = NULL,
    closeButton = FALSE
  )
  on.exit(shiny::removeNotification(report_id), add = TRUE)

  # setup parameters to pass into GLI Report

  # Knit the document in its own environment, isolating its code from the Shiny app code.
  rmarkdown::render(input = temp_path,
                    output_format = switch(output_format,
                                           "html" = rmarkdown::html_document(),
                                           "pdf" = rmarkdown::pdf_document(),
                                            "word" = rmarkdown::word_document()),
                    output_file = output_file,
                    params = params,
                    envir = new.env(parent = globalenv()),
                    ...)
}

