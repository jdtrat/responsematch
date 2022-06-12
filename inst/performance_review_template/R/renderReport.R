
#' Render an RMarkdown Report Based on User Input
#'
#' This function allows you to render an RMarkdown report within a Shiny app. It
#' displaying a notification "Generating report...".
#'
#' @param input_files The name of the .Rmd file (and any dependencies such as
#'   styling sheets) -- this must be placed within the "www" subdirectory of a
#'   Shiny app.
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
renderReport <- function(input_files, output_format, output_file, params = NULL, ...) {

  # process file in temp directory to avoid any user-permissions problems upon deployment
  temp_paths <- file.path(tempdir(), input_files)

  # For all input files, copy the original from www directory.
  for (i in seq_along(temp_paths)) {
    file.copy(paste0("www/", input_files[i]), temp_paths[i], overwrite = TRUE)
  }

  # Knit the Rmd document in its own environment, isolating its code from the Shiny app code.
  rmarkdown::render(input = grep("*.Rmd$", temp_paths, value = TRUE),
                    output_format = switch(output_format,
                                           "html" = rmarkdown::html_document(),
                                           "pdf" = pagedown::chrome_print(),
                                            "word" = rmarkdown::word_document()),
                    output_file = output_file,
                    params = params,
                    envir = new.env(parent = globalenv()),
                    ...)
}

