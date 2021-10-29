
downloaderInput <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("docFormat"), label = "Document Format",
                 choiceNames = c("PDF", "HTML"),
                 choiceValues = c("pdf", "html")),
    downloadButton(ns("download")),
  )
}

downloaderServer <- function(id, reportData, input_file, params, ...) {

  moduleServer(id, function(input, output, session) {

    # Define downloadable report
    output$download <- downloadHandler(
      # Create a file name using the response ID
      filename = function() {
        paste0(reportData$response_id, "_", extract_before(input_file, ".Rmd"), ".", input$docFormat)
      },
      content = function(file) {

        # If the document format to download is HTML,
        # just serve the rendered HTML file.
        if (input$docFormat == "html") {
          file.copy(from = reportData$output_file_name,
                    to = file)
          # Otherwise, re-render RMarkdown document as PDF.
        } else if (input$docFormat == "pdf") {

          pagedown::chrome_print(input = reportData$output_file_name,
                                 output = file)

        } else {

          renderReport(input_file = input_file,
                       output_format = input$docFormat,
                       output_file = file,
                       params = params,
                       ...)
        }
      }
    )
  })
}
