viewReportUI <- function(id) {
  ns <- NS(id)
  tagList(
    includeScript("www/hideSurvey.js"),
    uiOutput(ns("surveyReport"))
  )

}

viewReportServer <- function(id, reportData, input_file, params, ...) {

  moduleServer(id, function(input, output, session) {

    observeEvent(reportData$htmlRendered, {
      removeModal()
      session$sendCustomMessage('hideSurvey', list())

      ns <- session$ns

      output$surveyReport <- renderUI({

        sidebarLayout(
          sidebarPanel(width = 2,
                       style = "margin-top: 45px;",
                       downloaderInput(id = ns("reportDL"))
          ),
          mainPanel(
            includeHTML(reportData$output_file_name)
          )
        )
      })

      downloaderServer(id = "reportDL", # change ID based on survey
                       reportData = reportData, # DON'T TOUCH
                       input_file = input_file, # Change Rmd file name as needed. This file must be in www/ subdirectory.
                       # Add params as needed to pass into RMarkdown document
                       params = params,
                       ...
      )

    })

  })
}
