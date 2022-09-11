library(tidyverse)
library(shiny)
library(shinysurveys)
library(googledrive)
library(googlesheets4)
library(shinyauthr)
library(shinyjs)
library(shinyWidgets)

# Setting Gargle Options for Specifying Authentication Token Save Location
# Read JT Blog post on connecting shiny and Google Drive
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
)

employee_survey_data_sheet_id <- "{{employe_survey_metadata_id}}"

employee_login_data_sheet_id <- "{{employee_login_data_id}}"

survey_responses_sheet_id <- "{{employe_survey_responses_id}}"
response_folder_id <- googledrive::as_id("{{employee_response_folder_id}}")


employee_dependency_sheet_id <- "{{employee_dependencies_id}}"

ui <- fluidPage(
  shinyauthr::loginUI(id = "login"),
  # setup table output to show user info after login
  uiOutput("surveySelection"),
  uiOutput("surveyQuestions"),
  viewReportUI(id = "{{name}}_report")
)

server <- function(input, output, session) {

  employee_survey_data <- read_sheet(employee_survey_data_sheet_id)
  employee_login_data <- read_sheet(employee_login_data_sheet_id)

  r <- reactiveValues()

  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = employee_login_data,
    user_col = full_name,
    pwd_col = password
  )

  observe({

    req(credentials()$user_auth)

    employee_dependencies <- read_sheet(
      employee_dependency_sheet_id
      )

    output$surveySelection <- renderUI(
      surveySelectionInput("metadata")
    )

    r$selection <- surveySelectionServer(
      "metadata",
      curr_user = credentials()$info$full_name,
      employee_database = employee_survey_data,
      dependence_data = employee_dependencies
    )

  })

  observe({
    req(credentials()$user_auth) # Check we're logged in
    req(r$selection$surveySelection()) # Check we've selected a person

    # Save survey info
    r$survey_info <- employee_survey_data %>%
      dplyr::filter(
        reviewer == credentials()$info$full_name,
        reviewee == r$selection$surveySelection()
      )

    # Render questions
    output$surveyQuestions <- renderUI({
      shinyjs::hideElement("surveySelection")
      surveyOutput(
        customize_survey_questions(
          survey_info = r$survey_info
        )
      )

    })

    renderSurvey()

  })

  observeEvent(input$submit, {

    r$response_id <- paste0(
      r$survey_info$reviewee, "_",
      r$survey_info$reviewer, "_",
      format(Sys.Date(), "%B_%Y")
    )

    r$response <- getSurveyData(custom_id = r$response_id)

    # Show a notification that responses are being saved
    saving_id <- showNotification(
      "Recording responses...",
      duration = NULL,
      closeButton = FALSE
    )
    on.exit(removeNotification(saving_id), add = TRUE)

    sheet_write(
      data = r$response,
      ss = survey_responses_sheet_id,
      # Write to a new sheet based on reviewee/reviewer
      sheet = r$response_id
    )

    # Save the response ID for checking dependencies, filtering out the month
    # and year to avoid complications with dependency checking for reviews over
    # prolonged times
    sheet_append(
      ss = employee_dependency_sheet_id,
      data = data.frame(
        dependency_subject_id = stringr::str_remove(
          r$response_id,
          paste0("_", format(Sys.Date(), "%B_%Y"))
        )
        ),
      sheet = 1
    )

    # Show response modal with different messages depending on if
    # this person has a file indicated in 'report_file' column
    show_response_modal(
      survey_info = r$survey_info
    )

  })


  # When the generateReport button is pressed,
  # render the report as HTML and display it on the web page
  observeEvent(input$generateReport, {

    r$output_file_name <- tempfile(fileext = ".html")
    r$output_pdf_file <- tempfile(fileext = ".pdf")

    shiny::showNotification(
      "Generating report...This may take up to 10 seconds as we create HTML and PDF versions.",
      duration = 5,
      closeButton = FALSE
    )

    renderReport(
      input_files = c(r$survey_info$report_file, "report.scss"),
      output_format = "html",
      output_file = r$output_file_name,
      params = list(
        person = r$survey_info$reviewer,
        survey_responses = r$response
        )
      )

    r$htmlRendered <- TRUE

    # Render PDF Report
    pagedown::chrome_print(input = r$output_file_name,
                           output = r$output_pdf_file,
                           extra_args = c("--disable-gpu",
                                          "--no-sandbox")
    )

    # Upload PDF report to Google drive
    googledrive::drive_upload(
      media = r$output_pdf_file,
      path = response_folder_id,
      name = paste0(r$response_id, "_report.pdf")
    )

  })

  # Server logic for viewing and downloading the generated report. Must be here!
  viewReportServer(id = "{{name}}_report",
                   reportData = r, # DON'T TOUCH
                   input_files = c(r$survey_info$report_file, "report.scss"), # These files must be in www/ subdirectory.
                   # Add params as needed to pass into RMarkdown document
                   params = list(
                     person = r$survey_info$reviewer,
                     survey_responses = r$response
                     )
  )

}

shinyApp(ui, server)
