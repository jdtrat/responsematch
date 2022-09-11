library(tidyverse)
library(shiny)
library(shinysurveys)
library(googledrive)
library(googlesheets4)
library(shinyauthr)
library(shinyjs)
library(shinyWidgets)
library(blastula)

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
  uiOutput("peerSelection")
)

server <- function(input, output, session) {

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

    output$peerSelection <- renderUI(
      peerSelectionInput(
        "peerData",
        num_managers = 1, # How many managers can be selected
        num_peers = 3 # How many peers can be selected
      )
    )

    r$selection <- peerSelectionServer(
      "peerData",
      curr_user = credentials()$info$full_name,
      employee_login_data = employee_login_data
    )

    observeEvent(
      r$selection$confirmedSelection(), {

        survey_metadata <- format_survey_metadata(
          peerSelectionData = r$selection
        )

        # Show a notification that responses are being saved
        saving_id <- showNotification(
          "Recording responses...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(saving_id), add = TRUE)

        # Save the response ID for checking dependencies
        sheet_append(
          ss = employee_survey_data_sheet_id,
          data = survey_metadata,
          sheet = 1
        )

        # Show modal dialog!
        shiny::showModal(
          shiny::modalDialog(
            title = "Thank you!",
            "Thanks for initiating your performance review.
        You will receive an email shortly with a link to complete your personal reflection. \n
        You may now close your browser window.",
        easyClose = FALSE
          )
        )

        # Send the confirmation email!
        send_confirmation_email(
          # File path to RMarkdown document with email's contents
          email_rmd = "www/performanceReviewLink.Rmd",
          # Subject line of email to be sent
          subject_line = "Complete Your Personal Performance Reflection",
          # Replace this with your email. You can use a named character vector like
          # ("CompanySurvey" = "name@email.com") to change the appearance of who the
          # email is from in some email clients, like GMail
          from_address = c("name@email.com"),
          # Replace this with the url for the performance review on shinyapps.io
          performance_review_link = "https://www.jdtrat.com",
          # Don't change this credentials line!
          userCredentials = credentials(),
          # Setup using blastula::create_smtp_creds_file
          emailCredentials = creds_file("your_email_credentials_file")
        )

      })

  })



}

shinyApp(ui, server)
