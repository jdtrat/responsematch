# Set Up & Load Packages --------------------------------------------------------

library(shiny)
library(shinysurveys)
library(readr)
library(dplyr)
library(tidyr)
{{ #google_sheets }}
library(googledrive)
library(googlesheets4)

# Setting Gargle Options for Specifying Authentication Token Save Location
# Read JT Blog post on connecting shiny and Google Drive
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)
{{ /google_sheets }}

{{ #google_sheets }}
# Assumes there is a Google Sheet in your Google Drive Account with the name "GLI_data"
sheet_id <- drive_get("{{ survey_name }}_data")$id
{{ /google_sheets }}

# This reads in the shinysurveys formatted questions
# stored in the www subdirectory of the Shiny app
# For new projects, go to File > New File > Shiny Web App
# and copy the www folder into that new directory,
# change the Rmd file and csv file, and you're good to go!
data <- readr::read_csv("www/questions.csv")


# Extend Custom Input Types -----------------------------------------------
# If applicable, place custom input extensions here.



# Shiny Application -------------------------------------------------------

# Define user-interface for the Shiny application
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "responsematch.css")
  ),
  surveyOutput(data), # Rendering survey (from shinysurveys)
  viewReportUI(id = "{{ survey_name }}") # hidden until we view the report (a placeholder)
)

# Define the server side for the Shiny application
server <- function(input, output, session) {

  # shinysurveys' server logic must be here!
  renderSurvey()

  # Create reactive values object to store data
  rv <- reactiveValues()

  # On submit
  observeEvent(input$submit, {

    # Show a notification that responses are being saved
    saving_id <- showNotification(
      "Recording responses...",
      duration = NULL,
      closeButton = FALSE
    )
    on.exit(removeNotification(saving_id), add = TRUE)

    # Get a response ID
    # If you are using an employee ID
    # or have a name question, set that here
    # instead of time_string, e.g.
    # rv$response_id <- input$name
    rv$response_id <- time_string(5)

    # Save a wide form of response data to Google Sheets
    rv$response <- getSurveyData(custom_id = rv$response_id) %>%
      dplyr::select(-question_type) %>%
      tidyr::pivot_wider(names_from = question_id, values_from = response)


    {{ #mailchimp }}
    # Add a subscriber to Mailchimp audience only if the proper inputs are
    # available Be sure to add a .Renviron file or manually pass in mailchimp
    # credentials. If you add a .Renviron file (recommended method), make sure
    # it has two fields: `MAILCHIMP_LISTID` and `MAILCHIMP_KEY`. This should
    # also be git ignored if you are using GitHub. Lastly, the file must be read
    # in to this Shiny Application file with readRenviron(".Renviron").
    add_chimp_subscriber <- vapply(c("email", "full_name", "company"),
                                   function(x, session) is.null(session$input[[x]]),
                                   logical(1),
                                   session = shiny::getDefaultReactiveDomain()
                                   )

    if (!any(add_chimp_subscriber)) {
      tryCatch(chimp_add_subscriber(email_address = input$email,
                                    status = "subscribed",
                                    first_name = get_first_name(input$full_name),
                                    last_name = get_last_name(input$full_name),
                                    company = input$company),
               error = function(cond) {print("Couldn't add subscriber.")})
    }

    {{ /mailchimp }}


    {{ #google_sheets }}
    # Write responses to Google Sheets
    sheet_write(data = rv$response,
                ss = sheet_id,
                # Write to a new sheet based on a
                sheet = rv$response_id
    )

    {{ /google_sheets }}


      {{ #include_report }}

    # Show message that allows data download
      shiny::showModal(
        shiny::modalDialog(title = "Thank you!",
                           "Your response has been recorded. \n
                         You may now close your browser window or click 'Generate Report' to view and/or download your results.",
                         footer = actionButton("generateReport", "Generate Report")
        )
      )

      {{ /include_report }}

      {{ #not_include_report }}

      shiny::showModal(
        shiny::modalDialog(title = "Thank you!",
                           "Your response has been recorded. \n
                         You may now close your browser window."
        )
      )

      {{ /not_include_report }}

  })

  {{ #include_report }}
  # When the generateReport button is pressed,
  # render the report as HTML and display it on the web page
  observeEvent(input$generateReport, {

    rv$output_file_name <- tempfile(fileext = ".html")
    rv$output_pdf_file <- tempfile(fileext = ".pdf")

    shiny::showNotification(
      "Generating report...This may take up to 10 seconds as we create HTML and PDF versions.",
      duration = 5,
      closeButton = FALSE
    )

    renderReport(input_files = c("report.Rmd", "report.scss"),
                 output_format = "html",
                 output_file = rv$output_file_name,
                 params = list(person = rv$response_id,
                               survey_responses = rv$response))
    rv$htmlRendered <- TRUE

    # Render PDF Report
    pagedown::chrome_print(input = rv$output_file_name,
                           output = rv$output_pdf_file,
                           extra_args = c("--disable-gpu",
                                          "--no-sandbox")
                           )

    # Upload PDF report to Google drive
    googledrive::drive_upload(
      media = rv$output_pdf_file,
      path = "{{ survey_name }}_survey/rendered_reports/",
      name = paste0(rv$response_id, "_report.pdf")
      )

  })

  # Server logic for viewing and downloading the generated report. Must be here!
  viewReportServer(id = "{{ survey_name }}",
                   reportData = rv, # DON'T TOUCH
                   input_files = c("report.Rmd", "report.scss"), # These files must be in www/ subdirectory.
                   # Add params as needed to pass into RMarkdown document
                   params = list(person = rv$response_id,
                                 survey_responses = rv$response)
  )

  {{ /include_report }}

}

shinyApp(ui, server)
