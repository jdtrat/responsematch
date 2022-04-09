
sv <- new.env(parent = emptyenv())

#' Setup survey infrastructure in Google Drive and Google Sheets
#'
#' This function takes in a survey name, e.g. "GLI", and will create a folder in
#' Google Drive with a corresponding Google Sheet for all survey data to be
#' written.
#'
#' @param survey_name The name of the new survey to create
#'
#' @return NA; used to establish survey infrastructure with Google Drive and Sheets.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#' setup_survey_sheet("testSurvey")
#' }
#'
setup_survey_sheet <- function(survey_name) {

  sv$setup <- TRUE

  if (!missing(survey_name)) {
    sv$survey_name <- survey_name
  }

  dir_name <- paste0(sv$survey_name, "_survey")
  sheet_name <- paste0(sv$survey_name, "_data")

  # Suppress Google Drive Print Methods
  googledrive::local_drive_quiet()

  # Create folder
  googledrive::drive_mkdir(dir_name)

  # Create folder for saving reports
  googledrive::drive_mkdir(
    name = "rendered_reports",
    path = dir_name
  )

  # Create "GLI_data" spreadsheet
  googlesheets4::gs4_create(sheet_name)
  # Find the folder ID
  sv$folder_id <- googledrive::drive_find(dir_name, n_max = 5)$id
  # Find the file ID for the "GLI_data" spreadsheet
  sv$file_id <- googledrive::drive_get(sheet_name)$id
  # Move the spreadsheet into the proper folder
  googledrive::drive_mv(googledrive::as_id(sv$file_id), googledrive::as_id(sv$folder_id))

  cli::cli_alert_success("Created folder and main spreadsheet for {.strong '{sv$survey_name}'} on {.emph Google}.")

}


auth_google <- function() {

  op <- options(
    # whenever there is one account token found, use the cached token
    gargle_oauth_email = TRUE,
    # specify auth tokens should be stored in a hidden directory ".secrets"
    gargle_oauth_cache = file.path(sv$app_path, paste0(sv$survey_name, "-app"), "/.secrets/")
  )

  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")

  googlesheets4::gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")

  on.exit(options(op))

}

