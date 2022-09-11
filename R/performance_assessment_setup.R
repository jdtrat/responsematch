
auth_google_pa <- function() {

  op <- options(
    # whenever there is one account token found, use the cached token
    gargle_oauth_email = TRUE,
    # specify auth tokens should be stored in a hidden directory ".secrets"
    gargle_oauth_cache = file.path(sv$app_path, paste0(sv$name, "-app"), "/.secrets/")
  )

  googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive")

  googlesheets4::gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")

  on.exit(options(op))

}

#' Setup survey infrastructure in Google Drive and Google Sheets
#'
#' This function takes in a name and will create a folder in
#' Google Drive with a corresponding Google Sheet for all survey data to be
#' written.
#'
#' @param name The name of the new performance assessment
#' @param include_employee_setup_app Logical: `TRUE` and a Shiny app allowing
#'   employees to select their manager and peers will be created.
#' @param employee_data A data frame with columns "full_name" and "email" for
#'   all employees included in the performance review.
#' @param encryption_key A character string used to encrypt the employees' names
#'   into passwords. This should be kept secret. If `NULL` (default), the
#'   environmental variable `RESPONSEMATCH_ENCRYPT_KEY` is checked.
#' @param password_length The length of the passwords created for each user.
#'   Defaults to six.
#'
#' @return A named list of Google Sheets IDs for customizing the Performance Review Shiny app `c(
#'   'employee_response_folder_id', 'employe_survey_metadata_id',
#'   'employee_login_data_id', 'employe_survey_responses_id', 'employee_dependencies_id'
#'   )`. This named list is used internally [create_performance_review()].
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#' setup_performance_review("testPA")
#' }
#'
setup_performance_review <- function(
    name,
    include_employee_setup_app,
    employee_data,
    encryption_key,
    password_length
    ) {

  sv$setup <- TRUE

  if (!missing(name)) {
    sv$name <- name
  }

  dir_name <- paste0(sv$name, "_performance_review")
  report_dir_name <- paste0(sv$name, "_employee_reports")

  survey_responses_name <- paste0(sv$name, "_survey_responses")
  employee_survey_metadata <- paste0(sv$name, "_employee_survey_metadata")
  employee_login_data <- paste0(sv$name, "_employee_login_data")
  employee_dependency_ids <- paste0(sv$name, "employee_dependency_ids")

  # Suppress Google Drive Print Methods
  googledrive::local_drive_quiet()

  # Create folder
  parent_folder <- googledrive::drive_mkdir(dir_name)

  # Create folder for saving reports
  response_folder <- googledrive::drive_mkdir(
    name = report_dir_name,
    path = dir_name
  )

  employee_data_to_upload <- employee_data %>%
    dplyr::group_by(
      .data$full_name
    ) %>%
    dplyr::mutate(
      # Generate a six-character password with the secret key "nickPass" based on
      # each person's name.
      password = gen_pw_from_char(
        char = .data$full_name,
        key = encryption_key,
        pw_length = password_length
      )
    ) %>%
    dplyr::ungroup()

  # Create new sheets with the correct names (.x) and
  # template data (.y) and move them into the folder ID
  sheet_ids <- purrr::map2(
    .x = list(
      "employe_survey_responses_id" = survey_responses_name,
      "employe_survey_metadata_id" = employee_survey_metadata,
      "employee_login_data_id" = employee_login_data,
      "employee_dependencies_id" = employee_dependency_ids
    ),
    .y = list(
      survey_responses_name = data.frame(),
      employee_survey_metadata = data.frame(
        reviewee = "Person to be reviewed",
        reviewer = "Person doing the review",
        question_file = "The name of the .csv file (e.g., 'self.csv') with questions for the reviewer to complete",
        report_file = "The name of the RMarkdown file (e.g., 'self.Rmd') with the report based on the performance review",
        dependency = "One or more names separated with '|' indicating who needs to review the reviewee before this person can do so"
      ),
      employee_login_data = employee_data_to_upload,
      employee_dependency_ids = data.frame(
        dependency_subject_id = "PLACEHOLDER_SUBJECT_ID -- KEEP THIS HERE"
      )
    ),
    ~ {
      new_sheet <- googlesheets4::gs4_create(
        name = .x,
        sheets = .y
      )
      googledrive::drive_mv(
        file = new_sheet,
        path = parent_folder$id
      )
      new_sheet
    },
    .id = "sheet_name"
  )

  cli::cli_alert_success(
    "Created folder and spreadsheets for {.strong '{sv$name}'} on {.emph Google}."
    )

  if (include_employee_setup_app) {
    fs::dir_copy(
      path = file.path(sv$app_path, paste0(sv$name, "-app"), "/.secrets/"),
      new_path = file.path(sv$app_path, paste0(sv$name, "-employee-setup-app"), "/.secrets/")
    )
  }

  c(
    list(
      "employee_response_folder_id" = response_folder$id
      ),
    sheet_ids
  )

}
