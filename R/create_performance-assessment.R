
check_encrypt_key <- function(x) {
  key <- if (is.null(x)) Sys.getenv("RESPONSEMATCH_ENCRYPT_KEY", "") else x
  if (key == "") {
    cli::cli_abort(
      "Cannot create passwords for employees without a valid encryption key.
      Please pass one in or set the environment variable {.envvar RESPONSEMATCH_PA_KEY}."
    )
  } else {
    key
  }
}

#' Create a responsematch 'Shiny' Survey Performance Review
#'
#' This function can be used to create a Shiny Survey application. It takes in
#' two parameters: \code{name}, which specifies the name of the performance
#' review and \code{path}, which specifies where the application should be
#' created.
#'
#' Assuming the data should be stored to Google Drive/Google Sheets (see
#' function arguments for more details), running this function will result in
#' your internet browser will open and you will be asked to authenticate your
#' Google Drive and Google Sheets information. This is necessary to remotely
#' store respondent's data.
#'
#' After Google Drive and Google Sheets authentication, a new \url{RStudio
#' Project}{https://r4ds.had.co.nz/workflow-projects.html} will be created at
#' the specified path, with a suffix 'survey-project'.
#'
#' The RStudio project will open in a new R Session automatically. In the file
#' pane, there will be a folder with a suffix '-app', containing an 'app.R' file
#' -- defining the user-interface and server-side logic of the survey -- a 'www'
#' folder -- housing some example questions, RMarkdown reports, and custom
#' JavaScript -- and an 'R' directory containing additional functions used by
#' the application.
#'
#' The Shiny application is fully functioning at this point, however, you must
#' modify the survey questions and parameterized RMarkdown report located in the
#' 'www' subdirectory.
#'
#' In order to deploy the survey to [shinyapps.io](https://shinyapps.io/), you
#' may click the 'deploy' button at the top right of the RStudio IDE. For more
#' details on deployment, please see my \url{blog post on getting started with
#' [shinyapps.io]}{https://www.jdtrat.com/blog/getting-started-shinyapps/}.
#'
#' @param name The name of the survey application to create
#' @param path The path specifying where the survey application should be
#'   created. Default is the current working directory.
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
#' @return A new RStudio Project the infrastructure needed to launch a new
#'   performance review in Shiny.
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   create_performance_review(name = "testPerformanceReview")
#'
#'   create_performance_review(
#'     name = "testPerformanceReview",
#'     path = "~/Desktop/",
#'     include_employe_setup_app = TRUE,
#'     employee_data = data.frame(
#'       full_name = c("First Next", "Second Last"),
#'       email = c("name@website.com", "name2@website.com")
#'     ),
#'     encryption_key = "superSecretEncryptionPassword",
#'     password_length = 6
#'   )
#' }
#'
create_performance_review <- function(
    name,
    path,
    include_employee_setup_app,
    employee_data,
    encryption_key = NULL,
    password_length = 6
    ) {

  if (!all(names(employee_data) %in% c("full_name", "email"))) {
    cli::cli_abort("{.arg employee_data} must have columns {.val {c('full_name', 'email')}}")
  }

  key <- check_encrypt_key(encryption_key)

  new_proj_path <- file.path(path, paste0(name, "-performance-review-project"))
  dir.create(new_proj_path, recursive = TRUE, showWarnings = FALSE)

  sv$name <- name
  sv$app_path <- new_proj_path

  fs::dir_copy(
    path = system.file("performance_review_template", package = "responsematch"),
    new_path = file.path(sv$app_path, paste0(sv$name, "-app"))
    )

  cli::cli_alert_success(
    "Created a {.pkg shiny} directory for {.strong '{name}'} at {.file {sv$app_path}}."
    )

  if (include_employee_setup_app) {
    fs::dir_copy(
      path = system.file("performance_review_employee_setup_template", package = "responsematch"),
      new_path = file.path(sv$app_path, paste0(sv$name, "-employee-setup-app"))
    )

    cli::cli_alert_success(
      "Created a {.pkg shiny} directory for {.strong '{name}'} employee setup app at {.file {sv$app_path}}."
    )
  }

  auth_google_pa()

  # Save a named list of Google Sheet info for passing into the template
  google_structure <- setup_performance_review(
    name = name,
    include_employee_setup_app = include_employee_setup_app,
    employee_data = employee_data,
    encryption_key = key,
    password_length = password_length
  )

  data <- c(
    list(name = sv$name),
    google_structure
  )

  # Customize survey template
  perf_app_file <- whisker::whisker.render(
    template = readLines(
      system.file("performance_review_template/app.R", package = "responsematch")
      ),
    data
    )
  fs::file_delete(file.path(sv$app_path, paste0(sv$name, "-app"), "app.R"))
  writeLines(perf_app_file, con = file.path(sv$app_path, paste0(sv$name, "-app"), "app.R"))

  cli::cli_alert_success("Updated {.file app.R} file specific to {.strong '{name}'}.")


  if (include_employee_setup_app) {

    # Customize survey template
    setup_app_file <- whisker::whisker.render(
      template = readLines(
        system.file("performance_review_employee_setup_template/app.R", package = "responsematch")
      ),
      data
    )

    fs::file_delete(file.path(sv$app_path, paste0(sv$name, "-employee-setup-app"), "app.R"))
    writeLines(setup_app_file, con = file.path(sv$app_path, paste0(sv$name, "-employee-setup-app"), "app.R"))

    cli::cli_alert_success("Updated employee setup {.file app.R} file specific to {.strong '{name}'}.")

    send_initial_email_file <- whisker::whisker.render(
      template = readLines(
        system.file(
          "performance_review_email_templates/send_initial_performance_review_email.R",
          package = "responsematch"
          )
        ),
      data
    )

    writeLines(send_initial_email_file, file.path(sv$app_path, "send_initial_performance_review_email.R"))
    fs::file_copy(
      path = system.file(
        "performance_review_email_templates/initial_performance_review_email.Rmd",
        package = "responsematch"
        ),
      new_path = file.path(sv$app_path, "initial_performance_review_email.Rmd")
      )

  }


  make_project(name = sv$name)

  usethis::proj_activate(path = sv$app_path)

}

