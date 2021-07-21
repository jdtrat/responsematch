
#' Create a responsematch 'Shiny' Survey Application
#'
#' This function can be used to create a Shiny Survey application. It takes in
#' two parameters: \code{survey_name}, which specifies the name of the survey --
#' e.g. "GLI" -- and \code{path}, which specifies where the application should
#' be created.
#'
#' When run, your internet browser will open and you will be asked to
#' authenticate your Google Drive and Google Sheets information. This is
#' necessary to remotely store respondent's data. The resulting Shiny app will
#' be created in a folder specified by the `path` argument. Before publishing
#' the survey, you must modify the survey questions and parameterized RMarkdown
#' report  located in the 'www' subdirectory.
#'
#' @param survey_name The name of the survey application to create, e.g. "GLI".
#' @param path The path specifying where the survey application should be
#'   created. Default is the current working directory.
#'
#' @return A Shiny application inside a folder of `survey_name`.
#' It contains an 'app.R' file -- defining the user-interface and server-side
#' logic of the survey -- a 'www' folder -- housing the survey questions,
#' RMarkdown report, and custom JavaScript -- and an 'R' directory containing
#' additional functions used by the application.
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#' create_survey(survey_name = "testSurvey")
#' }
#'
create_survey <- function(survey_name, path = getwd()) {

  sv$survey_name <- survey_name

  sv$app_path <- paste0(path, "/", sv$survey_name)

  fs::dir_copy(path = system.file("survey_template", package = "responsematch"),
               new_path = sv$app_path)


  cli::cli_alert_success("Created a {.pkg shiny} directory for {.strong '{survey_name}'} at {.file {sv$app_path}}.")

  auth_google()

  cli::cli_alert_success("Saved {.emph Google} authentication information for {.strong '{survey_name}'}.")


  data <- list(survey_name = sv$survey_name)

  # Customize survey template
  file <- whisker::whisker.render(template = readLines(system.file("survey_template/app.R", package = "responsematch")),
                                  data)
  fs::file_delete(paste0(sv$app_path, "/app.R"))
  writeLines(file, con = paste0(sv$app_path, "/app.R"))

  cli::cli_alert_success("Updated {.file app.R} file specific to {.strong '{survey_name}'}.")

  # Customize report.Rmd
  file <- whisker::whisker.render(template = readLines(system.file("survey_template/www/report.Rmd", package = "responsematch")),
                                  data)
  fs::file_delete(paste0(sv$app_path, "/www/report.Rmd"))
  writeLines(file, con = paste0(sv$app_path, "/www/report.Rmd"))

  cli::cli_alert_success("Updated template {.file report.Rmd} file for {.strong '{survey_name}'}.")

  setup_survey_sheet()

  cli::cli_text("To successfully launch your survey, make sure to do the following:")
  cli::cli_ul("Modify the survey questions in {.file www/questions.csv}.")
  cli::cli_ul("Modify the generated report in {.file www/report.Rmd}.")
  cli::cli_ul("Publish the application to {.url https://shinyapps.io}.")

}
