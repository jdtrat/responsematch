
#' Create a responsematch 'Shiny' Survey Application
#'
#' This function can be used to create a Shiny Survey application. It takes in
#' two parameters: \code{survey_name}, which specifies the name of the survey --
#' e.g. "GLI" -- and \code{path}, which specifies where the application should
#' be created.
#'
#' When run, your internet browser will open and you will be asked to
#' authenticate your Google Drive and Google Sheets information. This is
#' necessary to remotely store respondent's data.
#'
#' After Google Drive and Google Sheets authentication, a new \url{RStudio
#' Project}{https://r4ds.had.co.nz/workflow-projects.html} will be created at
#' the specified path, with a suffix 'survey-project'.
#'
#' The RStudio project will open in a new R Session automatically. In the file
#' pane, there will be a folder with a suffix '-app', containing an 'app.R' file
#' -- defining the user-interface and server-side logic of the survey -- a 'www'
#' folder -- housing the survey questions, RMarkdown report, and custom
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
#' @param survey_name The name of the survey application to create, e.g. "GLI".
#' @param path The path specifying where the survey application should be
#'   created. Default is the current working directory.
#'
#' @return A new RStudio Project the infrastructure needed to launch a survey in
#'   Shiny.
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#' create_survey(survey_name = "testSurvey")
#' }
#'
create_survey <- function(survey_name, path) {

  new_proj_path <- file.path(path, paste0(survey_name, "-survey-project"))

  dir.create(new_proj_path, recursive = TRUE, showWarnings = FALSE)

  sv$survey_name <- survey_name

  sv$app_path <- new_proj_path

  fs::dir_copy(path = system.file("survey_template", package = "responsematch"),
               new_path = file.path(sv$app_path, paste0(sv$survey_name, "-app")))


  cli::cli_alert_success("Created a {.pkg shiny} directory for {.strong '{survey_name}'} at {.file {sv$app_path}}.")

  auth_google()

  cli::cli_alert_success("Saved {.emph Google} authentication information for {.strong '{survey_name}'}.")


  data <- list(survey_name = sv$survey_name)

  # Customize survey template
  file <- whisker::whisker.render(template = readLines(system.file("survey_template/app.R", package = "responsematch")),
                                  data)
  fs::file_delete(file.path(sv$app_path, paste0(sv$survey_name, "-app"), "app.R"))
  writeLines(file, con = file.path(sv$app_path, paste0(sv$survey_name, "-app"), "app.R"))

  cli::cli_alert_success("Updated {.file app.R} file specific to {.strong '{survey_name}'}.")

  # Customize report.Rmd
  file <- whisker::whisker.render(template = readLines(system.file("survey_template/www/report.Rmd", package = "responsematch")),
                                  data)
  fs::file_delete(file.path(sv$app_path, paste0(sv$survey_name, "-app"), "/www/report.Rmd"))
  writeLines(file, con = file.path(sv$app_path, paste0(sv$survey_name, "-app"), "/www/report.Rmd"))

  cli::cli_alert_success("Updated template {.file report.Rmd} file for {.strong '{survey_name}'}.")

  setup_survey_sheet()

  cli::cli_text("To successfully launch your survey, make sure to do the following:")
  cli::cli_ul("Add Mailchimp integration information (if applicable).")
  cli::cli_ul("Modify the survey questions in {.file www/questions.csv}.")
  cli::cli_ul("Modify the generated report in {.file www/report.Rmd}.")
  cli::cli_ul("Publish the application to {.url https://shinyapps.io}.")

  make_project()

  usethis::proj_activate(path = sv$app_path)

}


