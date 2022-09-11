library(blastula)
library(responsematch)
library(googlesheets4)

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = "{{name}}-employee-setup-app/.secrets"
)


responsematch::send_initial_performance_review_email(
  email_rmd = "initial_performance_review_email.Rmd",
  employee_login_data_sheet_id = "{{employee_login_data_id}}",
  subject_line = "Start {{name}} Performance Review!",
  # Replace this with your email. You can use a named character vector like
  # ("CompanySurvey" = "name@email.com") to change the appearance of who the
  # email is from in some email clients, like GMail
  from_address = c(
    "ResponseMatch Performance Reviews" = "name@email.com"
  ),
  # Replace this with the url for the employee setup app shinyapps.io
  employee_setup_link = "https://www.jdtrat.com/",
  # Setup using blastula::create_smtp_creds_file, replace the file name for the
  # blastula credentials
  emailCredentials = creds_file(
    "{{name}}-employee-setup-app/blastula_credentials"
  )
)
