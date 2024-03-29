% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-performance_assessment.R
\name{send_initial_performance_review_email}
\alias{send_initial_performance_review_email}
\title{Send Initial Performance Review Email}
\usage{
send_initial_performance_review_email(
  email_rmd,
  employee_login_data_sheet_id,
  subject_line,
  from_address,
  employee_setup_link,
  emailCredentials
)
}
\arguments{
\item{email_rmd}{An RMarkdown document with the email's contents. Must have
parameters 'employee_setup_link', 'full_name', and 'password' (see examples
for details.)}

\item{employee_login_data_sheet_id}{The Google Sheet created as part of the
performance review with \code{\link[=create_performance_review]{create_performance_review()}} that contains the
employees' full name, email, and password.}

\item{subject_line}{The subject of the message. If not provided, an empty
string will be used (which is handled differently by email clients).}

\item{from_address}{The email address of the sender. Often this needs to be
the same email address that is associated with the account actually sending
the message. As with to, cc, and bcc, we can either supply a single email
address or use a named character vector with the sender name and email
address (e.g., \verb{c("John Doe" = "john_doe@example.com"))}.}

\item{employee_setup_link}{Link to deployed Shiny app}

\item{emailCredentials}{Email credentials passed onto
\code{\link[blastula:smtp_send]{blastula::smtp_send()}}.}
}
\value{
A data frame with the full name and email for those who were sent
emails.
}
\description{
Send Initial Performance Review Email
}
\examples{

if (interactive()) {
  # Create temp RMarkdown file
  tmp <- tempfile(fileext = ".Rmd")

  # Write Lines to temporary RMarkdown file
  writeLines(
    c(
      "---",
      "title: 'Performance Review'",
      "output: blastula::blastula_email",
      "params:",
      "  full_name: 'Valued Team Member'",
      "  password: 'SecretPassword'",
      "  employee_setup_link: 'jdtrat.com'",
      "---",
      "
      Hi `r params$full_name`,

      This year we're doing performance reviews differently.
      We've partnered with ResponseMatch® to figure out how we,
       as a company, can help you better achieve your personal
       and professional goals.

      To do this, we'd like you to kick-off your review by logging
      in to **[this tool](`r params$employee_setup_link`)**
      with the following credentials.
      You'll be asked to indicate your manager and select up
      to three colleagues who can comment on your work.
      Once you do so, you'll receive a confirmation email
      with details on completing your review.

      ---

      **Username:** `r params$full_name`

      **Password:** `r params$password`

      ---

      All the best,
      🥳
      "
    ),
    con = tmp
  )


  send_initial_performance_review_email(
    email_rmd = tmp,
    employee_login_data_sheet_id = "SHEET_ID",
    subject_line = "Start your Performance Review",
    from_address = c("jdt@jdtrat.com"),
    # Replace this with the URL for the performance
    # review on shinyapps.ioREPLACE THIS WITH THE URL
    # FOR THE PERFORMANCE REVIEW ON SHINYAPPS.IO
    employee_setup_link = "https://responsematch.com",
    emailCredentials = creds_file("yourBlastulaCredentials")
  )

  # Delete the temporary file
  unlink(tmp)
}


}
