
#' Send Confirmation Email
#'
#' @param email_rmd An RMarkdown document with the email's contents. Must have
#'   parameters 'performance_review_link' and 'full_name'.
#' @param subject_line The subject of the message. If not provided, an empty
#'   string will be used (which is handled differently by email clients).
#' @param from_address The email address of the sender. Often this needs to be
#'   the same email address that is associated with the account actually sending
#'   the message. As with to, cc, and bcc, we can either supply a single email
#'   address or use a named character vector with the sender name and email
#'   address (e.g., `c("John Doe" = "john_doe@example.com"))`.
#' @param performance_review_link A link to the published responsematch
#'   performance review.
#' @param userCredentials User credentials (such as the reactive object from
#'   `shinyauthr`) with a list 'info' and items 'full_name' and 'email'.
#' @param emailCredentials Email credentials passed onto
#'   [blastula::smtp_send()].
#'
send_confirmation_email <- function(
    email_rmd,
    subject_line,
    from_address,
    performance_review_link,
    userCredentials,
    emailCredentials
) {

  blastula::smtp_send(
    email = blastula::render_email(
      input = email_rmd,
      render_options = list(
        params = list(
          full_name = userCredentials$info$full_name,
          performance_review_link = performance_review_link
        )
      )
    ),
    to = userCredentials$info$email,
    from = from_address,
    subject = subject_line,
    credentials = emailCredentials
  )

}


#' Convert numbers to English Words=
#'
#' @return
#' @export
#'
#' @details Adapted from [xfun::numbers_to_words]
#'
#' @examples
#'
#' n2w(3)
#'
n2w <- function(x) {

  n2w_internal <- function(n) {
    if (nchar(as.character(n)) > 1) stop("Decimals not supported.")
    if (n >= 20) stop("Numbers above 20 not supported")

    zero_to_19 <- c(
      'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten',
      'eleven', 'twelve', paste0(c('thir', 'four', 'fif', 'six', 'seven', 'eigh', 'nine'), 'teen')
    )

    zero_to_19[n + 1]
  }

  vapply(x, n2w_internal, character(1))

}

format_survey_metadata <- function(peerSelectionData) {
  tibble::tibble(
    reviewee = peerSelectionData$curr_user(),
    reviewer = c(
      peerSelectionData$curr_user(),
      peerSelectionData$peerSelection(),
      peerSelectionData$managerSelection()
    ),
    question_file = c(
      "self.csv",
      rep("peer.csv",
          length(peerSelectionData$peerSelection())
      ),
      rep("supervisor.csv",
          length(peerSelectionData$managerSelection())
      )
    ),
    report_file = c(
      "self.Rmd",
      rep("peer.Rmd",
          length(peerSelectionData$peerSelection())
      ),
      rep("supervisor.Rmd",
          length(peerSelectionData$managerSelection())
      )
    ),
    dependency = c(
      "",
      rep("",
          length(peerSelectionData$peerSelection())
      ),
      paste0(
        c(
          peerSelectionData$curr_user(),
          peerSelectionData$peerSelection()
        ),
        collapse = "|"
      )
    )
  )
}
