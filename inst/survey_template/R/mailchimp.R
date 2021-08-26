
# Install JT's fork of chimpr package until posting features are
# integrated into the main repository (if they get integrated).
if (!require("remotes")) install.packages("remotes")
remotes::install_github("jdtrat/chimpr")

#' Add a subscriber
#'
#' Add a subscriber to a mailchimp audience list. This requires the dev version
#' from [jdtrat/chimpr](https://github.com/jdtrat/chimpr) of the chimpr package,
#' providing API access to Mailchimp. It also requires an environmental variable
#' set for `MAILCHIMP_KEY` or an actual key supplied.
#'
#' @param list_id **Required:** The unique ID for the list. May be passed in as
#'   an environmental variable `MAILCHIMP_LISTID`.
#' @param email_address **Required:** Email address for a subscriber.
#' @param status **Required:** Subscriber's current status. Possible values:
#'   "subscribed", "unsubscribed", "cleaned", "pending", or "transactional".
#' @param first_name The first name of the subscriber.
#' @param last_name The last name of the subscriber.
#' @param company The company of the subscriber.
#' @param invisibly Logical: TRUE by default. If TRUE, the HTTP request will be
#'   returned invisibly; FALSE and it will be returned explicitly.
#' @param key A Mailchimp API key. See
#'   https://developer.mailchimp.com/documentation/mailchimp/ to get a key
#'
#' @return
#' @noRd
#'
#' @examples
#'
#'
chimp_add_subscriber <- function(list_id = NULL,
                           email_address = NULL, status = NULL,
                           first_name = NULL, last_name = NULL, company = NULL,
                           invisibly = TRUE, key = NULL) {


  dc <- strsplit(check_key(key), "-")[[1]][2]
  stopifnot(is.character(dc))

  client <- chimpr::ChmpClient$new(dc = dc,
                                   key = key)

  chimpr::chmp_post_list(client,
                         list_id = list_id %||% Sys.getenv("MAILCHIMP_LISTID"),
                         email_address = email_address,
                         status = status,
                         merge_fields = list(FNAME = first_name,
                                             LNAME = last_name,
                                             COMP = company),
                         invisibly = invisibly)

}
