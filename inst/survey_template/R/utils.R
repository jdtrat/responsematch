
extract_before <- function (full_string, before_string, trim_spaces = TRUE)
{
  reg_pattern <- paste0(".*(?=", before_string, ")")
  out <- regmatches(full_string, gregexpr(pattern = reg_pattern,
                                          text = full_string, perl = T))[[1]]
  if (length(out > 1))
    out <- out[1]
  if (trim_spaces)
    out <- trimws(out)
  return(out)
}

# Utility Functions for MailcHimp Integration -------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Get first name of a name field where there is a space separating the first and
# last name
get_first_name <- function(.x) {
  strsplit(.x, " ")[[1]][1]
}

# Get second name of a name field where there is a space separating the first and
# last name
get_last_name <- function(.x) {
  strsplit(.x, " ")[[1]][2]
}

check_key <- getFromNamespace("check_key", "chimpr")
