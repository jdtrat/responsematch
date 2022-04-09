
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

# From jdtools
time_string <- function(length) {
  time <- format(Sys.time(), "%s")
  n_time <- nchar(time)
  if (length > n_time) {
    warning(paste0("Only ", n_time, " digits make up the number of seconds since January, 1970. Ignoring supplied length, and defaulting to ",
                   n_time, " digits."))
  }
  substr(time, start = (1 + n_time - length), stop = n_time)
  }
