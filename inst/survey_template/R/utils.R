
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
