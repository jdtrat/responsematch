
get_required_internal <- function(x) {
  do.call(c, lapply(x, shinysurveys:::getID))
}

assignInNamespace(
  "getRequired_internal",
  get_required_internal,
  ns = "shinysurveys"
  )
