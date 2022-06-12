
make_project <- function(name, line_ending = c("posix", "windows")) {

  line_ending <- rlang::arg_match(line_ending)
  line_ending <- c(posix = "Posix", windows = "Windows")[[line_ending]]
  rproj_file <- paste0(name, ".Rproj")
  file <- whisker::whisker.render(template = readLines(system.file("templates/project.RProj",
                                                           package = "responsematch")),
                          data = list(line_ending = line_ending))

  writeLines(file, con = file.path(sv$app_path, rproj_file))


}

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
