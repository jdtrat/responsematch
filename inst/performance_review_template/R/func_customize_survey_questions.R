
#' Read and Customize Survey Questions
#'
#' @param survey_info
#'
#' @return
#' @export
#'
#' @examples
customize_survey_questions <- function(
    survey_info
) {

  readr::read_csv(
    paste0("www/", survey_info$question_file)
    ) %>%
    rowwise() %>%
    dplyr::mutate(
      question = glue::glue_data(
        survey_info,
        question
      )
    ) %>%
    ungroup()
}
