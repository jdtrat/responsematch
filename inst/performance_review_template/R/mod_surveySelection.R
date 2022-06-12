surveySelectionInput <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      img(
        class = "logo",
        width = "20%",
        # Place an image the www/ directory, or use a url like
        # "https://www.jdtrat.com/images/jdtrat_logo.png"
        src = "shinysurveys-hex.png"
      ),
      fluidRow(
        style = "
        margin: auto;
        width: 50%;
        border: 3px solid black;
        padding: 0.5em;
        border-radius: 0.5em;
        ",
        radioGroupButtons(
          ns("surveySelection"),
          label = "Please select the individual to review.",
          choices = "",
          selected = character(0),
          status = "primary",
          size = "lg",
          direction = "vertical",
          individual = TRUE
        )
      )
    )
  )
}

surveySelectionServer <- function(
    id,
    curr_user,
    employee_database,
    dependence_data
    ) {

  filtered_database <- employee_database %>%
    dplyr::filter(
      reviewer == curr_user
    )

  dependency <- filtered_database %>%
    filter(
      !is.na(dependency)
    ) %>%
    mutate(
      dependency_subject_id = map(
        dependency,
        ~ str_split(.x, pattern = "\\|")[[1]]
      )
    ) %>%
    unnest(
      dependency_subject_id
    ) %>%
    mutate(
      dependency_subject_id = paste0(
        reviewee, "_", dependency_subject_id
      )
    ) %>%
    anti_join(
      dependence_data,
      by = "dependency_subject_id"
    ) %>%
    pull(
      reviewee
      )

  moduleServer(
    id,
    function(input, output, session) {

      updateRadioGroupButtons(
        session = session,
        inputId = "surveySelection",
        choices = filtered_database$reviewee,
        selected = character(0),
        status = "primary",
        size = "lg",
        disabledChoices = dependency
      )

      return(
        list(
          surveySelection = reactive(input$surveySelection)
        )
      )
    }
  )
}
