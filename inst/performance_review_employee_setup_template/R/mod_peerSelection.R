
peerSelectionInput <- function(
    id,
    num_managers = 1,
    num_peers = 3
) {

  ns <- NS(id)

  if (num_managers > 1) {
    manager_label <- "Please select your managers."
  } else {
    manager_label <- "Please select your manager."
  }

  if (num_peers > 1) {
    peer_label <- paste0("Please select up to ", n2w(num_peers), " peers.")
  } else {
    peer_label <- "Please select your manager."
  }

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
        uiOutput(ns("welcome")),
        selectizeInput(
          ns("managerSelection"),
          label = manager_label,
          choices = NULL,
          options = list(
            maxItems = num_managers
          )
        ),
        selectizeInput(
          ns("peerSelection"),
          label = peer_label,
          choices = NULL,
          options = list(
            maxItems = num_peers
          )
        ),
        actionButton(
          ns("submit"),
          label = "Submit Selection"
        )
      )
    )
  )
}

peerSelectionServer <- function(
    id,
    curr_user,
    employee_login_data
) {

  moduleServer(
    id,
    function(input, output, session) {

      output$welcome <- renderUI(p(curr_user))

      possiblePeople <- reactive({
        employee_login_data %>%
          dplyr::filter(
            full_name != curr_user
          ) %>%
          dplyr::pull(full_name)
      })

      observe({

        updateSelectizeInput(
          session = session,
          inputId = "managerSelection",
          choices = possiblePeople(),
          selected = character(0),
          options = list(
            placeholder = "Manager"
          ),
          server = TRUE
        )


        updateSelectizeInput(
          session = session,
          inputId = "peerSelection",
          choices = possiblePeople(),
          options = list(
            placeholder = "Peer"
          ),
          server = TRUE
        )

      })

      return(
        list(
          curr_user = reactive(curr_user),
          managerSelection = reactive(input$managerSelection),
          peerSelection = reactive(input$peerSelection),
          confirmedSelection = reactive(input$submit)
        )
      )

    }
  )
}

