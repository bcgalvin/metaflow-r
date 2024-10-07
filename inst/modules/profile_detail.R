profile_detail_ui <- function(id) {
  ns <- shiny::NS(id)
  create_styled_page(
    shiny::fluidRow(
      shiny::column(
        width = 12L,
        shiny::div(
          style = "display: flex; align-items: center;",
          shiny::h3("Active Metaflow Profile", style = "margin: 0;"),
          shiny::actionButton(
            ns("open_file"),
            "Open File",
            class = "btn btn-primary margin-left-10"
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12L,
        shiny::uiOutput(ns("profile_table"))
      )
    )
  )
}

profile_detail_server <- function(id, profile, profile_path, profile_df) {
  checkmate::assert_list(profile)
  checkmate::assert_string(profile_path)
  checkmate::assert_data_frame(profile_df)

  shiny::moduleServer(id, function(input, output, session) {
    # Retrieve profile descriptions
    descriptions <- get_profile_descriptions(profile)

    shiny::observeEvent(input[["open_file"]], {
      tryCatch(
        {
          if (rstudioapi::isAvailable()) {
            rstudioapi::navigateToFile(profile_path)
          } else {
            stop("RStudio API not available")
          }
        },
        error = function(e) {
          shiny::showNotification(paste("Error:", e[["message"]]), type = "error")
        }
      )
    })

    output[["profile_table"]] <- shiny::renderUI({
      tryCatch(
        {
          generate_generic_table(
            profile_df,
            cell_renderer = function(value, name, row_data) {
              if (name == "key") {
                key_id <- paste0("key_", value)
                shiny::tags[["td"]](
                  shiny::actionLink(
                    inputId = session[["ns"]](key_id),
                    label = value,
                    title = "Click to view description"
                  )
                )
              } else {
                shiny::tags[["td"]](value)
              }
            }
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error generating table:", e[["message"]]),
            type = "error"
          )
          NULL
        }
      )
    })

    # Set up observers for each key
    purrr::walk(profile_df[["key"]], function(key) {
      key_id <- paste0("key_", key)
      shiny::observeEvent(input[[key_id]], {
        tryCatch(
          {
            # Retrieve the description for the selected key
            desc <- descriptions[["description"]][descriptions[["name"]] == key]
            if (length(desc) == 1L) {
              # Create the modal dialog UI
              modal_ui <- shiny::modalDialog(
                title = paste("Description for", key),
                shiny::p(desc),
                easyClose = TRUE,
                size = "m"
              )
              # Show the modal dialog
              shiny::showModal(modal_ui)
            } else {
              shiny::showNotification(
                paste("Description not found for key:", key),
                type = "warning"
              )
            }
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error displaying description:", e[["message"]]),
              type = "error"
            )
          }
        )
      })
    })
  })
}
