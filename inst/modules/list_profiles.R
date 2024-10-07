list_profiles_ui <- function() {
  create_styled_page(
    additional_css = "
      .btn-sm {
        padding: 5px 10px;
        font-size: 12px;
        line-height: 1.5;
        border-radius: 3px;
      }
    ",
    shiny::fluidRow(
      shiny::column(
        width = 12L,
        shiny::h3("Available Metaflow Profiles")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12L,
        shiny::uiOutput("profiles_table")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12L,
        shiny::uiOutput("profile_detail_ui")
      )
    )
  )
}

list_profiles_server <- function(profiles_df) {
  checkmate::assert_data_frame(profiles_df)
  checkmate::assert_names(names(profiles_df), must.include = c("profile", "path"))
  function(input, output, session) {
    output[["profiles_table"]] <- shiny::renderUI({
      tryCatch(
        {
          create_action_table(
            profiles_df,
            action_column_fn = function(row_data) {
              shiny::actionButton(
                inputId = paste0("set_profile_", row_data[["profile"]]),
                label = "Set Active Profile",
                class = "btn btn-primary btn-sm"
              )
            },
            cell_renderer_fn = function(value, name, row_data) {
              if (name == "profile") {
                profile_id <- row_data[["profile"]]
                shiny::tags[["td"]](
                  shiny::actionLink(
                    inputId = paste0("profile_link_", profile_id),
                    label = value,
                    title = paste("Click to view profile details")
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

    setup_profile_observers(profiles_df, input, output, session)
  }
}
