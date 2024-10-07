#' Profile Detail Module UI
#' @keywords internal
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

#' Profile Detail Module Server
#' @keywords internal
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

#' Create UI for profiles app
#' @keywords internal
available_metaflow_profiles_ui <- function() {
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


#' Create server function for profiles app
#' @keywords internal
available_metaflow_profiles_server <- function(profiles_df) {
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

#' Setup Profile Observers
#' @keywords internal
setup_profile_observers <- function(profiles_df, input, output, session) {
  purrr::walk(seq_len(nrow(profiles_df)), function(i) {
    # Set Active Profile Observer
    input_id <- paste0("set_profile_", profiles_df[["profile"]][i])
    shiny::observeEvent(input[[input_id]], {
      handle_set_active_profile(profiles_df[["profile"]][i], profiles_df[["path"]][i])
    })

    # Profile Link Observer
    profile_id <- profiles_df[["profile"]][i]
    profile_path <- profiles_df[["path"]][i]
    profile_link_id <- paste0("profile_link_", profile_id)
    shiny::observeEvent(input[[profile_link_id]], {
      tryCatch(
        {
          # Read the profile data
          profile <- read_profile(profile_path)
          # Prepare the profile data frame
          profile_df <- data.frame(
            key = names(profile),
            value = unlist(profile),
            stringsAsFactors = FALSE,
            row.names = NULL
          )
          # Show the profile modal
          show_profile_modal(profile_id, profile, profile_path, profile_df)
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error displaying profile details:", e[["message"]]),
            type = "error"
          )
        }
      )
    })
  })
}

#' Handle Set Active Profile
#' @keywords internal
handle_set_active_profile <- function(profile_name, profile_path) {
  tryCatch(
    {
      set_active_profile(profile_path)
      shiny::showNotification(
        paste("Active profile set to", profile_name),
        type = "message"
      )
    },
    error = function(e) {
      shiny::showNotification(
        paste("Error setting profile:", e[["message"]]),
        type = "error"
      )
    }
  )
}

#' Show Profile Detail Modal
#' @keywords internal
show_profile_modal <- function(profile_id, profile, profile_path, profile_df) {
  # Create the modal dialog UI
  modal_ui <- shiny::modalDialog(
    profile_detail_ui(profile_id),
    easyClose = TRUE,
    size = "l"
  )
  # Show the modal dialog
  shiny::showModal(modal_ui)
  # Call the profile_detail server
  profile_detail_server(
    id = profile_id,
    profile = profile,
    profile_path = profile_path,
    profile_df = profile_df
  )
}

#' Create an Action Table with Custom Rendering
#' @keywords internal
create_action_table <- function(data, action_column_fn, cell_renderer_fn) {
  generate_generic_table(
    data,
    action_column = action_column_fn,
    cell_renderer = cell_renderer_fn
  )
}

#' Create a standard Shiny page with custom styles
#' @keywords internal
create_styled_page <- function(..., additional_css = "") {
  shiny::fluidPage(
    shiny::tags[["head"]](
      shiny::tags[["style"]](shiny::HTML(get_custom_styles(additional_css)))
    ),
    ...
  )
}


#' Get Descriptions for Active Metaflow Profile Keys
#' @keywords internal
get_profile_descriptions <- function(profile) {
  # Assert that profile is a named list
  checkmate::assert_list(profile, names = "named")

  # Read the YAML file
  yml_path <- system.file("metaflow_profile.yml", package = "mfshiny")
  if (!fs::file_exists(yml_path)) {
    cli::cli_abort("YAML file not found at {.path {yml_path}}")
  }
  yml_data <- yaml::read_yaml(yml_path)

  # Extract names from profile
  profile_keys <- names(profile)

  # Find entries in YAML data that match profile keys
  matching_entries <- lapply(yml_data, function(entry) {
    if (entry[["name"]] %in% profile_keys) {
      return(entry)
    } else {
      return(NULL)
    }
  })
  # Remove NULLs
  matching_entries <- Filter(Negate(is.null), matching_entries)

  # Extract 'name' and 'description' into a data frame
  df <- data.frame(
    name = vapply(matching_entries, function(entry) entry[["name"]], character(1L)),
    description = vapply(matching_entries, function(entry) trimws(entry[["description"]]), character(1L)),
    stringsAsFactors = FALSE
  )

  return(df)
}

#' Check if Interactive Viewer is Available
#' @keywords internal
is_interactive_viewer_available <- function() {
  requireNamespace("shiny", quietly = TRUE) &&
    rstudioapi::isAvailable() &&
    !is.null(rstudioapi::viewer)
}

#' Generate a generic table
#' @keywords internal
generate_generic_table <- function(data, action_column = NULL, cell_renderer = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_function(action_column, null.ok = TRUE)
  checkmate::assert_function(cell_renderer, null.ok = TRUE)

  header <- shiny::tags[["thead"]](
    shiny::tags[["tr"]](
      purrr::map(names(data), shiny::tags[["th"]])
    )
  )

  table_rows <- purrr::map(seq_len(nrow(data)), function(i) {
    row_data <- purrr::imap(data[i, ], function(value, name) {
      if (!is.null(cell_renderer)) {
        cell_renderer(value, name, data[i, ])
      } else {
        shiny::tags[["td"]](value)
      }
    })
    if (!is.null(action_column)) {
      row_data <- c(
        row_data,
        list(shiny::tags[["td"]](action_column(data[i, ])))
      )
    }
    shiny::tags[["tr"]](row_data)
  })

  shiny::tags[["table"]](
    class = "table table-striped table-hover table-bordered",
    header,
    shiny::tags[["tbody"]](table_rows)
  )
}

#' Display Metaflow Profile Information
#'
#' This function displays Metaflow profile information either via a Shiny app in the RStudio Viewer
#' or provides a warning if the interactive viewer is not available.
#' @keywords internal
metaflow_profile_viewer <- function(data_df, shiny_app_func, title, ...) {
  if (is_interactive_viewer_available()) {
    shiny::runApp(
      shiny_app_func(data_df, ...),
      launch.browser = rstudioapi::viewer,
      quiet = TRUE
    )
  } else {
    cli::cli_alert_warning(
      "Shiny or RStudio Viewer not available. Please use the equivalent get_ function for these data"
    )
  }
}

#' Create Shiny app for displaying the current active Metaflow profile
#'
#' @param profile The active profile
#' @param profile_path path to the profile file
#' @param profile_df A data frame containing profile information
#' @return A Shiny app object
#' @importFrom shiny shinyApp
#' @keywords internal
active_metaflow_profile_app <- function(profile_df, profile, profile_path) {
  checkmate::assert_list(profile)
  checkmate::assert_string(profile_path)
  checkmate::assert_data_frame(profile_df)
  ui <- profile_detail_ui("profile_detail")

  server <- function(input, output, session) {
    profile_detail_server("profile_detail", profile, profile_path, profile_df)
  }
  shiny::shinyApp(ui = ui, server = server)
}

#' Create Shiny app to display all Metaflow profiles
#'
#' This function creates a Shiny app that displays all available Metaflow
#' profiles and allows the user to set the active profile.
#'
#' @param profiles_df profiles df with 'Profile' and 'path'.
#' @return A Shiny app object.
#' @importFrom shiny shinyApp
#' @keywords internal
available_metaflow_profiles_app <- function(profiles_df) {
  checkmate::assert_data_frame(profiles_df)
  checkmate::assert_names(names(profiles_df), must.include = c("profile", "path"))
  ui <- available_metaflow_profiles_ui()
  server <- available_metaflow_profiles_server(profiles_df)
  shiny::shinyApp(ui = ui, server = server)
}



#' Get custom CSS styles for Shiny apps
#' @keywords internal
get_custom_styles <- function(additional_css = "") {
  checkmate::assert_string(additional_css)
  common_css <- "
body {
  font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  margin: 0;
  padding: 20px;
  background-color: #f0f0f0;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  background-color: white;
  border-radius: 8px;
  overflow: hidden;
  box-shadow: 0 0 20px rgba(0, 0, 0, 0.1);
}

.well {
  background-color: #f9f9f9;
  padding: 20px;
  border-radius: 8px;
}

h3 {
  color: #2c3e50;
  margin-top: 0;
  margin-bottom: 20px;
}

      .btn-sm {
        padding: 5px 10px;
        font-size: 12px;
        line-height: 1.5;
        border-radius: 3px;
      }

.btn-primary:hover {
  background-color: #2980b9;
  border-color: #2980b9;
}

.margin-left-10 {
  margin-left: 10px;
}

table {
  width: 100%;
  border-collapse: collapse;
  margin-top: 20px;
}

th, td {
  padding: 12px 15px;
  text-align: left;
  border-bottom: 1px solid #ddd;
}

th {
  background-color: #f8f8f8;
  font-weight: bold;
  text-transform: uppercase;
  color: #333;
}

tr:hover {
  background-color: #f5f5f5;
}

@media screen and (max-width: 600px) {
  body {
    padding: 10px;
  }

  table {
    font-size: 14px;
  }

  th, td {
    padding: 8px 10px;
  }

  h3 {
    font-size: 18px;
  }

  .btn-primary {
    font-size: 14px;
    padding: 8px 16px;
  }
}
  "
  paste(common_css, additional_css, sep = "\n")
}
